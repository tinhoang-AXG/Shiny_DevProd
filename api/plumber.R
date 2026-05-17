# api/plumber.R
#
# Plumber API — database credentials and behaviour come entirely from config.yml.
# Switch environments by setting R_CONFIG_ACTIVE in .Renviron (local) or
# Vars & Secrets (Posit Connect). No code changes needed between dev and prod.

library(plumber)
#library(config)
library(DBI)
library(RPostgres)
library(pool)
library(logger)   # install.packages("logger")

# ── 1. Load environment config ────────────────────────────────────────────────
cfg <- config::get()

# ── 2. Configure logging ──────────────────────────────────────────────────────
# cfg$log_level is "DEBUG" in dev, "WARN" in prod
log_threshold(get(cfg$log_level, envir = asNamespace("logger")))

active_env <- Sys.getenv("R_CONFIG_ACTIVE", unset = "default")
log_info("Plumber API starting | env: {active_env} | db: {cfg$db_name}")

# ── 3. Connection pool ────────────────────────────────────────────────────────
# Created ONCE at startup. Pool manages multiple concurrent connections safely.
# cfg automatically points at the right host/db/user/password per environment.

db_pool <- tryCatch({
  pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = cfg$db_host,
    port     = cfg$db_port,
    dbname   = cfg$db_name,       # "dev"  or  "prod"
    user     = cfg$db_user,
    password = cfg$db_password,
    minSize  = 1,
    maxSize  = 10,
    idleTimeout = 300             # close idle connections after 5 min
  )
}, error = function(e) {
  log_error("Failed to connect to database '{cfg$db_name}': {e$message}")
  stop(e)
})

# Cleanly drain the pool when the R process exits
# reg.finalizer works outside Shiny (onStop is Shiny-only)
reg.finalizer(globalenv(), function(e) {
  log_info("Shutting down — closing DB pool for '{cfg$db_name}'")
  pool::poolClose(db_pool)
}, onexit = TRUE)

# ── 4. Helpers ────────────────────────────────────────────────────────────────

# Centralised query runner — keeps endpoint code clean
run_query <- function(sql, params = list()) {
  log_debug("Query: {sql}")
  tryCatch(
    pool::dbGetQuery(db_pool, sql, params = params),
    error = function(e) {
      log_error("Query failed: {e$message}")
      stop(e)
    }
  )
}

# Standard error response
api_error <- function(res, status, message) {
  res$status <- status
  list(error = message, env = active_env)
}

# ── 5. Auth filter ────────────────────────────────────────────────────────────
# Every request passes through here before reaching an endpoint.

#* @filter check-auth
function(req, res) {
  # Skip auth on the health endpoint so load balancers can probe freely
  if (grepl("^/health", req$PATH_INFO)) {
    return(plumber::forward())
  }

  key <- req$HTTP_X_API_KEY

  if (is.null(key) || !nzchar(key)) {
    log_warn("Request with missing API key from {req$REMOTE_ADDR}")
    return(api_error(res, 401, "Missing X-API-KEY header"))
  }

  if (!identical(key, cfg$api_key)) {
    log_warn("Request with invalid API key from {req$REMOTE_ADDR}")
    return(api_error(res, 403, "Invalid API key"))
  }

  log_debug("Auth passed for {req$PATH_INFO}")
  plumber::forward()
}

# ── 6. Endpoints ──────────────────────────────────────────────────────────────

#* Health check — confirms API is up and which DB it's connected to
#* @get /health
function() {
  db_ok <- tryCatch({
    pool::dbGetQuery(db_pool, "SELECT 1")
    TRUE
  }, error = function(e) FALSE)

  list(
    status = if (db_ok) "ok" else "degraded",
    env    = active_env,
    db     = cfg$db_name,
    db_ok  = db_ok
  )
}

#* Fetch all measurements with optional limit
#* @param limit:int Maximum rows to return (default 100)
#* @get /data/measurements
function(res, limit = 100) {
  limit <- as.integer(limit)
  if (is.na(limit) || limit < 1L || limit > 10000L) {
    return(api_error(res, 400, "limit must be between 1 and 10000"))
  }

  log_info("Fetching {limit} measurements from '{cfg$db_name}'")

  rows <- tryCatch(
    run_query(
      "SELECT id, value, created_at FROM measurements ORDER BY created_at DESC LIMIT $1",
      params = list(limit)
    ),
    error = function(e) return(api_error(res, 500, "Database query failed"))
  )

  list(
    data  = rows,
    n     = nrow(rows),
    db    = cfg$db_name,    # confirm which DB responded — helpful during testing
    env   = active_env
  )
}

#* Summary statistics for measurements
#* @param limit:int Rows to include in calculation (default 1000)
#* @get /data/summary
function(res, limit = 1000) {
  limit <- as.integer(limit)
  log_info("Computing summary stats on '{cfg$db_name}'")

  rows <- tryCatch(
    run_query(
      "SELECT value FROM measurements ORDER BY created_at DESC LIMIT $1",
      params = list(limit)
    ),
    error = function(e) return(api_error(res, 500, "Database query failed"))
  )

  if (nrow(rows) == 0) {
    return(api_error(res, 404, "No data found in measurements table"))
  }

  list(
    mean   = round(mean(rows$value), 4),
    sd     = round(sd(rows$value), 4),
    min    = round(min(rows$value), 4),
    max    = round(max(rows$value), 4),
    median = round(median(rows$value), 4),
    n      = nrow(rows),
    db     = cfg$db_name,
    env    = active_env
  )
}

#* Insert a new measurement
#* @param value:numeric The measurement value
#* @post /data/measurements
function(res, value) {
  value <- suppressWarnings(as.numeric(value))
  
  if (is.na(value)) {
    return(api_error(res, 400, "value must be numeric"))
  }

  log_info("Inserting value={value} into '{cfg$db_name}'")

  tryCatch({
    pool::dbExecute(
      db_pool,
      "INSERT INTO measurements (value, created_at) VALUES ($1, NOW())",
      params = list(value)
    )
    res$status <- 201
    list(status = "created", value = value, db = cfg$db_name)
  },
  error = function(e) {
    log_error("Insert failed: {e$message}")
    api_error(res, 500, "Insert failed")
  })
}