# db_setup.R
# Run this ONCE per database to create the table and seed data.
# Toggle R_CONFIG_ACTIVE in .Renviron to target dev or prod, then
# restart R before running.

library(DBI)
library(RPostgres)

# ── Load config (use :: directly — never library(config)) ────────────────────
cfg        <- config::get()
active_env <- Sys.getenv("R_CONFIG_ACTIVE", unset = "default")

cat(sprintf("Connecting to: %s (env: %s)\n", cfg$db_name, active_env))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = cfg$db_host,
  port     = cfg$db_port,
  dbname   = cfg$db_name,
  user     = cfg$db_user,
  password = cfg$db_password
)

# ── Create table ──────────────────────────────────────────────────────────────
DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS measurements (
    id         SERIAL PRIMARY KEY,
    value      NUMERIC NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
  )
")
cat("Table created (or already exists)\n")

# ── Seed data — different means per env so you can tell them apart ────────────
set.seed(if (active_env == "production") 42 else 99)

seed_data <- data.frame(
  value      = round(rnorm(200,
                           mean = if (active_env == "production") 50 else 20,
                           sd   = 5), 4),
  created_at = Sys.time() - sample(1:10000, 200, replace = TRUE)
)

DBI::dbWriteTable(con, "measurements", seed_data, append = TRUE, row.names = FALSE)
cat(sprintf("Seeded 200 rows into '%s'\n", cfg$db_name))

# ── Verify ────────────────────────────────────────────────────────────────────
count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM measurements")
cat(sprintf("Total rows now in '%s': %s\n", cfg$db_name, count$n))  # %s not %d

DBI::dbDisconnect(con)
cat("Done — connection closed\n")