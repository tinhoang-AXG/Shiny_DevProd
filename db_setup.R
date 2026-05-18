# db_setup.R
# Run this ONCE per database to create tables and seed data.
# Toggle R_CONFIG_ACTIVE in .Renviron to target dev or prod, then
# restart R before running.

library(DBI)
library(RPostgres)

# ── Load config (use :: directly — never library(config)) ────────────────────
cfg        <- config::get(file = "/Users/tinhoang/Desktop/Shiny_DevProd/config.yml")
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

# ── 1. Measurements table ─────────────────────────────────────────────────────
DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS measurements (
    id         SERIAL PRIMARY KEY,
    value      NUMERIC NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
  )
")
cat("measurements table created (or already exists)\n")

# Seed only if empty
count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM measurements")
if (count$n == 0) {
  set.seed(if (active_env == "production") 42 else 99)
  seed_data <- data.frame(
    value      = round(rnorm(200,
                             mean = if (active_env == "production") 50 else 20,
                             sd   = 5), 4),
    created_at = Sys.time() - sample(1:10000, 200, replace = TRUE)
  )
  DBI::dbWriteTable(con, "measurements", seed_data, append = TRUE, row.names = FALSE)
  cat(sprintf("Seeded 200 rows into measurements in '%s'\n", cfg$db_name))
} else {
  cat(sprintf("measurements already has %s rows — skipping seed\n", count$n))
}

# ── 2. Projects table ─────────────────────────────────────────────────────────
DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS projects (
    id           SERIAL PRIMARY KEY,
    project_name TEXT NOT NULL,
    client_name  TEXT NOT NULL,
    category     TEXT,
    start_date   DATE,
    budget       NUMERIC,
    status       TEXT DEFAULT 'Active',
    notes        TEXT,
    created_at   TIMESTAMPTZ DEFAULT NOW(),
    updated_at   TIMESTAMPTZ DEFAULT NOW()
  )
")
cat("projects table created (or already exists)\n")

# Seed only if empty — different data per env so you can tell them apart
count_proj <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM projects")
if (count_proj$n == 0) {

  if (active_env == "production") {
    projects_seed <- data.frame(
      project_name = c("Alpha Launch", "Beta Rollout", "Gamma Initiative"),
      client_name  = c("Acme Corp", "Globex Inc", "Initech LLC"),
      category     = c("Marketing", "Technology", "Operations"),
      start_date   = as.Date(c("2026-01-15", "2026-02-01", "2026-03-10")),
      budget       = c(150000, 200000, 95000),
      status       = c("Active", "Active", "On Hold"),
      notes        = c("Q1 priority", "Flagship project", "Pending approval")
    )
  } else {
    projects_seed <- data.frame(
      project_name = c("Dev Project A", "Dev Project B", "Dev Project C"),
      client_name  = c("Test Client 1", "Test Client 2", "Test Client 3"),
      category     = c("Marketing", "Technology", "Operations"),
      start_date   = as.Date(c("2026-01-01", "2026-02-01", "2026-03-01")),
      budget       = c(10000, 20000, 30000),
      status       = c("Active", "Completed", "On Hold"),
      notes        = c("Dev seed record", "Dev seed record", "Dev seed record")
    )
  }

  DBI::dbWriteTable(con, "projects", projects_seed, append = TRUE, row.names = FALSE)
  cat(sprintf("Seeded %s projects into '%s'\n", nrow(projects_seed), cfg$db_name))
} else {
  cat(sprintf("projects already has %s rows — skipping seed\n", count_proj$n))
}

# ── Verify ────────────────────────────────────────────────────────────────────
m <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM measurements")
p <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM projects")
cat(sprintf("\nSummary for '%s':\n", cfg$db_name))
cat(sprintf("  measurements : %s rows\n", m$n))
cat(sprintf("  projects     : %s rows\n", p$n))

DBI::dbDisconnect(con)
cat("\nDone — connection closed\n")