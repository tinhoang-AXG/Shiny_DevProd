
# auth/credentials.R
#
# Run this ONCE per environment to create the SQLite credentials database.
# Toggle R_CONFIG_ACTIVE in .Renviron to target dev or prod, then
# restart R before running.
#
# Usage:
#   Rscript auth/credentials.R

library(shinymanager)
library(DBI)
library(RSQLite)

active_env <- Sys.getenv("R_CONFIG_ACTIVE", unset = "default")

# ── Credentials DB path ───────────────────────────────────────────────────────
# Separate SQLite file per environment so dev and prod users are isolated.
db_path <- if (active_env == "production") {
  "auth/credentials_prod.sqlite"
} else {
  "auth/credentials_dev.sqlite"
}

cat(sprintf("Setting up credentials DB for env: %s\n", active_env))
cat(sprintf("DB path: %s\n", db_path))

# ── Seed users ────────────────────────────────────────────────────────────────
# Passwords are hashed by shinymanager — never stored in plain text.
# Change these before deploying to production!

if (active_env == "production") {
  users <- data.frame(
    user       = c("admin", "manager1", "user1"),
    password   = c(
      Sys.getenv("ADMIN_PASSWORD",    unset = "changeme_admin"),
      Sys.getenv("MANAGER_PASSWORD",  unset = "changeme_manager"),
      Sys.getenv("USER_PASSWORD",     unset = "changeme_user")
    ),
    role       = c("admin", "manager", "user"),
    name       = c("Administrator", "Manager One", "Regular User"),
    is_hashed_password = FALSE,
    stringsAsFactors = FALSE
  )
} else {
  users <- data.frame(
    user       = c("admin", "manager1", "user1"),
    password   = c("admin123", "manager123", "user123"),
    role       = c("admin", "manager", "user"),
    name       = c("Dev Admin", "Dev Manager", "Dev User"),
    is_hashed_password = FALSE,
    stringsAsFactors = FALSE
  )
}

# ── Create the credentials DB ─────────────────────────────────────────────────
shinymanager::create_db(
  credentials_data = users,
  sqlite_path      = db_path,
  passphrase       = Sys.getenv("DB_PASSPHRASE", unset = "local-dev-passphrase")
)

cat(sprintf("\nCredentials DB created at: %s\n", db_path))
cat("Users seeded:\n")
print(users[, c("user", "role", "name")])
cat("\nDone!\n")