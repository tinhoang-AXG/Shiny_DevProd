# api/run_api.R
#
# Local development launcher — NOT deployed to Posit Connect.
# Run this from your R console or terminal to start the API locally:
#
#   Rscript api/run_api.R
#
# To test prod config locally, set R_CONFIG_ACTIVE=production in .Renviron
# and restart R before running this script.

library(plumber)

port <- as.integer(Sys.getenv("PLUMBER_PORT", unset = "8000"))

cat(sprintf("Starting Plumber API on http://localhost:%d\n", port))
cat(sprintf("Swagger docs at http://localhost:%d/__docs__/\n", port))
cat(sprintf("Environment: %s\n", Sys.getenv("R_CONFIG_ACTIVE", "default")))

plumber::pr("api/plumber.R") |>
  plumber::pr_run(host = "127.0.0.1", port = port)