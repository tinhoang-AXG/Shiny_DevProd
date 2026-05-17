# shiny/global.R
#
# Runs ONCE when Shiny starts. All objects here are available to app.R.
# Never touches the database directly — all data comes through the Plumber API.

library(shiny)
library(httr)
library(jsonlite)
library(logger)

# ── 1. Load config (use :: directly — never library(config)) ──────────────────
cfg <- config::get(file = "/Users/tinhoang/Desktop/Shiny_DevProd/config.yml")

active_env <- Sys.getenv("R_CONFIG_ACTIVE", unset = "default")
log_threshold(as.character(cfg$log_level))
log_info("Shiny starting | env: {active_env} | api: {cfg$api_url}")

# ── 2. Core API caller ─────────────────────────────────────────────────────────
# All requests go through here. Picks up cfg$api_url and cfg$api_key
# automatically — no endpoint code needs to know which environment it's in.

call_api <- function(method = "GET", endpoint, params = list(), body = NULL) {
  url <- paste0(cfg$api_url, endpoint)
  headers <- httr::add_headers("X-API-KEY" = cfg$api_key)

  log_debug("{method} {url}")

  response <- tryCatch({
    if (method == "GET") {
      httr::GET(url, query = params, headers, httr::timeout(30))
    } else if (method == "POST") {
      httr::POST(url, query = params, headers, body = body,
                 encode = "json", httr::timeout(30))
    }
  }, error = function(e) {
    log_error("API call failed: {e$message}")
    stop(sprintf("Could not reach API at %s — is it running?", cfg$api_url))
  })

  status <- httr::status_code(response)

  if (status == 401) stop("API key missing — check API_KEY in .Renviron")
  if (status == 403) stop("API key invalid — check API_KEY in .Renviron")
  if (httr::http_error(response)) {
    msg <- httr::content(response, "parsed")$error %||% "Unknown API error"
    stop(sprintf("API error %s: %s", status, msg))
  }

  httr::content(response, "parsed")
}

# ── 3. Typed API helpers ───────────────────────────────────────────────────────
# One function per endpoint — keeps server.R clean and easy to test.

api_health <- function() {
  call_api("GET", "/health")
}

api_get_measurements <- function(limit = 100) {
  call_api("GET", "/data/measurements", params = list(limit = limit))
}

api_get_summary <- function(limit = 1000) {
  call_api("GET", "/data/summary", params = list(limit = limit))
}

api_post_measurement <- function(value) {
  call_api("POST", "/data/measurements", params = list(value = value))
}

# ── 4. UI helpers ──────────────────────────────────────────────────────────────
# Environment badge shown in the header — makes it immediately obvious
# which environment you're working in.

env_badge <- function() {
  if (active_env == "production") {
    tags$span(
      style = "background:#c0392b; color:white; padding:3px 10px;
               border-radius:4px; font-size:12px; font-weight:bold;",
      "PROD"
    )
  } else {
    tags$span(
      style = "background:#27ae60; color:white; padding:3px 10px;
               border-radius:4px; font-size:12px; font-weight:bold;",
      "DEV"
    )
  }
}

# Null coalescing operator (R doesn't have one built in)
`%||%` <- function(x, y) if (!is.null(x)) x else y