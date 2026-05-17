# shiny/app.R
# global.R has already run — cfg, all api_*() helpers, and env_badge() available.

source("global.R")

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel(
    div(
      style = "display:flex; align-items:center; gap:12px;",
      paste(cfg$app_name, "v1.0"),
      env_badge()          # green DEV / red PROD badge in the title bar
    )
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Fetch Data"),
      sliderInput("limit", "Number of rows", min = 10, max = 1000, value = 100),
      actionButton("fetch", "Fetch Measurements", class = "btn-primary"),
      hr(),

      h4("Add Measurement"),
      numericInput("new_value", "Value", value = NULL),
      actionButton("submit", "Submit", class = "btn-success"),
      hr(),

      # API status indicator
      h4("API Status"),
      actionButton("check_health", "Check Health"),
      verbatimTextOutput("health_out")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
          br(),
          plotOutput("plot")
        ),
        tabPanel("Summary Stats",
          br(),
          tableOutput("summary_table")
        ),
        tabPanel("Raw Data",
          br(),
          tableOutput("data_table")
        ),

        # Debug tab — only present in dev
        if (cfg$debug) {
          tabPanel("Debug",
            br(),
            h5("Active config values"),
            tableOutput("config_table")
          )
        }
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Health check ─────────────────────────────────────────────────────────────
  output$health_out <- renderPrint({
    input$check_health   # re-run on button press
    isolate({
      result <- tryCatch(api_health(), error = function(e) list(error = e$message))
      cat(sprintf("Status : %s\n", result$status %||% "error"))
      cat(sprintf("Env    : %s\n", result$env    %||% "—"))
      cat(sprintf("DB     : %s\n", result$db     %||% "—"))
      cat(sprintf("DB ok  : %s\n", result$db_ok  %||% "—"))
    })
  })

  # ── Fetch measurements ────────────────────────────────────────────────────────
  measurements <- eventReactive(input$fetch, {
    tryCatch(
      api_get_measurements(limit = input$limit),
      error = function(e) {
        showNotification(e$message, type = "error", duration = 8)
        NULL
      }
    )
  }, ignoreNULL = FALSE)

  summary_data <- eventReactive(input$fetch, {
    tryCatch(
      api_get_summary(limit = input$limit),
      error = function(e) NULL
    )
  }, ignoreNULL = FALSE)

  # ── Submit new measurement ────────────────────────────────────────────────────
  observeEvent(input$submit, {
    req(input$new_value)
    tryCatch({
      result <- api_post_measurement(input$new_value)
      showNotification(
        sprintf("Inserted %.4f into %s", result$value, result$db),
        type = "message"
      )
      # Auto-refresh data after insert
      shinyjs::click("fetch")
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
    })
  })

  # ── Plot ──────────────────────────────────────────────────────────────────────
  output$plot <- renderPlot({
    req(measurements())
    #values <- unlist(measurements()$data$value)
    values <- sapply(measurements()$data, function(x) x$value)
    req(length(values) > 0)

    hist(values,
         main  = sprintf("Measurements — %s (%s)", cfg$db_name, active_env),
         xlab  = "Value",
         col   = if (active_env == "production") "#c0392b" else "#27ae60",
         border = "white")
  })

  # ── Summary table ─────────────────────────────────────────────────────────────
  output$summary_table <- renderTable({
    req(summary_data())
    s <- summary_data()
    data.frame(
      Metric = c("Mean", "Std Dev", "Min", "Max", "Median", "N", "Database", "Env"),
      Value  = c(s$mean, s$sd, s$min, s$max, s$median, s$n, s$db, s$env)
    )
  })

  # ── Raw data table ────────────────────────────────────────────────────────────
  output$data_table <- renderTable({
      req(measurements())
  
      # convert list of lists to data frame
      do.call(rbind, lapply(measurements()$data, function(x) {
        data.frame(id = x$id, value = x$value, created_at = x$created_at)
      }))
  })

  # ── Debug tab (dev only) ──────────────────────────────────────────────────────
  if (cfg$debug) {
    output$config_table <- renderTable({
      data.frame(
        Setting = c("R_CONFIG_ACTIVE", "app_name", "db_host", "db_name",
                    "api_url", "debug", "log_level"),
        Value   = c(active_env, cfg$app_name, cfg$db_host, cfg$db_name,
                    cfg$api_url, as.character(cfg$debug), cfg$log_level)
      )
    })
  }
}

shinyApp(ui, server)