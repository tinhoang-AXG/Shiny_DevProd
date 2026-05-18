# shiny/app.R
# global.R has already run — cfg, all api_*() helpers, and env_badge() available.

# shiny/app.R
source("global.R")
source("modules/projects_module.R")

enableBookmarking(store = "url")

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(

  titlePanel(
    div(
      style = "display:flex; align-items:center; gap:12px;",
      paste(cfg$app_name, "v1.0"),
      env_badge(),
      bookmarkButton(label = "Bookmark this view",
                     icon  = icon("bookmark"),
                     style = "margin-left:auto; font-size:12px;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      # ── Measurements section ──────────────────────────────────────────────────
      h4("Measurements"),
      sliderInput("limit", "Number of rows", min = 10, max = 1000, value = 100),
      actionButton("fetch", "Fetch Measurements", class = "btn-primary"),
      hr(),

      h4("Add Measurement"),
      numericInput("new_value", "Value", value = NULL),
      actionButton("submit", "Submit", class = "btn-success"),
      hr(),

      # ── API status ────────────────────────────────────────────────────────────
      h4("API Status"),
      actionButton("check_health", "Check Health"),
      verbatimTextOutput("health_out")
    ),

    mainPanel(
      tabsetPanel(
        id = "main_tabs",
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
        tabPanel("Projects",
          br(),
          projectsUI("projects")    # ← module UI lives in the Projects tab
        ),

        # Debug tab — dev only
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

  # Exclude inputs that shouldn't be in the bookmark URL
  setBookmarkExclude(c(
    "submit",
    "fetch",
    "check_health",
    "new_value",
    "projects-open_modal",
    "projects-submit_project",
    "projects-cancel_project",
    "projects-edit_project_id",
    "projects-edit_1",      # ← add these
    "projects-edit_2",
    "projects-edit_3",
    "projects-edit_4",
    "projects-edit_5",
    "projects-edit_6",
    "projects-edit_7",
    "projects-edit_8",
    "projects-edit_9",
    "projects-edit_10"
  ))

  # # Restore tab from bookmark on load
  # observe({
  #   session$doBookmark
  # })
  # Restore tab from bookmark on load
  onRestored(function(state) {
    updateTabsetPanel(session, "main_tabs", selected = state$input$main_tabs)
  })

  # ── Wire in the projects module ───────────────────────────────────────────────
  projectsServer("projects")    # ← must match the id in projectsUI()

  # ── Health check ─────────────────────────────────────────────────────────────
  output$health_out <- renderPrint({
    input$check_health
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
      shinyjs::click("fetch")
    }, error = function(e) {
      showNotification(e$message, type = "error", duration = 8)
    })
  })

  # ── Plot ──────────────────────────────────────────────────────────────────────
  output$plot <- renderPlot({
    req(measurements())
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
    do.call(rbind, lapply(measurements()$data, function(x) {
      data.frame(id = x$id, value = x$value, created_at = x$created_at)
    }))
  })

  # ── Debug tab ─────────────────────────────────────────────────────────────────
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