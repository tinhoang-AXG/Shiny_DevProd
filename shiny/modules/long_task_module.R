
# shiny/modules/long_task_module.R
#
# Demonstrates ExtendedTask for long-running operations.
# The task runs in the background — UI stays fully interactive during processing.
#
# Usage in app.R:
#   source("shiny/modules/long_task_module.R")
#   longTaskUI("long_task")
#   longTaskServer("long_task")

library(shiny)
library(promises)
library(mirai)

# ── UI ─────────────────────────────────────────────────────────────────────────
longTaskUI <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Long Running Task Demo"),
    p(class = "text-muted",
      "Simulates a slow 7-second task. Notice the rest of the app stays",
      "fully interactive while this runs in the background."
    ),
    hr(),

    # ── Controls ──────────────────────────────────────────────────────────────
    fluidRow(
      column(4,
        numericInput(
          ns("n_records"),
          "Records to process",
          value = 1000,
          min   = 100,
          max   = 100000,
          step  = 100
        )
      ),
      column(4,
        selectInput(
          ns("task_type"),
          "Task type",
          choices = c(
            "Aggregation"  = "aggregation",
            "Simulation"   = "simulation",
            "Reporting"    = "reporting"
          )
        )
      ),
      column(4,
        br(),
        actionButton(
          ns("start_task"),
          "Start Task",
          class = "btn-primary btn-lg",
          icon  = icon("play")
        ),
        actionButton(
          ns("cancel_task"),
          "Cancel",
          class = "btn-danger btn-lg",
          style = "display:none;",   # hidden until task starts
          icon  = icon("stop")
        )
      )
    ),

    hr(),

    # ── Progress section ──────────────────────────────────────────────────────
    uiOutput(ns("progress_section")),

    hr(),

    # ── Results section ───────────────────────────────────────────────────────
    uiOutput(ns("results_section"))
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
longTaskServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Reactive state ──────────────────────────────────────────────────────────
    task_status  <- reactiveVal("idle")    # idle | running | done | cancelled | error
    task_result  <- reactiveVal(NULL)
    progress_pct <- reactiveVal(0)
    status_log   <- reactiveVal(character(0))

    # ── Helper: append to status log ───────────────────────────────────────────
    add_log <- function(msg) {
      current <- status_log()
      timestamp <- format(Sys.time(), "%H:%M:%S")
      status_log(c(current, sprintf("[%s] %s", timestamp, msg)))
    }

    # ── Define the ExtendedTask ─────────────────────────────────────────────────
    # The function inside ExtendedTask runs in a separate mirai worker process.
    # It cannot access reactive values or session — only plain R objects
    # passed as arguments.
    long_task <- ExtendedTask$new(function(n_records, task_type) {
      promises::promise(function(resolve, reject) {
        mirai::mirai({
          # Simulate a multi-step slow task
          steps <- list(
            list(name = "Initializing",   duration = 1, pct_end = 20),
            list(name = "Fetching data",  duration = 2, pct_end = 50),
            list(name = "Processing",     duration = 3, pct_end = 80),
            list(name = "Finalizing",     duration = 1, pct_end = 100)
          )

          results <- list()
          for (step in steps) {
            Sys.sleep(step$duration)
            results[[step$name]] <- list(
              step     = step$name,
              pct      = step$pct_end,
              duration = step$duration
            )
          }

          # Final result
          list(
            status     = "done",
            n_records  = n_records,
            task_type  = task_type,
            steps      = results,
            summary    = data.frame(
              Step     = names(results),
              Duration = sapply(results, function(x) paste0(x$duration, "s")),
              Progress = sapply(results, function(x) paste0(x$pct, "%"))
            ),
            completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )
        }, .args = list(n_records = n_records, task_type = task_type)) |>
          promises::then(resolve, reject)
      })
    })

    # ── Start task ──────────────────────────────────────────────────────────────
    observeEvent(input$start_task, {
      task_status("running")
      task_result(NULL)
      progress_pct(0)
      status_log(character(0))

      add_log(sprintf("Starting %s task with %s records",
                      input$task_type, format(input$n_records, big.mark = ",")))

      # Show cancel button, hide start button
      shinyjs::hide("start_task")
      shinyjs::show("cancel_task")

      # Launch the task
      long_task$invoke(input$n_records, input$task_type)

      # Poll progress while task runs
      progress_timer <- observe({
        invalidateLater(500, session)  # check every 500ms

        status <- long_task$status()

        if (status == "running") {
          # Simulate incremental progress updates
          current <- progress_pct()
          if (current < 20) {
            progress_pct(min(current + 5, 19))
            if (current == 0) add_log("Step 1/4 — Initializing...")
          } else if (current < 50) {
            progress_pct(min(current + 4, 49))
            if (current == 20) add_log("Step 2/4 — Fetching data...")
          } else if (current < 80) {
            progress_pct(min(current + 3, 79))
            if (current == 50) add_log("Step 3/4 — Processing...")
          } else if (current < 99) {
            progress_pct(min(current + 2, 99))
            if (current == 80) add_log("Step 4/4 — Finalizing...")
          }

        } else if (status == "success") {
          progress_pct(100)
          add_log("Task completed successfully!")
          task_status("done")
          task_result(long_task$result())

          shinyjs::show("start_task")
          shinyjs::hide("cancel_task")
          progress_timer$destroy()

        } else if (status == "error") {
          add_log(paste("Error:", conditionMessage(long_task$result())))
          task_status("error")

          shinyjs::show("start_task")
          shinyjs::hide("cancel_task")
          progress_timer$destroy()
        }
      })
    })

    # ── Cancel task ─────────────────────────────────────────────────────────────
    observeEvent(input$cancel_task, {
      task_status("cancelled")
      progress_pct(0)
      add_log("Task cancelled by user")

      shinyjs::show("start_task")
      shinyjs::hide("cancel_task")
    })

    # ── Progress section UI ─────────────────────────────────────────────────────
    output$progress_section <- renderUI({
      status <- task_status()
      pct    <- progress_pct()
      logs   <- status_log()

      if (status == "idle") {
        return(p(class = "text-muted", icon("info-circle"),
                 " Click 'Start Task' to begin."))
      }

      # Progress bar color per status
      bar_class <- switch(status,
        "running"   = "progress-bar progress-bar-striped active bg-primary",
        "done"      = "progress-bar bg-success",
        "cancelled" = "progress-bar bg-warning",
        "error"     = "progress-bar bg-danger",
        "progress-bar"
      )

      tagList(
        h5("Progress"),
        tags$div(class = "progress",
          tags$div(
            class = bar_class,
            role  = "progressbar",
            style = sprintf("width: %s%%", pct),
            `aria-valuenow` = pct,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100",
            sprintf("%s%%", pct)
          )
        ),
        h5("Status Log"),
        tags$div(
          style = "background:#f8f9fa; padding:10px; border-radius:4px;
                   font-family:monospace; font-size:12px; max-height:150px;
                   overflow-y:auto;",
          lapply(rev(logs), function(log) tags$div(log))
        )
      )
    })

    # ── Results section UI ──────────────────────────────────────────────────────
    output$results_section <- renderUI({
      status <- task_status()
      result <- task_result()

      if (status != "done" || is.null(result)) return(NULL)

      tagList(
        h5("Results"),
        fluidRow(
          column(3,
            tags$div(class = "panel panel-success",
              tags$div(class = "panel-heading", "Status"),
              tags$div(class = "panel-body",
                tags$span(class = "text-success", icon("check"), " Completed")
              )
            )
          ),
          column(3,
            tags$div(class = "panel panel-info",
              tags$div(class = "panel-heading", "Records Processed"),
              tags$div(class = "panel-body",
                format(result$n_records, big.mark = ",")
              )
            )
          ),
          column(3,
            tags$div(class = "panel panel-info",
              tags$div(class = "panel-heading", "Task Type"),
              tags$div(class = "panel-body", result$task_type)
            )
          ),
          column(3,
            tags$div(class = "panel panel-info",
              tags$div(class = "panel-heading", "Completed At"),
              tags$div(class = "panel-body", result$completed_at)
            )
          )
        ),
        h5("Step Breakdown"),
        renderTable(result$summary)
      )
    })
  })
}