
# shiny/modules/projects_module.R
#
# Shiny module for the projects intake form.
# Handles fetching, inserting, and editing projects via the Plumber API.
#
# Usage in app.R:
#   source("shiny/modules/projects_module.R")
#   projectsUI("projects")
#   projectsServer("projects")

library(shiny)

# ── UI ─────────────────────────────────────────────────────────────────────────
projectsUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("new_project_btn")),  # ← rendered server-side based on role
    br(), br(),
    uiOutput(ns("projects_table")),
    uiOutput(ns("modal_ui"))
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
projectsServer <- function(id, user_role = reactive("user")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Role-based UI ─────────────────────────────────────────────────────────────
    output$new_project_btn <- renderUI({
      if (can_edit(user_role())) {
        actionButton(
          ns("open_modal"),
          "New Project",
          class = "btn-primary",
          icon  = icon("plus")
        )
      } else {
        # Regular users see a read-only message instead
        tags$p(
          class = "text-muted",
          icon("lock"), " View only — contact an admin to add projects"
        )
      }
    })

    # ── Reactive state ──────────────────────────────────────────────────────────
    # NULL = insert mode, integer = edit mode (holds project id)
    editing_id <- reactiveVal(NULL)

    # Trigger to refresh the projects table
    refresh_trigger <- reactiveVal(0)

    # ── Fetch all projects ──────────────────────────────────────────────────────
    projects <- reactive({
      refresh_trigger()  # re-run when trigger changes
      tryCatch(
        api_get_projects(),
        error = function(e) {
          showNotification(e$message, type = "error", duration = 8)
          NULL
        }
      )
    })

    # ── Projects table ──────────────────────────────────────────────────────────
    output$projects_table <- renderUI({
      req(projects())
      rows <- projects()$data

      if (length(rows) == 0) {
        return(p("No projects found"))
      }

      # Build table rows with an Edit button in each row
      table_rows <- lapply(rows, function(x) {
        tags$tr(
          tags$td(x$id),
          tags$td(x$project_name),
          tags$td(x$client_name),
          tags$td(x$category %||% ""),
          tags$td(x$start_date %||% ""),
          tags$td(formatC(as.numeric(x$budget %||% 0),
                          format = "f", digits = 2, big.mark = ",")),
          tags$td(x$status %||% ""),
          tags$td(
              if (can_edit(user_role())) {
                actionButton(
                  inputId = ns(paste0("edit_", x$id)),
                  label   = "Edit",
                  class   = "btn-xs btn-warning",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', %s, {priority: 'event'})",
                    ns("edit_project_id"), x$id
                  )
                )
              }
          )
        )
      })

      tags$table(
        class = "table table-striped table-hover",
        tags$thead(
          tags$tr(
            tags$th("ID"), tags$th("Project"), tags$th("Client"),
            tags$th("Category"), tags$th("Start Date"), tags$th("Budget"),
            tags$th("Status"), tags$th("")
          )
        ),
        tags$tbody(table_rows)
      )
    })

    # ── Open modal ──────────────────────────────────────────────────────────────
    observeEvent(input$open_modal, {
      editing_id(NULL)   # reset to insert mode
      show_modal(session, ns, mode = "new", data = NULL)
    })

    # ── Edit button ─────────────────────────────────────────────────────────────
    observeEvent(input$edit_project_id, {
      req(input$edit_project_id)
      id <- as.integer(input$edit_project_id)
      editing_id(id)

      # Fetch the project to pre-fill the form
      project <- tryCatch(
        api_get_project(id),
        error = function(e) {
          showNotification(e$message, type = "error")
          NULL
        }
      )
      req(project)

      # data is a list of 1 — extract the actual record with [[1]]
      data <- project$data[[1]]

      show_modal(session, ns, mode = "edit", data = data)
    })

    # ── Submit form ─────────────────────────────────────────────────────────────
    observeEvent(input$submit_project, {
      # Validate required fields
      if (!nzchar(trimws(input$project_name %||% ""))) {
        showNotification("Project name is required", type = "error")
        return()
      }
      if (!nzchar(trimws(input$client_name %||% ""))) {
        showNotification("Client name is required", type = "error")
        return()
      }

      # Build payload
      payload <- list(
        project_name = trimws(input$project_name),
        client_name  = trimws(input$client_name),
        category     = trimws(input$category),
        start_date   = as.character(input$start_date),
        budget       = as.numeric(input$budget),
        status       = input$status,
        notes        = trimws(input$notes)
      )

      # Insert or update depending on mode
      result <- if (is.null(editing_id())) {
        tryCatch(
          api_post_project(payload),
          error = function(e) {
            showNotification(paste("Insert failed:", e$message), type = "error")
            NULL
          }
        )
      } else {
        tryCatch(
          api_put_project(editing_id(), payload),
          error = function(e) {
            showNotification(paste("Update failed:", e$message), type = "error")
            NULL
          }
        )
      }

      req(result)

      action <- if (is.null(editing_id())) "created" else "updated"
      showNotification(
        sprintf("Project '%s' %s in %s", payload$project_name, action, result$db),
        type = "message"
      )

      removeModal()
      editing_id(NULL)
      refresh_trigger(refresh_trigger() + 1)  # trigger table refresh
    })

    # ── Cancel ──────────────────────────────────────────────────────────────────
    observeEvent(input$cancel_project, {
      removeModal()
      editing_id(NULL)
    })
  })
}

# ── Modal builder ──────────────────────────────────────────────────────────────
# Separated from server so it stays clean and reusable for both insert and edit.

show_modal <- function(session, ns, mode = "new", data = NULL) {
  title     <- if (mode == "new") "New Project" else "Edit Project"
  btn_label <- if (mode == "new") "Save Project" else "Update Project"

  val <- function(field, default = "") {
    v <- data[[field]]
    if (is.null(v) || length(v) == 0) return(default)
    if (length(v) == 1 && is.na(v)) return(default)
    as.character(v)
  }
  

  showModal(modalDialog(
    title = title,
    size  = "l",
    easyClose = FALSE,

    # ── Form fields ─────────────────────────────────────────────────────────────
    fluidRow(
      column(6,
        textInput(ns("project_name"), "Project Name *",
                  value = val("project_name"),
                  placeholder = "Enter project name")
      ),
      column(6,
        textInput(ns("client_name"), "Client Name *",
                  value = val("client_name"),
                  placeholder = "Enter client name")
      )
    ),

    fluidRow(
      column(6,
        selectInput(ns("category"), "Category",
                    choices  = c("Marketing", "Technology", "Operations",
                                 "Finance", "HR", "Other"),
                    selected = val("category", "Marketing"))
      ),
      column(6,
        selectInput(ns("status"), "Status",
                    choices  = c("Active", "On Hold", "Completed", "Cancelled"),
                    selected = val("status", "Active"))
      )
    ),

    fluidRow(
      column(6,
        dateInput(ns("start_date"), "Start Date",
                  value = val("start_date", Sys.Date()))
      ),
      column(6,
        numericInput(ns("budget"), "Budget ($)",
                     value = val("budget", NA),
                     min   = 0,
                     step  = 1000)
      )
    ),

    textAreaInput(ns("notes"), "Notes",
                  value = val("notes"),
                  rows  = 3,
                  placeholder = "Any additional notes..."),

    footer = tagList(
      actionButton(ns("submit_project"), btn_label, class = "btn-primary"),
      actionButton(ns("cancel_project"), "Cancel",  class = "btn-default")
    )
  ))
}