# UI
calendarUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "calendar",
    fluidPage(
      titlePanel("calendar"),
      sidebarLayout(
        sidebarPanel(
          textInput(
            ns("service_id"),
            label = tagList(
              "Service ID",
              info_popover(
                "Identifies a set of dates when service is available for one or more routes.",
                "https://gtfs.org/schedule/reference/#calendartxt"
              )
            ),
            placeholder = "Enter service ID"
          ),

          h4("Days of Operation"),
          fluidRow(
            column(
              6,
              checkboxInput(ns("monday"), "Monday", value = FALSE),
              checkboxInput(ns("tuesday"), "Tuesday", value = FALSE),
              checkboxInput(ns("wednesday"), "Wednesday", value = FALSE),
              checkboxInput(ns("thursday"), "Thursday", value = FALSE),
            ),
            column(
              6,
              checkboxInput(ns("friday"), "Friday", value = FALSE),
              checkboxInput(ns("saturday"), "Saturday", value = FALSE),
              checkboxInput(ns("sunday"), "Sunday", value = FALSE)
            )
          ),

          h4("Service Period"),
          dateInput(
            ns("start_date"),
            "Start Date",
            value = "2000-01-01",
            min = "1970-01-01",
            max = "2099-12-31",
            format = "yyyy-mm-dd"
          ),
          dateInput(
            ns("end_date"),
            "End Date",
            value = "2000-12-31",
            min = "1970-01-01",
            max = "2099-12-31",
            format = "yyyy-mm-dd"
          ),

          actionButton(ns("add_service"), "Add service", class = "btn-success"),
          actionButton(ns("clear_service"), "Clear form", class = "btn-warning")
        ),
        mainPanel(
          DT::DTOutput(ns("calendar_table")),
          actionButton(
            ns("delete_selected_service"),
            "Delete selected service",
            class = "btn-danger",
          )
        )
      )
    )
  )
}

# Server functions
calendarServer <- function(id, ssfs) {
  moduleServer(id, function(input, output, session) {
    # Calendar table display
    output$calendar_table <- DT::renderDT({
      current_data <- ssfs()
      DT::datatable(
        current_data$calendar,
        selection = "single",
        options = list(
          pageLength = 10,
          ordering = FALSE,
          dom = "t"
        )
      )
    })

    # Generate next default service ID
    get_next_service_id <- function() {
      current_data <- ssfs()
      if (nrow(current_data$calendar) == 0) {
        return("S1")
      }
      existing_ids <- current_data$calendar$service_id
      numeric_part <- as.integer(gsub("S", "", existing_ids))
      sprintf("S%d", max(numeric_part) + 1)
    }

    # Clear service form
    observeEvent(input$clear_service, {
      updateTextInput(session, "service_id", value = "")
      updateCheckboxInput(session, "monday", value = FALSE)
      updateCheckboxInput(session, "tuesday", value = FALSE)
      updateCheckboxInput(session, "wednesday", value = FALSE)
      updateCheckboxInput(session, "thursday", value = FALSE)
      updateCheckboxInput(session, "friday", value = FALSE)
      updateCheckboxInput(session, "saturday", value = FALSE)
      updateCheckboxInput(session, "sunday", value = FALSE)
      updateDateInput(session, "start_date", value = "2000-01-01")
      updateDateInput(session, "end_date", value = "2099-12-31")
    })

    # Add new service
    observeEvent(input$add_service, {
      current_data <- ssfs()

      service_id <- if (input$service_id == "") {
        get_next_service_id()
      } else {
        input$service_id
      }

      if (service_id %in% current_data$calendar$service_id) {
        showNotification(
          "Service ID already exists. Please use a different ID.",
          type = "warning"
        )
        return()
      }

      start_date <- as.character(input$start_date)
      end_date <- as.character(input$end_date)

      if (start_date > end_date) {
        showNotification(
          "Start date must be before end date.",
          type = "warning"
        )
        return()
      }

      # Add the new service to the calendar
      new_service <- data.frame(
        service_id = service_id,
        monday = as.integer(input$monday),
        tuesday = as.integer(input$tuesday),
        wednesday = as.integer(input$wednesday),
        thursday = as.integer(input$thursday),
        friday = as.integer(input$friday),
        saturday = as.integer(input$saturday),
        sunday = as.integer(input$sunday),
        start_date = start_date,
        end_date = end_date,
        stringsAsFactors = FALSE
      )

      current_data$calendar <- rbind(current_data$calendar, new_service)

      ssfs(current_data)

      updateTextInput(session, "service_id", value = "")
      showNotification("Service added successfully!", type = "message")
    })

    # Delete selected service
    observeEvent(input$delete_selected_service, {
      req(input$calendar_table_rows_selected)
      current_data <- ssfs()

      if (length(input$calendar_table_rows_selected) > 0) {
        current_data$calendar <- current_data$calendar[
          -input$calendar_table_rows_selected,
        ]
        ssfs(current_data)
        showNotification("Service deleted successfully!", type = "message")
      }
    })
  })
}