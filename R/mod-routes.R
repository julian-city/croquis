# UI
routesUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "routes",
    fluidPage(
      titlePanel("routes"),
      # Map container with floating panels
      div(
        class = "map-container",
        # Full-width map
        leaflet::leafletOutput(
          ns("routes_map"),
          height = "100%",
          width = "100%"
        ),

        # Floating panel: Routes list (top-left)
        div(
          id = "routes-control-panel",
          class = "floating-panel floating-panel-left",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('routes-control-panel')",
            h4("Routes"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            # Instruction when editing itinerary
            uiOutput(ns("routes_editing_instruction")),
            # Route list
            div(class = "route-list-container", uiOutput(ns("route_list_ui")))
          )
        ),

        # Floating panel: Drawing Mode (bottom-left)
        div(
          id = "routes-drawing-panel",
          class = "floating-panel floating-panel-bottom-left",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('routes-drawing-panel')",
            h4("Drawing Mode"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            radioButtons(
              ns("drawing_mode"),
              NULL,
              choices = c(
                "Road Network" = "network",
                "Free Drawing" = "free"
              ),
              selected = "network"
            ),
            tags$small(
              "Network mode routes along streets. Free mode draws straight lines."
            )
          )
        ),

        # Floating panel: Stop Sequence (top-right)
        div(
          id = "routes-stopseq-panel",
          class = "floating-panel floating-panel-top-right",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('routes-stopseq-panel')",
            h4("Stop Sequence"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            DT::DTOutput(ns("selected_stops_table"))
          )
        )
      )
    )
  )
}
