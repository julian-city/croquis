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
            uiOutput(ns("drawing_mode_toggle_ui")),
            tags$small(
              "Network mode routes along streets. Free mode draws straight lines between stops and waypoints."
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

# Server
routesServer <- function(id, ssfs, map_center, current_zoom, routing_server) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Send namespace prefix to JS so Shiny.setInputValue calls are namespaced
    session$sendCustomMessage("setRoutesNs", ns(""))

    # Reactive values for routes list panel
    routes_editing_id <- reactiveVal(NULL)
    routes_adding_new <- reactiveVal(FALSE)
    routes_expanded_id <- reactiveVal(NULL)

    # Reactive values replacing old selectInput/textInput for itinerary details
    active_route_id <- reactiveVal(NULL)
    active_direction_id <- reactiveVal(0L)
    active_trip_headsign <- reactiveVal("")

    # Reactive values for inline itinerary editing
    itin_editing_id <- reactiveVal(NULL)
    itin_adding_for_route <- reactiveVal(NULL)

    # itin ids highlighted on map
    highlighted_itin_ids <- reactiveVal(character(0))

    # Reactive values for the combined routes/shapes functionality
    current_sequence <- reactiveVal(data.frame(
      itin_id = character(),
      stop_id = character(),
      stop_sequence = integer(),
      speed_factor = double(),
      stop_name = character(),
      stringsAsFactors = FALSE
    ))

    route_nodes <- reactiveVal(data.frame(
      node_id = integer(),
      lng = numeric(),
      lat = numeric(),
      is_stop = logical(),
      stop_id = character(),
      stop_name = character(),
      speed_factor = double(),
      index = integer(),
      stringsAsFactors = FALSE
    ))

    route_points <- reactiveVal(data.frame(
      index = numeric(),
      lng = numeric(),
      lat = numeric()
    ))

    route_editing_mode <- reactiveVal(FALSE)
    selected_point_index <- reactiveVal(NULL)
    active_itin_id <- reactiveVal(NULL)
    editing_existing_itin <- reactiveVal(FALSE)
    prepend_mode <- reactiveVal(FALSE)
    drawing_mode_reactive <- reactiveVal("network")

    # Track when a marker was last clicked
    last_marker_click_time <- reactiveVal(0)

    # Track map state
    map_ready <- reactiveVal(FALSE)

    # Clear all inputs function
    clearInputs <- function() {
      active_route_id(NULL)
      active_direction_id(0L)
      active_trip_headsign("")
      editing_existing_itin(FALSE)
      itin_editing_id(NULL)
      itin_adding_for_route(NULL)

      current_sequence(data.frame(
        itin_id = character(),
        stop_id = character(),
        stop_sequence = integer(),
        stop_name = character(),
        speed_factor = double(),
        stringsAsFactors = FALSE
      ))

      route_points(data.frame(
        index = integer(),
        lng = numeric(),
        lat = numeric()
      ))

      route_nodes(data.frame(
        node_id = integer(),
        lng = numeric(),
        lat = numeric(),
        is_stop = logical(),
        stop_id = character(),
        stop_name = character(),
        speed_factor = double(),
        index = integer(),
        stringsAsFactors = FALSE
      ))

      active_itin_id(NULL)
      selected_point_index(NULL)
      prepend_mode(FALSE)
      drawing_mode_reactive("network")
    }

    # --- UI Renderers ---

    # Render editing instruction for route itinerary drawing
    # Includes prepend mode toggle
    output$routes_editing_instruction <- renderUI({
      is_editing <- !is.null(active_itin_id()) &&
        (editing_existing_itin() || !is.null(itin_adding_for_route()))
      if (is_editing) {
        itin_id_display <- active_itin_id()
        is_prepending <- isTRUE(prepend_mode())

        div(
          class = "editing-instruction",
          paste0("Editing: ", itin_id_display),
          tags$br(),
          tags$small(
            if (is_prepending) {
              "Prepend mode: next stop clicks will be added to the START of the sequence."
            } else {
              "Click stops to build sequence. Right-click to remove."
            }
          ),
          div(
            class = "prepend-toggle-container",
            tags$label(
              class = "toggle-switch",
              tags$input(
                type = "checkbox",
                checked = if (is_prepending) "checked" else NULL,
                onchange = sprintf(
                  "Shiny.setInputValue('%s', this.checked, {priority: 'event'})",
                  session$ns("prepend_mode_toggle_state")
                )
              ),
              tags$span(class = "toggle-slider")
            ),
            tags$span(
              style = "font-size: 12px;",
              "Prepend stops to start of sequence"
            )
          )
        )
      } else {
        NULL
      }
    })

    # Render the routes list UI
    output$route_list_ui <- renderUI({
      current_data <- ssfs()
      editing_route_id <- routes_editing_id()
      adding_new <- routes_adding_new()
      expanded_route <- routes_expanded_id()
      current_active_itin <- active_itin_id()
      editing_itin <- itin_editing_id()
      adding_itin_route <- itin_adding_for_route()

      rows <- list()

      # "Add new route" button / form at the top
      if (adding_new) {
        rows[[length(rows) + 1]] <- build_route_form(current_data$agency)
      } else {
        rows[[length(rows) + 1]] <- div(
          class = "stop-list-row add-row",
          onclick = "startAddingRoute()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); startAddingRoute()",
            title = "Add new route",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new route")
        )
      }

      # Route rows
      # We order the rows by route type (low to high) and then by short name to have a consistent order (e.g. bus routes grouped together and ordered by route number)
      if (nrow(current_data$routes) > 0) {
        sorted_routes <- current_data$routes[
          order(
            current_data$routes$route_type,
            current_data$routes$route_short_name
          ),
        ]

        for (i in 1:nrow(sorted_routes)) {
          route <- sorted_routes[i, ]
          is_expanded <- !is.null(expanded_route) &&
            expanded_route == route$route_id
          is_editing <- !is.null(editing_route_id) &&
            editing_route_id == route$route_id

          rows[[length(rows) + 1]] <- build_route_row(
            route,
            is_expanded
          )

          if (is_expanded) {
            expanded_children <- list()

            # Actions bar
            expanded_children[[length(expanded_children) + 1]] <- div(
              class = "route-actions-bar",
              tags$button(
                class = "route-action-btn edit-btn",
                onclick = sprintf(
                  "event.stopPropagation(); editRouteFromList('%s')",
                  route$route_id
                ),
                title = "Edit route",
                htmltools::HTML("&#9998; Edit")
              ),
              tags$button(
                class = "route-action-btn",
                onclick = sprintf(
                  "event.stopPropagation(); copyRouteFromList('%s')",
                  route$route_id
                ),
                title = "Duplicate route",
                htmltools::HTML('<i class="fa-solid fa-clone"></i> Copy')
              ),
              tags$button(
                class = "route-action-btn delete-btn",
                onclick = sprintf(
                  "event.stopPropagation(); deleteRouteFromList('%s')",
                  route$route_id
                ),
                title = "Delete route",
                htmltools::HTML('<i class="fa-solid fa-trash"></i> Delete')
              )
            )

            # Route edit form (if editing this route and no itin is being edited)
            is_itin_editing_here <- (!is.null(editing_itin) ||
              !is.null(adding_itin_route))

            if (is_editing && !is_itin_editing_here) {
              expanded_children[[length(expanded_children) + 1]] <-
                build_route_form(
                  current_data$agency,
                  route
                )
            }

            # Itineraries for this route
            route_itins <- current_data$itin[
              current_data$itin$route_id == route$route_id,
            ]
            route_itins <- route_itins[order(route_itins$itin_id), ]

            # "Add itinerary" button or form
            if (
              !is.null(adding_itin_route) &&
                adding_itin_route == route$route_id
            ) {
              current_dir <- as.integer(active_direction_id())
              existing_itins_for_dir <- current_data$itin[
                current_data$itin$route_id == route$route_id &
                  current_data$itin$direction_id == current_dir,
              ]
              if (nrow(existing_itins_for_dir) == 0) {
                variant_num <- 1
              } else {
                variant_nums <- sapply(
                  existing_itins_for_dir$itin_id,
                  function(id) {
                    parts <- strsplit(id, "_")[[1]]
                    if (length(parts) >= 3) as.integer(parts[3]) else 0
                  }
                )
                variant_num <- max(variant_nums) + 1
              }
              default_itin_id <- paste0(
                route$route_id,
                "_",
                current_dir,
                "_",
                variant_num
              )
              expanded_children[[length(expanded_children) + 1]] <-
                build_itin_form(
                  default_itin_id,
                  current_dir
                )
            } else {
              expanded_children[[length(expanded_children) + 1]] <- div(
                class = "stop-list-row add-row",
                style = "padding: 4px 8px; font-size: 12px;",
                onclick = sprintf("startAddingItin('%s')", route$route_id),
                tags$button(
                  class = "stop-action-btn add-btn",
                  style = "font-size: 14px; width: 22px; height: 22px;",
                  onclick = sprintf(
                    "event.stopPropagation(); startAddingItin('%s')",
                    route$route_id
                  ),
                  title = "Add new itinerary",
                  htmltools::HTML("+")
                ),
                span(
                  style = "margin-left: 6px; font-size: 12px;",
                  "Add itinerary"
                )
              )
            }

            # Existing itinerary rows
            if (nrow(route_itins) > 0) {
              for (j in 1:nrow(route_itins)) {
                itin <- route_itins[j, ]
                is_active <- !is.null(current_active_itin) &&
                  current_active_itin == itin$itin_id
                is_editing_itin <- !is.null(editing_itin) &&
                  editing_itin == itin$itin_id

                if (is_editing_itin) {
                  expanded_children[[length(expanded_children) + 1]] <-
                    build_itin_row(itin, is_active)
                  expanded_children[[length(expanded_children) + 1]] <-
                    build_itin_form(
                      itin$itin_id,
                      itin$direction_id,
                      itin$trip_headsign,
                      is_new = FALSE
                    )
                } else {
                  expanded_children[[length(expanded_children) + 1]] <-
                    build_itin_row(itin, is_active)
                }
              }
            }

            rows[[length(rows) + 1]] <- div(
              class = "route-expanded-content",
              do.call(tagList, expanded_children)
            )
          }
        }
      }

      do.call(tagList, rows)
    })

    # --- Route list event handlers ---

    # Toggle expand/collapse
    observeEvent(input$route_list_toggle_expand, {
      route_id <- input$route_list_toggle_expand$id
      current_data <- ssfs()

      if (!is.null(routes_expanded_id()) && routes_expanded_id() == route_id) {
        routes_expanded_id(NULL)
        highlighted_itin_ids(character(0))
      } else {
        routes_expanded_id(route_id)
        route_itin_ids <- current_data$itin$itin_id[
          current_data$itin$route_id == route_id
        ]
        highlighted_itin_ids(route_itin_ids)

        # Zoom map to route's itineraries
        route_itins <- current_data$itin[
          current_data$itin$route_id == route_id,
        ]
        if (nrow(route_itins) > 0) {
          bbox <- st_bbox(route_itins$geometry)
          leaflet::leafletProxy("routes_map") |>
            leaflet::fitBounds(
              lng1 = bbox[["xmin"]],
              lat1 = bbox[["ymin"]],
              lng2 = bbox[["xmax"]],
              lat2 = bbox[["ymax"]]
            )
        }
      }
    })

    # Start adding new route
    observeEvent(input$route_list_add_click, {
      routes_adding_new(TRUE)
      routes_editing_id(NULL)
    })

    # Edit route (pencil icon)
    observeEvent(input$route_list_edit_click, {
      clicked_id <- input$route_list_edit_click$id
      if (!is.null(routes_editing_id()) && routes_editing_id() == clicked_id) {
        # Toggle off: cancel route edit, keep expanded
        clearInputs()
        routes_editing_id(NULL)
        routes_adding_new(FALSE)
        routes_expanded_id(clicked_id)
      } else {
        clearInputs()
        routes_editing_id(clicked_id)
        routes_expanded_id(clicked_id)
        routes_adding_new(FALSE)
      }
    })

    # Duplicate route (copy icon)
    observeEvent(input$route_list_copy_click, {
      route_to_copy <- input$route_list_copy_click$id
      current_data <- ssfs()

      source_route <- current_data$routes[
        current_data$routes$route_id == route_to_copy,
      ]

      new_route_id <- paste0(route_to_copy, "b")
      while (new_route_id %in% current_data$routes$route_id) {
        new_route_id <- paste0(new_route_id, "b")
      }
      suffix <- sub(
        paste0("^", route_to_copy),
        "",
        new_route_id
      )

      # 1. Duplicate route row
      new_route <- source_route
      new_route$route_id <- new_route_id
      new_route$route_short_name <- paste0(
        source_route$route_short_name,
        suffix
      )
      current_data$routes <- rbind(current_data$routes, new_route)

      # 2. Duplicate itineraries and build itin_id mapping
      source_itins <- current_data$itin[
        current_data$itin$route_id == route_to_copy,
      ]

      if (nrow(source_itins) > 0) {
        new_itins <- source_itins
        new_itins$route_id <- new_route_id

        itin_id_map <- setNames(
          sub(
            paste0("^", route_to_copy),
            new_route_id,
            source_itins$itin_id
          ),
          source_itins$itin_id
        )
        new_itins$itin_id <- unname(itin_id_map[new_itins$itin_id])

        current_data$itin <- rbind(current_data$itin, new_itins)

        # 3. Duplicate stop_seq
        old_itin_ids <- names(itin_id_map)
        source_stop_seq <- current_data$stop_seq[
          current_data$stop_seq$itin_id %in% old_itin_ids,
        ]
        if (nrow(source_stop_seq) > 0) {
          new_stop_seq <- source_stop_seq
          new_stop_seq$itin_id <- itin_id_map[new_stop_seq$itin_id]
          current_data$stop_seq <- rbind(current_data$stop_seq, new_stop_seq)
        }

        # 4. Duplicate span
        source_span <- current_data$span[
          current_data$span$itin_id %in% old_itin_ids,
        ]
        if (nrow(source_span) > 0) {
          new_span <- source_span
          new_span$itin_id <- itin_id_map[new_span$itin_id]
          current_data$span <- rbind(current_data$span, new_span)
        }

        # 5. Duplicate hsh
        source_hsh <- current_data$hsh[
          current_data$hsh$itin_id %in% old_itin_ids,
        ]
        if (nrow(source_hsh) > 0) {
          new_hsh <- source_hsh
          new_hsh$itin_id <- itin_id_map[new_hsh$itin_id]
          current_data$hsh <- rbind(current_data$hsh, new_hsh)
        }
      }

      ssfs(current_data)
      routes_expanded_id(new_route_id)

      showNotification(
        paste("Duplicated route as:", new_route_id),
        type = "message"
      )
    })

    # Cancel route edit
    observeEvent(input$route_list_cancel_click, {
      editing_id <- routes_editing_id()
      routes_editing_id(NULL)
      routes_adding_new(FALSE)
      # Keep the route expanded after canceling
      if (!is.null(editing_id)) {
        routes_expanded_id(editing_id)
        session$sendCustomMessage("scrollToRoute", editing_id)
      }
    })

    # Save route from inline form (handles both add and edit)
    observeEvent(input$route_list_save_data, {
      data <- input$route_list_save_data
      new_route_id <- trimws(data$route_id)

      if (is.null(new_route_id) || new_route_id == "") {
        showNotification("Route ID cannot be empty.", type = "warning")
        return()
      }

      if (is.null(data$agency_id) || data$agency_id == "") {
        showNotification(
          "Please define at least one agency first.",
          type = "warning"
        )
        return()
      }

      current_data <- ssfs()
      route_color <- gsub("^#", "", data$route_color)
      route_text_color <- gsub("^#", "", data$route_text_color)

      if (routes_adding_new()) {
        if (new_route_id %in% current_data$routes$route_id) {
          showNotification("This route ID already exists.", type = "warning")
          return()
        }

        new_route <- data.frame(
          route_id = new_route_id,
          agency_id = data$agency_id,
          route_short_name = trimws(data$short_name),
          route_long_name = trimws(data$long_name),
          route_type = as.integer(data$route_type),
          route_color = route_color,
          route_text_color = route_text_color,
          stringsAsFactors = FALSE
        )

        current_data$routes <- rbind(current_data$routes, new_route)
        ssfs(current_data)
        routes_adding_new(FALSE)
        routes_expanded_id(new_route_id)
        session$sendCustomMessage("scrollToRoute", new_route_id)
        showNotification("Route added successfully", type = "message")
      } else if (!is.null(routes_editing_id())) {
        old_route_id <- routes_editing_id()
        idx <- which(current_data$routes$route_id == old_route_id)

        if (length(idx) == 0) {
          showNotification("Route not found.", type = "error")
          return()
        }

        if (
          new_route_id != old_route_id &&
            new_route_id %in% current_data$routes$route_id
        ) {
          showNotification("This route ID already exists.", type = "warning")
          return()
        }

        current_data$routes$route_id[idx] <- new_route_id
        current_data$routes$agency_id[idx] <- data$agency_id
        current_data$routes$route_short_name[idx] <- trimws(data$short_name)
        current_data$routes$route_long_name[idx] <- trimws(data$long_name)
        current_data$routes$route_type[idx] <- as.integer(data$route_type)
        current_data$routes$route_color[idx] <- route_color
        current_data$routes$route_text_color[idx] <- route_text_color

        if (new_route_id != old_route_id && nrow(current_data$itin) > 0) {
          current_data$itin$route_id[
            current_data$itin$route_id == old_route_id
          ] <- new_route_id
        }

        ssfs(current_data)

        if (
          !is.null(routes_expanded_id()) && routes_expanded_id() == old_route_id
        ) {
          routes_expanded_id(new_route_id)
        }

        routes_editing_id(NULL)
        session$sendCustomMessage("scrollToRoute", new_route_id)
        showNotification("Route updated successfully", type = "message")
      }
    })

    # Delete route
    observeEvent(input$route_list_delete_click, {
      route_to_delete <- input$route_list_delete_click$id
      current_data <- ssfs()

      if (
        nrow(current_data$itin) > 0 &&
          route_to_delete %in% current_data$itin$route_id
      ) {
        showNotification(
          paste0(
            "Cannot delete route '",
            route_to_delete,
            "'. It is referenced by one or more itineraries. ",
            "Delete the itineraries first."
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      current_data$routes <- current_data$routes[
        current_data$routes$route_id != route_to_delete,
      ]
      ssfs(current_data)

      if (
        !is.null(routes_expanded_id()) &&
          routes_expanded_id() == route_to_delete
      ) {
        routes_expanded_id(NULL)
      }

      showNotification("Route deleted successfully", type = "message")
    })

    # --- Itinerary list event handlers ---

    # View/center itinerary on map
    observeEvent(input$itin_list_view_click, {
      itin_id <- input$itin_list_view_click$id
      current_data <- ssfs()

      selected_itin <- current_data$itin[current_data$itin$itin_id == itin_id, ]
      if (nrow(selected_itin) > 0) {
        bbox <- st_bbox(selected_itin$geometry)
        leaflet::leafletProxy("routes_map") |>
          leaflet::fitBounds(
            lng1 = bbox[["xmin"]],
            lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]],
            lat2 = bbox[["ymax"]]
          )
      }

      highlighted_itin_ids(itin_id)
    })

    # Edit itinerary (pencil icon)
    observeEvent(input$itin_list_edit_click, {
      itin_id <- input$itin_list_edit_click$id

      # If already editing this same itin, toggle off → back to route edit
      if (!is.null(itin_editing_id()) && itin_editing_id() == itin_id) {
        clearInputs()
        return()
      }

      # Clear any existing itin edit state before switching
      clearInputs()

      current_ssfs_data <- ssfs()

      selected_itin <- current_ssfs_data$itin[
        current_ssfs_data$itin$itin_id == itin_id,
      ]

      if (nrow(selected_itin) == 0) {
        showNotification("Itinerary not found", type = "error")
        return()
      }

      editing_existing_itin(TRUE)
      itin_editing_id(itin_id)
      itin_adding_for_route(NULL)

      highlighted_itin_ids(character(0))

      active_route_id(selected_itin$route_id)
      active_direction_id(as.integer(selected_itin$direction_id))
      active_trip_headsign(selected_itin$trip_headsign)
      active_itin_id(itin_id)

      routes_expanded_id(selected_itin$route_id)

      # Load stop sequence
      stop_seq <- current_ssfs_data$stop_seq[
        current_ssfs_data$stop_seq$itin_id == itin_id,
      ]
      current_sequence(stop_seq)

      # Load shape and extract nodes
      shape_data <- current_ssfs_data$itin[
        current_ssfs_data$itin$itin_id == itin_id,
      ]

      if (nrow(shape_data) > 0) {
        coords <- st_coordinates(shape_data$geometry)
        full_points <- data.frame(
          index = 1:nrow(coords),
          lng = coords[, 1],
          lat = coords[, 2]
        )
        route_points(full_points)

        nodes_df <- data.frame(
          node_id = integer(),
          lng = numeric(),
          lat = numeric(),
          is_stop = logical(),
          stop_id = character(),
          stop_name = character(),
          speed_factor = double(),
          index = integer(),
          stringsAsFactors = FALSE
        )

        if (nrow(stop_seq) > 0) {
          for (i in 1:nrow(stop_seq)) {
            stop_id <- stop_seq$stop_id[i]
            stop_data <- current_ssfs_data$stops[
              current_ssfs_data$stops$stop_id == stop_id,
            ]

            if (nrow(stop_data) > 0) {
              stop_coords <- st_coordinates(stop_data$geometry)
              distances <- sqrt(
                (full_points$lng - stop_coords[1, 1])^2 +
                  (full_points$lat - stop_coords[1, 2])^2
              )
              closest_idx <- which.min(distances)

              nodes_df <- rbind(
                nodes_df,
                data.frame(
                  node_id = i,
                  lng = stop_coords[1, 1],
                  lat = stop_coords[1, 2],
                  is_stop = TRUE,
                  stop_id = stop_id,
                  stop_name = stop_seq$stop_name[i],
                  speed_factor = stop_seq$speed_factor[i],
                  index = closest_idx,
                  stringsAsFactors = FALSE
                )
              )
            }
          }
          row.names(nodes_df) <- 1:nrow(nodes_df)
        }

        route_nodes(nodes_df)
      }

      # Center map on itinerary
      bbox <- st_bbox(shape_data$geometry)
      leaflet::leafletProxy("routes_map") |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )

      showNotification(paste("Editing itinerary:", itin_id), type = "message")
    })

    # Start adding new itinerary
    observeEvent(input$itin_list_add_click, {
      route_id <- input$itin_list_add_click$id

      clearInputs()

      highlighted_itin_ids(character(0))

      active_route_id(route_id)
      active_direction_id(0L)
      active_trip_headsign("")
      itin_adding_for_route(route_id)
      itin_editing_id(NULL)

      current_data <- ssfs()
      direction_id <- 0L
      existing_itins <- current_data$itin[
        current_data$itin$route_id == route_id,
      ]

      if (nrow(existing_itins) == 0) {
        variant_num <- 1
      } else {
        variant_nums <- sapply(existing_itins$itin_id, function(id) {
          parts <- strsplit(id, "_")[[1]]
          if (length(parts) >= 3) as.integer(parts[3]) else 0
        })
        variant_num <- max(variant_nums) + 1
      }

      new_itin_id <- paste0(route_id, "_", direction_id, "_", variant_num)
      active_itin_id(new_itin_id)
      editing_existing_itin(FALSE)
    })

    # Recalculate itin_id when direction changes in inline form
    observeEvent(input$inline_direction_changed, {
      new_direction <- as.integer(input$inline_direction_changed$direction_id)

      route_id <- NULL
      if (!is.null(itin_adding_for_route())) {
        route_id <- itin_adding_for_route()
      } else if (!is.null(itin_editing_id())) {
        current_data <- ssfs()
        idx <- which(current_data$itin$itin_id == itin_editing_id())
        if (length(idx) > 0) {
          route_id <- current_data$itin$route_id[idx]
        }
      }

      if (is.null(route_id)) {
        return()
      }

      current_data <- ssfs()

      existing_itins <- current_data$itin[
        current_data$itin$route_id == route_id &
          current_data$itin$direction_id == new_direction,
      ]

      if (nrow(existing_itins) == 0) {
        variant_num <- 1
      } else {
        variant_nums <- sapply(existing_itins$itin_id, function(id) {
          parts <- strsplit(id, "_")[[1]]
          if (length(parts) >= 3) as.integer(parts[3]) else 0
        })
        variant_num <- max(variant_nums) + 1
      }

      new_itin_id <- paste0(route_id, "_", new_direction, "_", variant_num)

      session$sendCustomMessage("updateInlineItinId", new_itin_id)

      active_direction_id(new_direction)
      active_itin_id(new_itin_id)
    })

    # Save itinerary from inline form
    observeEvent(input$itin_list_save_data, {
      data <- input$itin_list_save_data
      new_itin_id <- trimws(data$itin_id)
      new_direction <- as.integer(data$direction_id)
      new_headsign <- trimws(data$trip_headsign)

      if (new_itin_id == "") {
        showNotification("Itinerary ID cannot be empty.", type = "warning")
        return()
      }

      if (new_headsign == "") {
        showNotification("Trip headsign cannot be empty.", type = "warning")
        return()
      }

      current_data <- ssfs()
      curr_points <- route_points()

      if (!is.null(itin_editing_id())) {
        # --- EDITING EXISTING ITINERARY ---
        old_itin_id <- itin_editing_id()

        if (
          new_itin_id != old_itin_id &&
            new_itin_id %in% current_data$itin$itin_id
        ) {
          showNotification(
            "This itinerary ID already exists.",
            type = "warning"
          )
          return()
        }

        if (nrow(curr_points) < 2) {
          showNotification(
            "Itinerary must have at least 2 points.",
            type = "warning"
          )
          return()
        }

        route_id <- current_data$itin$route_id[
          current_data$itin$itin_id == old_itin_id
        ]

        curr_points_sorted <- curr_points[order(curr_points$index), ]
        coords_matrix <- as.matrix(curr_points_sorted[, c("lng", "lat")])
        line_feature <- st_linestring(coords_matrix)

        new_itin_entry <- st_sf(
          itin_id = new_itin_id,
          route_id = route_id,
          direction_id = new_direction,
          trip_headsign = new_headsign,
          geometry = st_sfc(line_feature, crs = 4326),
          stringsAsFactors = FALSE
        )

        active_itin_id(new_itin_id)
        stop_seq <- generateStopSequenceFromNodes(
          route_nodes(),
          active_itin_id()
        )

        current_data$itin <- current_data$itin[
          current_data$itin$itin_id != old_itin_id,
        ]
        current_data$stop_seq <- current_data$stop_seq[
          current_data$stop_seq$itin_id != old_itin_id,
        ]

        current_data$itin <- rbind(current_data$itin, new_itin_entry)
        if (nrow(stop_seq) > 0) {
          current_data$stop_seq <- rbind(current_data$stop_seq, stop_seq)
        }

        if (new_itin_id != old_itin_id) {
          current_data$span$itin_id[
            current_data$span$itin_id == old_itin_id
          ] <- new_itin_id
          current_data$hsh$itin_id[
            current_data$hsh$itin_id == old_itin_id
          ] <- new_itin_id
        }

        ssfs(current_data)
        clearInputs()
        session$sendCustomMessage("scrollToRoute", route_id)
        showNotification("Itinerary saved successfully", type = "message")
      } else if (!is.null(itin_adding_for_route())) {
        # --- ADDING NEW ITINERARY ---

        if (new_itin_id %in% current_data$itin$itin_id) {
          showNotification(
            "This itinerary ID already exists.",
            type = "warning"
          )
          return()
        }

        if (nrow(curr_points) < 2) {
          showNotification(
            "Please draw the route on the map before saving.",
            type = "warning"
          )
          return()
        }

        route_id <- itin_adding_for_route()

        curr_points_sorted <- curr_points[order(curr_points$index), ]
        coords_matrix <- as.matrix(curr_points_sorted[, c("lng", "lat")])
        line_feature <- st_linestring(coords_matrix)

        new_itin_entry <- st_sf(
          itin_id = new_itin_id,
          route_id = route_id,
          direction_id = new_direction,
          trip_headsign = new_headsign,
          geometry = st_sfc(line_feature, crs = 4326),
          stringsAsFactors = FALSE
        )

        active_itin_id(new_itin_id)
        stop_seq <- generateStopSequenceFromNodes(
          route_nodes(),
          active_itin_id()
        )

        current_data$itin <- rbind(current_data$itin, new_itin_entry)
        if (nrow(stop_seq) > 0) {
          current_data$stop_seq <- rbind(current_data$stop_seq, stop_seq)
        }

        ssfs(current_data)
        clearInputs()
        session$sendCustomMessage("scrollToRoute", route_id)
        showNotification("Itinerary saved successfully", type = "message")
      }
    })

    # Cancel itinerary editing
    observeEvent(input$itin_list_cancel_click, {
      clearInputs()
    })

    # Delete itinerary
    observeEvent(input$itin_list_delete_click, {
      itin_to_delete <- input$itin_list_delete_click$id
      current_data <- ssfs()

      if (!itin_to_delete %in% current_data$itin$itin_id) {
        showNotification("Itinerary not found", type = "error")
        return()
      }

      current_data$itin <- current_data$itin |>
        filter(itin_id != itin_to_delete)
      current_data$stop_seq <- current_data$stop_seq |>
        filter(itin_id != itin_to_delete)
      current_data$span <- current_data$span |>
        filter(itin_id != itin_to_delete)
      current_data$hsh <- current_data$hsh |> filter(itin_id != itin_to_delete)
      ssfs(current_data)

      if (!is.null(active_itin_id()) && active_itin_id() == itin_to_delete) {
        clearInputs()
      }

      showNotification(
        paste("Deleted itinerary:", itin_to_delete),
        type = "message"
      )
    })

    # Copy/duplicate itinerary
    observeEvent(input$itin_list_copy_click, {
      itin_to_copy <- input$itin_list_copy_click$id
      current_data <- ssfs()

      source_itin <- current_data$itin[
        current_data$itin$itin_id == itin_to_copy,
      ]
      if (nrow(source_itin) == 0) {
        showNotification("Itinerary not found", type = "error")
        return()
      }

      route_id <- source_itin$route_id
      direction_id <- as.integer(source_itin$direction_id)

      existing_itins <- current_data$itin[
        current_data$itin$route_id == route_id &
          current_data$itin$direction_id == direction_id,
      ]
      if (nrow(existing_itins) == 0) {
        variant_num <- 1
      } else {
        variant_nums <- sapply(existing_itins$itin_id, function(id) {
          parts <- strsplit(id, "_")[[1]]
          if (length(parts) >= 3) as.integer(parts[3]) else 0
        })
        variant_num <- max(variant_nums) + 1
      }
      new_itin_id <- paste0(route_id, "_", direction_id, "_", variant_num)

      new_itin_entry <- source_itin
      new_itin_entry$itin_id <- new_itin_id
      new_itin_entry$trip_headsign <- paste0(source_itin$trip_headsign, "_copy")

      source_stop_seq <- current_data$stop_seq[
        current_data$stop_seq$itin_id == itin_to_copy,
      ]
      new_stop_seq <- source_stop_seq
      if (nrow(new_stop_seq) > 0) {
        new_stop_seq$itin_id <- new_itin_id
      }

      current_data$itin <- rbind(current_data$itin, new_itin_entry)
      if (nrow(new_stop_seq) > 0) {
        current_data$stop_seq <- rbind(current_data$stop_seq, new_stop_seq)
      }

      ssfs(current_data)
      routes_expanded_id(route_id)

      showNotification(paste("Duplicated as:", new_itin_id), type = "message")
    })

    # --- Prepend mode toggle

    observeEvent(input$prepend_mode_toggle_state, {
      req(active_itin_id())
      prepend_mode(isTRUE(input$prepend_mode_toggle_state))
    })

    # --- Drawing mode toggle
    # Checked = "free" (straight lines), unchecked = "network" (road routing, default).
    # Render inside the Drawing Mode floating panel; the `checked` attribute is
    # driven by drawing_mode_reactive(), so resetting the reactive re-renders the
    # DOM to match (same pattern as the prepend toggle).

    output$drawing_mode_toggle_ui <- renderUI({
      is_free <- isTRUE(drawing_mode_reactive() == "free")

      div(
        class = "prepend-toggle-container",
        tags$span(
          style = "font-size: 12px;",
          "Road Network"
        ),
        tags$label(
          class = "toggle-switch",
          tags$input(
            type = "checkbox",
            checked = if (is_free) "checked" else NULL,
            onchange = sprintf(
              "Shiny.setInputValue('%s', this.checked, {priority: 'event'})",
              session$ns("drawing_mode_toggle_state")
            )
          ),
          tags$span(class = "toggle-slider")
        ),
        tags$span(
          style = "font-size: 12px;",
          "Free Drawing"
        )
      )
    })

    observeEvent(input$drawing_mode_toggle_state, {
      drawing_mode_reactive(
        if (isTRUE(input$drawing_mode_toggle_state)) "free" else "network"
      )
    })

    # --- Map initialization and rendering ---

    # Initialize routes map
    output$routes_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") |>
        leaflet::addMapPane("routes_pane", zIndex = 410) |>
        leaflet::addMapPane("highlight_pane", zIndex = 420) |>
        leaflet::addMapPane("stops_pane", zIndex = 430) |>
        leaflet::addMapPane("route_nodes_pane", zIndex = 440) |>
        leaflet::addMapPane("current_route_pane", zIndex = 450) |>
        leaflet::setView(lng = center$lng, lat = center$lat, zoom = 12) |>
        leaflet::addLayersControl(
          baseGroups = c("Positron", "Satellite", "OSM"),
          position = "bottomright",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::showGroup("stops") |>
        leaflet::showGroup("routes") |>
        leaflet::showGroup("current_route") |>
        htmlwidgets::onRender(sprintf(
          "
          function(el, x) {
            var ns = '%s';
            var map = this;

            function calcMarkerSize(zoom) {
              var base = 2;
              var adjusted = base * Math.pow(1.2, zoom - 10);
              return Math.min(Math.max(adjusted, 1), 15);
            }

            function resizeStopMarkers() {
              var zoom = map.getZoom();
              var r = calcMarkerSize(zoom);
              map.eachLayer(function(layer) {
                if (layer.options && layer.options.group === 'stops' &&
                    typeof layer.setRadius === 'function') {
                  layer.setRadius(r);
                }
              });
            }

            map.on('zoomend', function(e) {
              Shiny.setInputValue(ns + 'routes_map_zoom', map.getZoom());
              resizeStopMarkers();
            });

            map.on('contextmenu', function(e) {
              Shiny.setInputValue(ns + 'routes_map_right_click', {
                lat: e.latlng.lat,
                lng: e.latlng.lng
              }, {priority: 'event'});
            });
          }
          ",
          ns("")
        ))
    })

    # Update zoom level when map is zoomed
    observeEvent(input$routes_map_zoom, {
      current_zoom(input$routes_map_zoom)
    })

    observeEvent(
      map_center(),
      {
        map_ready(FALSE)
      },
      priority = 10
    )

    observeEvent(
      input$routes_map_bounds,
      {
        map_ready(TRUE)
      },
      once = FALSE
    )

    # --- Map Observers ----
    # ---- Itinerary Polylines ----
    observe({
      req(map_ready())
      current_data <- ssfs()
      current_active <- active_itin_id()

      proxy <- leaflet::leafletProxy("routes_map") |>
        leaflet::clearGroup("routes")

      if (!is.null(current_data$itin) && nrow(current_data$itin) > 0) {
        # Build draw order: High route_type first, low route_type last (drawn on stop)
        # We also order by route_short_name to have a consistent order for same-type routes (e.g. bus routes with same route_type)

        draw_order <- itineraryDrawOrder(current_data$itin, current_data$routes)

        for (i in draw_order) {
          if (
            !is.null(current_active) &&
              current_data$itin$itin_id[i] == current_active
          ) {
            next
          }

          line_coords <- st_coordinates(current_data$itin$geometry[i])

          route_id_i <- current_data$itin$route_id[i]
          itin_id_i <- current_data$itin$itin_id[i]
          trip_headsign_i <- current_data$itin$trip_headsign[i]

          route_row <- current_data$routes[
            current_data$routes$route_id == route_id_i,
          ]

          route_color_i <- route_row$route_color
          route_short <- if (nrow(route_row) > 0) {
            route_row$route_short_name[1]
          } else {
            ""
          }

          route_long <- if (nrow(route_row) > 0) {
            route_row$route_long_name[1]
          } else {
            ""
          }

          route_display <- paste0(
            htmltools::htmlEscape(route_short),
            " - ",
            htmltools::htmlEscape(route_long)
          )

          itinerary_display <- paste0(
            htmltools::htmlEscape(itin_id_i),
            " - ",
            htmltools::htmlEscape(trip_headsign_i)
          )

          hover_label <- htmltools::HTML(paste0(
            "<span style='font-size:11px;'>",
            "<b>",
            route_display,
            "</b>",
            "<br>Itinerary: ",
            itinerary_display,
            "</span>"
          ))

          line_color <- if (
            length(route_color_i) > 0 &&
              !is.na(route_color_i[1]) &&
              nchar(route_color_i[1]) > 0
          ) {
            paste0("#", route_color_i[1])
          } else {
            "#05AEEF"
          }

          route_type_i <- if (nrow(route_row) > 0) {
            route_row$route_type[1]
          } else {
            NA
          }
          line_weight <- routeLineWeight(route_type_i)

          proxy <- proxy |>
            leaflet::addPolylines(
              lng = line_coords[, 1],
              lat = line_coords[, 2],
              group = "routes",
              color = line_color,
              weight = line_weight,
              opacity = 0.6,
              label = hover_label,
              options = leaflet::pathOptions(pane = "routes_pane"),
              labelOptions = leaflet::labelOptions(
                style = list("font-size" = "11px", "padding" = "3px 6px"),
                direction = "top",
                offset = c(0, -8)
              ),
              highlightOptions = leaflet::highlightOptions(
                weight = line_weight + 4,
                opacity = 0.9,
                bringToFront = TRUE
              )
            )
        }
      }
    })

    # ---- Highlight underlay ----
    observe({
      req(map_ready())
      hl_ids <- highlighted_itin_ids()
      current_data <- ssfs()

      proxy <- leaflet::leafletProxy("routes_map") |>
        leaflet::clearGroup("highlight")

      if (length(hl_ids) > 0 && nrow(current_data$itin) > 0) {
        hl_itins <- current_data$itin[current_data$itin$itin_id %in% hl_ids, ]
        for (j in seq_len(nrow(hl_itins))) {
          hl_coords <- st_coordinates(hl_itins$geometry[j])
          proxy <- proxy |>
            leaflet::addPolylines(
              lng = hl_coords[, 1],
              lat = hl_coords[, 2],
              group = "highlight",
              options = leaflet::pathOptions(pane = "highlight_pane"),
              color = "#FFE999",
              weight = 10,
              opacity = 0.4,
              stroke = TRUE
            )
        }
      }
    })

    # ---- Stop markers ----
    observe({
      req(map_ready())
      current_data <- ssfs()
      curr_nodes <- route_nodes()

      proxy <- leaflet::leafletProxy("routes_map") |>
        leaflet::clearGroup("stops")

      if (!is.null(current_data$stops) && nrow(current_data$stops) > 0) {
        marker_size <- calculateMarkerSize(isolate(current_zoom()))

        stop_ids_in_nodes <- curr_nodes$stop_id[curr_nodes$is_stop]

        fill_colors <- ifelse(
          current_data$stops$stop_id %in% stop_ids_in_nodes,
          "#B2182B",
          "#7f7f7f"
        )

        proxy <- proxy |>
          leaflet::addCircleMarkers(
            data = current_data$stops,
            radius = marker_size,
            color = "white",
            weight = 1,
            stroke = TRUE,
            fillColor = fill_colors,
            fillOpacity = 0.7,
            layerId = ~stop_id,
            popup = ~ paste("ID:", stop_id, "<br>Name:", stop_name),
            group = "stops",
            options = leaflet::pathOptions(pane = "stops_pane")
          )
      }
    })

    # ---- Current route being edited + node markers ----
    observe({
      req(map_ready())

      curr_nodes <- route_nodes()
      curr_points <- route_points()

      proxy <- leaflet::leafletProxy("routes_map") |>
        leaflet::clearGroup("current_route") |>
        leaflet::clearGroup("route_nodes")

      # Add current route being edited
      if (nrow(curr_points) > 1) {
        proxy <- proxy |>
          leaflet::addPolylines(
            lng = curr_points$lng,
            lat = curr_points$lat,
            group = "current_route",
            options = leaflet::pathOptions(pane = "current_route_pane"),
            color = "#B2182B",
            weight = 4,
            opacity = 0.8
          )
      }

      # Add node markers
      if (nrow(curr_nodes) > 0) {
        stop_nodes <- curr_nodes[curr_nodes$is_stop, ]

        if (nrow(stop_nodes) > 0) {
          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = stop_nodes$lng,
              lat = stop_nodes$lat,
              group = "route_nodes",
              options = leaflet::pathOptions(pane = "route_nodes_pane"),
              radius = 8,
              color = "#B2182B",
              fillColor = "#B2182B",
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 2,
              layerId = paste0("stop_", stop_nodes$node_id),
              label = paste0("Stop: ", stop_nodes$stop_name)
            )
        }
      }

      # Waypoint nodes
      waypoint_nodes <- curr_nodes[!curr_nodes$is_stop, ]
      if (nrow(waypoint_nodes) > 0) {
        proxy <- proxy |>
          leaflet::addCircleMarkers(
            lng = waypoint_nodes$lng,
            lat = waypoint_nodes$lat,
            group = "route_nodes",
            options = leaflet::pathOptions(pane = "route_nodes_pane"),
            radius = 6,
            color = "orange",
            fillColor = "orange",
            fillOpacity = 0.9,
            stroke = TRUE,
            weight = 2,
            layerId = paste0("wp_", waypoint_nodes$node_id),
            label = "Waypoint"
          )
      }

      if (!is.null(selected_point_index())) {
        selected_node <- curr_nodes[
          curr_nodes$node_id == selected_point_index(),
        ]
        if (nrow(selected_node) > 0) {
          proxy <- proxy |>
            leaflet::addCircleMarkers(
              lng = selected_node$lng,
              lat = selected_node$lat,
              group = "route_nodes",
              options = leaflet::pathOptions(pane = "route_nodes_pane"),

              radius = 8,
              color = "#FFE999",
              fillColor = "#FFE999",
              fillOpacity = 0.9,
              stroke = TRUE,
              weight = 3,
              layerId = "selected_node"
            )
        }
      }
    })

    # Auto-update active_itin_id
    observe({
      req(active_route_id())

      if (!editing_existing_itin()) {
        current_data <- ssfs()
        route_id <- active_route_id()
        direction_id <- active_direction_id()

        existing_itins <- current_data$itin |>
          filter(route_id == !!route_id, direction_id == !!direction_id)

        if (nrow(existing_itins) == 0) {
          variant_num <- 1
        } else {
          variant_nums <- sapply(existing_itins$itin_id, function(id) {
            parts <- strsplit(id, "_")[[1]]
            if (length(parts) >= 3) as.integer(parts[3]) else 0
          })
          variant_num <- max(variant_nums) + 1
        }

        new_itin_id <- paste0(route_id, "_", direction_id, "_", variant_num)
        active_itin_id(new_itin_id)
      }
    })

    # --- Map interaction handlers ---

    # Stop click handler - adds stop nodes
    observeEvent(input$routes_map_marker_click, {
      req(active_itin_id())
      click <- input$routes_map_marker_click

      last_marker_click_time(as.numeric(Sys.time()))

      if (!is.null(click) && grepl("^wp_", click$id)) {
        node_id <- as.numeric(gsub("wp_", "", click$id))
        selected_point_index(node_id)
        showNotification(
          "Waypoint selected. Click on map to move it.",
          type = "message"
        )
      } else if (!is.null(click) && click$id == "selected_node") {
        selected_point_index(NULL)
        showNotification(
          "Waypoint deselected. Movement cancelled.",
          type = "message"
        )
      } else if (!is.null(click) && grepl("^stop_", click$id)) {
        showNotification(
          "Stop already in route stop sequence. Cannot add stop again.",
          type = "warning"
        )
      } else if (!is.null(click)) {
        current_data <- ssfs()
        clicked_stop <- current_data$stops[
          current_data$stops$stop_id == click$id,
        ]
        curr_nodes <- route_nodes()
        curr_points <- route_points()

        if (!is.null(selected_point_index())) {
          is_last_node <- (selected_point_index() == nrow(curr_nodes))

          if (is_last_node) {
            if (selected_point_index() == 1) {
              curr_nodes <- data.frame(
                node_id = 1,
                lng = st_coordinates(clicked_stop)[1],
                lat = st_coordinates(clicked_stop)[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1,
                index = 1,
                stringsAsFactors = FALSE
              )
              curr_points <-
                data.frame(
                  index = 1,
                  lng = st_coordinates(clicked_stop)[1],
                  lat = st_coordinates(clicked_stop)[2]
                )
            } else {
              before_idx <- selected_point_index() - 1

              nodes_a <- curr_nodes[1:before_idx, ]
              nodes_a_idx_max <- max(nodes_a$index)
              points_a <- curr_points[1:nodes_a_idx_max, ]

              from_point <- c(
                curr_nodes[before_idx, ]$lng,
                curr_nodes[before_idx, ]$lat
              )
              to_point <- c(
                st_coordinates(clicked_stop)[1],
                st_coordinates(clicked_stop)[2]
              )

              segment_b <- generateRouteSegment(
                from_point,
                to_point,
                drawing_mode = drawing_mode_reactive(),
                routing_server = routing_server()
              )

              points_b <-
                segment_b[2:nrow(segment_b), ] |>
                mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

              points_b_idx_max <- max(points_b$index)

              node_new <- data.frame(
                node_id = selected_point_index(),
                lng = st_coordinates(clicked_stop)[1],
                lat = st_coordinates(clicked_stop)[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1,
                index = points_b_idx_max,
                stringsAsFactors = FALSE
              )

              curr_points <- rbind(points_a, points_b)
              curr_nodes <- rbind(nodes_a, node_new)

              row.names(curr_points) <- 1:nrow(curr_points)
              row.names(curr_nodes) <- 1:nrow(curr_nodes)
            }
          } else {
            before_idx <- selected_point_index() - 1
            after_idx <- selected_point_index() + 1

            nodes_a <- curr_nodes[1:before_idx, ]
            nodes_a_idx_max <- max(nodes_a$index)
            points_a <- curr_points[1:nodes_a_idx_max, ]

            nodes_d <- curr_nodes[after_idx:nrow(curr_nodes), ]
            nodes_d_idx_min <- min(nodes_d$index)
            points_d <- curr_points[nodes_d_idx_min:nrow(curr_points), ]

            nb_points_bc_before <-
              min(points_d$index) - max(points_a$index) - 1

            from_point <- c(
              curr_nodes[before_idx, ]$lng,
              curr_nodes[before_idx, ]$lat
            )
            to_point <- c(
              st_coordinates(clicked_stop)[1],
              st_coordinates(clicked_stop)[2]
            )

            segment_b <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            points_b <-
              segment_b[2:nrow(segment_b), ] |>
              mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

            points_b_idx_max <- max(points_b$index)

            from_point <- c(
              st_coordinates(clicked_stop)[1],
              st_coordinates(clicked_stop)[2]
            )
            to_point <- c(
              curr_nodes[after_idx, ]$lng,
              curr_nodes[after_idx, ]$lat
            )

            segment_c <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            points_c <-
              segment_c[2:(nrow(segment_c) - 1), ] |>
              mutate(index = row_number() + points_b_idx_max, .before = "lng")

            points_bc <- rbind(points_b, points_c)

            nb_points_bc_after <- nrow(points_bc)
            adj_index_d <- nb_points_bc_after - nb_points_bc_before

            points_d <-
              points_d |>
              mutate(index = index + adj_index_d)

            nodes_d <-
              nodes_d |>
              mutate(index = index + adj_index_d)

            node_bc <-
              data.frame(
                node_id = selected_point_index(),
                lng = st_coordinates(clicked_stop)[1],
                lat = st_coordinates(clicked_stop)[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1,
                index = points_b_idx_max
              )

            curr_points <- rbind(points_a, points_b, points_c, points_d)
            curr_nodes <- rbind(nodes_a, node_bc, nodes_d)

            row.names(curr_points) <- 1:nrow(curr_points)
            row.names(curr_nodes) <- 1:nrow(curr_nodes)
          }

          route_points(curr_points)
          route_nodes(curr_nodes)
          selected_point_index(NULL)

          showNotification(
            "Waypoint moved to stop & adopted stop properties.",
            type = "message"
          )
        } else {
          stop_coords <- st_coordinates(clicked_stop$geometry)

          if (nrow(curr_nodes) >= 1) {
            if (clicked_stop$stop_id %in% curr_nodes$stop_id) {
              showNotification(
                "Stop already in route stop sequence. Cannot add stop again.",
                type = "warning"
              )
              return()
            }

            if (isTRUE(prepend_mode())) {
              # --- PREPEND: add clicked stop BEFORE the current first node ---
              from_point <- c(stop_coords[1], stop_coords[2])
              to_point <- c(curr_nodes[1, ]$lng, curr_nodes[1, ]$lat)

              new_segment <- generateRouteSegment(
                from_point,
                to_point,
                drawing_mode = drawing_mode_reactive(),
                routing_server = routing_server()
              )

              # All points except the last one become the new leading points;
              # the last point of the segment coincides with the old first node,
              # which already exists in curr_points at its old position.
              nb_new_points <- nrow(new_segment) - 1

              new_points <- new_segment[1:nb_new_points, ] |>
                mutate(index = row_number(), .before = "lng")

              # Shift all existing points' indices up by nb_new_points
              curr_points <- curr_points |>
                mutate(index = index + nb_new_points)

              curr_points <- rbind(new_points, curr_points)
              row.names(curr_points) <- 1:nrow(curr_points)

              # Shift existing node indices up by nb_new_points; the new node
              # takes index = 1 (matching the first point).
              curr_nodes <- curr_nodes |>
                mutate(index = index + nb_new_points)

              new_node <- data.frame(
                node_id = 1,
                lng = stop_coords[1],
                lat = stop_coords[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1,
                index = 1,
                stringsAsFactors = FALSE
              )

              curr_nodes <- rbind(new_node, curr_nodes) |>
                mutate(node_id = row_number())
              row.names(curr_nodes) <- 1:nrow(curr_nodes)

              route_points(curr_points)
              route_nodes(curr_nodes)
            } else {
              #Default append mode : add stops to end of sequence

              nodes_a_idx_max <- max(curr_nodes$index)

              from_lng <- curr_nodes[nrow(curr_nodes), ]$lng
              from_lat <- curr_nodes[nrow(curr_nodes), ]$lat

              from_point <- c(from_lng, from_lat)
              to_point <- c(stop_coords[1], stop_coords[2])

              new_segment <- generateRouteSegment(
                from_point,
                to_point,
                drawing_mode = drawing_mode_reactive(),
                routing_server = routing_server()
              )

              new_points <-
                new_segment[2:nrow(new_segment), ] |>
                mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

              curr_points <- rbind(curr_points, new_points)
              row.names(curr_points) <- 1:nrow(curr_points)

              new_node_index <- max(curr_points$index)

              new_node <- data.frame(
                node_id = max(curr_nodes$node_id) + 1,
                lng = stop_coords[1],
                lat = stop_coords[2],
                is_stop = TRUE,
                stop_id = clicked_stop$stop_id,
                stop_name = clicked_stop$stop_name,
                speed_factor = 1,
                index = new_node_index,
                stringsAsFactors = FALSE
              )

              curr_nodes <- rbind(curr_nodes, new_node)

              route_points(curr_points)
              route_nodes(curr_nodes)
            }
          } else {
            # first ever node
            route_nodes(data.frame(
              node_id = 1,
              lng = stop_coords[1],
              lat = stop_coords[2],
              is_stop = TRUE,
              stop_id = clicked_stop$stop_id,
              stop_name = clicked_stop$stop_name,
              speed_factor = 1,
              index = 1,
              stringsAsFactors = FALSE
            ))
            route_points(data.frame(
              index = 1,
              lng = stop_coords[1],
              lat = stop_coords[2]
            ))
          }
        }

        current_sequence(generateStopSequenceFromNodes(
          route_nodes(),
          active_itin_id()
        ))
      }
    })

    # Map click handler
    observeEvent(input$routes_map_click, {
      click <- input$routes_map_click

      # EDITING MODE
      if (!is.null(active_itin_id())) {
        current_time <- as.numeric(Sys.time())
        time_since_marker_click <- current_time - last_marker_click_time()

        if (time_since_marker_click < 0.1) {
          return()
        }

        curr_nodes <- route_nodes()
        curr_points <- route_points()

        if (!is.null(selected_point_index())) {
          idx <- which(curr_nodes$node_id == selected_point_index())

          if (nrow(curr_nodes) == 1) {
            curr_nodes$lat <- click$lat
            curr_nodes$lng <- click$lng
            curr_points$lat <- click$lat
            curr_points$lng <- click$lng
          } else if (idx == 1) {
            nb_points_before <- curr_nodes[2, ]$index - 1

            from_point <- c(click$lng, click$lat)
            to_point <- c(curr_nodes[2, ]$lng, curr_nodes[2, ]$lat)

            new_segment <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            new_points <-
              new_segment[1:(nrow(new_segment) - 1), ] |>
              mutate(index = row_number(), .before = "lng")

            adj_index <- nrow(new_points) - nb_points_before

            curr_points <-
              rbind(
                new_points,
                curr_points[(nb_points_before + 1):nrow(curr_points), ] |>
                  mutate(index = index + adj_index)
              )

            row.names(curr_points) <- 1:nrow(curr_points)

            curr_nodes[1, ]$lng <- click$lng
            curr_nodes[1, ]$lat <- click$lat

            curr_nodes <-
              rbind(
                curr_nodes[1, ],
                curr_nodes[2:nrow(curr_nodes), ] |>
                  mutate(index = index + adj_index)
              )
          } else if (idx == nrow(curr_nodes)) {
            from_point <- c(
              curr_nodes[idx - 1, ]$lng,
              curr_nodes[idx - 1, ]$lat
            )
            to_point <- c(click$lng, click$lat)

            new_segment <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            nb_points_retained <- curr_nodes[idx - 1, ]$index

            new_points <-
              new_segment[2:(nrow(new_segment)), ] |>
              mutate(index = row_number() + nb_points_retained, .before = "lng")

            curr_points <-
              rbind(
                curr_points[1:nb_points_retained, ],
                new_points
              )

            row.names(curr_points) <- 1:nrow(curr_points)

            curr_nodes[idx, ]$lng <- click$lng
            curr_nodes[idx, ]$lat <- click$lat
            curr_nodes[idx, ]$index <- max(curr_points$index)
          } else {
            before_idx <- idx - 1
            after_idx <- idx + 1

            nodes_a <- curr_nodes[1:before_idx, ]
            nodes_a_idx_max <- max(nodes_a$index)
            points_a <- curr_points[1:nodes_a_idx_max, ]

            nodes_d <- curr_nodes[after_idx:nrow(curr_nodes), ]
            nodes_d_idx_min <- min(nodes_d$index)
            points_d <- curr_points[nodes_d_idx_min:nrow(curr_points), ]

            nb_points_bc_before <-
              min(points_d$index) - max(points_a$index) - 1

            from_point <- c(
              curr_nodes[before_idx, ]$lng,
              curr_nodes[before_idx, ]$lat
            )
            to_point <- c(click$lng, click$lat)

            segment_b <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            points_b <-
              segment_b[2:nrow(segment_b), ] |>
              mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

            points_b_idx_max <- max(points_b$index)

            from_point <- c(click$lng, click$lat)
            to_point <- c(
              curr_nodes[after_idx, ]$lng,
              curr_nodes[after_idx, ]$lat
            )

            segment_c <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            points_c <-
              segment_c[2:(nrow(segment_c) - 1), ] |>
              mutate(index = row_number() + points_b_idx_max, .before = "lng")

            points_bc <- rbind(points_b, points_c)

            nb_points_bc_after <- nrow(points_bc)
            adj_index_d <- nb_points_bc_after - nb_points_bc_before

            points_d <-
              points_d |>
              mutate(index = index + adj_index_d)

            nodes_d <-
              nodes_d |>
              mutate(index = index + adj_index_d)

            node_bc <-
              data.frame(
                node_id = idx,
                lng = click$lng,
                lat = click$lat,
                is_stop = FALSE,
                stop_id = "",
                stop_name = "",
                speed_factor = NA_real_,
                index = points_b_idx_max
              )

            curr_points <- rbind(points_a, points_b, points_c, points_d)
            curr_nodes <- rbind(nodes_a, node_bc, nodes_d)

            row.names(curr_points) <- 1:nrow(curr_points)
            row.names(curr_nodes) <- 1:nrow(curr_nodes)
          }

          route_points(curr_points)
          route_nodes(curr_nodes)
          selected_point_index(NULL)
          showNotification("Waypoint moved", type = "message")
        } else if (nrow(curr_nodes) >= 1) {
          if (nrow(curr_nodes) >= 2) {
            point_added <- FALSE

            for (i in 1:(nrow(curr_points) - 1)) {
              p1 <- curr_points[i, ]
              p2 <- curr_points[i + 1, ]

              d <- abs(
                (p2$lat - p1$lat) *
                  click$lng -
                  (p2$lng - p1$lng) * click$lat +
                  p2$lng * p1$lat -
                  p2$lat * p1$lng
              ) /
                sqrt((p2$lat - p1$lat)^2 + (p2$lng - p1$lng)^2)

              within_bounds <- (min(p1$lng, p2$lng) <= click$lng &&
                click$lng <= max(p1$lng, p2$lng) &&
                min(p1$lat, p2$lat) <= click$lat &&
                click$lat <= max(p1$lat, p2$lat))

              if (
                d < calculateThreshold(current_zoom()) &&
                  within_bounds
              ) {
                new_pt_idx <- p1$index + 1

                new_point <- data.frame(
                  index = new_pt_idx,
                  lng = click$lng,
                  lat = click$lat
                )

                new_points <- rbind(
                  curr_points[1:i, ],
                  new_point,
                  curr_points[(i + 1):nrow(curr_points), ]
                )

                new_points$index <- 1:nrow(new_points)
                curr_points <- new_points

                nodes_a <-
                  curr_nodes |>
                  filter(index <= i)

                nodes_b <-
                  curr_nodes |>
                  filter(index > i) |>
                  mutate(
                    node_id = node_id + 1,
                    index = index + 1
                  )

                new_node <-
                  data.frame(
                    node_id = max(nodes_a$node_id) + 1,
                    lng = click$lng,
                    lat = click$lat,
                    is_stop = FALSE,
                    stop_id = "",
                    stop_name = "",
                    speed_factor = NA_real_,
                    index = new_pt_idx
                  )

                curr_nodes <- rbind(nodes_a, new_node, nodes_b)

                row.names(curr_points) <- 1:nrow(curr_points)
                row.names(curr_nodes) <- 1:nrow(curr_nodes)

                point_added <- TRUE

                route_points(curr_points)
                route_nodes(curr_nodes)

                showNotification("Waypoint added along route", type = "message")

                break
              }
            }

            if (!point_added) {
              nodes_a_idx_max <- max(curr_nodes$index)

              from_lng <- curr_nodes[nrow(curr_nodes), ]$lng
              from_lat <- curr_nodes[nrow(curr_nodes), ]$lat

              from_point <- c(from_lng, from_lat)
              to_point <- c(click$lng, click$lat)

              new_segment <- generateRouteSegment(
                from_point,
                to_point,
                drawing_mode = drawing_mode_reactive(),
                routing_server = routing_server()
              )

              new_points <-
                new_segment[2:nrow(new_segment), ] |>
                mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

              curr_points <- rbind(curr_points, new_points)
              row.names(curr_points) <- 1:nrow(curr_points)

              new_node_index <- max(curr_points$index)

              new_node <- data.frame(
                node_id = max(curr_nodes$node_id) + 1,
                lng = click$lng,
                lat = click$lat,
                is_stop = FALSE,
                stop_id = "",
                stop_name = "",
                speed_factor = NA_real_,
                index = new_node_index,
                stringsAsFactors = FALSE
              )

              curr_nodes <- rbind(curr_nodes, new_node)

              route_points(curr_points)
              route_nodes(curr_nodes)
            }
          } else if (nrow(curr_nodes) == 1) {
            nodes_a_idx_max <- max(curr_nodes$index)

            from_lng <- curr_nodes[nrow(curr_nodes), ]$lng
            from_lat <- curr_nodes[nrow(curr_nodes), ]$lat

            from_point <- c(from_lng, from_lat)
            to_point <- c(click$lng, click$lat)

            new_segment <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            new_points <-
              new_segment[2:nrow(new_segment), ] |>
              mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

            curr_points <- rbind(curr_points, new_points)
            row.names(curr_points) <- 1:nrow(curr_points)

            new_node_index <- max(curr_points$index)

            new_node <- data.frame(
              node_id = max(curr_nodes$node_id) + 1,
              lng = click$lng,
              lat = click$lat,
              is_stop = FALSE,
              stop_id = "",
              stop_name = "",
              speed_factor = NA_real_,
              index = new_node_index,
              stringsAsFactors = FALSE
            )

            curr_nodes <- rbind(curr_nodes, new_node)

            route_points(curr_points)
            route_nodes(curr_nodes)
          } else {
            showNotification(
              "Click on a stop to start your route",
              type = "warning"
            )
          }
        }
      } else {
        # BROWSE MODE
        current_data <- ssfs()
        if (is.null(current_data$itin) || nrow(current_data$itin) == 0) {
          return()
        }

        click_point <- sf::st_sfc(
          sf::st_point(c(click$lng, click$lat)),
          crs = 4326
        )

        zoom <- current_zoom()
        threshold_m <- if (!is.null(zoom) && zoom >= 10) {
          200 / (2^(zoom - 12))
        } else {
          200
        }

        distances <- as.numeric(sf::st_distance(
          current_data$itin$geometry,
          click_point
        ))

        nearby_idx <- which(distances <= threshold_m)

        if (length(nearby_idx) == 0) {
          leaflet::leafletProxy("routes_map") |>
            leaflet::clearPopups()

          highlighted_itin_ids(character(0))
          return()
        }

        nearby_itins <- current_data$itin[nearby_idx, ]

        route_groups <- split(
          seq_len(nrow(nearby_itins)),
          nearby_itins$route_id
        )

        popup_sections <- vapply(
          names(route_groups),
          function(rid) {
            idxs <- route_groups[[rid]]
            route_row <- current_data$routes[
              current_data$routes$route_id == rid,
            ]

            short_name <- if (nrow(route_row) > 0) {
              htmltools::htmlEscape(route_row$route_short_name[1])
            } else {
              ""
            }
            long_name <- if (nrow(route_row) > 0) {
              htmltools::htmlEscape(route_row$route_long_name[1])
            } else {
              ""
            }

            rcol <- if (
              nrow(route_row) > 0 &&
                !is.na(route_row$route_color[1]) &&
                nchar(route_row$route_color[1]) > 0
            ) {
              paste0("#", route_row$route_color[1])
            } else {
              "#05AEEF"
            }

            itin_lines <- vapply(
              idxs,
              function(j) {
                iid <- htmltools::htmlEscape(nearby_itins$itin_id[j])
                headsign <- nearby_itins$trip_headsign[j]
                headsign_text <- if (
                  !is.na(headsign) && nchar(trimws(headsign)) > 0
                ) {
                  paste0(" - ", htmltools::htmlEscape(trimws(headsign)))
                } else {
                  ""
                }
                paste0(iid, headsign_text)
              },
              character(1)
            )

            paste0(
              "<span style='color:",
              rcol,
              "; font-size:14px;'>\u25CF</span> ",
              "<b>",
              short_name,
              " - ",
              long_name,
              "</b>",
              "<br><span style='font-size:10px; color:grey;'>",
              paste(itin_lines, collapse = "<br>"),
              "</span>"
            )
          },
          character(1)
        )

        popup_html <- paste0(
          "<div style='font-size:11px; line-height:1.6;'>",
          paste(popup_sections, collapse = "<hr style='margin:4px 0;'>"),
          "</div>"
        )

        leaflet::leafletProxy("routes_map") |>
          leaflet::clearPopups() |>
          leaflet::addPopups(
            lng = click$lng,
            lat = click$lat,
            popup = popup_html,
            options = leaflet::popupOptions(
              closeButton = TRUE,
              maxWidth = 300
            )
          )

        highlighted_itin_ids(nearby_itins$itin_id)
      }
    })

    # Right-click handler to remove nodes
    observeEvent(input$routes_map_right_click, {
      req(active_itin_id())

      click <- input$routes_map_right_click
      curr_nodes <- route_nodes()
      curr_points <- route_points()

      #DEBUG
      assign("curr_nodes_i", curr_nodes, envir = .GlobalEnv)
      assign("curr_points_i", curr_points, envir = .GlobalEnv)
      assign("click_i", click, envir = .GlobalEnv)
      assign("current_zoom_i", current_zoom(), envir = .GlobalEnv)

      if (nrow(curr_nodes) == 0) {
        return()
      }

      distances <- sqrt(
        (curr_nodes$lng - click$lng)^2 + (curr_nodes$lat - click$lat)^2
      )
      closest_idx <- which.min(distances)

      if (distances[closest_idx] < calculateThreshold(current_zoom())) {
        if (closest_idx == 1) {
          # REMOVING FIRST NODE OR ONLY NODE
          if (nrow(curr_nodes) > 1) {
            curr_nodes <- curr_nodes[-closest_idx, ]
            curr_nodes$node_id <- 1:nrow(curr_nodes)

            index_adj <- curr_nodes[1, ]$index - 1

            curr_nodes <- curr_nodes |> mutate(index = index - index_adj)

            curr_points <-
              curr_points[(index_adj + 1):nrow(curr_points), ] |>
              mutate(index = row_number())

            row.names(curr_nodes) <- 1:nrow(curr_nodes)
            row.names(curr_points) <- 1:nrow(curr_points)

            route_nodes(curr_nodes)
            route_points(curr_points)
          } else {
            route_nodes(data.frame(
              node_id = integer(),
              lng = numeric(),
              lat = numeric(),
              is_stop = logical(),
              stop_id = character(),
              stop_name = character(),
              speed_factor = double(),
              index = integer(),
              stringsAsFactors = FALSE
            ))
            route_points(data.frame(
              index = integer(),
              lng = numeric(),
              lat = numeric()
            ))
          }

          # REMOVING LAST NODE
        } else if (closest_idx == nrow(curr_nodes)) {
          #LAST NODE
          curr_nodes <- curr_nodes[-closest_idx, ]
          row.names(curr_nodes) <- 1:nrow(curr_nodes)

          if (nrow(curr_nodes) > 0) {
            max_index <- max(curr_nodes$index)
            curr_points <-
              curr_points |>
              filter(index <= max_index)
            row.names(curr_points) <- 1:nrow(curr_points)
          } else {
            curr_points <- data.frame(
              index = integer(),
              lng = numeric(),
              lat = numeric()
            )
          }

          route_nodes(curr_nodes)
          route_points(curr_points)
        } else if (nrow(curr_nodes) > 2) {
          # REMOVING NODE IN MIDDLE
          before_idx <- closest_idx - 1
          after_idx <- closest_idx + 1

          nodes_a <- curr_nodes[1:before_idx, ]
          nodes_a_idx_max <- max(nodes_a$index)
          points_a <- curr_points[1:nodes_a_idx_max, ]

          nodes_c <- curr_nodes[after_idx:nrow(curr_nodes), ]
          nodes_c_idx_min <- min(nodes_c$index)
          points_c <- curr_points[nodes_c_idx_min:nrow(curr_points), ]

          nb_points_b_before <-
            min(points_c$index) - max(points_a$index) - 1

          if (drawing_mode_reactive() == "free") {
            #if drawing mode is free, then there is no need to calculate the segment in between. It disappears.
            nb_points_b_after <- 0
          } else {
            #Draw by network : calculate segment

            from_point <- c(
              curr_nodes[before_idx, ]$lng,
              curr_nodes[before_idx, ]$lat
            )
            to_point <- c(
              curr_nodes[after_idx, ]$lng,
              curr_nodes[after_idx, ]$lat
            )

            segment_b <- generateRouteSegment(
              from_point,
              to_point,
              drawing_mode = drawing_mode_reactive(),
              routing_server = routing_server()
            )

            #check : segment_b must contain at least 3 rows. Otherwise, nb_points_b_after is 0
            if (nrow(segment_b) < 3) {
              nb_points_b_after <- 0
            } else {
              points_b <-
                segment_b[2:(nrow(segment_b) - 1), ] |>
                mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

              nb_points_b_after <- nrow(points_b)
            }
          }
          adj_index_c <- nb_points_b_after - nb_points_b_before

          points_c <-
            points_c |>
            mutate(index = index + adj_index_c)

          nodes_c <-
            nodes_c |>
            mutate(index = index + adj_index_c)

          if (nb_points_b_after == 0) {
            curr_points <- rbind(points_a, points_c)
          } else {
            curr_points <- rbind(points_a, points_b, points_c)
          }

          curr_nodes <-
            rbind(nodes_a, nodes_c) |>
            mutate(node_id = row_number())

          row.names(curr_points) <- 1:nrow(curr_points)
          row.names(curr_nodes) <- 1:nrow(curr_nodes)

          route_points(curr_points)
          route_nodes(curr_nodes)
        }

        current_sequence(generateStopSequenceFromNodes(
          route_nodes(),
          active_itin_id()
        ))
        showNotification("Node removed", type = "message")
      }
    })

    # Backspace key handler
    observeEvent(input$backspace_pressed, {
      curr_nodes <- route_nodes()
      curr_points <- route_points()

      if (nrow(curr_nodes) > 0) {
        curr_nodes <-
          curr_nodes[-nrow(curr_nodes), ]

        row.names(curr_nodes) <- 1:nrow(curr_nodes)

        if (nrow(curr_nodes) > 0) {
          max_index <- max(curr_nodes$index)
          curr_points <-
            curr_points |>
            filter(index <= max_index)
        } else {
          curr_points <- data.frame(
            index = integer(),
            lng = numeric(),
            lat = numeric()
          )
        }

        route_nodes(curr_nodes)
        route_points(curr_points)
        current_sequence(generateStopSequenceFromNodes(
          route_nodes(),
          active_itin_id()
        ))

        showNotification("Last node removed", type = "message")
      }
    })

    # --- Stop sequence table rendering ---

    output$selected_stops_table <- DT::renderDT({
      req(current_sequence())

      # Select only the columns we want to display, in display order.
      display_df <- current_sequence()[,
        c("stop_sequence", "stop_name"),
        drop = FALSE
      ]

      DT::datatable(
        display_df,
        selection = "single",
        rownames = FALSE,
        colnames = c("#" = "stop_sequence", "Stop name" = "stop_name"),
        options = list(
          pageLength = -1,
          dom = "t",
          ordering = FALSE
        )
      )
    })
  })
}
