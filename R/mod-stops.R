# UI
stopsUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "stops",
    fluidPage(
      titlePanel("stops"),
      # Map container with floating panels
      div(
        class = "map-container",
        # Full-width map
        leaflet::leafletOutput(
          ns("stops_map"),
          height = "100%",
          width = "100%"
        ),

        # Floating control panel (left side)
        div(
          id = "stops-control-panel",
          class = "floating-panel floating-panel-left",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('stops-control-panel')",
            h4("Stops"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            # Search bar
            div(
              class = "stop-search-container",
              tags$input(
                type = "text",
                id = "stop_search",
                placeholder = "Search stops..."
              )
            ),

            # Editing instruction (shown when editing)
            uiOutput(ns("stops_editing_instruction")),

            # Stop list
            div(class = "stop-list-container", uiOutput(ns("stop_list_ui")))
          )
        ),

        # Import/Export floating panel (bottom-right)
        div(
          id = "stops-import-export-panel",
          class = "floating-panel floating-panel-bottom-right panel-import-export",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('stops-import-export-panel')",
            h4("Import / Export / Generate"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            h5("Import Stops"),
            fileInput(
              ns("stops_import_file"),
              label = NULL,
              accept = c(".geojson", ".kml"),
              placeholder = "GeoJSON or KML file"
            ),
            actionButton(
              ns("stops_import_confirm"),
              "Import",
              class = "btn-success btn-sm"
            ),
            hr(),
            h5("Export Stops"),
            selectInput(
              ns("stops_export_format"),
              label = NULL,
              choices = c(
                "GeoJSON" = "geojson",
                "KML" = "kml",
                "Shapefile" = "shp"
              ),
              selected = "geojson"
            ),
            downloadButton(
              ns("stops_export_download"),
              "Download",
              class = "btn-primary btn-sm"
            ),
            hr(),
            h5(tagList(
              "Auto-generate stops",
              info_popover(
                paste(
                  "Automatically generate stops at road intersections",
                  "within a drawn zone using OpenStreetMap data.",
                  "Stops are placed at intersections based on minimum",
                  "stop spacing set in",
                  icon("gear"),
                  "Settings."
                )
              )
            )),
            uiOutput(ns("stops_generate_ui"))
          )
        )
      )
    )
  )
}

# Server
stopsServer <- function(
  id,
  ssfs,
  map_center,
  current_zoom,
  min_stop_dist,
  osm_provider
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Send namespace prefix to JS
    session$sendCustomMessage("setStopsNs", ns(""))

    # Reactive values for stops editing state
    stops_temp_point <- reactiveVal(NULL)
    stops_editing_id <- reactiveVal(NULL)
    stops_adding_new <- reactiveVal(FALSE)
    stops_search_term <- reactiveVal("")

    # Temporary storage for edit field values
    stops_edit_stop_id <- reactiveVal("")
    stops_edit_stop_name <- reactiveVal("")

    # Track previously editing stop for marker restore
    prev_stops_editing_id <- reactiveVal(NULL)

    # Check if map is ready
    stops_map_ready <- reactiveVal(FALSE)

    # Check previous itin
    prev_itin_hash <- reactiveVal(NULL)

    # Stop generation reactive values
    stops_generate_mode <- reactiveVal(FALSE)
    stops_generate_polygon <- reactiveVal(NULL)
    stops_draw_vertices <- reactiveVal(list())
    autostop_batch_id <- reactiveVal(1L)

    # Handle stop search input
    observeEvent(
      input$stop_search_term,
      {
        stops_search_term(input$stop_search_term)
      },
      ignoreNULL = FALSE
    )

    # --- Initialize stops map ---
    output$stops_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE)) |>
        addBaseMaps() |>
        leaflet::addMapPane("stops_shapes_pane", zIndex = 410) |>
        leaflet::addMapPane("stops_markers_pane", zIndex = 430) |>
        leaflet::addMapPane("stops_temp_pane", zIndex = 450) |>
        leaflet::setView(lng = center$lng, lat = center$lat, zoom = 12) |>
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

            function resizeMarkers() {
              var zoom = map.getZoom();
              var r = calcMarkerSize(zoom);
              map.eachLayer(function(layer) {
                if (layer.options && layer.options.className !== 'temp-marker' &&
                    typeof layer.setRadius === 'function') {
                  layer.setRadius(r);
                }
              });
            }

            map.on('zoomend', function(e) {
              Shiny.setInputValue(ns + 'stops_map_zoom', map.getZoom());
              resizeMarkers();
            });
          }
          ",
          ns("")
        ))
    })

    # Observer for level of zoom on stops map
    observeEvent(input$stops_map_zoom, {
      current_zoom(input$stops_map_zoom)
    })

    observeEvent(
      map_center(),
      {
        stops_map_ready(FALSE)
      },
      priority = 10
    )

    observeEvent(
      input$stops_map_bounds,
      {
        stops_map_ready(TRUE)
      },
      once = FALSE
    )

    # ---- Itinerary shapes ----
    observe({
      req(stops_map_ready())
      current_data <- ssfs()

      # Skip redraw if visible itinerary or route styling data hasn't changed
      new_hash <- digest::digest(list(
        itin = current_data$itin,
        routes = current_data$routes[,
          c("route_id", "route_short_name", "route_color", "route_type"),
          drop = FALSE
        ]
      ))
      if (identical(new_hash, isolate(prev_itin_hash()))) {
        return()
      }
      prev_itin_hash(new_hash)

      proxy <- leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("shapes")

      if (nrow(current_data$itin) > 0) {
        draw_order <- itineraryDrawOrder(
          current_data$itin,
          current_data$routes
        )

        for (i in draw_order) {
          line_coords <- st_coordinates(current_data$itin$geometry[i])
          route_id_i <- current_data$itin$route_id[i]

          route_row <- current_data$routes[
            current_data$routes$route_id == route_id_i,
          ]

          route_color_i <- route_row$route_color
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
              group = "shapes",
              options = leaflet::pathOptions(pane = "stops_shapes_pane"),
              color = line_color,
              weight = line_weight,
              opacity = 0.5
            )
        }
      }
    })

    # ---- Stop markers ----
    observe({
      req(stops_map_ready())
      current_data <- ssfs()
      editing_id <- isolate(stops_editing_id())

      proxy <- leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("stops")

      if (nrow(current_data$stops) > 0) {
        stops_to_show <- current_data$stops
        if (!is.null(editing_id)) {
          stops_to_show <- stops_to_show[stops_to_show$stop_id != editing_id, ]
        }

        if (nrow(stops_to_show) > 0) {
          # Build hover labels with stop info and associated itin_ids
          stop_itin_lookup <- current_data$stop_seq |>
            group_by(stop_id) |>
            summarise(
              itin_ids = paste(unique(itin_id), collapse = ", "),
              .groups = "drop"
            )

          stops_df <- merge(
            as.data.frame(stops_to_show)[,
              c("stop_id", "stop_name"),
              drop = FALSE
            ],
            stop_itin_lookup,
            by = "stop_id",
            all.x = TRUE,
            sort = FALSE
          )

          # Preserves row order
          stops_df <- stops_df[match(stops_to_show$stop_id, stops_df$stop_id), ]

          itin_text <- ifelse(
            is.na(stops_df$itin_ids),
            "None",
            stops_df$itin_ids
          )

          hover_labels <- lapply(
            paste0(
              "<span style='font-size:11px;'><b>",
              htmltools::htmlEscape(stops_df$stop_id),
              "</b> \u2014 ",
              htmltools::htmlEscape(stops_df$stop_name),
              "<br>Itineraries: ",
              htmltools::htmlEscape(itin_text),
              "</span>"
            ),
            htmltools::HTML
          )

          proxy <- proxy |>
            leaflet::addCircleMarkers(
              data = stops_to_show,
              layerId = ~stop_id,
              color = "white",
              weight = 1,
              stroke = TRUE,
              fillColor = "#7f7f7f",
              fillOpacity = 0.7,
              radius = calculateMarkerSize(isolate(current_zoom())),
              label = hover_labels,
              labelOptions = leaflet::labelOptions(
                style = list("font-size" = "11px", "padding" = "3px 6px"),
                direction = "top",
                offset = c(0, -8)
              ),
              group = "stops",
              options = leaflet::pathOptions(pane = "stops_markers_pane")
            )
        }
      }
    })

    # ---- Hide/show single marker on edit state change ----
    observeEvent(
      stops_editing_id(),
      {
        req(stops_map_ready())
        editing_id <- stops_editing_id()
        prev_id <- prev_stops_editing_id()

        # Re-add previously hidden marker
        if (!is.null(prev_id)) {
          current_data <- isolate(ssfs())
          stop_row <- current_data$stops[
            current_data$stops$stop_id == prev_id,
          ]
          if (nrow(stop_row) > 0) {
            leaflet::leafletProxy("stops_map") |>
              leaflet::addCircleMarkers(
                data = stop_row,
                layerId = ~stop_id,
                color = "white",
                weight = 1,
                stroke = TRUE,
                fillColor = "#7f7f7f",
                fillOpacity = 0.7,
                radius = calculateMarkerSize(isolate(current_zoom())),
                group = "stops",
                options = leaflet::pathOptions(pane = "stops_markers_pane")
              )
          }
        }

        # Remove the marker being edited
        if (!is.null(editing_id)) {
          leaflet::leafletProxy("stops_map") |>
            leaflet::removeMarker(editing_id)
        }

        prev_stops_editing_id(editing_id)
      },
      ignoreNULL = FALSE
    )

    # ---- Temporary stop marker (when editing) ----
    observe({
      req(stops_map_ready())
      temp <- stops_temp_point()

      proxy <- leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("temp_marker")

      if (!is.null(temp)) {
        icon_size <- as.integer(
          (calculateMarkerSize(isolate(current_zoom())) + 4) * 2
        )

        # Create SVG circle as data URI
        svg_string <- sprintf(
          '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d"><circle cx="%d" cy="%d" r="%d" fill="#B2182B" stroke="white" stroke-width="2"/></svg>',
          icon_size,
          icon_size,
          as.integer(icon_size / 2),
          as.integer(icon_size / 2),
          as.integer((icon_size / 2) - 2)
        )

        icon_url <- paste0(
          "data:image/svg+xml,",
          URLencode(svg_string, reserved = TRUE)
        )

        red_circle_icon <- leaflet::makeIcon(
          iconUrl = icon_url,
          iconWidth = icon_size,
          iconHeight = icon_size,
          iconAnchorX = as.integer(icon_size / 2),
          iconAnchorY = as.integer(icon_size / 2)
        )

        proxy |>
          leaflet::addMarkers(
            lng = temp[1],
            lat = temp[2],
            layerId = "temp_drag",
            icon = red_circle_icon,
            options = leaflet::markerOptions(
              draggable = TRUE,
              pane = "stops_temp_pane"
            ),
            group = "temp_marker",
          )
      }
    })

    # Render editing instruction
    output$stops_editing_instruction <- renderUI({
      if (!is.null(stops_editing_id()) || stops_adding_new()) {
        if (is.null(stops_temp_point())) {
          div(
            class = "editing-instruction",
            "Click on the map to place the stop"
          )
        } else {
          div(
            class = "editing-instruction",
            "Drag the marker to adjust position"
          )
        }
      } else {
        NULL
      }
    })

    # Render stop list UI
    output$stop_list_ui <- renderUI({
      current_data <- ssfs()
      editing_id <- stops_editing_id()
      adding_new <- stops_adding_new()
      edit_id_val <- stops_edit_stop_id()
      edit_name_val <- stops_edit_stop_name()
      search_term <- stops_search_term()

      rows <- list()

      if (adding_new) {
        rows[[length(rows) + 1]] <- build_stop_form(edit_id_val, edit_name_val)
      } else {
        rows[[length(rows) + 1]] <- div(
          class = "stop-list-row add-row",
          onclick = "startAddingStop()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); startAddingStop()",
            title = "Add new stop",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new stop")
        )
      }

      if (!is.null(editing_id) && !adding_new) {
        rows[[length(rows) + 1]] <- build_stop_form(
          edit_id_val,
          edit_name_val,
          is_new = FALSE
        )
      }

      if (nrow(current_data$stops) > 0) {
        stops_df <- current_data$stops |> as.data.frame()

        if (!is.null(search_term) && search_term != "") {
          search_lower <- tolower(search_term)
          stops_df <- stops_df |>
            filter(
              grepl(search_lower, tolower(stop_name), fixed = TRUE) |
                grepl(search_lower, tolower(stop_id), fixed = TRUE)
            )
        }

        if (!is.null(editing_id)) {
          stops_df <- stops_df |> filter(stop_id != editing_id)
        }

        if (nrow(stops_df) > 0) {
          rows_html <- paste0(
            "<div class='stop-list-row' onclick=\"viewStopFromList('",
            htmltools::htmlEscape(stops_df$stop_id),
            "')\">",
            "<div class='stop-info'><div class='stop-info-display'>",
            "<span class='stop-name'>",
            htmltools::htmlEscape(stops_df$stop_name),
            "</span>",
            "<span class='stop-id-display'>(",
            htmltools::htmlEscape(stops_df$stop_id),
            ")</span>",
            "</div></div>",
            "<div class='stop-actions'>",
            "<button class='stop-action-btn edit-btn' onclick=\"event.stopPropagation(); editStopFromList('",
            htmltools::htmlEscape(stops_df$stop_id),
            "')\" title='Edit'>&#9998;</button>",
            "<button class='stop-action-btn delete-btn' onclick=\"event.stopPropagation(); deleteStopFromList('",
            htmltools::htmlEscape(stops_df$stop_id),
            "')\" title='Delete stop'><i class='fa-solid fa-trash'></i></button>",
            "</div>",
            "</div>",
            collapse = ""
          )
          rows[[length(rows) + 1]] <- htmltools::HTML(rows_html)
        } else if (
          !is.null(search_term) && search_term != "" && is.null(editing_id)
        ) {
          rows[[length(rows) + 1]] <- div(
            class = "stop-list-row",
            style = "justify-content: center; color: #888; font-style: italic;",
            "No stops match your search"
          )
        }
      }

      do.call(tagList, rows)
    })

    # Handle "Add new stop" click
    observeEvent(input$stop_list_add_click, {
      stops_editing_id(NULL)
      stops_temp_point(NULL)
      stops_adding_new(TRUE)
      stops_edit_stop_id("")
      stops_edit_stop_name("")
    })

    # Handle "Edit stop" click from list
    observeEvent(input$stop_list_edit_click, {
      stop_id <- input$stop_list_edit_click
      current_data <- ssfs()

      selected_stop <- current_data$stops[
        current_data$stops$stop_id == stop_id,
      ]
      if (nrow(selected_stop) > 0) {
        point_lng <- st_coordinates(selected_stop$geometry)[[1]]
        point_lat <- st_coordinates(selected_stop$geometry)[[2]]

        stops_adding_new(FALSE)
        stops_editing_id(stop_id)
        stops_temp_point(c(point_lng, point_lat))
        stops_edit_stop_id(selected_stop$stop_id)
        stops_edit_stop_name(selected_stop$stop_name)

        edit_zoom <- max(current_zoom(), 16)
        leaflet::leafletProxy("stops_map") |>
          leaflet::setView(lng = point_lng, lat = point_lat, zoom = edit_zoom)
      }
    })

    # Handle "View stop" click from list row (just centers map)
    observeEvent(input$stop_list_view_click, {
      stop_id <- input$stop_list_view_click
      current_data <- ssfs()

      selected_stop <- current_data$stops[
        current_data$stops$stop_id == stop_id,
      ]
      if (nrow(selected_stop) > 0) {
        point_lng <- st_coordinates(selected_stop$geometry)[[1]]
        point_lat <- st_coordinates(selected_stop$geometry)[[2]]

        view_zoom <- max(current_zoom(), 16)
        leaflet::leafletProxy("stops_map") |>
          leaflet::setView(lng = point_lng, lat = point_lat, zoom = view_zoom)
      }
    })

    # Handle map clicks
    observeEvent(input$stops_map_click, {
      click <- input$stops_map_click

      # Polygon drawing mode: capture vertex instead of placing a stop
      if (stops_generate_mode()) {
        verts <- stops_draw_vertices()
        verts[[length(verts) + 1]] <- c(click$lng, click$lat)
        stops_draw_vertices(verts)
        return()
      }

      if (!is.null(stops_editing_id()) || stops_adding_new()) {
        stops_temp_point(c(click$lng, click$lat))
      }
    })

    # Handle drag end for the temporary stop marker
    observeEvent(input$stops_map_marker_dragend, {
      drag_event <- input$stops_map_marker_dragend

      if (!is.null(drag_event$id) && drag_event$id == "temp_drag") {
        stops_temp_point(c(drag_event$lng, drag_event$lat))
      }
    })

    # Handle existing stop marker clicks (enters edit mode)
    observeEvent(input$stops_map_marker_click, {
      click <- input$stops_map_marker_click

      if (!is.null(click$id) && click$id != "temp") {
        current_data <- ssfs()
        selected_stop <- current_data$stops[
          current_data$stops$stop_id == click$id,
        ]

        if (nrow(selected_stop) > 0) {
          point_lng <- st_coordinates(selected_stop$geometry)[[1]]
          point_lat <- st_coordinates(selected_stop$geometry)[[2]]

          stops_adding_new(FALSE)
          stops_editing_id(click$id)
          stops_temp_point(c(point_lng, point_lat))
          stops_edit_stop_id(selected_stop$stop_id)
          stops_edit_stop_name(selected_stop$stop_name)

          edit_zoom <- max(current_zoom(), 16)
          leaflet::leafletProxy("stops_map") |>
            leaflet::setView(lng = point_lng, lat = point_lat, zoom = edit_zoom)

          shinyjs::runjs(
            "document.querySelector('.stop-list-container').scrollTop = 0;"
          )
        }
      }
    })

    # Handle save click
    observeEvent(input$stop_list_save_data, {
      temp <- stops_temp_point()

      save_data <- input$stop_list_save_data
      stop_id_val <- trimws(save_data$stop_id)
      stop_name_val <- trimws(save_data$stop_name)

      if (is.null(temp)) {
        showNotification(
          "Please click on the map to place the stop",
          type = "warning"
        )
        return()
      }

      if (is.null(stop_id_val) || stop_id_val == "") {
        showNotification("Please enter a Stop ID", type = "warning")
        return()
      }

      if (is.null(stop_name_val) || stop_name_val == "") {
        showNotification("Please enter a Stop Name", type = "warning")
        return()
      }

      current_data <- ssfs()
      editing_id <- stops_editing_id()
      adding_new <- stops_adding_new()

      if (adding_new || (!is.null(editing_id) && stop_id_val != editing_id)) {
        if (stop_id_val %in% current_data$stops$stop_id) {
          showNotification("A stop with this ID already exists", type = "error")
          return()
        }
      }

      new_stop <- st_sf(
        stop_id = stop_id_val,
        stop_name = stop_name_val,
        geometry = st_sfc(st_point(c(temp[1], temp[2])), crs = 4326),
        stringsAsFactors = FALSE
      )

      if (adding_new) {
        current_data$stops <- rbind(current_data$stops, new_stop)
        showNotification(paste("Stop", stop_id_val, "added"), type = "message")
      } else if (!is.null(editing_id)) {
        current_data$stops <- current_data$stops[
          current_data$stops$stop_id != editing_id,
        ]
        current_data$stops <- rbind(current_data$stops, new_stop)

        if (editing_id != stop_id_val) {
          current_data$stop_seq$stop_id[
            current_data$stop_seq$stop_id == editing_id
          ] <- stop_id_val
          if ("stop_name" %in% names(current_data$stop_seq)) {
            current_data$stop_seq$stop_name[
              current_data$stop_seq$stop_id == stop_id_val
            ] <- stop_name_val
          }
        } else if ("stop_name" %in% names(current_data$stop_seq)) {
          current_data$stop_seq$stop_name[
            current_data$stop_seq$stop_id == stop_id_val
          ] <- stop_name_val
        }

        showNotification(
          paste("Stop", stop_id_val, "updated"),
          type = "message"
        )
      }

      ssfs(current_data)
      stops_editing_id(NULL)
      stops_adding_new(FALSE)
      stops_temp_point(NULL)
      stops_edit_stop_id("")
      stops_edit_stop_name("")
    })

    # Handle cancel click
    observeEvent(input$stop_list_cancel_click, {
      stops_editing_id(NULL)
      stops_adding_new(FALSE)
      stops_temp_point(NULL)
      stops_edit_stop_id("")
      stops_edit_stop_name("")
    })

    # Handle stop deletion
    observeEvent(input$stop_list_delete_click, {
      stop_to_delete <- input$stop_list_delete_click$id
      current_data <- ssfs()

      if (
        nrow(current_data$stop_seq) > 0 &&
          stop_to_delete %in% current_data$stop_seq$stop_id
      ) {
        associated_itins <- paste(
          unique(current_data$stop_seq$itin_id[
            current_data$stop_seq$stop_id == stop_to_delete
          ]),
          collapse = ", "
        )
        showNotification(
          paste0(
            "Cannot delete stop '",
            stop_to_delete,
            "'. It is used in itineraries: ",
            associated_itins,
            ". Remove it from those itineraries first."
          ),
          type = "error",
          duration = 5
        )
        return()
      }

      current_data$stops <- current_data$stops[
        current_data$stops$stop_id != stop_to_delete,
      ]
      ssfs(current_data)

      if (
        !is.null(stops_editing_id()) && stops_editing_id() == stop_to_delete
      ) {
        stops_editing_id(NULL)
        stops_temp_point(NULL)
        stops_edit_stop_id("")
        stops_edit_stop_name("")
      }

      showNotification("Stop deleted successfully", type = "message")
    })

    ### STOPS GENERATION FUNCTIONALITY ----

    # Render the generate panel UI (state-dependent)
    output$stops_generate_ui <- renderUI({
      drawing <- stops_generate_mode()
      polygon <- stops_generate_polygon()
      verts <- stops_draw_vertices()

      if (drawing) {
        tagList(
          div(
            class = "editing-instruction",
            paste0(
              "Click on the map to draw the zone (",
              length(verts),
              if (length(verts) == 1) " vertex)" else " vertices)"
            )
          ),
          div(
            style = "display: flex; gap: 5px; margin-top: 8px;",
            actionButton(
              ns("stops_generate_complete"),
              "Complete",
              class = "btn-success btn-sm"
            ),
            actionButton(
              ns("stops_generate_cancel"),
              "Cancel",
              class = "btn-outline-secondary btn-sm"
            )
          )
        )
      } else if (!is.null(polygon)) {
        tagList(
          div(
            class = "editing-instruction",
            style = "background-color: #D1E5F0; border-color: #2166AC; color: #2166AC",
            icon("check-circle", style = "color: #2166AC;"),
            " Zone drawn. Click Generate below to confirm"
          ),
          div(
            style = "display: flex; gap: 5px; margin-top: 8px;",
            actionButton(
              ns("stops_generate_run"),
              "Generate",
              class = "btn-success btn-sm"
            ),
            actionButton(
              ns("stops_generate_clear"),
              "Clear zone",
              class = "btn-outline-secondary btn-sm"
            )
          )
        )
      } else {
        actionButton(
          ns("stops_generate_draw"),
          "Draw zone on map",
          class = "btn-info btn-sm"
        )
      }
    })

    # Enter draw mode
    observeEvent(input$stops_generate_draw, {
      # Exit any stop editing mode
      stops_editing_id(NULL)
      stops_adding_new(FALSE)
      stops_temp_point(NULL)

      stops_generate_mode(TRUE)
      stops_draw_vertices(list())
      stops_generate_polygon(NULL)

      leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("draw_zone")
    })

    # Redraw in-progress polygon as vertices are added
    observe({
      verts <- stops_draw_vertices()

      if (!stops_generate_mode()) {
        return()
      }

      proxy <- leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("draw_zone")

      if (length(verts) == 0) {
        return()
      }

      lngs <- vapply(verts, `[`, numeric(1), 1)
      lats <- vapply(verts, `[`, numeric(1), 2)

      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = lngs,
          lat = lats,
          radius = 5,
          color = "#2166AC",
          fillColor = "#2166AC",
          fillOpacity = 0.5,
          weight = 1,
          stroke = TRUE,
          group = "draw_zone",
          options = leaflet::pathOptions(pane = "stops_temp_pane")
        )

      if (length(verts) >= 2) {
        proxy |>
          leaflet::addPolylines(
            lng = lngs,
            lat = lats,
            color = "#3388ff",
            weight = 2,
            dashArray = "5,5",
            group = "draw_zone",
            options = leaflet::pathOptions(pane = "stops_temp_pane")
          )
      }
    })

    # Complete the polygon
    observeEvent(input$stops_generate_complete, {
      verts <- stops_draw_vertices()

      if (length(verts) < 3) {
        showNotification(
          "Draw at least 3 points to define a zone.",
          type = "warning"
        )
        return()
      }

      # Build closed ring: append first vertex to close
      coords <- do.call(rbind, lapply(verts, function(v) c(v[1], v[2])))
      coords <- rbind(coords, coords[1, ])

      polygon_sf <- sf::st_sf(
        geometry = sf::st_sfc(
          sf::st_polygon(list(coords)),
          crs = 4326
        )
      )

      stops_generate_polygon(polygon_sf)
      stops_generate_mode(FALSE)
      stops_draw_vertices(list())

      # Redraw as filled polygon
      leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("draw_zone") |>
        leaflet::addPolygons(
          lng = coords[, 1],
          lat = coords[, 2],
          color = "#92C5DE",
          weight = 2,
          fillColor = "#92C5DE",
          fillOpacity = 0.2,
          group = "draw_zone",
          options = leaflet::pathOptions(pane = "stops_temp_pane")
        )
    })

    # Cancel drawing
    observeEvent(input$stops_generate_cancel, {
      stops_generate_mode(FALSE)
      stops_draw_vertices(list())
      leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("draw_zone")
    })

    # Clear a completed polygon
    observeEvent(input$stops_generate_clear, {
      stops_generate_polygon(NULL)
      stops_draw_vertices(list())
      leaflet::leafletProxy("stops_map") |>
        leaflet::clearGroup("draw_zone")
    })

    # Run stop generation
    observeEvent(input$stops_generate_run, {
      polygon <- stops_generate_polygon()

      if (is.null(polygon)) {
        showNotification(
          "Draw a zone on the map first.",
          type = "warning"
        )
        return()
      }

      current_data <- ssfs()
      dist_val <- min_stop_dist()
      provider_val <- osm_provider()

      showNotification(
        "Downloading OSM data and generating stops. This may take a while depending on region and OSM provider (manage in Settings).",
        id = "gen_progress",
        duration = NULL,
        type = "message"
      )

      tryCatch(
        {
          result <- generate_stops_from_osm(
            polygon_sf = polygon,
            current_stops = current_data$stops,
            min_stop_dist = dist_val,
            batch_id = autostop_batch_id(),
            provider = provider_val
          )

          removeNotification("gen_progress")

          if (is.null(result) || nrow(result$new_stops) == 0) {
            showNotification(
              "No eligible stop locations found in this zone.",
              type = "warning"
            )
            return()
          }

          current_data$stops <- rbind(current_data$stops, result$new_stops)
          ssfs(current_data)
          autostop_batch_id(result$next_batch_id)

          # Clear drawn polygon from map
          leaflet::leafletProxy("stops_map") |>
            leaflet::clearGroup("draw_zone")
          stops_generate_polygon(NULL)

          showNotification(
            paste(nrow(result$new_stops), "stops generated and added."),
            type = "message"
          )
        },
        error = function(e) {
          removeNotification("gen_progress")
          showNotification(
            paste(
              "Stop generation failed,",
              e$message,
              "...Try changing OSM Provider in Settings."
            ),
            type = "error"
          )
        }
      )
    })

    ### STOPS IMPORT/EXPORT FUNCTIONALITY ----

    # Handle stops import
    observeEvent(input$stops_import_confirm, {
      req(input$stops_import_file)

      file_path <- input$stops_import_file$datapath
      file_name <- input$stops_import_file$name
      file_ext <- tolower(tools::file_ext(file_name))

      if (!file_ext %in% c("geojson", "kml")) {
        showNotification(
          "Invalid file format. Please upload a GeoJSON or KML file.",
          type = "error"
        )
        return()
      }

      tryCatch(
        {
          imported_sf <- st_read(file_path, quiet = TRUE)
          imported_sf <- st_transform(imported_sf, 4326)

          geom_types <- unique(st_geometry_type(imported_sf))
          if (!all(geom_types %in% c("POINT", "MULTIPOINT"))) {
            showNotification(
              "Only point geometries can be imported.",
              type = "error"
            )
            return()
          }

          if ("MULTIPOINT" %in% geom_types) {
            imported_sf <- st_cast(imported_sf, "POINT")
          }

          if (nrow(imported_sf) > 99999) {
            showNotification(
              "File contains more than 99,999 features. Please reduce the file size.",
              type = "error"
            )
            return()
          }

          col_names <- setdiff(names(imported_sf), "geometry")
          imported_df <- as.data.frame(imported_sf)

          # Identify stop_id column
          stop_id_col <- NULL
          for (col in col_names) {
            col_values <- imported_df[[col]]
            if (
              is.character(col_values) ||
                is.numeric(col_values) ||
                is.integer(col_values)
            ) {
              col_as_char <- as.character(col_values)
              has_no_spaces <- !any(grepl("\\s", col_as_char, perl = TRUE))
              is_unique <- length(unique(col_as_char)) == length(col_as_char)

              if (has_no_spaces && is_unique && length(col_as_char) > 0) {
                stop_id_col <- col
                break
              }
            }
          }

          if (is.null(stop_id_col)) {
            imported_sf$stop_id <- sprintf("%05d", seq_len(nrow(imported_sf)))
          } else {
            imported_sf$stop_id <- as.character(imported_df[[stop_id_col]])
          }

          # Identify stop_name column
          stop_name_col <- NULL
          for (col in col_names) {
            if (!is.null(stop_id_col) && col == stop_id_col) {
              next
            }
            col_values <- imported_df[[col]]
            if (is.character(col_values)) {
              stop_name_col <- col
              break
            }
          }

          if (is.null(stop_name_col)) {
            imported_sf$stop_name <- imported_sf$stop_id
          } else {
            imported_sf$stop_name <- as.character(imported_df[[stop_name_col]])
          }

          imported_stops <- imported_sf |>
            select(stop_id, stop_name, geometry)

          current_data <- ssfs()
          existing_ids <- current_data$stops$stop_id
          incoming_ids <- imported_stops$stop_id
          duplicate_ids <- intersect(existing_ids, incoming_ids)

          if (length(duplicate_ids) > 0) {
            imported_stops <- imported_stops |>
              filter(!stop_id %in% duplicate_ids)

            if (nrow(imported_stops) == 0) {
              showNotification(
                paste(
                  "All",
                  length(duplicate_ids),
                  "stops have duplicate IDs and were not imported."
                ),
                type = "warning"
              )
              return()
            }

            showNotification(
              paste(
                length(duplicate_ids),
                "stops with duplicate IDs were skipped."
              ),
              type = "warning"
            )
          }

          current_data$stops <- rbind(current_data$stops, imported_stops)
          ssfs(current_data)

          showNotification(
            paste("Successfully imported", nrow(imported_stops), "stops."),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Error importing file:", e$message),
            type = "error"
          )
        }
      )
    })

    # Handle stops export/download
    output$stops_export_download <- downloadHandler(
      filename = function() {
        format <- input$stops_export_format
        base_name <- "stops"

        switch(
          format,
          "geojson" = paste0(base_name, ".geojson"),
          "kml" = paste0(base_name, ".kml"),
          "shp" = paste0(base_name, ".zip")
        )
      },
      content = function(file) {
        current_data <- ssfs()

        if (nrow(current_data$stops) == 0) {
          showNotification("No stops to export.", type = "warning")
          return()
        }

        format <- input$stops_export_format

        tryCatch(
          {
            if (format == "shp") {
              temp_dir <- tempdir()
              shp_base <- file.path(temp_dir, "stops")

              st_write(
                current_data$stops,
                paste0(shp_base, ".shp"),
                delete_layer = TRUE,
                quiet = TRUE
              )

              shp_files <- list.files(
                temp_dir,
                pattern = "^stops\\.",
                full.names = TRUE
              )

              zip(file, files = shp_files, flags = "-j")
            } else if (format == "kml") {
              kml_stops <- current_data$stops |>
                mutate(
                  Name = stop_name,
                  Description = stop_id
                ) |>
                select(Name, Description, geometry)

              st_write(
                kml_stops,
                file,
                driver = "KML",
                delete_dsn = TRUE,
                quiet = TRUE
              )
            } else {
              st_write(
                current_data$stops,
                file,
                driver = "GeoJSON",
                delete_dsn = TRUE,
                quiet = TRUE
              )
            }
          },
          error = function(e) {
            showNotification(
              paste("Error exporting file:", e$message),
              type = "error"
            )
          }
        )
      }
    )
  })
}
