scheduleUI <- function(id) {
  ns <- NS(id)

  tabPanel(
    "schedule",
    fluidPage(
      titlePanel("schedule"),

      # Map with floating panels
      div(
        class = "sched-map-container",

        # Full-width map
        leaflet::leafletOutput(
          ns("sched_map"),
          height = "100%",
          width = "100%"
        ),

        # Floating panel: Routes list (top-left)
        div(
          id = "sched-routes-panel",
          class = "floating-panel",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('sched-routes-panel')",
            h4("Routes"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            div(
              class = "sched-route-list-container",
              uiOutput(ns("sched_route_list_ui"))
            )
          )
        ),

        # Floating panel: Service & Hour filter (top-right)
        div(
          id = "sched-filter-panel",
          class = "floating-panel",
          div(
            class = "floating-panel-header",
            onclick = "togglePanel('sched-filter-panel')",
            h4("Service & Hour"),
            tags$button(
              class = "floating-panel-toggle",
              htmltools::HTML("&minus;")
            )
          ),
          div(
            class = "floating-panel-content",
            tags$label("Service"),
            selectInput(
              ns("sched_service_id"),
              label = NULL,
              choices = NULL,
              width = "100%"
            ),
            tags$label("Hour"),
            selectInput(
              ns("sched_hour"),
              label = NULL,
              choices = sprintf("%02d:00:00", 1:29),
              selected = "08:00:00",
              width = "100%"
            ),
            tags$small(
              "Click on any route segment on the map to view cumulative service level for this service and hour."
            )
          )
        )
      ),

      # Below-map area: placeholder for schedule editing
      div(
        id = "sched-editing-area",
        style = "margin-top: 15px;",
        uiOutput(ns("sched_editing_ui"))
      )
    )
  )
}

scheduleServer <- function(id, ssfs, map_center, service_patterns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Send namespace prefix to JS
    session$sendCustomMessage("setSchedNs", ns(""))

    # -- Reactive values
    sched_highlighted_route <- reactiveVal(NULL)
    sched_highlighted_itin_ids <- reactiveVal(character(0))
    sched_editing_route_id <- reactiveVal(NULL)
    sched_zoom <- reactiveVal(12)

    # itin_id selected for editing in the itinerary-level panel (right side)
    sched_editing_itin_id <- reactiveVal(NULL)

    # service_id selected in the route-level schedule panel
    # (separate from the map filter panel's sched_service_id)
    sched_edit_service_id <- reactiveVal(NULL)

    # Initialise service_id dropdown from calendar
    observe({
      current_data <- ssfs()
      if (nrow(current_data$calendar) > 0) {
        service_choices <- current_data$calendar$service_id
        current_sel <- isolate(input$sched_service_id)
        selected <- if (
          !is.null(current_sel) && current_sel %in% service_choices
        ) {
          current_sel
        } else {
          service_choices[1]
        }
        updateSelectInput(
          session,
          "sched_service_id",
          choices = service_choices,
          selected = selected
        )
      }
    })

    # Render schedule map
    output$sched_map <- leaflet::renderLeaflet({
      center <- map_center()
      leaflet::leaflet(
        options = leaflet::leafletOptions(zoomControl = TRUE)
      ) |>
        leaflet::addProviderTiles("CartoDB.Positron", group = "Positron") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") |>
        leaflet::setView(lng = center$lng, lat = center$lat, zoom = 12) |>
        leaflet::addLayersControl(
          baseGroups = c("Positron", "Satellite", "OSM"),
          position = "bottomright",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        htmlwidgets::onRender(sprintf(
          "
          function(el, x) {
            var ns = '%s';
            this.on('zoomend', function(e) {
              Shiny.setInputValue(ns + 'sched_map_zoom', this.getZoom());
            });
          }
          ",
          ns("")
        ))
    })

    # Track zoom
    observeEvent(input$sched_map_zoom, {
      sched_zoom(input$sched_map_zoom)
    })

    # Observe: redraw shapes, stops and highlights on schedule map
    observe({
      current_data <- ssfs()

      proxy <- leaflet::leafletProxy("sched_map") |>
        leaflet::clearGroup("sched_stops") |>
        leaflet::clearGroup("sched_routes") |>
        leaflet::clearGroup("sched_highlight")

      # Draw highlight underlay
      hl_ids <- sched_highlighted_itin_ids()
      if (length(hl_ids) > 0 && nrow(current_data$itin) > 0) {
        hl_itins <- current_data$itin[
          current_data$itin$itin_id %in% hl_ids,
        ]
        for (j in seq_len(nrow(hl_itins))) {
          hl_coords <- st_coordinates(hl_itins$geometry[j])
          proxy <- proxy |>
            leaflet::addPolylines(
              lng = hl_coords[, 1],
              lat = hl_coords[, 2],
              group = "sched_highlight",
              color = "#FFE999",
              weight = 10,
              opacity = 0.4,
              stroke = TRUE
            )
        }
      }

      # Draw all itinerary shapes
      if (!is.null(current_data$itin) && nrow(current_data$itin) > 0) {
        for (i in seq_len(nrow(current_data$itin))) {
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

          proxy <- proxy |>
            leaflet::addPolylines(
              lng = line_coords[, 1],
              lat = line_coords[, 2],
              group = "sched_routes",
              color = line_color,
              weight = 2,
              opacity = 0.6,
              label = hover_label,
              labelOptions = leaflet::labelOptions(
                style = list("font-size" = "11px", "padding" = "3px 6px"),
                direction = "top",
                offset = c(0, -8)
              )
            )
        }
      }

      # Draw stops
      if (!is.null(current_data$stops) && nrow(current_data$stops) > 0) {
        stop_itin_lookup <- current_data$stop_seq |>
          group_by(stop_id) |>
          summarise(
            itin_ids = paste(unique(itin_id), collapse = ", "),
            .groups = "drop"
          )

        label_data <- merge(
          as.data.frame(current_data$stops)[, "stop_id", drop = FALSE],
          stop_itin_lookup,
          by = "stop_id",
          all.x = TRUE
        )

        hover_labels <- lapply(
          seq_len(nrow(current_data$stops)),
          function(i) {
            sid <- current_data$stops$stop_id[i]
            sname <- current_data$stops$stop_name[i]
            itins <- label_data$itin_ids[label_data$stop_id == sid]
            itin_text <- if (is.na(itins) || length(itins) == 0) {
              "None"
            } else {
              itins
            }
            htmltools::HTML(paste0(
              "<span style='font-size:11px;'>",
              "<b>",
              htmltools::htmlEscape(sid),
              "</b> \u2014 ",
              htmltools::htmlEscape(sname),
              "<br>Itineraries: ",
              htmltools::htmlEscape(itin_text),
              "</span>"
            ))
          }
        )

        proxy <- proxy |>
          leaflet::addCircleMarkers(
            data = current_data$stops,
            layerId = ~ paste0("sched_stop_", stop_id),
            color = "white",
            weight = 1,
            stroke = TRUE,
            fillColor = "#7f7f7f",
            fillOpacity = 0.7,
            radius = calculateMarkerSize(sched_zoom()),
            label = hover_labels,
            labelOptions = leaflet::labelOptions(
              style = list("font-size" = "11px", "padding" = "3px 6px"),
              direction = "top",
              offset = c(0, -8)
            ),
            group = "sched_stops"
          )
      }

      return(proxy)
    })

    # Helper: build the service-level popup HTML table
    sched_build_popup_table <- function(
      nearby_itin_ids,
      current_data,
      service_id,
      hour,
      header = NULL
    ) {
      hsh <- current_data$hsh

      hsh_filtered <- hsh |>
        filter(
          itin_id %in% nearby_itin_ids,
          service_id == !!service_id,
          hour_dep == !!hour
        )

      if (nrow(hsh_filtered) == 0) {
        return(paste0(
          "<div style='font-size:11px;'>",
          if (!is.null(header)) header else "",
          "<em>No service at this hour / service.</em></div>"
        ))
      }

      rows <- lapply(seq_len(nrow(hsh_filtered)), function(i) {
        iid <- hsh_filtered$itin_id[i]
        hdwy <- hsh_filtered$headway[i]

        itin_row <- current_data$itin[current_data$itin$itin_id == iid, ]
        if (nrow(itin_row) == 0) {
          return(NULL)
        }

        rid <- itin_row$route_id[1]
        route_row <- current_data$routes[
          current_data$routes$route_id == rid,
        ]

        route_display <- if (nrow(route_row) > 0) {
          paste0(
            htmltools::htmlEscape(route_row$route_short_name[1]),
            " - ",
            htmltools::htmlEscape(route_row$route_long_name[1])
          )
        } else {
          htmltools::htmlEscape(rid)
        }

        itin_display <- paste0(
          htmltools::htmlEscape(iid),
          if (
            !is.na(itin_row$trip_headsign[1]) &&
              nchar(trimws(itin_row$trip_headsign[1])) > 0
          ) {
            paste0(
              " - ",
              htmltools::htmlEscape(trimws(itin_row$trip_headsign[1]))
            )
          } else {
            ""
          }
        )

        hdwy_display <- if (is.na(hdwy)) "-" else as.character(hdwy)
        trips_h <- if (is.na(hdwy) || hdwy == 0) {
          "-"
        } else {
          as.character(floor(60 / hdwy))
        }

        list(
          route = route_display,
          itin = itin_display,
          headway = hdwy_display,
          headway_numeric = if (is.na(hdwy)) NA_real_ else as.numeric(hdwy),
          trips_h = trips_h,
          trips_numeric = if (is.na(hdwy) || hdwy == 0) {
            0L
          } else {
            floor(60 / hdwy)
          }
        )
      })

      rows <- Filter(Negate(is.null), rows)

      if (length(rows) == 0) {
        return(paste0(
          "<div style='font-size:11px;'>",
          if (!is.null(header)) header else "",
          "<em>No service at this hour / service.</em></div>"
        ))
      }

      body_html <- paste0(
        sapply(rows, function(r) {
          paste0(
            "<tr>",
            "<td>",
            r$route,
            "</td>",
            "<td>",
            r$itin,
            "</td>",
            "<td style='text-align:center;'>",
            r$headway,
            "</td>",
            "<td style='text-align:center;'>",
            r$trips_h,
            "</td>",
            "</tr>"
          )
        }),
        collapse = ""
      )

      total_trips <- sum(sapply(rows, function(r) r$trips_numeric))

      raw_headways <- sapply(rows, function(r) r$headway_numeric)
      valid_hdwys <- raw_headways[!is.na(raw_headways) & raw_headways > 0]
      exact_trips_sum <- sum(60 / valid_hdwys)
      total_hdwy <- if (exact_trips_sum > 0) {
        as.character(ceiling(60 / exact_trips_sum))
      } else {
        "-"
      }

      totals_html <- paste0(
        "<tr class='totals-row'>",
        "<td colspan='2'><b>Total</b></td>",
        "<td style='text-align:center;'>",
        total_hdwy,
        "</td>",
        "<td style='text-align:center;'>",
        total_trips,
        "</td>",
        "</tr>"
      )

      paste0(
        "<div style='font-size:11px; max-width: 420px;'>",
        if (!is.null(header)) header else "",
        "<table class='sched-popup-table'>",
        "<thead><tr>",
        "<th>Route</th><th>Itinerary</th>",
        "<th style='text-align:center;'>Headway</th>",
        "<th style='text-align:center;'>Trips/h</th>",
        "</tr></thead>",
        "<tbody>",
        body_html,
        totals_html,
        "</tbody></table></div>"
      )
    }

    # Map click: show service-level popup
    observeEvent(input$sched_map_click, {
      click <- input$sched_map_click
      current_data <- ssfs()
      if (is.null(current_data$itin) || nrow(current_data$itin) == 0) {
        return()
      }

      service_id <- input$sched_service_id
      hour <- input$sched_hour

      # Check if click is near a stop
      if (!is.null(current_data$stops) && nrow(current_data$stops) > 0) {
        click_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
        stop_distances <- as.numeric(
          st_distance(current_data$stops$geometry, click_point)
        )

        zoom <- sched_zoom()
        stop_threshold_m <- if (!is.null(zoom) && zoom >= 10) {
          100 / (2^(zoom - 12))
        } else {
          100
        }

        nearest_stop_idx <- which.min(stop_distances)

        if (
          length(nearest_stop_idx) > 0 &&
            stop_distances[nearest_stop_idx] <= stop_threshold_m
        ) {
          clicked_stop <- current_data$stops[nearest_stop_idx, ]
          stop_id_clicked <- clicked_stop$stop_id
          stop_name_clicked <- clicked_stop$stop_name

          associated_itin_ids <- current_data$stop_seq |>
            filter(stop_id == stop_id_clicked) |>
            pull(itin_id) |>
            unique()

          header <- paste0(
            "<b>",
            htmltools::htmlEscape(stop_id_clicked),
            " \u2014 ",
            htmltools::htmlEscape(stop_name_clicked),
            "</b><hr style='margin:4px 0;'>"
          )

          popup_html <- sched_build_popup_table(
            associated_itin_ids,
            current_data,
            service_id,
            hour,
            header = header
          )

          leaflet::leafletProxy("sched_map") |>
            leaflet::clearPopups() |>
            leaflet::addPopups(
              lng = click$lng,
              lat = click$lat,
              popup = popup_html,
              options = leaflet::popupOptions(
                closeButton = TRUE,
                maxWidth = 450
              )
            )

          sched_highlighted_itin_ids(associated_itin_ids)
          return()
        }
      }

      # Otherwise: find nearby itinerary segments
      click_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)

      zoom <- sched_zoom()
      threshold_m <- if (!is.null(zoom) && zoom >= 10) {
        200 / (2^(zoom - 12))
      } else {
        200
      }

      distances <- as.numeric(
        st_distance(current_data$itin$geometry, click_point)
      )
      nearby_idx <- which(distances <= threshold_m)

      if (length(nearby_idx) == 0) {
        leaflet::leafletProxy("sched_map") |> leaflet::clearPopups()
        sched_highlighted_itin_ids(character(0))
        return()
      }

      nearby_itin_ids <- current_data$itin$itin_id[nearby_idx]

      popup_html <- sched_build_popup_table(
        nearby_itin_ids,
        current_data,
        service_id,
        hour
      )

      leaflet::leafletProxy("sched_map") |>
        leaflet::clearPopups() |>
        leaflet::addPopups(
          lng = click$lng,
          lat = click$lat,
          popup = popup_html,
          options = leaflet::popupOptions(
            closeButton = TRUE,
            maxWidth = 450
          )
        )

      sched_highlighted_itin_ids(nearby_itin_ids)
    })

    # Render the route list panel
    output$sched_route_list_ui <- renderUI({
      current_data <- ssfs()
      selected_route <- sched_highlighted_route()
      editing_route <- sched_editing_route_id()

      rows <- list()

      if (nrow(current_data$routes) == 0) {
        rows[[1]] <- tags$small(
          style = "color: grey;",
          "No routes defined. Add routes in the Routes module."
        )
        return(do.call(tagList, rows))
      }

      for (r in seq_len(nrow(current_data$routes))) {
        route <- current_data$routes[r, ]

        rcol <- if (!is.na(route$route_color) && nchar(route$route_color) > 0) {
          paste0("#", route$route_color)
        } else {
          "#05AEEF"
        }

        is_selected <- !is.null(selected_route) &&
          selected_route == route$route_id
        is_editing <- !is.null(editing_route) &&
          editing_route == route$route_id

        row_class <- paste0(
          "route-list-row",
          if (is_editing) {
            " editing-route"
          } else if (is_selected) {
            " expanded"
          } else {
            ""
          }
        )

        rows[[length(rows) + 1]] <- div(
          class = row_class,
          onclick = sprintf("schedToggleRoute('%s')", route$route_id),
          div(
            class = "route-color-badge",
            style = paste0("background-color: ", rcol, ";")
          ),
          div(
            class = "route-info",
            div(
              class = "route-info-display",
              span(class = "route-short-name", route$route_short_name),
              span(class = "route-long-name", route$route_long_name)
            )
          ),
          div(
            class = "route-actions",
            tags$button(
              class = "route-action-btn edit-btn",
              onclick = sprintf(
                "event.stopPropagation(); schedEditRoute('%s')",
                route$route_id
              ),
              title = "Edit schedule",
              htmltools::HTML("&#9998;")
            )
          )
        )
      }

      do.call(tagList, rows)
    })

    # Route click handler: highlight itineraries
    observeEvent(input$sched_route_click, {
      route_id <- input$sched_route_click$id
      current_data <- ssfs()

      if (
        !is.null(sched_highlighted_route()) &&
          sched_highlighted_route() == route_id
      ) {
        sched_highlighted_route(NULL)
        sched_highlighted_itin_ids(character(0))
      } else {
        sched_highlighted_route(route_id)
        route_itin_ids <- current_data$itin$itin_id[
          current_data$itin$route_id == route_id
        ]
        sched_highlighted_itin_ids(route_itin_ids)
      }
    })

    # Pencil click handler: set editing route
    observeEvent(input$sched_route_edit_click, {
      route_id <- input$sched_route_edit_click$id
      current_data <- ssfs()

      sched_editing_route_id(route_id)
      sched_editing_itin_id(NULL) # reset itin selection

      # Highlight this route's itineraries
      sched_highlighted_route(route_id)
      route_itin_ids <- current_data$itin$itin_id[
        current_data$itin$route_id == route_id
      ]
      sched_highlighted_itin_ids(route_itin_ids)

      # Auto-select service_id: first service that has spans for this route
      route_itin_ids_vec <- current_data$itin$itin_id[
        current_data$itin$route_id == route_id
      ]
      services_with_spans <- current_data$span |>
        filter(itin_id %in% route_itin_ids_vec) |>
        pull(service_id) |>
        unique()

      if (length(services_with_spans) > 0) {
        sched_edit_service_id(services_with_spans[1])
      } else if (nrow(current_data$calendar) > 0) {
        sched_edit_service_id(current_data$calendar$service_id[1])
      }
    })

    observeEvent(input$sched_itin_select, {
      itin_id <- input$sched_itin_select$id
      current_data <- ssfs()

      # Toggle: if already selected, deselect
      if (
        !is.null(sched_editing_itin_id()) &&
          sched_editing_itin_id() == itin_id
      ) {
        sched_editing_itin_id(NULL)
        # Restore route-level highlight
        editing_route <- sched_editing_route_id()
        if (!is.null(editing_route)) {
          route_itin_ids <- current_data$itin$itin_id[
            current_data$itin$route_id == editing_route
          ]
          sched_highlighted_itin_ids(route_itin_ids)
        }
      } else {
        sched_editing_itin_id(itin_id)
        sched_highlighted_itin_ids(itin_id)
      }
    })

    observeEvent(input$sched_itin_edit_click, {
      itin_id <- input$sched_itin_edit_click$id
      sched_editing_itin_id(itin_id)
      sched_highlighted_itin_ids(itin_id)
    })

    observeEvent(input$sched_edit_service_select, {
      sched_edit_service_id(input$sched_edit_service_select)
    })

    # Route-level schedule editing UI renderer
    output$sched_editing_ui <- renderUI({
      editing_route <- sched_editing_route_id()

      if (is.null(editing_route)) {
        return(
          div(
            style = "color: grey; text-align: center; padding: 20px;",
            tags$em("Click the pencil icon on a route to edit its schedule.")
          )
        )
      }

      current_data <- ssfs()
      ns <- session$ns

      route_row <- current_data$routes[
        current_data$routes$route_id == editing_route,
      ]
      route_display <- if (nrow(route_row) > 0) {
        paste0(
          route_row$route_short_name[1],
          " \u2014 ",
          route_row$route_long_name[1]
        )
      } else {
        editing_route
      }

      # Get itineraries for this route
      route_itins <- current_data$itin[
        current_data$itin$route_id == editing_route,
      ]

      # Service choices
      service_choices <- if (nrow(current_data$calendar) > 0) {
        current_data$calendar$service_id
      } else {
        character(0)
      }

      current_edit_service <- sched_edit_service_id()
      selected_service <- if (
        !is.null(current_edit_service) &&
          current_edit_service %in% service_choices
      ) {
        current_edit_service
      } else if (length(service_choices) > 0) {
        service_choices[1]
      } else {
        NULL
      }

      editing_itin <- sched_editing_itin_id()

      # Service level preset choices
      sp_data <- service_patterns()
      preset_choices <- if (
        !is.null(sp_data$service_pattern_names) &&
          nrow(sp_data$service_pattern_names) > 0
      ) {
        setNames(
          sp_data$service_pattern_names$pattern_id,
          paste0(
            sp_data$service_pattern_names$pattern_id,
            " - ",
            sp_data$service_pattern_names$pattern_name
          )
        )
      } else {
        character(0)
      }

      # ── Build itinerary rows ──
      itin_rows <- list()

      if (nrow(route_itins) > 0) {
        for (i in seq_len(nrow(route_itins))) {
          itin <- route_itins[i, ]
          itin_id <- itin$itin_id

          # Check spans for this itin + service
          itin_spans <- current_data$span[
            current_data$span$itin_id == itin_id &
              current_data$span$service_id == selected_service,
          ]

          has_spans <- nrow(itin_spans) > 0

          # Build span display text
          span_text <- if (has_spans) {
            paste(
              sapply(seq_len(nrow(itin_spans)), function(s) {
                paste0(itin_spans$first_dep[s], " - ", itin_spans$last_dep[s])
              }),
              collapse = "; "
            )
          } else {
            NULL
          }

          # Average speed for itin + service
          itin_hsh <- current_data$hsh[
            current_data$hsh$itin_id == itin_id &
              current_data$hsh$service_id == selected_service,
          ]
          avg_speed <- if (
            nrow(itin_hsh) > 0 &&
              any(!is.na(itin_hsh$speed))
          ) {
            paste0(round(mean(itin_hsh$speed, na.rm = TRUE), 1), " km/h")
          } else {
            NULL
          }

          is_active_itin <- !is.null(editing_itin) && editing_itin == itin_id

          row_class <- paste0(
            "sched-itin-row",
            if (is_active_itin) {
              " sched-itin-active"
            } else if (!has_spans) {
              " sched-itin-inactive"
            } else {
              ""
            }
          )

          itin_rows[[length(itin_rows) + 1]] <- div(
            class = row_class,
            onclick = sprintf("schedSelectItin('%s')", itin_id),

            # Direction badge
            span(
              class = "itin-direction-badge",
              if (as.integer(itin$direction_id) == 0) "Out" else "In"
            ),

            # Main info area
            div(
              class = "sched-itin-main",
              # Header line: headsign + itin_id
              div(
                class = "sched-itin-header",
                span(class = "itin-headsign", itin$trip_headsign),
                span(class = "itin-id-display", paste0("(", itin_id, ")"))
              ),
              # Span info
              if (!is.null(span_text)) {
                div(class = "sched-itin-spans", span_text)
              },
              # Speed info
              if (!is.null(avg_speed)) {
                div(class = "sched-itin-speed", avg_speed)
              }
            ),

            # Pencil icon (far right)
            div(
              class = "route-actions",
              tags$button(
                class = "route-action-btn edit-btn",
                onclick = sprintf(
                  "event.stopPropagation(); schedEditItin('%s')",
                  itin_id
                ),
                title = "Edit itinerary schedule",
                htmltools::HTML("&#9998;")
              )
            )
          )
        }
      } else {
        itin_rows[[1]] <- tags$small(
          style = "color: grey;",
          "No itineraries for this route."
        )
      }

      # ── Assemble layout ──
      div(
        class = "sched-editing-container",

        # === LEFT SIDE: Route-level schedule panel ===
        div(
          class = "sched-route-panel",
          h4(paste0("Schedule: ", route_display)),

          # Service selector
          selectInput(
            ns("sched_edit_service_select"),
            label = "Service",
            choices = service_choices,
            selected = selected_service,
            width = "100%"
          ),

          # Itinerary rows
          h5("Itineraries"),
          do.call(tagList, itin_rows),

          # ── Batch actions ──
          div(
            class = "sched-batch-section",

            # Apply span to all itineraries
            h5("Apply span to all route itineraries"),
            div(
              class = "sched-batch-row",
              div(
                tags$label("First departure"),
                tags$input(
                  type = "text",
                  id = ns("sched_batch_first_dep"),
                  class = "sched-time-input",
                  value = "05:00:00",
                  placeholder = "HH:MM:SS"
                )
              ),
              div(
                tags$label("Last departure"),
                tags$input(
                  type = "text",
                  id = ns("sched_batch_last_dep"),
                  class = "sched-time-input",
                  value = "23:00:00",
                  placeholder = "HH:MM:SS"
                )
              ),
              tags$button(
                class = "btn-save",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                  ns("sched_batch_apply_span")
                ),
                "Apply"
              )
            ),

            # Apply service level preset to all itineraries
            h5("Apply service level preset to all route itineraries"),
            div(
              class = "sched-batch-row",
              div(
                style = "flex: 1;",
                selectInput(
                  ns("sched_batch_preset"),
                  label = NULL,
                  choices = preset_choices,
                  width = "100%"
                )
              ),
              tags$button(
                class = "btn-save",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                  ns("sched_batch_apply_preset")
                ),
                "Apply"
              )
            ),

            # Apply speed to all itineraries
            h5("Apply speed to all route itineraries"),
            div(
              class = "sched-batch-row",
              div(
                tags$label("Speed (km/h)"),
                numericInput(
                  ns("sched_batch_speed"),
                  label = NULL,
                  value = 20,
                  min = 5,
                  max = 431,
                  width = "100px"
                )
              ),
              tags$button(
                class = "btn-save",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                  ns("sched_batch_apply_speed")
                ),
                "Apply"
              )
            )
          )
        ),

        # === RIGHT SIDE: Itinerary-level schedule panel ===
        div(
          class = "sched-itin-panel",
          uiOutput(ns("sched_itin_editing_ui"))
        )
      )
    })

    # Placeholder for right-side itinerary level schedule editor
    output$sched_itin_editing_ui <- renderUI({
      editing_itin <- sched_editing_itin_id()

      if (is.null(editing_itin)) {
        return(
          div(
            style = "color: grey; text-align: center; padding: 40px 20px;",
            tags$em(
              "Click the pencil icon on an itinerary to edit its
                      headways and speeds."
            )
          )
        )
      }

      current_data <- ssfs()
      itin_row <- current_data$itin[
        current_data$itin$itin_id == editing_itin,
      ]

      if (nrow(itin_row) == 0) {
        return(NULL)
      }

      itin_display <- paste0(
        itin_row$trip_headsign[1],
        " (",
        editing_itin,
        ")"
      )

      wellPanel(
        h4(paste0("Itinerary: ", itin_display)),
        tags$em(
          style = "color: grey;",
          "Headway and speed editing table will appear here."
        )
      )
    })

    # route-level batch action edit observers

    # --- Apply span to all route itineraries ---

    observeEvent(input$sched_batch_apply_span, {
      editing_route <- sched_editing_route_id()
      req(editing_route)

      service_id <- sched_edit_service_id()
      req(service_id)

      current_data <- ssfs()

      first_dep <- sched_format_time(input$sched_batch_first_dep)
      last_dep <- sched_format_time(input$sched_batch_last_dep)

      if (is.null(first_dep) || is.null(last_dep)) {
        showNotification(
          "Invalid time format. Use HH:MM:SS (00-30:00-59:00-59).",
          type = "error"
        )
        return()
      }

      if (first_dep >= last_dep) {
        showNotification(
          "First departure must be before last departure.",
          type = "warning"
        )
        return()
      }

      # Get all itin_ids for this route
      route_itin_ids <- current_data$itin$itin_id[
        current_data$itin$route_id == editing_route
      ]

      if (length(route_itin_ids) == 0) {
        showNotification("No itineraries for this route.", type = "warning")
        return()
      }

      first_dep_hour <- as.numeric(substr(first_dep, 1, 2))
      last_dep_hour <- as.numeric(substr(last_dep, 1, 2))
      new_hours <- sprintf("%02d:00:00", first_dep_hour:last_dep_hour)

      for (itin_id in route_itin_ids) {
        # Remove existing spans for this itin + service
        current_data$span <- current_data$span[
          !(current_data$span$itin_id == itin_id &
            current_data$span$service_id == service_id),
        ]

        # Stash existing hsh entries for this itin + service
        # (to join later and add headway and speeds info)

        stash_headways_speeds <-
          current_data$hsh[
            (current_data$hsh$itin_id == itin_id &
              current_data$hsh$service_id == service_id),
          ] |>
          select(hour_dep, headway, speed)

        # Remove existing hsh entries for this itin + service
        current_data$hsh <- current_data$hsh[
          !(current_data$hsh$itin_id == itin_id &
            current_data$hsh$service_id == service_id),
        ]

        # Add new span
        new_span <- data.frame(
          itin_id = itin_id,
          service_id = service_id,
          service_window = 1L,
          first_dep = first_dep,
          last_dep = last_dep,
          stringsAsFactors = FALSE
        )
        current_data$span <- rbind(current_data$span, new_span)

        # speed values for new hour deps
        if (length(stash_headways_speeds$speed) > 0) {
          speed_value <- round(mean(stash_headways_speeds$speed), 1)
        } else {
          speed_value <- sched_get_speed_for_itin(itin_id, current_data)
        }

        # Add new hsh entries
        new_hsh <- data.frame(
          itin_id = rep(itin_id, length(new_hours)),
          service_id = rep(service_id, length(new_hours)),
          hour_dep = new_hours,
          stringsAsFactors = FALSE
        )

        # apply stashed headways and speeds
        if (length(stash_headways_speeds$speed) > 0) {
          new_hsh <-
            new_hsh |>
            left_join(stash_headways_speeds, by = "hour_dep") |>
            mutate(
              headway = if_else(is.na(headway), NA_real_, headway),
              speed = if_else(is.na(speed), speed_value, speed)
            )
        } else {
          new_hsh <-
            new_hsh |>
            mutate(headway = NA_real_, speed = speed_value)
        }

        current_data$hsh <- rbind(current_data$hsh, new_hsh)
      }

      ssfs(current_data)

      showNotification(
        paste0(
          "Span ",
          first_dep,
          " - ",
          last_dep,
          " applied to ",
          length(route_itin_ids),
          " itinerary(ies) for service ",
          service_id
        ),
        type = "message"
      )
    })

    # --- Apply service level preset to all route itineraries ---

    observeEvent(input$sched_batch_apply_preset, {
      editing_route <- sched_editing_route_id()
      req(editing_route)

      service_id <- sched_edit_service_id()
      req(service_id)

      pattern_id <- input$sched_batch_preset
      req(pattern_id)

      current_data <- ssfs()
      sp_data <- service_patterns()

      if (!pattern_id %in% names(sp_data$service_patterns)) {
        showNotification("Selected preset not found.", type = "error")
        return()
      }

      pattern_data <- sp_data$service_patterns[[pattern_id]]
      pattern_headways <- setNames(pattern_data$headway, pattern_data$hour)

      route_itin_ids <- current_data$itin$itin_id[
        current_data$itin$route_id == editing_route
      ]

      updated_count <- 0L

      for (itin_id in route_itin_ids) {
        match_idx <- which(
          current_data$hsh$itin_id == itin_id &
            current_data$hsh$service_id == service_id
        )

        for (idx in match_idx) {
          hour <- current_data$hsh$hour_dep[idx]
          if (hour %in% names(pattern_headways)) {
            current_data$hsh$headway[idx] <- pattern_headways[[hour]]
            updated_count <- updated_count + 1L
          }
        }
      }

      ssfs(current_data)

      pattern_name <- sp_data$service_pattern_names$pattern_name[
        sp_data$service_pattern_names$pattern_id == pattern_id
      ]

      showNotification(
        paste0(
          "Applied '",
          pattern_name,
          "' to ",
          length(route_itin_ids),
          " itinerary(ies). ",
          updated_count,
          " hour entries updated."
        ),
        type = "message"
      )
    })

    # --- Apply speed to all route itineraries ---

    observeEvent(input$sched_batch_apply_speed, {
      editing_route <- sched_editing_route_id()
      req(editing_route)

      service_id <- sched_edit_service_id()
      req(service_id)

      speed_value <- input$sched_batch_speed
      req(speed_value)

      current_data <- ssfs()

      route_itin_ids <- current_data$itin$itin_id[
        current_data$itin$route_id == editing_route
      ]

      match_idx <- which(
        current_data$hsh$itin_id %in%
          route_itin_ids &
          current_data$hsh$service_id == service_id
      )

      if (length(match_idx) == 0) {
        showNotification(
          "No headway entries found. Define spans first.",
          type = "warning"
        )
        return()
      }

      current_data$hsh$speed[match_idx] <- speed_value
      ssfs(current_data)

      showNotification(
        paste0(
          "Speed set to ",
          speed_value,
          " km/h for ",
          length(match_idx),
          " entries across ",
          length(route_itin_ids),
          " itinerary(ies)."
        ),
        type = "message"
      )
    })
  })
}
