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

scheduleServer <- function(id, ssfs, map_center) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Send namespace prefix to JS
    session$sendCustomMessage("setSchedNs", ns(""))

    # -- Reactive values
    sched_highlighted_route <- reactiveVal(NULL)
    sched_highlighted_itin_ids <- reactiveVal(character(0))
    sched_editing_route_id <- reactiveVal(NULL)
    sched_zoom <- reactiveVal(12)

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

        rows[[length(rows) + 1]] <- div(
          class = paste0(
            "route-list-row",
            if (is_selected) " expanded" else ""
          ),
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
      sched_editing_route_id(route_id)

      current_data <- ssfs()
      sched_highlighted_route(route_id)
      route_itin_ids <- current_data$itin$itin_id[
        current_data$itin$route_id == route_id
      ]
      sched_highlighted_itin_ids(route_itin_ids)

      showNotification(
        paste0(
          "Schedule editing for route '",
          route_id,
          "' \u2014 coming soon"
        ),
        type = "message"
      )
    })

    # Placeholder: editing UI below the map
    output$sched_editing_ui <- renderUI({
      editing_route <- sched_editing_route_id()

      if (is.null(editing_route)) {
        return(
          div(
            style = "color: grey; text-align: center; padding: 20px;",
            tags$em(
              "Click the pencil icon on a route to edit its schedule."
            )
          )
        )
      }

      current_data <- ssfs()
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

      wellPanel(
        h4(paste0("Schedule: ", route_display)),
        tags$em(
          style = "color: grey;",
          "Span and headway editing will appear here."
        )
      )
    })
  })
}
