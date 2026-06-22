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

      # Below-map area: placeholder for schedule editing and config buttons for calendar and service level presets
      div(
        id = "sched-editing-area",
        style = "margin-top: 15px;",
        uiOutput(ns("sched_editing_ui")),

        # Configuration buttons: always visible below editing panels
        div(
          class = "sched-config-buttons",
          tags$button(
            class = "sched-config-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("sched_open_calendar")
            ),
            tags$span(icon("gear")),
            "Configure service calendar"
          ),
          tags$button(
            class = "sched-config-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("sched_open_presets")
            ),
            tags$span(icon("gear")),
            "Manage service level presets"
          )
        )
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

    sched_map_ready <- reactiveVal(FALSE)
    sched_prev_routes_hash <- reactiveVal(NULL)
    sched_prev_stops_hash <- reactiveVal(NULL)
    sched_prev_highlight_hash <- reactiveVal(NULL)

    # itin_id selected for editing in the itinerary-level panel (right side)
    sched_editing_itin_id <- reactiveVal(NULL)

    # service_id selected in the route-level schedule panel
    # (separate from the map filter panel's sched_service_id)
    sched_edit_service_id <- reactiveVal(NULL)

    # span editing
    sched_span_editing_idx <- reactiveVal(NULL)
    sched_span_adding <- reactiveVal(FALSE)

    #service level presets
    service_patterns <- reactiveVal(
      list(
        service_patterns = list(
          SP1 = data.frame(
            hour = c(
              "05:00:00",
              "06:00:00",
              "07:00:00",
              "08:00:00",
              "09:00:00",
              "10:00:00",
              "11:00:00",
              "12:00:00",
              "13:00:00",
              "14:00:00",
              "15:00:00",
              "16:00:00",
              "17:00:00",
              "18:00:00",
              "19:00:00",
              "20:00:00",
              "21:00:00",
              "22:00:00",
              "23:00:00",
              "24:00:00",
              "25:00:00"
            ),
            headway = c(
              15,
              5,
              5,
              5,
              5,
              8,
              8,
              8,
              8,
              8,
              5,
              5,
              5,
              5,
              12,
              12,
              15,
              15,
              15,
              15,
              15
            ),
            stringsAsFactors = FALSE
          ),
          SP2 = data.frame(
            hour = c(
              "05:00:00",
              "06:00:00",
              "07:00:00",
              "08:00:00",
              "09:00:00",
              "10:00:00",
              "11:00:00",
              "12:00:00",
              "13:00:00",
              "14:00:00",
              "15:00:00",
              "16:00:00",
              "17:00:00",
              "18:00:00",
              "19:00:00",
              "20:00:00",
              "21:00:00",
              "22:00:00",
              "23:00:00",
              "24:00:00",
              "25:00:00"
            ),
            headway = c(
              30,
              10,
              10,
              10,
              10,
              30,
              30,
              30,
              30,
              30,
              10,
              10,
              10,
              10,
              30,
              30,
              30,
              30,
              30,
              30,
              30
            ),
            stringsAsFactors = FALSE
          )
        ),
        service_pattern_names = data.frame(
          pattern_id = c("SP1", "SP2"),
          pattern_name = c("All Day Frequent", "Peak Frequent"),
          stringsAsFactors = FALSE
        )
      )
    )

    #schedule preset choices
    sched_preset_choices <- reactive({
      sp_data <- service_patterns()
      if (
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
    })

    # Hour dep of hsh row being edited
    sched_hsh_editing_hour <- reactiveVal(NULL)

    #calendar editing
    sched_cal_editing_id <- reactiveVal(NULL) # service_id being edited
    sched_cal_adding <- reactiveVal(FALSE) # TRUE when add form is open
    sched_cal_cost_result <- reactiveVal(NULL) # result of service cost calculation

    #Service level preset editing
    sched_preset_editing_id <- reactiveVal(NULL) # pattern_id being viewed/edited
    sched_preset_adding <- reactiveVal(FALSE) # TRUE when creating new preset
    sched_preset_hour_editing <- reactiveVal(NULL) # hour being edited in detail table
    sched_preset_hour_adding <- reactiveVal(FALSE) # TRUE when adding new hour row

    #Speed factor editing
    sched_sp_speed_factors <- reactiveVal(NULL)
    sched_sp_stop_data <- reactiveVal(NULL)
    sched_sp_base_speed <- reactiveVal(20)
    sched_sp_observers_created <- reactiveVal(0L)
    sched_sp_text_outputs_created <- reactiveVal(0L)
    sched_sp_factors_visible <- reactiveVal(FALSE)

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
        leaflet::addMapPane("sched_routes_pane", zIndex = 410) |>
        leaflet::addMapPane("sched_highlight_pane", zIndex = 420) |>
        leaflet::addMapPane("sched_stops_pane", zIndex = 430) |>
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
                if (layer.options && layer.options.group === 'sched_stops' &&
                    typeof layer.setRadius === 'function') {
                  layer.setRadius(r);
                }
              });
            }

            map.on('zoomend', function(e) {
              Shiny.setInputValue(ns + 'sched_map_zoom', map.getZoom());
              resizeStopMarkers();
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

    observeEvent(
      map_center(),
      {
        sched_map_ready(FALSE)
        sched_prev_routes_hash(NULL)
        sched_prev_stops_hash(NULL)
        sched_prev_highlight_hash(NULL)
      },
      priority = 10
    )

    observeEvent(
      input$sched_map_bounds,
      {
        sched_map_ready(TRUE)
      },
      once = FALSE
    )

    sched_clear_itin_subedits <- function() {
      sched_span_editing_idx(NULL)
      sched_span_adding(FALSE)
      sched_hsh_editing_hour(NULL)
    }

    sched_fit_bounds <- function(geom) {
      if (is.null(geom) || length(geom) == 0) {
        return(invisible(NULL))
      }

      bbox <- st_bbox(geom)
      leaflet::leafletProxy("sched_map") |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    }

    sched_draw_highlight_group <- function(proxy, current_data, hl_ids) {
      proxy <- proxy |>
        leaflet::clearGroup("sched_highlight")

      if (
        length(hl_ids) == 0 ||
          is.null(current_data$itin) ||
          nrow(current_data$itin) == 0
      ) {
        return(proxy)
      }

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
            options = leaflet::pathOptions(pane = "sched_highlight_pane"),
            color = "#FFE999",
            weight = 10,
            opacity = 0.4,
            stroke = TRUE
          )
      }

      proxy
    }

    # Observe: redraw shapes, stops and highlights on schedule map when relevant data or highlights change,
    # using hashing to skip if no relevant changes

    # ---- Route shapes ----
    observe({
      req(sched_map_ready())
      current_data <- ssfs()

      routes_hash <- digest::digest(list(
        itin = current_data$itin,
        routes = current_data$routes[,
          c(
            "route_id",
            "route_short_name",
            "route_long_name",
            "route_color",
            "route_type"
          ),
          drop = FALSE
        ]
      ))

      if (identical(routes_hash, isolate(sched_prev_routes_hash()))) {
        return()
      }
      sched_prev_routes_hash(routes_hash)

      proxy <- leaflet::leafletProxy("sched_map") |>
        leaflet::clearGroup("sched_routes")

      # Draw all itinerary shapes
      if (!is.null(current_data$itin) && nrow(current_data$itin) > 0) {
        draw_order <- itineraryDrawOrder(
          current_data$itin,
          current_data$routes
        )

        for (i in draw_order) {
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
              group = "sched_routes",
              options = leaflet::pathOptions(pane = "sched_routes_pane"),
              color = line_color,
              weight = line_weight,
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
    })

    # ---- Highlight underlay ----
    observe({
      req(sched_map_ready())
      current_data <- ssfs()
      hl_ids <- sched_highlighted_itin_ids()

      highlight_hash <- digest::digest(list(
        itin = current_data$itin,
        highlighted = hl_ids
      ))

      if (identical(highlight_hash, isolate(sched_prev_highlight_hash()))) {
        return()
      }
      sched_prev_highlight_hash(highlight_hash)

      sched_draw_highlight_group(
        leaflet::leafletProxy("sched_map"),
        current_data,
        hl_ids
      )
    })

    # ---- Stop markers ----
    observe({
      req(sched_map_ready())
      current_data <- ssfs()

      stops_hash <- digest::digest(list(
        stops = current_data$stops,
        stop_itins = current_data$stop_seq[,
          c("stop_id", "itin_id"),
          drop = FALSE
        ]
      ))

      if (identical(stops_hash, isolate(sched_prev_stops_hash()))) {
        return()
      }
      sched_prev_stops_hash(stops_hash)

      proxy <- leaflet::leafletProxy("sched_map") |>
        leaflet::clearGroup("sched_stops")

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
              "</b> - ",
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
            radius = calculateMarkerSize(isolate(sched_zoom())),
            label = hover_labels,
            labelOptions = leaflet::labelOptions(
              style = list("font-size" = "11px", "padding" = "3px 6px"),
              direction = "top",
              offset = c(0, -8)
            ),
            group = "sched_stops",
            options = leaflet::pathOptions(pane = "sched_stops_pane")
          )
      }
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

      sorted_routes <- current_data$routes[
        order(
          current_data$routes$route_type,
          current_data$routes$route_short_name
        ),
      ]

      for (r in seq_len(nrow(sorted_routes))) {
        route <- sorted_routes[r, ]

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
          onclick = sprintf("schedEditRoute('%s')", route$route_id),
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
          )
        )
      }

      do.call(tagList, rows)
    })

    # Route lick handler: set editing route
    observeEvent(input$sched_route_edit_click, {
      route_id <- input$sched_route_edit_click$id
      current_data <- ssfs()

      route_itins <- current_data$itin[
        current_data$itin$route_id == route_id,
      ]

      route_itin_ids <- current_data$itin$itin_id[
        current_data$itin$route_id == route_id
      ]

      if (
        !is.null(sched_editing_route_id()) &&
          sched_editing_route_id() == route_id
      ) {
        sched_editing_route_id(NULL)
        sched_editing_itin_id(NULL)
        sched_clear_itin_subedits()
        sched_highlighted_route(route_id)
        sched_highlighted_itin_ids(route_itin_ids)
        return()
      }

      sched_clear_itin_subedits()
      sched_editing_route_id(route_id)
      sched_editing_itin_id(NULL)

      sched_highlighted_route(route_id)
      sched_highlighted_itin_ids(route_itin_ids)

      if (nrow(route_itins) > 0) {
        sched_fit_bounds(route_itins$geometry)
      }

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

      itin_row <- current_data$itin[
        current_data$itin$itin_id == itin_id,
      ]

      if (
        !is.null(sched_editing_itin_id()) &&
          sched_editing_itin_id() == itin_id
      ) {
        sched_editing_itin_id(NULL)

        editing_route <- sched_editing_route_id()
        if (!is.null(editing_route)) {
          route_itins <- current_data$itin[
            current_data$itin$route_id == editing_route,
          ]
          route_itin_ids <- route_itins$itin_id
          sched_highlighted_itin_ids(route_itin_ids)

          if (nrow(route_itins) > 0) {
            sched_fit_bounds(route_itins$geometry)
          }
        }
      } else {
        sched_editing_itin_id(itin_id)
        sched_highlighted_itin_ids(itin_id)

        if (nrow(itin_row) > 0) {
          sched_fit_bounds(itin_row$geometry)
        }
      }
    })

    observeEvent(input$sched_itin_edit_click, {
      itin_id <- input$sched_itin_edit_click$id
      current_data <- ssfs()

      if (
        !is.null(sched_editing_itin_id()) &&
          sched_editing_itin_id() == itin_id
      ) {
        sched_editing_itin_id(NULL)
        sched_clear_itin_subedits()

        editing_route <- sched_editing_route_id()
        if (!is.null(editing_route)) {
          route_itin_ids <- current_data$itin$itin_id[
            current_data$itin$route_id == editing_route
          ]
          sched_highlighted_itin_ids(route_itin_ids)
        }
        return()
      }

      sched_clear_itin_subedits()
      sched_editing_itin_id(itin_id)
      sched_highlighted_itin_ids(itin_id)
    })

    observeEvent(input$sched_edit_service_select, {
      sched_edit_service_id(input$sched_edit_service_select)
    })

    # Route-level schedule editing UI renderer
    output$sched_editing_ui <- renderUI({
      editing_route <- sched_editing_route_id()
      ns <- session$ns

      if (is.null(editing_route)) {
        return(
          div(
            style = "color: grey; text-align: center; padding: 20px;",
            tags$em("Click on a route to edit its schedule.")
          )
        )
      }

      div(
        div(
          class = "sched-editing-container",

          div(
            class = "sched-route-panel",
            uiOutput(ns("sched_route_panel_header_ui")),

            h5(tagList(
              "Itineraries",
              info_popover(
                "Each itinerary consists of a unique stop pattern or variant for trips for this route"
              )
            )),
            uiOutput(ns("sched_route_itin_rows_ui")),

            uiOutput(ns("sched_route_batch_actions_ui")),

            uiOutput(ns("sched_route_cost_ui"))
          ),

          div(
            class = "sched-itin-panel",
            uiOutput(ns("sched_itin_editing_ui")),
            uiOutput(ns("sched_itin_cost_ui"))
          )
        ),
        uiOutput(ns("sched_speed_profile_ui"))
      )
    })

    output$sched_route_panel_header_ui <- renderUI({
      current_data <- ssfs()
      editing_route <- sched_editing_route_id()
      req(editing_route)

      ns <- session$ns

      route_row <- current_data$routes[
        current_data$routes$route_id == editing_route,
      ]
      route_display <- if (nrow(route_row) > 0) {
        paste0(
          route_row$route_short_name[1],
          " - ",
          route_row$route_long_name[1]
        )
      } else {
        editing_route
      }

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

      tagList(
        h4(paste0("Schedule: ", route_display)),
        selectInput(
          ns("sched_edit_service_select"),
          label = tagList(
            "Service",
            info_popover(
              "A service is a set of dates and days of the week during which different route schedules operate (e.g. weekday service vs. weekend), as configured in the Service Calendar (bottom left of this module)."
            )
          ),
          choices = service_choices,
          selected = selected_service,
          width = "100%"
        )
      )
    })

    output$sched_route_batch_actions_ui <- renderUI({
      ns <- session$ns
      preset_choices <- sched_preset_choices()

      div(
        class = "sched-batch-section",

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

        h5(tagList(
          "Apply service level preset to all route itineraries",
          info_popover(
            "A service level preset defines a headway pattern by hour of day, reusable across itineraries. Applying one here will overwrite the hourly headways of all itineraries on this route for the selected service. The presets manager is at the bottom right of this module."
          )
        )),

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

        h5("Apply headway and speed to all route itineraries"),
        div(
          class = "sched-batch-row",
          div(
            tags$label("Headway (min)"),
            numericInput(
              ns("sched_batch_headway"),
              label = NULL,
              value = 10,
              min = 1,
              max = 120,
              width = "100px"
            )
          ),
          tags$button(
            class = "btn-save",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("sched_batch_apply_headway")
            ),
            "Apply"
          ),
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
    })

    output$sched_route_cost_ui <- renderUI({
      current_data <- ssfs()
      editing_route <- sched_editing_route_id()
      service_id <- sched_edit_service_id()
      req(editing_route, service_id)

      ns <- session$ns

      cost <- tryCatch(
        generate_service_cost(
          ssfs = current_data,
          id_type = "route_id",
          id = editing_route,
          service = service_id
        ),
        error = function(e) NULL
      )

      total_h <- if (
        !is.null(cost) &&
          nrow(cost) > 0 &&
          is.numeric(cost$total_h) &&
          !all(is.na(cost$total_h))
      ) {
        sum(cost$total_h, na.rm = TRUE)
      } else {
        NULL
      }

      total_km <- if (
        !is.null(cost) &&
          nrow(cost) > 0 &&
          is.numeric(cost$total_km) &&
          !all(is.na(cost$total_km))
      ) {
        sum(cost$total_km, na.rm = TRUE)
      } else {
        NULL
      }

      display_h <- if (!is.null(total_h)) total_h else "-"
      display_km <- if (!is.null(total_km)) total_km else "-"

      div(
        style = "border-top: 1px solid var(--border-color); margin-top: 12px; 
        padding-top: 10px; font-size: 14px; color: var(--text-color);",
        div(paste0("Daily vehicle-hours (in service): ", display_h, " hours")),
        div(paste0("Daily vehicle-km (in service): ", display_km, " km"))
      )
    })

    output$sched_route_itin_rows_ui <- renderUI({
      current_data <- ssfs()
      editing_route <- sched_editing_route_id()
      req(editing_route)

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

      route_itins <- current_data$itin[
        current_data$itin$route_id == editing_route,
      ]

      itin_rows <- list()

      if (nrow(route_itins) > 0) {
        for (i in seq_len(nrow(route_itins))) {
          itin <- route_itins[i, ]
          itin_id <- itin$itin_id
          itin_length <- round(as.numeric(st_length(itin$geometry)) / 1000, 1)

          itin_spans <- current_data$span[
            current_data$span$itin_id == itin_id &
              current_data$span$service_id == selected_service,
          ]

          has_spans <- nrow(itin_spans) > 0

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

          itin_hsh <- current_data$hsh[
            current_data$hsh$itin_id == itin_id &
              current_data$hsh$service_id == selected_service,
          ]

          len_avg_speed <- if (
            nrow(itin_hsh) > 0 &&
              any(!is.na(itin_hsh$speed))
          ) {
            paste0(
              itin_length,
              " km | ",
              round(mean(itin_hsh$speed, na.rm = TRUE), 1),
              " km/h"
            )
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

            span(
              class = "itin-direction-badge",
              if (as.integer(itin$direction_id) == 0) "Out" else "In"
            ),

            div(
              class = "sched-itin-main",
              div(
                class = "sched-itin-header",
                span(class = "itin-headsign", itin$trip_headsign),
                span(class = "itin-id-display", paste0("(", itin_id, ")"))
              ),
              if (!is.null(span_text)) {
                div(class = "sched-itin-spans", span_text)
              },
              if (!is.null(len_avg_speed)) {
                div(class = "sched-itin-speed", len_avg_speed)
              }
            ),
          )
        }
      } else {
        itin_rows[[1]] <- tags$small(
          style = "color: grey;",
          "No itineraries for this route."
        )
      }

      do.call(tagList, itin_rows)
    })

    output$sched_speed_profile_ui <- renderUI({
      editing_itin <- sched_editing_itin_id()

      if (is.null(editing_itin)) {
        return(NULL)
      }

      current_data <- ssfs()
      ns <- session$ns

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

      div(
        class = "sched-speed-profile-section",

        div(
          class = "sched-speed-profile-top",

          div(
            class = "sched-speed-profile-controls",
            h4(paste0("Speed profile: ", itin_display)),
            tags$label("Hour"),
            selectInput(
              ns("sched_sp_hour"),
              label = NULL,
              choices = NULL,
              width = "100%"
            ),
            div(
              class = "info-text",
              "Speed factors are defined once per itinerary ",
              "and apply to all services and hours. Changing ",
              "hour only changes the displayed speeds (km/h)"
            )
          ),

          div(
            class = "sched-speed-profile-graph",
            plotly::plotlyOutput(
              ns("sched_sp_plot"),
              height = "300px"
            )
          )
        ),

        # Collapsible speed factors adjustment
        div(
          class = "sched-speed-factors-section",
          div(
            class = "sched-speed-factors-toggle",
            onclick = "schedSpToggleFactors()",
            span(
              id = ns("sched_sf_arrow"),
              class = paste0(
                "toggle-arrow",
                if (isolate(sched_sp_factors_visible())) " expanded" else ""
              ),
              htmltools::HTML("&#9654;")
            ),
            "Adjust speed factors"
          ),
          div(
            id = ns("sched_sf_content"),
            style = if (isolate(sched_sp_factors_visible())) {
              "display: block;"
            } else {
              "display: none;"
            },
            uiOutput(ns("sched_sp_table_ui"))
          )
        )
      )
    })

    # Right-side itinerary level schedule editor
    output$sched_itin_editing_ui <- renderUI({
      editing_itin <- sched_editing_itin_id()
      span_editing_idx <- sched_span_editing_idx()
      is_adding <- sched_span_adding()
      ns <- session$ns

      if (is.null(editing_itin)) {
        return(
          div(
            style = "color: grey; text-align: center; padding: 40px 20px;",
            tags$em(
              "Click on an itinerary to edit its headways and speeds."
            )
          )
        )
      }

      current_data <- ssfs()
      service_id <- sched_edit_service_id()
      preset_choices <- sched_preset_choices()

      itin_row <- current_data$itin[
        current_data$itin$itin_id == editing_itin,
      ]
      if (nrow(itin_row) == 0) {
        return(NULL)
      }

      itin_len_km <- round(as.numeric(st_length(itin_row$geometry)) / 1000, 1)

      itin_display <- paste0(
        itin_row$trip_headsign[1],
        " (",
        editing_itin,
        ")"
      )

      itin_spans <- current_data$span[
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id,
      ]
      if (nrow(itin_spans) > 0) {
        itin_spans <- itin_spans[order(itin_spans$service_window), ]
      }

      span_rows <- list()

      if (nrow(itin_spans) > 0) {
        for (s in seq_len(nrow(itin_spans))) {
          sw <- itin_spans$service_window[s]
          fd <- itin_spans$first_dep[s]
          ld <- itin_spans$last_dep[s]
          is_editing_this <- !is.null(span_editing_idx) &&
            span_editing_idx == s

          if (is_editing_this) {
            span_rows[[length(span_rows) + 1]] <- div(
              class = "sched-span-edit-form",
              div(
                class = "sched-span-label",
                paste0("Service window ", sw)
              ),
              div(
                style = "display: flex; gap: 8px; align-items: flex-end;",
                div(
                  tags$label("First departure"),
                  tags$input(
                    type = "text",
                    id = ns("sched_span_edit_first_dep"),
                    value = fd,
                    placeholder = "HH:MM:SS"
                  )
                ),
                div(
                  tags$label("Last departure"),
                  tags$input(
                    type = "text",
                    id = ns("sched_span_edit_last_dep"),
                    value = ld,
                    placeholder = "HH:MM:SS"
                  )
                )
              ),
              div(
                class = "btn-row",
                tags$button(
                  class = "btn-save",
                  onclick = "schedSaveSpanEdit()",
                  htmltools::HTML("&#10003; Save")
                ),
                tags$button(
                  class = "btn-cancel",
                  onclick = "schedCancelSpanEdit()",
                  "Cancel"
                )
              )
            )
          } else {
            span_rows[[length(span_rows) + 1]] <- div(
              class = "sched-span-row",
              div(
                class = "sched-span-info",
                div(class = "sched-span-label", paste0("Window ", sw)),
                div(class = "sched-span-times", paste0(fd, " \u2014 ", ld))
              ),
              div(
                class = "sched-span-actions",
                tags$button(
                  class = "route-action-btn edit-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); schedEditSpan(%d)",
                    s
                  ),
                  title = "Edit service window",
                  htmltools::HTML("&#9998;")
                ),
                tags$button(
                  class = "route-action-btn delete-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); schedDeleteSpan(%d)",
                    s
                  ),
                  title = "Delete service window",
                  htmltools::HTML(
                    '<i class="fa-solid fa-trash"></i>'
                  )
                )
              )
            )
          }
        }
      }

      if (is_adding) {
        existing_spans <- current_data$span[
          current_data$span$itin_id == editing_itin &
            current_data$span$service_id == service_id,
        ]
        if (nrow(existing_spans) == 0) {
          default_first <- "05:00:00"
          default_last <- "23:00:00"
        } else {
          max_window <- max(existing_spans$service_window, na.rm = TRUE)
          prev_last <- existing_spans$last_dep[
            existing_spans$service_window == max_window
          ][1]
          prev_last_hour <- as.numeric(substr(prev_last, 1, 2))
          default_first <- sprintf("%02d:00:00", prev_last_hour + 1)
          default_last <- sprintf("%02d:00:00", min(prev_last_hour + 4, 29))
        }

        span_rows[[length(span_rows) + 1]] <- div(
          class = "sched-span-edit-form",
          div(
            class = "sched-span-label",
            "New service window"
          ),
          div(
            style = "display: flex; gap: 8px; align-items: flex-end;",
            div(
              tags$label("First departure"),
              tags$input(
                type = "text",
                id = ns("sched_span_edit_first_dep"),
                value = default_first,
                placeholder = "HH:MM:SS"
              )
            ),
            div(
              tags$label("Last departure"),
              tags$input(
                type = "text",
                id = ns("sched_span_edit_last_dep"),
                value = default_last,
                placeholder = "HH:MM:SS"
              )
            )
          ),
          div(
            class = "btn-row",
            tags$button(
              class = "btn-save",
              onclick = "schedSaveNewSpan()",
              "Create"
            ),
            tags$button(
              class = "btn-cancel",
              onclick = "schedCancelSpanEdit()",
              "Cancel"
            )
          )
        )
      } else {
        span_rows[[length(span_rows) + 1]] <- div(
          class = "sched-span-add-row",
          onclick = "schedAddSpan()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); schedAddSpan()",
            title = "Add new service window",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new service window")
        )
      }

      # headways and speeds editing UI
      hsh_editing_hour <- sched_hsh_editing_hour()

      itin_hsh <- current_data$hsh[
        current_data$hsh$itin_id == editing_itin &
          current_data$hsh$service_id == service_id,
      ]

      if (nrow(itin_hsh) > 0) {
        itin_hsh <- itin_hsh[order(itin_hsh$hour_dep), ]

        hsh_rows <- list()

        for (h in seq_len(nrow(itin_hsh))) {
          hour_val <- itin_hsh$hour_dep[h]
          hdwy_val <- itin_hsh$headway[h]
          speed_val <- itin_hsh$speed[h]
          runtime_val <- if (!is.na(speed_val) && speed_val > 0) {
            round((itin_len_km / speed_val) * 60, 1)
          } else {
            NA
          }
          is_editing_this <- !is.null(hsh_editing_hour) &&
            hsh_editing_hour == hour_val

          if (is_editing_this) {
            hsh_rows[[length(hsh_rows) + 1]] <- tags$tr(
              class = "sched-hsh-row sched-hsh-editing",
              tags$td(
                colspan = "5",
                div(
                  class = "sched-hsh-edit-form",
                  div(
                    class = "edit-fields",
                    div(
                      tags$label("Hour"),
                      tags$input(
                        type = "text",
                        value = hour_val,
                        disabled = "disabled",
                        style = "background-color: #eee; color: #888;"
                      )
                    ),
                    div(
                      tags$label("Headway (min)"),
                      tags$input(
                        type = "number",
                        id = ns("sched_hsh_edit_headway"),
                        value = if (!is.na(hdwy_val)) hdwy_val else "",
                        min = "1",
                        max = "119",
                        placeholder = "e.g. 10"
                      )
                    ),
                    div(
                      tags$label("Speed (km/h)"),
                      tags$input(
                        type = "number",
                        id = ns("sched_hsh_edit_speed"),
                        value = if (!is.na(speed_val)) speed_val else "20",
                        min = "5",
                        max = "431"
                      )
                    )
                  ),
                  div(
                    class = "btn-row",
                    tags$button(
                      class = "btn-save",
                      onclick = "schedSaveHshEdit()",
                      htmltools::HTML("&#10003; Save")
                    ),
                    tags$button(
                      class = "btn-cancel",
                      onclick = "schedCancelHshEdit()",
                      "Cancel"
                    )
                  )
                )
              )
            )
          } else {
            hdwy_display <- if (is.na(hdwy_val)) {
              " - "
            } else {
              as.character(hdwy_val)
            }
            speed_display <- if (is.na(speed_val)) {
              " - "
            } else {
              as.character(speed_val)
            }
            hdwy_class <- if (is.na(hdwy_val)) "hsh-cell-na" else ""
            speed_class <- if (is.na(speed_val)) "hsh-cell-na" else ""
            runtime_display <- if (is.na(runtime_val)) {
              " - "
            } else {
              as.character(runtime_val)
            }
            runtime_class <- if (is.na(runtime_val)) "hsh-cell-na" else ""

            hsh_rows[[length(hsh_rows) + 1]] <- tags$tr(
              class = "sched-hsh-row",
              onclick = sprintf("schedEditHshRow('%s')", hour_val),
              tags$td(hour_val),
              tags$td(class = hdwy_class, hdwy_display),
              tags$td(class = speed_class, speed_display),
              tags$td(class = runtime_class, runtime_display),
              tags$td(
                style = "text-align: right;",
                tags$button(
                  class = "route-action-btn edit-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); schedEditHshRow('%s')",
                    hour_val
                  ),
                  title = "Edit row",
                  htmltools::HTML("&#9998;")
                )
              )
            )
          }
        }

        hsh_table_ui <- tags$table(
          class = "sched-hsh-table",
          tags$thead(
            tags$tr(
              tags$th("Hour"),
              tags$th("Headway (min)"),
              tags$th("Speed (km/h)"),
              tags$th("Runtime (mins)"),
              tags$th(style = "width: 40px;", "")
            )
          ),
          tags$tbody(
            do.call(tagList, hsh_rows)
          )
        )
      } else {
        hsh_table_ui <- div(
          style = "color: grey; text-align: center; padding: 15px;",
          tags$em("No headway entries. Add a service window first.")
        )
      }

      # Default preset name from route, itin, and service context
      route_id_for_itin <- current_data$itin$route_id[
        current_data$itin$itin_id == editing_itin
      ][1]
      route_short_for_preset <- if (!is.null(route_id_for_itin)) {
        r <- current_data$routes$route_short_name[
          current_data$routes$route_id == route_id_for_itin
        ]
        if (length(r) > 0) r[1] else ""
      } else {
        ""
      }
      default_preset_name <- paste0(
        route_short_for_preset,
        " - ",
        editing_itin,
        " - ",
        service_id
      )

      tagList(
        h4(paste0("Itinerary: ", itin_display, " - ", service_id)),
        h5(tagList(
          "Service windows",
          info_popover(
            "A service window defines a time span during which a given itinerary operates for a specific service, defined by a first departure time and a last departure time."
          )
        )),
        do.call(tagList, span_rows),
        hr(),

        # Apply service level preset
        div(
          class = "sched-itin-batch-row",
          div(
            style = "flex: 1; min-width: 0;",
            tags$label(tagList(
              "Apply service level preset",
              info_popover(
                "A service level preset defines a headway pattern by hour of day, reusable across itineraries. Applying one here will overwrite the hourly headways of this itinerary for the selected service. The presets manager is at the bottom right of this module."
              )
            )),
            div(
              style = "display: flex; gap: 6px; align-items: flex-end;",
              div(
                style = "flex: 1; min-width: 0;",
                selectInput(
                  ns("sched_itin_preset"),
                  label = NULL,
                  choices = preset_choices,
                  width = "100%"
                )
              ),
              tags$button(
                class = "btn-save",
                style = "margin-bottom: 0;",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                  ns("sched_itin_apply_preset")
                ),
                "Apply"
              )
            )
          )
        ),

        # Apply headway and speed to all hours
        div(
          class = "sched-itin-batch-row",

          # Apply headway
          div(
            style = "flex-shrink: 0;",
            tags$label("Apply headway to all hours (min)"),
            div(
              style = "display: flex; gap: 6px; align-items: flex-end;",
              numericInput(
                ns("sched_itin_headway"),
                label = NULL,
                value = 10,
                min = 1,
                max = 120,
                width = "80px"
              ),
              tags$button(
                class = "btn-save",
                style = "margin-bottom: 0;",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                  ns("sched_itin_apply_headway")
                ),
                "Apply"
              )
            )
          ),

          # Apply speed
          div(
            style = "flex-shrink: 0;",
            tags$label("Apply speed to all hours (km/h)"),
            div(
              style = "display: flex; gap: 6px; align-items: flex-end;",
              numericInput(
                ns("sched_itin_speed"),
                label = NULL,
                value = 20,
                min = 5,
                max = 431,
                width = "80px"
              ),
              tags$button(
                class = "btn-save",
                style = "margin-bottom: 0;",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                  ns("sched_itin_apply_speed")
                ),
                "Apply"
              )
            )
          )
        ),

        hr(),
        h5(tagList(
          "Headways & speeds by hour",
          info_popover(
            "A headway is the interval or duration between trips. Headways and speeds specified here are used to create trips and scheduled stop times based on distances between stops along the routes defined in the previous module."
          )
        )),
        hsh_table_ui,

        div(
          class = "sched-batch-row",
          style = "margin-top: 10px;",
          div(
            style = "flex: 1; min-width: 0;",
            tags$label(tagList(
              "Save current headways as a new service level preset",
              info_popover(
                "Saves the hourly headways currently defined for this itinerary and service as a reusable preset that can be applied to other itineraries."
              )
            )),
            tags$input(
              type = "text",
              id = ns("sched_save_as_preset_name"),
              value = default_preset_name,
              style = "width: 100%; padding: 4px 8px; border: 1px solid var(--border-color); border-radius: 4px; font-size: 12px;"
            )
          ),
          tags$button(
            class = "btn-save",
            style = "margin-bottom: 0;",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {name: document.getElementById('%s').value, ts: Math.random()}, {priority:'event'})",
              ns("sched_save_as_preset"),
              ns("sched_save_as_preset_name")
            ),
            "Save as preset"
          )
        ),
      )
    })

    output$sched_itin_cost_ui <- renderUI({
      current_data <- ssfs()
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      req(editing_itin, service_id)

      cost <- tryCatch(
        generate_service_cost(
          ssfs = current_data,
          id_type = "itin_id",
          id = editing_itin,
          service = service_id
        ),
        error = function(e) NULL
      )

      total_h <- if (
        !is.null(cost) &&
          nrow(cost) > 0 &&
          is.numeric(cost$total_h) &&
          !all(is.na(cost$total_h))
      ) {
        sum(cost$total_h, na.rm = TRUE)
      } else {
        NULL
      }

      total_km <- if (
        !is.null(cost) &&
          nrow(cost) > 0 &&
          is.numeric(cost$total_km) &&
          !all(is.na(cost$total_km))
      ) {
        sum(cost$total_km, na.rm = TRUE)
      } else {
        NULL
      }

      display_h <- if (!is.null(total_h)) total_h else "-"
      display_km <- if (!is.null(total_km)) total_km else "-"

      div(
        style = "border-top: 1px solid var(--border-color); margin-top: 12px; padding-top: 10px; font-size: 14px; color: var(--text-color);",
        div(paste0("Daily vehicle-hours (in service): ", display_h)),
        div(paste0("Daily vehicle-km (in service): ", display_km))
      )
    })

    # OBSERVERS : route-level batch action edit observers-----------

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

    # --- Apply headway to all route itineraries ---

    observeEvent(input$sched_batch_apply_headway, {
      editing_route <- sched_editing_route_id()
      req(editing_route)

      service_id <- sched_edit_service_id()
      req(service_id)

      headway_value <- input$sched_batch_headway
      req(headway_value)

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

      current_data$hsh$headway[match_idx] <- headway_value
      ssfs(current_data)

      showNotification(
        paste0(
          "Headway set to ",
          headway_value,
          " min for ",
          length(match_idx),
          " entries across ",
          length(route_itin_ids),
          " itinerary(ies)."
        ),
        type = "message"
      )
    })

    # OBSERVERS : ITINERARY-LEVEL SPAN, HEADWAY AND SPEED EDITING---------

    observeEvent(input$sched_span_edit_click, {
      idx <- input$sched_span_edit_click$idx

      if (
        !is.null(sched_span_editing_idx()) && sched_span_editing_idx() == idx
      ) {
        sched_span_editing_idx(NULL)
        sched_span_adding(FALSE)
        return()
      }

      sched_span_adding(FALSE)
      sched_span_editing_idx(idx)
    })

    observeEvent(input$sched_span_cancel_edit, {
      sched_span_editing_idx(NULL)
      sched_span_adding(FALSE)
    })

    observeEvent(input$sched_span_add_click, {
      sched_span_editing_idx(NULL)
      sched_span_adding(TRUE)
    })

    # Save edit to existing span
    observeEvent(input$sched_span_save_edit, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      idx <- sched_span_editing_idx()
      req(editing_itin, service_id, idx)

      data <- input$sched_span_save_edit

      new_first_dep <- sched_format_time(data$first_dep)
      new_last_dep <- sched_format_time(data$last_dep)

      if (is.null(new_first_dep) || is.null(new_last_dep)) {
        showNotification(
          "Invalid time format. Use HH:MM:SS (00-30:00-59:00-59).",
          type = "error"
        )
        return()
      }

      if (new_first_dep >= new_last_dep) {
        showNotification(
          "First departure must be before last departure.",
          type = "warning"
        )
        return()
      }

      current_data <- ssfs()

      itin_spans <- current_data$span[
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id,
      ]
      itin_spans <- itin_spans[order(itin_spans$service_window), ]

      if (idx < 1 || idx > nrow(itin_spans)) {
        showNotification("Span not found.", type = "error")
        return()
      }

      target_sw <- itin_spans$service_window[idx]
      old_first_dep <- itin_spans$first_dep[idx]
      old_last_dep <- itin_spans$last_dep[idx]

      if (idx > 1) {
        prev_last <- itin_spans$last_dep[idx - 1]
        prev_last_sec <- sched_parse_time_to_seconds(prev_last)
        new_first_sec <- sched_parse_time_to_seconds(new_first_dep)
        if (new_first_sec <= prev_last_sec + 59) {
          showNotification(
            paste0(
              "Must start after ",
              prev_last,
              " (end of previous window)."
            ),
            type = "error"
          )
          return()
        }
      }

      if (idx < nrow(itin_spans)) {
        next_first <- itin_spans$first_dep[idx + 1]
        next_first_sec <- sched_parse_time_to_seconds(next_first)
        new_last_sec <- sched_parse_time_to_seconds(new_last_dep)
        if (new_last_sec >= next_first_sec - 59) {
          showNotification(
            paste0("Must end before ", next_first, " (start of next window)."),
            type = "error"
          )
          return()
        }
      }

      full_idx <- which(
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id &
          current_data$span$service_window == target_sw
      )

      current_data$span$first_dep[full_idx] <- new_first_dep
      current_data$span$last_dep[full_idx] <- new_last_dep

      old_hours <- sched_get_hours_for_span(old_first_dep, old_last_dep)
      new_hours <- sched_get_hours_for_span(new_first_dep, new_last_dep)

      all_spans <- current_data$span[
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id,
      ]
      all_covered_hours <- unique(unlist(
        lapply(seq_len(nrow(all_spans)), function(r) {
          sched_get_hours_for_span(
            all_spans$first_dep[r],
            all_spans$last_dep[r]
          )
        })
      ))

      hours_to_remove <- setdiff(old_hours, new_hours)
      hours_to_remove <- setdiff(hours_to_remove, all_covered_hours)

      if (length(hours_to_remove) > 0) {
        current_data$hsh <- current_data$hsh[
          !(current_data$hsh$itin_id == editing_itin &
            current_data$hsh$service_id == service_id &
            current_data$hsh$hour_dep %in% hours_to_remove),
        ]
      }

      existing_hours <- current_data$hsh$hour_dep[
        current_data$hsh$itin_id == editing_itin &
          current_data$hsh$service_id == service_id
      ]
      hours_to_add <- setdiff(new_hours, existing_hours)

      if (length(hours_to_add) > 0) {
        existing_hsh <- current_data$hsh[
          current_data$hsh$itin_id == editing_itin &
            current_data$hsh$service_id == service_id,
        ]
        speed_value <- if (
          nrow(existing_hsh) > 0 &&
            any(!is.na(existing_hsh$speed))
        ) {
          round(mean(existing_hsh$speed, na.rm = TRUE), 1)
        } else {
          sched_get_speed_for_itin(editing_itin, current_data)
        }

        new_hsh_rows <- data.frame(
          itin_id = rep(editing_itin, length(hours_to_add)),
          service_id = rep(service_id, length(hours_to_add)),
          hour_dep = hours_to_add,
          headway = NA_real_,
          speed = speed_value,
          stringsAsFactors = FALSE
        )
        current_data$hsh <- rbind(current_data$hsh, new_hsh_rows)
      }

      ssfs(current_data)
      sched_span_editing_idx(NULL)

      showNotification("Service window updated.", type = "message")
    })

    # Save new span (created on save, not on add click)
    observeEvent(input$sched_span_save_new, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      req(editing_itin, service_id)

      data <- input$sched_span_save_new

      first_dep <- sched_format_time(data$first_dep)
      last_dep <- sched_format_time(data$last_dep)

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

      current_data <- ssfs()

      existing_spans <- current_data$span[
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id,
      ]

      if (nrow(existing_spans) == 0) {
        new_service_window <- 1L
      } else {
        max_window <- max(existing_spans$service_window, na.rm = TRUE)
        new_service_window <- as.integer(max_window + 1)

        prev_last <- existing_spans$last_dep[
          existing_spans$service_window == max_window
        ][1]
        prev_last_sec <- sched_parse_time_to_seconds(prev_last)
        new_first_sec <- sched_parse_time_to_seconds(first_dep)

        if (new_first_sec <= prev_last_sec + 59) {
          showNotification(
            paste0(
              "Service window ",
              new_service_window,
              " must start after ",
              prev_last,
              " (the end of service window ",
              max_window,
              ")."
            ),
            type = "error"
          )
          return()
        }
      }

      new_span <- data.frame(
        itin_id = editing_itin,
        service_id = service_id,
        service_window = new_service_window,
        first_dep = first_dep,
        last_dep = last_dep,
        stringsAsFactors = FALSE
      )
      current_data$span <- rbind(current_data$span, new_span)

      new_hours <- sched_get_hours_for_span(first_dep, last_dep)

      existing_hsh_hours <- current_data$hsh$hour_dep[
        current_data$hsh$itin_id == editing_itin &
          current_data$hsh$service_id == service_id
      ]
      hours_to_add <- setdiff(new_hours, existing_hsh_hours)

      if (length(hours_to_add) > 0) {
        existing_hsh <- current_data$hsh[
          current_data$hsh$itin_id == editing_itin &
            current_data$hsh$service_id == service_id,
        ]
        speed_value <- if (
          nrow(existing_hsh) > 0 &&
            any(!is.na(existing_hsh$speed))
        ) {
          round(mean(existing_hsh$speed, na.rm = TRUE), 1)
        } else {
          sched_get_speed_for_itin(editing_itin, current_data)
        }

        new_hsh_rows <- data.frame(
          itin_id = rep(editing_itin, length(hours_to_add)),
          service_id = rep(service_id, length(hours_to_add)),
          hour_dep = hours_to_add,
          headway = NA_real_,
          speed = speed_value,
          stringsAsFactors = FALSE
        )
        current_data$hsh <- rbind(current_data$hsh, new_hsh_rows)
      }

      ssfs(current_data)
      sched_span_adding(FALSE)

      showNotification(
        paste0(
          "Service window ",
          new_service_window,
          " added (",
          first_dep,
          " - ",
          last_dep,
          ") with ",
          length(hours_to_add),
          " headway entries created."
        ),
        type = "message"
      )
    })

    # Delete span
    observeEvent(input$sched_span_delete_click, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      idx <- input$sched_span_delete_click$idx
      req(editing_itin, service_id, idx)

      current_data <- ssfs()

      itin_spans <- current_data$span[
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id,
      ]
      itin_spans <- itin_spans[order(itin_spans$service_window), ]

      if (idx < 1 || idx > nrow(itin_spans)) {
        showNotification("Span not found.", type = "error")
        return()
      }

      target_sw <- itin_spans$service_window[idx]
      deleted_first <- itin_spans$first_dep[idx]
      deleted_last <- itin_spans$last_dep[idx]

      current_data$span <- current_data$span[
        !(current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id &
          current_data$span$service_window == target_sw),
      ]

      deleted_hours <- sched_get_hours_for_span(deleted_first, deleted_last)

      remaining_spans <- current_data$span[
        current_data$span$itin_id == editing_itin &
          current_data$span$service_id == service_id,
      ]

      if (nrow(remaining_spans) > 0) {
        remaining_hours <- unique(unlist(
          lapply(seq_len(nrow(remaining_spans)), function(r) {
            sched_get_hours_for_span(
              remaining_spans$first_dep[r],
              remaining_spans$last_dep[r]
            )
          })
        ))
        hours_to_remove <- setdiff(deleted_hours, remaining_hours)
      } else {
        hours_to_remove <- deleted_hours
      }

      if (length(hours_to_remove) > 0) {
        current_data$hsh <- current_data$hsh[
          !(current_data$hsh$itin_id == editing_itin &
            current_data$hsh$service_id == service_id &
            current_data$hsh$hour_dep %in% hours_to_remove),
        ]
      }

      ssfs(current_data)
      sched_span_editing_idx(NULL)

      showNotification(
        paste0("Service window ", target_sw, " deleted."),
        type = "message"
      )
    })

    # Apply service level preset to single itinerary
    observeEvent(input$sched_itin_apply_preset, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      pattern_id <- input$sched_itin_preset
      req(editing_itin, service_id, pattern_id)

      current_data <- ssfs()
      sp_data <- service_patterns()

      if (!pattern_id %in% names(sp_data$service_patterns)) {
        showNotification("Selected preset not found.", type = "error")
        return()
      }

      pattern_data <- sp_data$service_patterns[[pattern_id]]
      pattern_headways <- setNames(pattern_data$headway, pattern_data$hour)

      match_idx <- which(
        current_data$hsh$itin_id == editing_itin &
          current_data$hsh$service_id == service_id
      )

      if (length(match_idx) == 0) {
        showNotification(
          "No headway entries found. Define spans first.",
          type = "warning"
        )
        return()
      }

      updated_count <- 0L
      for (idx in match_idx) {
        hour <- current_data$hsh$hour_dep[idx]
        if (hour %in% names(pattern_headways)) {
          current_data$hsh$headway[idx] <- pattern_headways[[hour]]
          updated_count <- updated_count + 1L
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
          editing_itin,
          ". ",
          updated_count,
          " hour entries updated."
        ),
        type = "message"
      )
    })

    # Apply speed to single itinerary
    observeEvent(input$sched_itin_apply_speed, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      speed_value <- input$sched_itin_speed
      req(editing_itin, service_id, speed_value)

      current_data <- ssfs()

      match_idx <- which(
        current_data$hsh$itin_id == editing_itin &
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
          " entries on ",
          editing_itin,
          "."
        ),
        type = "message"
      )
    })

    # Apply headway to single itinerary

    observeEvent(input$sched_itin_apply_headway, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      headway_value <- input$sched_itin_headway
      req(editing_itin, service_id, headway_value)

      current_data <- ssfs()

      match_idx <- which(
        current_data$hsh$itin_id == editing_itin &
          current_data$hsh$service_id == service_id
      )

      if (length(match_idx) == 0) {
        showNotification(
          "No headway entries found. Define spans first.",
          type = "warning"
        )
        return()
      }

      current_data$hsh$headway[match_idx] <- headway_value
      ssfs(current_data)

      showNotification(
        paste0(
          "Headway set to ",
          headway_value,
          " min for ",
          length(match_idx),
          " entries on ",
          editing_itin,
          "."
        ),
        type = "message"
      )
    })

    # Edit hsh row (pencil click or row click)
    observeEvent(input$sched_hsh_edit_click, {
      sched_hsh_editing_hour(input$sched_hsh_edit_click$hour)
    })

    # Cancel hsh edit
    observeEvent(input$sched_hsh_cancel_edit, {
      sched_hsh_editing_hour(NULL)
    })

    # Save hsh edit
    observeEvent(input$sched_hsh_save_edit, {
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()
      editing_hour <- sched_hsh_editing_hour()
      req(editing_itin, service_id, editing_hour)

      data <- input$sched_hsh_save_edit

      # Parse headway
      new_headway <- suppressWarnings(as.numeric(data$headway))
      if (!is.na(new_headway)) {
        if (new_headway < 1 || new_headway > 119) {
          showNotification(
            "Headway must be between 1 and 119 minutes.",
            type = "error"
          )
          return()
        }
        new_headway <- as.integer(round(new_headway))
      }
      # If empty string or unparseable, set to NA
      if (is.na(new_headway) && nchar(trimws(data$headway)) > 0) {
        showNotification("Invalid headway value.", type = "error")
        return()
      }
      if (nchar(trimws(data$headway)) == 0) {
        new_headway <- NA_integer_
      }

      # Parse speed
      new_speed <- suppressWarnings(as.numeric(data$speed))
      if (is.na(new_speed) || new_speed < 5 || new_speed > 431) {
        showNotification(
          "Speed must be between 5 and 431 km/h.",
          type = "error"
        )
        return()
      }

      current_data <- ssfs()

      match_idx <- which(
        current_data$hsh$itin_id == editing_itin &
          current_data$hsh$service_id == service_id &
          current_data$hsh$hour_dep == editing_hour
      )

      if (length(match_idx) == 0) {
        showNotification("Row not found.", type = "error")
        return()
      }

      current_data$hsh$headway[match_idx] <- new_headway
      current_data$hsh$speed[match_idx] <- new_speed

      ssfs(current_data)
      sched_hsh_editing_hour(NULL)

      showNotification(
        paste0(
          "Updated ",
          editing_hour,
          ": headway = ",
          if (is.na(new_headway)) "-" else paste0(new_headway, " min"),
          ", speed = ",
          new_speed,
          " km/h"
        ),
        type = "message"
      )
    })

    # Save hour and headway combination for selected itin and service to service level preset
    observeEvent(input$sched_save_as_preset, {
      editing_itin <- sched_editing_itin_id()
      curr_service_id <- sched_edit_service_id()
      req(editing_itin, curr_service_id)

      preset_name <- trimws(input$sched_save_as_preset$name)

      if (nchar(preset_name) == 0) {
        showNotification("Preset name cannot be empty.", type = "error")
        return()
      }

      current_data <- ssfs()

      itin_hsh <- current_data$hsh |>
        filter(itin_id == editing_itin, service_id == curr_service_id)

      if (nrow(itin_hsh) == 0) {
        showNotification(
          "No headway values defined. Set headways before saving as preset.",
          type = "warning"
        )
        return()
      }

      itin_hsh <- itin_hsh[order(itin_hsh$hour_dep), ]

      sp_data <- service_patterns()

      existing_ids <- names(sp_data$service_patterns)
      nums <- suppressWarnings(as.numeric(gsub("\\D", "", existing_ids)))
      nums <- nums[!is.na(nums)]
      next_num <- if (length(nums) > 0) max(nums) + 1 else 1
      new_id <- paste0("SP", next_num)

      sp_data$service_patterns[[new_id]] <- data.frame(
        hour = itin_hsh$hour_dep,
        headway = ifelse(
          is.na(itin_hsh$headway),
          NA_integer_,
          as.integer(itin_hsh$headway)
        ),
        stringsAsFactors = FALSE
      )

      sp_data$service_pattern_names <- rbind(
        sp_data$service_pattern_names,
        data.frame(
          pattern_id = new_id,
          pattern_name = preset_name,
          stringsAsFactors = FALSE
        )
      )

      service_patterns(sp_data)

      showNotification(
        paste0(
          "Saved as '",
          new_id,
          " - ",
          preset_name,
          "' with ",
          nrow(itin_hsh),
          " hours."
        ),
        type = "message"
      )
    })

    # OBSERVERS : CALENDAR MODAL POPUP------------
    # Calendar modal opener
    observeEvent(input$sched_open_calendar, {
      sched_cal_editing_id(NULL)
      sched_cal_adding(FALSE)
      sched_cal_cost_result(NULL)
      showModal(modalDialog(
        title = "Service Calendar",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        uiOutput(ns("sched_calendar_modal_ui"))
      ))
    })

    # Calendar modal content
    output$sched_calendar_modal_ui <- renderUI({
      current_data <- ssfs()
      cal <- current_data$calendar
      editing_id <- sched_cal_editing_id()
      is_adding <- sched_cal_adding()
      ns <- session$ns

      day_cols <- c(
        "monday",
        "tuesday",
        "wednesday",
        "thursday",
        "friday",
        "saturday",
        "sunday"
      )
      day_abbrs <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

      cal_rows <- list()

      if (nrow(cal) > 0) {
        for (r in seq_len(nrow(cal))) {
          sid <- cal$service_id[r]
          is_editing_this <- !is.null(editing_id) && editing_id == sid

          if (is_editing_this) {
            # Inline edit form
            day_checkboxes <- mapply(
              function(col, abbr) {
                checked <- if (cal[[col]][r] == 1) "checked" else NULL
                tags$label(
                  tags$input(
                    type = "checkbox",
                    id = ns(paste0("sched_cal_", tolower(substr(abbr, 1, 3)))),
                    checked = checked
                  ),
                  abbr
                )
              },
              day_cols,
              day_abbrs,
              SIMPLIFY = FALSE
            )

            cal_rows[[length(cal_rows) + 1]] <- tags$tr(
              class = "sched-cal-row sched-cal-editing",
              tags$td(
                colspan = "11",
                div(
                  class = "sched-cal-edit-form",
                  div(
                    style = "margin-bottom: 6px;",
                    tags$label("Service ID"),
                    tags$input(
                      type = "text",
                      id = ns("sched_cal_edit_service_id"),
                      value = sid,
                      style = "width: 120px;"
                    )
                  ),
                  tags$label("Days of operation"),
                  div(
                    class = "day-checkboxes",
                    do.call(tagList, day_checkboxes)
                  ),
                  div(
                    class = "date-fields",
                    div(
                      tags$label("Start date"),
                      tags$input(
                        type = "date",
                        id = ns("sched_cal_start_date"),
                        value = cal$start_date[r]
                      )
                    ),
                    div(
                      tags$label("End date"),
                      tags$input(
                        type = "date",
                        id = ns("sched_cal_end_date"),
                        value = cal$end_date[r]
                      )
                    )
                  ),
                  div(
                    class = "btn-row",
                    tags$button(
                      class = "btn-save",
                      onclick = "schedSaveCalendarEdit()",
                      htmltools::HTML("&#10003; Save")
                    ),
                    tags$button(
                      class = "btn-cancel",
                      onclick = "schedCancelCalendarEdit()",
                      "Cancel"
                    )
                  )
                )
              )
            )
          } else {
            # Normal display row
            day_cells <- lapply(day_cols, function(col) {
              val <- cal[[col]][r]
              if (val == 1) {
                tags$td(
                  class = "sched-cal-day-active",
                  htmltools::HTML("&#10003;")
                )
              } else {
                tags$td(
                  class = "sched-cal-day-inactive",
                  htmltools::HTML("&mdash;")
                )
              }
            })

            cal_rows[[length(cal_rows) + 1]] <- tags$tr(
              class = "sched-cal-row",
              tags$td(sid),
              do.call(tagList, day_cells),
              tags$td(cal$start_date[r]),
              tags$td(cal$end_date[r]),
              tags$td(
                style = "text-align: right; white-space: nowrap;",
                tags$button(
                  class = "route-action-btn edit-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); schedEditCalendarRow('%s')",
                    sid
                  ),
                  title = "Edit service",
                  htmltools::HTML("&#9998;")
                ),
                tags$button(
                  class = "route-action-btn delete-btn",
                  onclick = sprintf(
                    "event.stopPropagation(); schedDeleteCalendarRow('%s')",
                    sid
                  ),
                  title = "Delete service",
                  htmltools::HTML('<i class="fa-solid fa-trash"></i>')
                )
              )
            )
          }
        }
      }

      # Add new service form or button
      if (is_adding) {
        # Generate next service ID
        if (nrow(cal) == 0) {
          next_id <- "S1"
        } else {
          existing_ids <- cal$service_id
          numeric_part <- suppressWarnings(
            as.integer(gsub("\\D", "", existing_ids))
          )
          numeric_part <- numeric_part[!is.na(numeric_part)]
          next_num <- if (length(numeric_part) > 0) {
            max(numeric_part) + 1
          } else {
            nrow(cal) + 1
          }
          next_id <- paste0("S", next_num)
        }

        day_checkboxes_new <- mapply(
          function(col, abbr) {
            # Default: check Mon-Fri
            default_checked <- col %in%
              c("monday", "tuesday", "wednesday", "thursday", "friday")
            tags$label(
              tags$input(
                type = "checkbox",
                id = ns(paste0("sched_cal_", tolower(substr(abbr, 1, 3)))),
                checked = if (default_checked) "checked" else NULL
              ),
              abbr
            )
          },
          day_cols,
          day_abbrs,
          SIMPLIFY = FALSE
        )

        add_form <- tags$tr(
          class = "sched-cal-row sched-cal-editing",
          tags$td(
            colspan = "11",
            div(
              class = "sched-cal-edit-form",
              div(
                style = "margin-bottom: 6px;",
                tags$label("Service ID"),
                tags$input(
                  type = "text",
                  id = ns("sched_cal_edit_service_id"),
                  value = next_id,
                  style = "width: 120px;"
                )
              ),
              tags$label("Days of operation"),
              div(
                class = "day-checkboxes",
                do.call(tagList, day_checkboxes_new)
              ),
              div(
                class = "date-fields",
                div(
                  tags$label("Start date"),
                  tags$input(
                    type = "date",
                    id = ns("sched_cal_start_date"),
                    value = format(Sys.Date(), "%Y-%m-%d")
                  )
                ),
                div(
                  tags$label("End date"),
                  tags$input(
                    type = "date",
                    id = ns("sched_cal_end_date"),
                    value = format(Sys.Date() + 365, "%Y-%m-%d")
                  )
                )
              ),
              div(
                class = "btn-row",
                tags$button(
                  class = "btn-save",
                  onclick = "schedSaveCalendarEdit()",
                  "Create"
                ),
                tags$button(
                  class = "btn-cancel",
                  onclick = "schedCancelCalendarEdit()",
                  "Cancel"
                )
              )
            )
          )
        )
        cal_rows[[length(cal_rows) + 1]] <- add_form
      }

      # Build service choices for cost calculator
      cost_service_choices <- if (nrow(cal) > 0) {
        cal$service_id
      } else {
        character(0)
      }

      # Build the table
      tagList(
        tags$table(
          class = "sched-cal-table",
          tags$thead(
            tags$tr(
              tags$th("Service ID"),
              tags$th("Mon"),
              tags$th("Tue"),
              tags$th("Wed"),
              tags$th("Thu"),
              tags$th("Fri"),
              tags$th("Sat"),
              tags$th("Sun"),
              tags$th("Start date"),
              tags$th("End date"),
              tags$th(style = "width: 60px;", "")
            )
          ),
          tags$tbody(
            do.call(tagList, cal_rows)
          )
        ),
        if (!is_adding) {
          div(
            class = "sched-cal-add-row",
            onclick = "schedAddCalendarRow()",
            tags$button(
              class = "stop-action-btn add-btn",
              onclick = "event.stopPropagation(); schedAddCalendarRow()",
              title = "Add new service",
              htmltools::HTML("+")
            ),
            span(style = "margin-left: 8px;", "Add new service")
          )
        },

        # Total daily service cost calculator
        hr(),
        h5("Total daily service cost"),
        tags$small(
          style = "color: #888; display: block; margin-bottom: 10px;",
          "Calculate total daily vehicle-km and vehicle-hours for all routes and itineraries on a selected service. This may take several minutes for larger networks."
        ),
        if (length(cost_service_choices) > 0) {
          div(
            style = "display: flex; gap: 8px; align-items: flex-end;",
            div(
              style = "flex: 1; max-width: 250px;",
              selectInput(
                ns("sched_cal_cost_service"),
                label = NULL,
                choices = cost_service_choices,
                width = "100%"
              )
            ),
            tags$button(
              class = "btn-save",
              style = "margin-bottom: 15px;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
                ns("sched_cal_calculate_cost")
              ),
              "Calculate"
            )
          )
        } else {
          tags$em(
            style = "color: grey;",
            "Add a service above to calculate costs."
          )
        },
        uiOutput(ns("sched_cal_cost_result_ui"))
      )
    })

    # -- Calendar CRUD observers --

    # Edit click
    observeEvent(input$sched_cal_edit_click, {
      sched_cal_adding(FALSE)
      sched_cal_editing_id(input$sched_cal_edit_click$id)
    })

    # Cancel edit
    observeEvent(input$sched_cal_cancel_edit, {
      sched_cal_editing_id(NULL)
      sched_cal_adding(FALSE)
    })

    # Add click (open form)
    observeEvent(input$sched_cal_add_click, {
      sched_cal_editing_id(NULL)
      sched_cal_adding(TRUE)
    })

    # Save edit (handles both edit and add)
    observeEvent(input$sched_cal_save_edit, {
      data <- input$sched_cal_save_edit
      current_data <- ssfs()

      service_id <- trimws(data$service_id)

      if (nchar(service_id) == 0) {
        showNotification("Service ID cannot be empty.", type = "error")
        return()
      }

      start_date <- data$start_date
      end_date <- data$end_date

      if (nchar(start_date) == 0 || nchar(end_date) == 0) {
        showNotification("Start and end dates are required.", type = "error")
        return()
      }

      if (start_date > end_date) {
        showNotification(
          "Start date must be before end date.",
          type = "warning"
        )
        return()
      }

      new_row <- data.frame(
        service_id = service_id,
        monday = as.integer(data$monday),
        tuesday = as.integer(data$tuesday),
        wednesday = as.integer(data$wednesday),
        thursday = as.integer(data$thursday),
        friday = as.integer(data$friday),
        saturday = as.integer(data$saturday),
        sunday = as.integer(data$sunday),
        start_date = start_date,
        end_date = end_date,
        stringsAsFactors = FALSE
      )

      if (sched_cal_adding()) {
        # Adding new service
        if (service_id %in% current_data$calendar$service_id) {
          showNotification(
            "Service ID already exists. Please use a different ID.",
            type = "warning"
          )
          return()
        }
        current_data$calendar <- rbind(current_data$calendar, new_row)
        ssfs(current_data)
        sched_cal_adding(FALSE)
        showNotification(
          paste0("Service '", service_id, "' created."),
          type = "message"
        )
      } else {
        # Editing existing service
        editing_id <- sched_cal_editing_id()
        req(editing_id)

        row_idx <- which(current_data$calendar$service_id == editing_id)
        if (length(row_idx) == 0) {
          showNotification("Service not found.", type = "error")
          return()
        }

        # If service_id changed, cascade the rename
        if (service_id != editing_id) {
          # Check uniqueness
          if (service_id %in% current_data$calendar$service_id) {
            showNotification(
              "Service ID already exists. Please use a different ID.",
              type = "warning"
            )
            return()
          }

          # Rename in span
          span_idx <- which(current_data$span$service_id == editing_id)
          if (length(span_idx) > 0) {
            current_data$span$service_id[span_idx] <- service_id
          }

          # Rename in hsh
          hsh_idx <- which(current_data$hsh$service_id == editing_id)
          if (length(hsh_idx) > 0) {
            current_data$hsh$service_id[hsh_idx] <- service_id
          }
        }

        current_data$calendar[row_idx, ] <- new_row
        ssfs(current_data)
        sched_cal_editing_id(NULL)
        showNotification(
          paste0("Service '", service_id, "' updated."),
          type = "message"
        )
      }
    })

    # Delete service
    observeEvent(input$sched_cal_delete_click, {
      service_id <- input$sched_cal_delete_click$id
      current_data <- ssfs()

      # Remove from calendar
      current_data$calendar <- current_data$calendar[
        current_data$calendar$service_id != service_id,
      ]

      # Remove associated spans
      current_data$span <- current_data$span[
        current_data$span$service_id != service_id,
      ]

      # Remove associated hsh entries
      current_data$hsh <- current_data$hsh[
        current_data$hsh$service_id != service_id,
      ]

      ssfs(current_data)
      sched_cal_editing_id(NULL)

      showNotification(
        paste0(
          "Service '",
          service_id,
          "' deleted with associated spans and headway entries."
        ),
        type = "message"
      )
    })

    # Calculate total daily service cost for selected service
    observeEvent(input$sched_cal_calculate_cost, {
      service_id <- input$sched_cal_cost_service
      req(service_id)

      current_data <- ssfs()

      all_route_ids <- current_data$routes$route_id

      if (length(all_route_ids) == 0) {
        sched_cal_cost_result(NULL)
        showNotification("No routes defined.", type = "warning")
        return()
      }

      # Check that at least some spans exist for this service
      service_spans <- current_data$span[
        current_data$span$service_id == service_id,
      ]
      if (nrow(service_spans) == 0) {
        sched_cal_cost_result(NULL)
        showNotification(
          paste0("No service windows defined for '", service_id, "'."),
          type = "warning"
        )
        return()
      }

      progress_id <- showNotification(
        "Calculating service cost...",
        duration = NULL,
        type = "message"
      )

      result <- tryCatch(
        generate_service_cost(
          ssfs = current_data,
          id_type = "route_id",
          id = all_route_ids,
          service = service_id
        ),
        error = function(e) {
          showNotification(
            paste0("Error: ", e$message),
            type = "error"
          )
          NULL
        }
      )

      removeNotification(progress_id)
      sched_cal_cost_result(result)

      if (!is.null(result) && nrow(result) > 0) {
        showNotification("Service cost calculated.", type = "message")
      }
    })

    # Display service cost result
    output$sched_cal_cost_result_ui <- renderUI({
      result <- sched_cal_cost_result()

      if (is.null(result) || nrow(result) == 0) {
        return(NULL)
      }

      result_rows <- list()
      for (r in seq_len(nrow(result))) {
        result_rows[[r]] <- tags$tr(
          tags$td(result$agency_id[r]),
          tags$td(style = "text-align: right;", result$total_km[r]),
          tags$td(style = "text-align: right;", result$total_h[r])
        )
      }

      if (nrow(result) > 1) {
        result_rows[[length(result_rows) + 1]] <- tags$tr(
          style = "font-weight: bold; border-top: 2px solid var(--border-color);",
          tags$td("Total"),
          tags$td(
            style = "text-align: right;",
            round(sum(result$total_km, na.rm = TRUE), 1)
          ),
          tags$td(
            style = "text-align: right;",
            round(sum(result$total_h, na.rm = TRUE), 1)
          )
        )
      }

      div(
        style = "margin-top: 10px;",
        tags$table(
          class = "sched-cal-table",
          style = "width: auto;",
          tags$thead(
            tags$tr(
              tags$th(style = "text-align: left;", "Agency"),
              tags$th(style = "text-align: right;", "Vehicle-km"),
              tags$th(style = "text-align: right;", "Vehicle-hours")
            )
          ),
          tags$tbody(
            do.call(tagList, result_rows)
          )
        )
      )
    })

    # OBSERVERS : SERVICE LEVEL PRESETS MODAL ---------------

    # Presets modal opener
    observeEvent(input$sched_open_presets, {
      sched_preset_editing_id(NULL)
      sched_preset_adding(FALSE)
      sched_preset_hour_editing(NULL)
      sched_preset_hour_adding(FALSE)
      showModal(modalDialog(
        title = "Service Level Presets",
        size = "m",
        easyClose = TRUE,
        footer = modalButton("Close"),
        uiOutput(ns("sched_presets_modal_ui"))
      ))
    })

    # Presets modal content
    output$sched_presets_modal_ui <- renderUI({
      sp_data <- service_patterns()
      names_df <- sp_data$service_pattern_names
      editing_id <- sched_preset_editing_id()
      is_adding <- sched_preset_adding()
      hour_editing <- sched_preset_hour_editing()
      hour_adding <- sched_preset_hour_adding()
      ns <- session$ns

      # ── Preset list rows ──
      preset_rows <- list()

      if (!is.null(names_df) && nrow(names_df) > 0) {
        for (r in seq_len(nrow(names_df))) {
          pid <- names_df$pattern_id[r]
          pname <- names_df$pattern_name[r]
          is_active <- !is.null(editing_id) && editing_id == pid

          preset_rows[[length(preset_rows) + 1]] <- div(
            class = paste0(
              "sched-preset-row",
              if (is_active) " sched-preset-active" else ""
            ),
            onclick = sprintf("schedEditPreset('%s')", pid),
            div(
              class = "sched-preset-info",
              span(class = "sched-preset-id", pid),
              span(class = "sched-preset-name", paste0("\u2014 ", pname))
            ),
            div(
              class = "sched-preset-actions",
              tags$button(
                class = "route-action-btn edit-btn",
                onclick = sprintf(
                  "event.stopPropagation(); schedEditPreset('%s')",
                  pid
                ),
                title = "Edit preset",
                htmltools::HTML("&#9998;")
              ),
              tags$button(
                class = "route-action-btn delete-btn",
                onclick = sprintf(
                  "event.stopPropagation(); schedDeletePreset('%s')",
                  pid
                ),
                title = "Delete preset",
                htmltools::HTML('<i class="fa-solid fa-trash"></i>')
              )
            )
          )
        }
      }

      # Add new preset row (only show when not already adding)
      if (!is_adding) {
        preset_rows[[length(preset_rows) + 1]] <- div(
          class = "sched-cal-add-row",
          onclick = "schedAddPreset()",
          tags$button(
            class = "stop-action-btn add-btn",
            onclick = "event.stopPropagation(); schedAddPreset()",
            title = "Add new service level preset",
            htmltools::HTML("+")
          ),
          span(style = "margin-left: 8px;", "Add new service level preset")
        )
      }

      # ── Detail area (shown when a preset is selected or being created) ──
      detail_ui <- NULL

      if (is_adding) {
        # Creating a new preset : show name input and empty hour table
        # with just the add-hour form
        if (hour_adding) {
          all_hours <- sprintf("%02d:00:00", 0:29)
          add_form <- div(
            class = "sched-preset-hour-edit-form",
            div(
              style = "display: flex; gap: 8px; align-items: flex-end;",
              div(
                tags$label("Hour"),
                tags$select(
                  id = ns("sched_preset_hour_new_hour"),
                  lapply(all_hours, function(h) {
                    tags$option(value = h, h)
                  })
                )
              ),
              div(
                tags$label("Headway (min)"),
                tags$input(
                  type = "number",
                  id = ns("sched_preset_hour_edit_headway"),
                  value = "10",
                  min = "1",
                  max = "119",
                  style = "width: 80px;"
                )
              )
            ),
            div(
              class = "btn-row",
              tags$button(
                class = "btn-save",
                onclick = "schedSavePresetNewHour()",
                "Create"
              ),
              tags$button(
                class = "btn-cancel",
                onclick = "schedCancelPresetHourEdit()",
                "Cancel"
              )
            )
          )
        } else {
          add_form <- NULL
        }

        detail_ui <- div(
          class = "sched-preset-detail",
          tags$label("Preset name"),
          tags$input(
            type = "text",
            id = ns("sched_preset_name_input"),
            class = "sched-preset-name-input",
            value = "",
            placeholder = "e.g. Peak Frequent"
          ),
          tags$button(
            class = "btn-save",
            style = "margin-bottom: 10px;",
            onclick = "schedSavePresetName()",
            "Save preset"
          ),
          tags$button(
            class = "btn-cancel",
            style = "margin-bottom: 10px; margin-left: 6px;",
            onclick = "schedCancelPresetHourEdit()",
            "Cancel"
          ),
          h5("Hours"),
          tags$em(
            style = "color: grey; font-size: 11px;",
            "Add hours to build the preset."
          ),
          add_form,
          if (!hour_adding) {
            div(
              class = "sched-cal-add-row",
              onclick = "schedAddPresetHour()",
              tags$button(
                class = "stop-action-btn add-btn",
                onclick = "event.stopPropagation(); schedAddPresetHour()",
                htmltools::HTML("+")
              ),
              span(style = "margin-left: 8px;", "Add new hour")
            )
          }
        )
      } else if (!is.null(editing_id)) {
        # Viewing/editing an existing preset
        pattern_data <- sp_data$service_patterns[[editing_id]]
        pattern_name <- names_df$pattern_name[
          names_df$pattern_id == editing_id
        ]

        # Build hour rows
        hour_rows <- list()

        if (!is.null(pattern_data) && nrow(pattern_data) > 0) {
          pattern_data <- pattern_data[order(pattern_data$hour), ]

          for (h in seq_len(nrow(pattern_data))) {
            hour_val <- pattern_data$hour[h]
            hdwy_val <- pattern_data$headway[h]
            is_editing_hour <- !is.null(hour_editing) &&
              hour_editing == hour_val

            if (is_editing_hour) {
              hour_rows[[length(hour_rows) + 1]] <- tags$tr(
                class = "sched-preset-hour-row sched-preset-hour-editing",
                tags$td(
                  colspan = "3",
                  div(
                    class = "sched-preset-hour-edit-form",
                    div(
                      style = "display: flex; gap: 8px; align-items: flex-end;",
                      div(
                        tags$label("Hour"),
                        tags$input(
                          type = "text",
                          value = hour_val,
                          disabled = "disabled",
                          style = "width: 100px; background-color: #eee; color: #888;"
                        )
                      ),
                      div(
                        tags$label("Headway (min)"),
                        tags$input(
                          type = "number",
                          id = ns("sched_preset_hour_edit_headway"),
                          value = if (!is.na(hdwy_val)) hdwy_val else "",
                          min = "1",
                          max = "119",
                          style = "width: 80px;"
                        )
                      )
                    ),
                    div(
                      class = "btn-row",
                      tags$button(
                        class = "btn-save",
                        onclick = "schedSavePresetHourEdit()",
                        htmltools::HTML("&#10003; Save")
                      ),
                      tags$button(
                        class = "btn-cancel",
                        onclick = "schedCancelPresetHourEdit()",
                        "Cancel"
                      )
                    )
                  )
                )
              )
            } else {
              hdwy_display <- if (is.na(hdwy_val)) {
                "\u2014"
              } else {
                as.character(hdwy_val)
              }

              hour_rows[[length(hour_rows) + 1]] <- tags$tr(
                class = "sched-preset-hour-row",
                tags$td(hour_val),
                tags$td(hdwy_display),
                tags$td(
                  style = "text-align: right; white-space: nowrap;",
                  tags$button(
                    class = "route-action-btn edit-btn",
                    onclick = sprintf(
                      "event.stopPropagation(); schedEditPresetHour('%s')",
                      hour_val
                    ),
                    title = "Edit headway",
                    htmltools::HTML("&#9998;")
                  ),
                  tags$button(
                    class = "route-action-btn delete-btn",
                    onclick = sprintf(
                      "event.stopPropagation(); schedDeletePresetHour('%s')",
                      hour_val
                    ),
                    title = "Delete hour",
                    htmltools::HTML('<i class="fa-solid fa-trash"></i>')
                  )
                )
              )
            }
          }
        }

        # Add new hour form or button
        if (hour_adding) {
          # Determine available hours
          existing_hours <- if (
            !is.null(pattern_data) &&
              nrow(pattern_data) > 0
          ) {
            pattern_data$hour
          } else {
            character(0)
          }

          # If there are existing hours, next hour is last + 1
          if (length(existing_hours) > 0) {
            last_hour_num <- max(as.numeric(
              substr(existing_hours, 1, 2)
            ))
            next_hour_num <- last_hour_num + 1
            if (next_hour_num > 29) {
              next_hour_display <- NULL # no more hours available
            } else {
              next_hour_display <- sprintf("%02d:00:00", next_hour_num)
            }
          } else {
            next_hour_display <- NULL # will show dropdown
          }

          if (is.null(next_hour_display) && length(existing_hours) > 0) {
            # All hours used up after last
            add_hour_ui <- div(
              style = "padding: 8px; color: grey;",
              tags$em("No more hours available (max 29:00:00).")
            )
          } else if (is.null(next_hour_display)) {
            # No existing hours : show dropdown to pick starting hour
            all_hours <- sprintf("%02d:00:00", 0:29)
            available <- setdiff(all_hours, existing_hours)

            add_hour_ui <- tags$tr(
              class = "sched-preset-hour-row sched-preset-hour-editing",
              tags$td(
                colspan = "3",
                div(
                  class = "sched-preset-hour-edit-form",
                  div(
                    style = "display: flex; gap: 8px; align-items: flex-end;",
                    div(
                      tags$label("Hour"),
                      tags$select(
                        id = ns("sched_preset_hour_new_hour"),
                        lapply(available, function(hh) {
                          tags$option(value = hh, hh)
                        })
                      )
                    ),
                    div(
                      tags$label("Headway (min)"),
                      tags$input(
                        type = "number",
                        id = ns("sched_preset_hour_edit_headway"),
                        value = "10",
                        min = "1",
                        max = "119",
                        style = "width: 80px;"
                      )
                    )
                  ),
                  div(
                    class = "btn-row",
                    tags$button(
                      class = "btn-save",
                      onclick = "schedSavePresetNewHour()",
                      "Create"
                    ),
                    tags$button(
                      class = "btn-cancel",
                      onclick = "schedCancelPresetHourEdit()",
                      "Cancel"
                    )
                  )
                )
              )
            )
          } else {
            # Next hour is deterministic
            add_hour_ui <- tags$tr(
              class = "sched-preset-hour-row sched-preset-hour-editing",
              tags$td(
                colspan = "3",
                div(
                  class = "sched-preset-hour-edit-form",
                  div(
                    style = "display: flex; gap: 8px; align-items: flex-end;",
                    div(
                      tags$label("Hour"),
                      tags$input(
                        type = "text",
                        id = ns("sched_preset_hour_new_hour"),
                        value = next_hour_display,
                        disabled = "disabled",
                        style = "width: 100px; background-color: #eee; color: #888;"
                      )
                    ),
                    div(
                      tags$label("Headway (min)"),
                      tags$input(
                        type = "number",
                        id = ns("sched_preset_hour_edit_headway"),
                        value = "10",
                        min = "1",
                        max = "119",
                        style = "width: 80px;"
                      )
                    )
                  ),
                  div(
                    class = "btn-row",
                    tags$button(
                      class = "btn-save",
                      onclick = "schedSavePresetNewHour()",
                      "Create"
                    ),
                    tags$button(
                      class = "btn-cancel",
                      onclick = "schedCancelPresetHourEdit()",
                      "Cancel"
                    )
                  )
                )
              )
            )
          }

          hour_rows[[length(hour_rows) + 1]] <- add_hour_ui
        }

        # Build the hour table
        hour_table <- tags$table(
          class = "sched-preset-hour-table",
          tags$thead(
            tags$tr(
              tags$th("Hour"),
              tags$th("Headway (min)"),
              tags$th(style = "width: 60px;", "")
            )
          ),
          tags$tbody(
            do.call(tagList, hour_rows)
          )
        )

        # Add hour button (show when not already adding)
        add_hour_btn <- if (!hour_adding) {
          div(
            class = "sched-cal-add-row",
            onclick = "schedAddPresetHour()",
            tags$button(
              class = "stop-action-btn add-btn",
              onclick = "event.stopPropagation(); schedAddPresetHour()",
              htmltools::HTML("+")
            ),
            span(style = "margin-left: 8px;", "Add new hour")
          )
        } else {
          NULL
        }

        detail_ui <- div(
          class = "sched-preset-detail",
          tags$label("Preset name"),
          div(
            style = "display: flex; gap: 6px; align-items: flex-end; margin-bottom: 10px;",
            tags$input(
              type = "text",
              id = ns("sched_preset_name_input"),
              class = "sched-preset-name-input",
              value = pattern_name,
              style = "margin-bottom: 0; flex: 1;"
            ),
            tags$button(
              class = "btn-save",
              onclick = "schedSavePresetName()",
              "Rename"
            )
          ),
          h5("Hours"),
          hour_table,
          add_hour_btn
        )
      }

      # ── Assemble ──
      tagList(
        do.call(tagList, preset_rows),
        detail_ui
      )
    })

    # ── Preset-level observers ──

    # Select/edit preset
    observeEvent(input$sched_preset_edit_click, {
      pid <- input$sched_preset_edit_click$id
      if (
        !is.null(sched_preset_editing_id()) &&
          sched_preset_editing_id() == pid
      ) {
        # Toggle off
        sched_preset_editing_id(NULL)
      } else {
        sched_preset_editing_id(pid)
        sched_preset_adding(FALSE)
        sched_preset_hour_editing(NULL)
        sched_preset_hour_adding(FALSE)
      }
    })

    # Add new preset (open form)
    observeEvent(input$sched_preset_add_click, {
      sched_preset_editing_id(NULL)
      sched_preset_adding(TRUE)
      sched_preset_hour_editing(NULL)
      sched_preset_hour_adding(FALSE)
    })

    # Save preset name (handles both rename and create)
    observeEvent(input$sched_preset_save_name, {
      new_name <- trimws(input$sched_preset_save_name$name)

      if (nchar(new_name) == 0) {
        showNotification("Preset name cannot be empty.", type = "error")
        return()
      }

      sp_data <- service_patterns()

      if (sched_preset_adding()) {
        # Creating new preset
        existing_ids <- names(sp_data$service_patterns)
        nums <- suppressWarnings(as.numeric(gsub("\\D", "", existing_ids)))
        nums <- nums[!is.na(nums)]
        next_num <- if (length(nums) > 0) max(nums) + 1 else 1
        new_id <- paste0("SP", next_num)

        # Create empty pattern
        sp_data$service_patterns[[new_id]] <- data.frame(
          hour = character(),
          headway = integer(),
          stringsAsFactors = FALSE
        )

        # Add to names
        sp_data$service_pattern_names <- rbind(
          sp_data$service_pattern_names,
          data.frame(
            pattern_id = new_id,
            pattern_name = new_name,
            stringsAsFactors = FALSE
          )
        )

        service_patterns(sp_data)
        sched_preset_adding(FALSE)
        sched_preset_editing_id(new_id)

        showNotification(
          paste0("Preset '", new_id, " - ", new_name, "' created."),
          type = "message"
        )
      } else {
        # Renaming existing preset
        editing_id <- sched_preset_editing_id()
        req(editing_id)

        name_idx <- which(
          sp_data$service_pattern_names$pattern_id == editing_id
        )
        if (length(name_idx) > 0) {
          sp_data$service_pattern_names$pattern_name[name_idx] <- new_name
          service_patterns(sp_data)
          showNotification(
            paste0("Preset renamed to '", new_name, "'."),
            type = "message"
          )
        }
      }
    })

    # Delete preset
    observeEvent(input$sched_preset_delete_click, {
      pid <- input$sched_preset_delete_click$id
      sp_data <- service_patterns()

      # Remove from patterns list
      sp_data$service_patterns[[pid]] <- NULL

      # Remove from names
      sp_data$service_pattern_names <- sp_data$service_pattern_names[
        sp_data$service_pattern_names$pattern_id != pid,
      ]

      service_patterns(sp_data)

      if (
        !is.null(sched_preset_editing_id()) &&
          sched_preset_editing_id() == pid
      ) {
        sched_preset_editing_id(NULL)
      }

      showNotification(
        paste0("Preset '", pid, "' deleted."),
        type = "message"
      )
    })

    # ── Hour-level observers ──

    # Edit hour row
    observeEvent(input$sched_preset_hour_edit_click, {
      sched_preset_hour_adding(FALSE)
      sched_preset_hour_editing(input$sched_preset_hour_edit_click$hour)
    })

    # Cancel hour edit/add
    observeEvent(input$sched_preset_hour_cancel_edit, {
      sched_preset_hour_editing(NULL)
      sched_preset_hour_adding(FALSE)

      # If was adding a new preset with no hours and cancelled, cancel the add
      if (sched_preset_adding()) {
        sched_preset_adding(FALSE)
      }
    })

    # Add hour (open form)
    observeEvent(input$sched_preset_hour_add_click, {
      sched_preset_hour_editing(NULL)
      sched_preset_hour_adding(TRUE)
    })

    # Save hour edit (existing hour)
    observeEvent(input$sched_preset_hour_save_edit, {
      editing_id <- sched_preset_editing_id()
      editing_hour <- sched_preset_hour_editing()
      req(editing_id, editing_hour)

      data <- input$sched_preset_hour_save_edit
      new_headway <- suppressWarnings(as.numeric(data$headway))

      # Allow blank/NA headway
      if (nchar(trimws(data$headway)) == 0) {
        new_headway <- NA_integer_
      } else if (is.na(new_headway) || new_headway < 1 || new_headway > 119) {
        showNotification(
          "Headway must be between 1 and 119, or left blank.",
          type = "error"
        )
        return()
      } else {
        new_headway <- as.integer(round(new_headway))
      }

      sp_data <- service_patterns()
      pattern_data <- sp_data$service_patterns[[editing_id]]

      hour_idx <- which(pattern_data$hour == editing_hour)
      if (length(hour_idx) > 0) {
        pattern_data$headway[hour_idx] <- new_headway
        sp_data$service_patterns[[editing_id]] <- pattern_data
        service_patterns(sp_data)
      }

      sched_preset_hour_editing(NULL)
      showNotification("Hour updated.", type = "message")
    })

    # Save new hour
    observeEvent(input$sched_preset_hour_save_new, {
      editing_id <- sched_preset_editing_id()
      data <- input$sched_preset_hour_save_new

      # For new presets being created, editing_id might be NULL
      # In that case this is adding to a preset that was just created
      if (is.null(editing_id) && sched_preset_adding()) {
        # Preset not yet created : save name first
        showNotification(
          "Please save the preset name first.",
          type = "warning"
        )
        return()
      }
      req(editing_id)

      hour_val <- data$hour
      if (nchar(trimws(hour_val)) == 0) {
        showNotification("Please select an hour.", type = "error")
        return()
      }

      new_headway <- suppressWarnings(as.numeric(data$headway))
      if (nchar(trimws(data$headway)) == 0) {
        new_headway <- NA_integer_
      } else if (is.na(new_headway) || new_headway < 1 || new_headway > 119) {
        showNotification(
          "Headway must be between 1 and 119, or left blank.",
          type = "error"
        )
        return()
      } else {
        new_headway <- as.integer(round(new_headway))
      }

      sp_data <- service_patterns()
      pattern_data <- sp_data$service_patterns[[editing_id]]

      # Check for duplicate hour
      if (!is.null(pattern_data) && hour_val %in% pattern_data$hour) {
        showNotification(
          "This hour already exists in the preset.",
          type = "warning"
        )
        return()
      }

      new_row <- data.frame(
        hour = hour_val,
        headway = new_headway,
        stringsAsFactors = FALSE
      )

      if (is.null(pattern_data) || nrow(pattern_data) == 0) {
        pattern_data <- new_row
      } else {
        pattern_data <- rbind(pattern_data, new_row)
      }

      pattern_data <- pattern_data[order(pattern_data$hour), ]
      sp_data$service_patterns[[editing_id]] <- pattern_data
      service_patterns(sp_data)
      sched_preset_hour_adding(FALSE)

      showNotification(
        paste0("Hour ", hour_val, " added."),
        type = "message"
      )
    })

    # Delete hour
    observeEvent(input$sched_preset_hour_delete_click, {
      editing_id <- sched_preset_editing_id()
      hour_val <- input$sched_preset_hour_delete_click$hour
      req(editing_id, hour_val)

      sp_data <- service_patterns()
      pattern_data <- sp_data$service_patterns[[editing_id]]

      pattern_data <- pattern_data[pattern_data$hour != hour_val, ]
      sp_data$service_patterns[[editing_id]] <- pattern_data
      service_patterns(sp_data)

      sched_preset_hour_editing(NULL)

      showNotification(
        paste0("Hour ", hour_val, " removed."),
        type = "message"
      )
    })

    # OBSERVERS : SPEED PROFILE PANEL-------------------

    #Load speed profile
    observe({
      editing_itin <- sched_editing_itin_id()
      service_id <- sched_edit_service_id()

      if (is.null(editing_itin) || is.null(service_id)) {
        sched_sp_speed_factors(NULL)
        sched_sp_stop_data(NULL)
        return()
      }

      current_data <- ssfs()

      # Get stop_seq for this itinerary
      stop_data <- current_data$stop_seq |>
        filter(itin_id == editing_itin) |>
        arrange(stop_sequence)

      if (nrow(stop_data) < 2) {
        sched_sp_speed_factors(NULL)
        sched_sp_stop_data(NULL)
        return()
      }

      # Exclude last stop (speed_factor is NA for last stop)
      stop_data <- stop_data[-nrow(stop_data), ]

      # Get available hours for this itin + service
      hour_choices <- current_data$hsh |>
        filter(itin_id == editing_itin, service_id == service_id) |>
        arrange(hour_dep) |>
        pull(hour_dep) |>
        unique()

      if (length(hour_choices) == 0) {
        sched_sp_speed_factors(NULL)
        sched_sp_stop_data(NULL)
        return()
      }

      # Update hour select
      updateSelectInput(
        session,
        "sched_sp_hour",
        choices = hour_choices,
        selected = hour_choices[1]
      )

      # Load base speed from first hour
      base_speed <- current_data$hsh |>
        filter(
          itin_id == editing_itin,
          service_id == service_id,
          hour_dep == hour_choices[1]
        ) |>
        pull(speed)

      if (length(base_speed) > 0) {
        sched_sp_base_speed(base_speed[1])
        sched_sp_stop_data(stop_data)
        sched_sp_speed_factors(stop_data$speed_factor)
      }
    })

    # -- Observer: update base speed when hour select changes --

    observeEvent(
      input$sched_sp_hour,
      {
        editing_itin <- sched_editing_itin_id()
        service_id <- sched_edit_service_id()
        req(editing_itin, service_id, input$sched_sp_hour)

        current_data <- ssfs()

        base_speed <- current_data$hsh |>
          filter(
            itin_id == editing_itin,
            service_id == service_id,
            hour_dep == input$sched_sp_hour
          ) |>
          pull(speed)

        if (length(base_speed) > 0) {
          sched_sp_base_speed(base_speed[1])
        }
      },
      ignoreInit = TRUE
    )

    # -- Render plotly graph --

    output$sched_sp_plot <- plotly::renderPlotly({
      req(sched_sp_speed_factors(), sched_sp_stop_data())

      stop_data <- sched_sp_stop_data()
      sf_values <- sched_sp_speed_factors()
      base_speed <- sched_sp_base_speed()

      actual_speeds <- round(sf_values * base_speed, 1)

      stop_labels <- if (
        "stop_name" %in% names(stop_data) && !all(is.na(stop_data$stop_name))
      ) {
        stop_data$stop_name
      } else {
        stop_data$stop_id
      }

      plot_data <- data.frame(
        stop_seq = stop_data$stop_sequence,
        speed = actual_speeds,
        speed_factor = sf_values,
        stop_name = stop_labels
      )

      plotly::plot_ly(
        plot_data,
        x = ~stop_seq,
        y = ~speed,
        text = ~ paste0(
          "Stop: ",
          stop_name,
          " (seq ",
          stop_seq,
          ")",
          "\nSpeed: ",
          speed,
          " km/h",
          "\nFactor: ",
          speed_factor
        ),
        hoverinfo = "text",
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 12, color = "#124559"),
        line = list(color = "#124559", width = 2)
      ) |>
        plotly::layout(
          xaxis = list(
            title = "Stop sequence",
            fixedrange = TRUE,
            dtick = 1,
            range = c(
              min(plot_data$stop_seq) - 0.5,
              max(plot_data$stop_seq) + 0.5
            )
          ),
          yaxis = list(
            title = "Speed (km/h)",
            range = c(0, max(actual_speeds) * 1.3),
            fixedrange = TRUE
          ),
          margin = list(t = 20)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # -- Render speed factors table --

    output$sched_sp_table_ui <- renderUI({
      req(sched_sp_speed_factors(), sched_sp_stop_data())

      stop_data <- sched_sp_stop_data()
      sf_values <- sched_sp_speed_factors()
      base_speed <- sched_sp_base_speed()
      n <- length(sf_values)
      ns <- session$ns

      table_rows <- lapply(1:n, function(i) {
        label <- if (
          "stop_name" %in% names(stop_data) && !is.na(stop_data$stop_name[i])
        ) {
          stop_data$stop_name[i]
        } else {
          stop_data$stop_id[i]
        }

        tags$tr(
          tags$td(label),
          tags$td(
            style = "text-align: center;",
            stop_data$stop_sequence[i]
          ),
          tags$td(
            style = "text-align: center;",
            textOutput(ns(paste0("sched_sp_sf_", i)), inline = TRUE)
          ),
          tags$td(
            style = "text-align: center;",
            textOutput(ns(paste0("sched_sp_spd_", i)), inline = TRUE)
          ),
          tags$td(
            div(
              class = "adjust-btns",
              tags$button(
                onclick = sprintf("schedSpDown(%d)", i),
                "\u2193"
              ),
              tags$button(
                onclick = sprintf("schedSpUp(%d)", i),
                "\u2191"
              )
            )
          )
        )
      })

      tagList(
        tags$table(
          class = "sched-sf-table",
          tags$thead(
            tags$tr(
              tags$th("From stop", style = "width: 30%;"),
              tags$th("Sequence", style = "width: 10%; text-align: center;"),
              tags$th(
                "Speed factor",
                style = "width: 15%; text-align: center;"
              ),
              tags$th(
                "Speed (km/h)",
                style = "width: 15%; text-align: center;"
              ),
              tags$th("Adjust", style = "width: 15%;")
            )
          ),
          tags$tbody(table_rows)
        ),
        div(
          class = "sched-sf-actions",
          tags$button(
            style = "background-color: #F4A582; color: white;",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Math.random(), {priority:'event'})",
              ns("sched_sp_reset")
            ),
            "Reset all to 1.0"
          )
        )
      )
    })

    # Force table to render even when collapsed, so it's ready on first expand
    outputOptions(output, "sched_sp_table_ui", suspendWhenHidden = FALSE)

    # -- Create text outputs for speed factor and speed values --

    observe({
      req(sched_sp_speed_factors(), sched_sp_stop_data())

      n <- length(sched_sp_speed_factors())
      already_created <- isolate(sched_sp_text_outputs_created())

      if (n > already_created) {
        lapply((already_created + 1):n, function(i) {
          output[[paste0("sched_sp_sf_", i)]] <- renderText({
            sf <- sched_sp_speed_factors()
            if (length(sf) >= i) sprintf("%.1f", sf[i]) else ""
          })
          output[[paste0("sched_sp_spd_", i)]] <- renderText({
            sf <- sched_sp_speed_factors()
            if (length(sf) >= i) {
              sprintf("%.1f", round(sf[i] * sched_sp_base_speed(), 1))
            } else {
              ""
            }
          })
        })
        sched_sp_text_outputs_created(n)
      }
    })

    # -- Up/down button observers --

    observe({
      req(sched_sp_speed_factors(), sched_sp_stop_data())

      n <- length(sched_sp_speed_factors())
      already_created <- isolate(sched_sp_observers_created())

      if (n > already_created) {
        lapply((already_created + 1):n, function(i) {
          observeEvent(
            input[[paste0("sched_sp_up_placeholder_", i)]],
            {
              NULL
            },
            ignoreInit = TRUE
          )
        })
        sched_sp_observers_created(n)
      }
    })

    observeEvent(input$sched_sp_up, {
      i <- input$sched_sp_up$index
      current <- sched_sp_speed_factors()
      if (length(current) >= i) {
        current[i] <- min(2.5, current[i] + 0.1)
        current[i] <- round(current[i], 1)
        current <- sched_sp_normalize(current)
        sched_sp_speed_factors(current)

        # Write directly to ssfs
        editing_itin <- sched_editing_itin_id()
        stop_data <- sched_sp_stop_data()
        if (!is.null(editing_itin) && !is.null(stop_data)) {
          current_data <- ssfs()
          for (j in seq_along(current)) {
            match_idx <- which(
              current_data$stop_seq$itin_id == editing_itin &
                current_data$stop_seq$stop_sequence ==
                  stop_data$stop_sequence[j]
            )
            if (length(match_idx) == 1) {
              current_data$stop_seq$speed_factor[match_idx] <- current[j]
            }
          }
          ssfs(current_data)
        }
      }
    })

    observeEvent(input$sched_sp_down, {
      i <- input$sched_sp_down$index
      current <- sched_sp_speed_factors()
      if (length(current) >= i) {
        current[i] <- max(0.1, current[i] - 0.1)
        current[i] <- round(current[i], 1)
        sched_sp_speed_factors(sched_sp_normalize(current))
      }
      # Write directly to ssfs
      editing_itin <- sched_editing_itin_id()
      stop_data <- sched_sp_stop_data()
      if (!is.null(editing_itin) && !is.null(stop_data)) {
        current_data <- ssfs()
        for (j in seq_along(current)) {
          match_idx <- which(
            current_data$stop_seq$itin_id == editing_itin &
              current_data$stop_seq$stop_sequence == stop_data$stop_sequence[j]
          )
          if (length(match_idx) == 1) {
            current_data$stop_seq$speed_factor[match_idx] <- current[j]
          }
        }
        ssfs(current_data)
      }
    })

    # -- Reset speed factors --

    observeEvent(input$sched_sp_reset, {
      req(sched_sp_speed_factors())
      n <- length(sched_sp_speed_factors())
      new_factors <- rep(1.0, n)
      sched_sp_speed_factors(new_factors)

      editing_itin <- sched_editing_itin_id()
      stop_data <- sched_sp_stop_data()
      if (!is.null(editing_itin) && !is.null(stop_data)) {
        current_data <- ssfs()
        for (j in seq_along(new_factors)) {
          match_idx <- which(
            current_data$stop_seq$itin_id == editing_itin &
              current_data$stop_seq$stop_sequence == stop_data$stop_sequence[j]
          )
          if (length(match_idx) == 1) {
            current_data$stop_seq$speed_factor[match_idx] <- new_factors[j]
          }
        }
        ssfs(current_data)
      }
    })

    #Track speed factors toggle
    observeEvent(input$sched_sf_toggle, {
      sched_sp_factors_visible(input$sched_sf_toggle)
    })
  })
}
