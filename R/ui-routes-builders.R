# Build the agency <option> HTML for a <select>
build_agency_options <- function(agencies, selected_id = NULL) {
  if (nrow(agencies) == 0) {
    return("")
  }
  paste0(
    sapply(seq_len(nrow(agencies)), function(i) {
      sel <- if (
        !is.null(selected_id) &&
          agencies$agency_id[i] == selected_id
      ) {
        " selected"
      } else {
        ""
      }
      paste0(
        '<option value="',
        agencies$agency_id[i],
        '"',
        sel,
        '>',
        agencies$agency_id[i],
        '</option>'
      )
    }),
    collapse = ""
  )
}

# Build the route type <option> HTML for a <select>
build_route_type_options <- function(selected_type = 3L) {
  route_types <- c(
    "3" = "Bus",
    "0" = "Tram",
    "1" = "Metro",
    "2" = "Rail",
    "4" = "Ferry",
    "5" = "Cable tram",
    "6" = "Gondola",
    "7" = "Funicular",
    "11" = "Trolleybus",
    "12" = "Monorail"
  )
  paste0(
    sapply(names(route_types), function(val) {
      sel <- if (as.character(selected_type) == val) " selected" else ""
      paste0(
        '<option value="',
        val,
        '"',
        sel,
        '>',
        route_types[val],
        '</option>'
      )
    }),
    collapse = ""
  )
}

# Build the route inline form (used for both add and edit)
build_route_form <- function(agencies, route = NULL) {
  agency_opts <- build_agency_options(
    agencies,
    selected_id = if (!is.null(route)) route$agency_id else NULL
  )
  rt_opts <- build_route_type_options(
    selected_type = if (!is.null(route)) route$route_type else 3L
  )

  div(
    class = "route-edit-form",
    tags$label(
      "Route ID",
      info_popover(
        "Unique identifier for route.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    tags$input(
      type = "text",
      id = "inline_route_id",
      value = if (!is.null(route)) route$route_id else NULL,
      placeholder = if (is.null(route)) "e.g., 14" else NULL
    ),
    tags$label(
      "Agency",
      info_popover(
        "Agency for specified route.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    htmltools::HTML(paste0(
      '<select id="inline_agency_id">',
      agency_opts,
      '</select>'
    )),
    tags$label(
      "Short name",
      info_popover(
        "Short name of a route. Often a short, abstract identifier (e.g., '32', '100X', 'Green') that riders use to identify a route.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    tags$input(
      type = "text",
      id = "inline_route_short_name",
      value = if (!is.null(route)) route$route_short_name else NULL,
      placeholder = if (is.null(route)) "e.g., 14" else NULL
    ),
    tags$label(
      "Long name",
      info_popover(
        "Full name of a route. This name is generally more descriptive than the route_short_name and often includes the route's destination or stop.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    tags$input(
      type = "text",
      id = "inline_route_long_name",
      value = if (!is.null(route)) route$route_long_name else NULL,
      placeholder = if (is.null(route)) "e.g., Hastings / UBC" else NULL
    ),
    tags$label(
      "Route type",
      info_popover(
        "Indicates the type of transportation used on a route.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    htmltools::HTML(paste0(
      '<select id="inline_route_type">',
      rt_opts,
      '</select>'
    )),
    tags$label(
      "Route colour",
      info_popover(
        "Route colour designation that matches public facing material.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    tags$input(
      type = "color",
      id = "inline_route_color",
      value = if (!is.null(route)) {
        paste0("#", route$route_color)
      } else {
        "#92C5DE"
      },
      style = "height: 30px; padding: 2px;"
    ),
    tags$label(
      "Text colour",
      info_popover(
        "Legible color to use for text drawn against a background of route_color.",
        "https://gtfs.org/schedule/reference/#routestxt"
      )
    ),
    tags$input(
      type = "color",
      id = "inline_route_text_color",
      value = if (!is.null(route)) {
        paste0("#", route$route_text_color)
      } else {
        "#000000"
      },
      style = "height: 30px; padding: 2px;"
    ),
    div(
      class = "btn-row",
      tags$button(
        class = "btn-save",
        onclick = "saveRouteFromForm()",
        htmltools::HTML("&#10003; Save")
      ),
      tags$button(class = "btn-cancel", onclick = "cancelRouteEdit()", "Cancel")
    )
  )
}

# Build a normal (non-editing) route row
build_route_row <- function(route, is_expanded) {
  expand_icon <- if (is_expanded) {
    htmltools::HTML("&#9660;")
  } else {
    htmltools::HTML("&#9654;")
  }

  div(
    class = paste0("route-list-row", if (is_expanded) " expanded" else ""),
    onclick = sprintf("toggleRouteExpand('%s')", route$route_id),
    tags$button(
      class = "route-action-btn expand-btn",
      onclick = sprintf(
        "event.stopPropagation(); toggleRouteExpand('%s')",
        route$route_id
      ),
      expand_icon
    ),
    div(
      class = "route-color-badge",
      style = paste0("background-color: #", route$route_color, ";")
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

# Build the itinerary inline form (used for both add and edit)
build_itin_form <- function(
  itin_id,
  direction_id,
  trip_headsign = "",
  is_new = TRUE
) {
  dir_sel_0 <- if (as.integer(direction_id) == 0) " selected" else ""
  dir_sel_1 <- if (as.integer(direction_id) == 1) " selected" else ""

  save_label <- if (is_new) "Create" else htmltools::HTML("&#10003; Save")

  div(
    class = "itin-edit-form",
    tags$label(
      "Itinerary ID",
      info_popover(
        "Unique ID for this itinerary or variant of the route. Will be used as the trip_id prefix in exported GTFS for trips of this itinerary."
      )
    ),
    tags$input(type = "text", id = "inline_itin_id", value = itin_id),
    tags$label(
      "Direction",
      info_popover(
        "Indicates the direction of travel for a trip. Routes generally have at least one outbound (e.g. Northbound or Eastbound) variant and at least inbound or return variant (e.g. Southbound or Westbound). 
                Outbond corresponds to 0 and Inbound corresponds to 1 in exported GTFS.",
        "https://gtfs.org/documentation/schedule/reference/#tripstxt"
      )
    ),
    htmltools::HTML(paste0(
      '<select id="inline_direction_id" onchange="onDirectionChanged()">',
      '<option value="0"',
      dir_sel_0,
      '>Outbound</option>',
      '<option value="1"',
      dir_sel_1,
      '>Inbound</option>',
      '</select>'
    )),
    tags$label(
      "Trip Headsign",
      info_popover(
        "Text that appears on signage identifying the trip's destination to riders.",
        "https://gtfs.org/documentation/schedule/reference/#tripstxt"
      )
    ),
    tags$input(
      type = "text",
      id = "inline_trip_headsign",
      value = if (!is_new) trip_headsign else NULL,
      placeholder = if (is_new) "e.g., Eastbound" else NULL
    ),
    div(
      class = "btn-row",
      tags$button(
        class = "btn-save",
        onclick = "saveItinFromForm()",
        save_label
      ),
      tags$button(class = "btn-cancel", onclick = "cancelItinEdit()", "Cancel")
    )
  )
}

# Build a normal (non-editing) itinerary row
build_itin_row <- function(itin, is_active) {
  div(
    class = paste0("itin-list-row", if (is_active) " active-itin" else ""),
    onclick = sprintf("viewItinFromList('%s')", itin$itin_id),
    span(
      class = "itin-direction-badge",
      if (as.integer(itin$direction_id) == 0) "Out" else "In"
    ),
    div(
      class = "itin-info",
      div(
        class = "itin-info-display",
        span(class = "itin-headsign", itin$trip_headsign),
        span(class = "itin-id-display", paste0("(", itin$itin_id, ")"))
      )
    ),
    div(
      class = "route-actions",
      tags$button(
        class = "route-action-btn edit-btn",
        onclick = sprintf(
          "event.stopPropagation(); editItinFromList('%s')",
          itin$itin_id
        ),
        title = "Edit itinerary",
        htmltools::HTML("&#9998;")
      ),
      tags$button(
        class = "route-action-btn",
        onclick = sprintf(
          "event.stopPropagation(); copyItinFromList('%s')",
          itin$itin_id
        ),
        title = "Duplicate itinerary",
        htmltools::HTML('<i class="fa-solid fa-clone"></i>')
      ),
      tags$button(
        class = "route-action-btn delete-btn",
        onclick = sprintf(
          "event.stopPropagation(); deleteItinFromList('%s')",
          itin$itin_id
        ),
        title = "Delete itinerary",
        htmltools::HTML('<i class="fa-solid fa-trash"></i>')
      )
    )
  )
}
