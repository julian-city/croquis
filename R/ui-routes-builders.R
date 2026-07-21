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
build_route_type_options <- function(selected_type = 3L, lang = "en") {
  route_types <- c(
    "3" = tr("route_type_bus", lang),
    "0" = tr("route_type_tram", lang),
    "1" = tr("route_type_metro", lang),
    "2" = tr("route_type_rail", lang),
    "4" = tr("route_type_ferry", lang),
    "5" = tr("route_type_cable_tram", lang),
    "6" = tr("route_type_gondola", lang),
    "7" = tr("route_type_funicular", lang),
    "11" = tr("route_type_trolleybus", lang),
    "12" = tr("route_type_monorail", lang)
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
build_route_form <- function(agencies, route = NULL, lang = "en") {
  agency_opts <- build_agency_options(
    agencies,
    selected_id = if (!is.null(route)) route$agency_id else NULL
  )
  rt_opts <- build_route_type_options(
    selected_type = if (!is.null(route)) route$route_type else 3L,
    lang = lang
  )

  div(
    class = "route-edit-form",
    tags$label(
      tr("lbl_route_id", lang),
      info_popover(
        tr("pop_route_id", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
      )
    ),
    tags$input(
      type = "text",
      id = "inline_route_id",
      value = if (!is.null(route)) route$route_id else NULL,
      placeholder = if (is.null(route)) tr("route_ph_id", lang) else NULL
    ),
    tags$label(
      tr("lbl_agency", lang),
      info_popover(
        tr("pop_route_agency", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
      )
    ),
    htmltools::HTML(paste0(
      '<select id="inline_agency_id">',
      agency_opts,
      '</select>'
    )),
    tags$label(
      tr("lbl_short_name", lang),
      info_popover(
        tr("pop_route_short_name", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
      )
    ),
    tags$input(
      type = "text",
      id = "inline_route_short_name",
      value = if (!is.null(route)) route$route_short_name else NULL,
      placeholder = if (is.null(route)) {
        tr("route_ph_short_name", lang)
      } else {
        NULL
      }
    ),
    tags$label(
      tr("lbl_long_name", lang),
      info_popover(
        tr("pop_route_long_name", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
      )
    ),
    tags$input(
      type = "text",
      id = "inline_route_long_name",
      value = if (!is.null(route)) route$route_long_name else NULL,
      placeholder = if (is.null(route)) tr("route_ph_long_name", lang) else NULL
    ),
    tags$label(
      tr("lbl_route_type", lang),
      info_popover(
        tr("pop_route_type", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
      )
    ),
    htmltools::HTML(paste0(
      '<select id="inline_route_type">',
      rt_opts,
      '</select>'
    )),
    tags$label(
      tr("lbl_route_colour", lang),
      info_popover(
        tr("pop_route_colour", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
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
      tr("lbl_text_colour", lang),
      info_popover(
        tr("pop_route_text_colour", lang),
        "https://gtfs.org/schedule/reference/#routestxt",
        lang = lang
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
        htmltools::HTML(paste0("&#10003; ", tr("btn_save", lang)))
      ),
      tags$button(
        class = "btn-cancel",
        onclick = "cancelRouteEdit()",
        tr("btn_cancel", lang)
      )
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
  is_new = TRUE,
  lang = "en"
) {
  dir_sel_0 <- if (as.integer(direction_id) == 0) " selected" else ""
  dir_sel_1 <- if (as.integer(direction_id) == 1) " selected" else ""

  save_label <- if (is_new) {
    tr("btn_create", lang)
  } else {
    htmltools::HTML(paste0("&#10003; ", tr("btn_save", lang)))
  }

  div(
    class = "itin-edit-form",
    tags$label(
      tr("lbl_itin_id", lang),
      info_popover(
        tr("pop_itin_id", lang),
        lang = lang
      )
    ),
    tags$input(type = "text", id = "inline_itin_id", value = itin_id),
    tags$label(
      tr("lbl_direction", lang),
      info_popover(
        tr("pop_direction", lang),
        "https://gtfs.org/documentation/schedule/reference/#tripstxt",
        lang = lang
      )
    ),
    htmltools::HTML(paste0(
      '<select id="inline_direction_id" onchange="onDirectionChanged()">',
      '<option value="0"',
      dir_sel_0,
      '>',
      tr("lbl_outbound", lang),
      '</option>',
      '<option value="1"',
      dir_sel_1,
      '>',
      tr("lbl_inbound", lang),
      '</option>',
      '</select>'
    )),
    tags$label(
      tr("lbl_trip_headsign", lang),
      info_popover(
        tr("pop_trip_headsign", lang),
        "https://gtfs.org/documentation/schedule/reference/#tripstxt",
        lang = lang
      )
    ),
    tags$input(
      type = "text",
      id = "inline_trip_headsign",
      value = if (!is_new) trip_headsign else NULL,
      placeholder = if (is_new) tr("itin_ph_headsign", lang) else NULL
    ),
    div(
      class = "btn-row",
      tags$button(
        class = "btn-save",
        onclick = "saveItinFromForm()",
        save_label
      ),
      tags$button(
        class = "btn-cancel",
        onclick = "cancelItinEdit()",
        tr("btn_cancel", lang)
      )
    )
  )
}

# Build a normal (non-editing) itinerary row
build_itin_row <- function(itin, is_active, lang = "en") {
  div(
    class = paste0("itin-list-row", if (is_active) " active-itin" else ""),
    onclick = sprintf("viewItinFromList('%s')", itin$itin_id),
    span(
      class = "itin-direction-badge",
      if (as.integer(itin$direction_id) == 0) {
        tr("lbl_dir_out", lang)
      } else {
        tr("lbl_dir_in", lang)
      }
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
        title = tr("itin_edit_title", lang),
        htmltools::HTML("&#9998;")
      ),
      tags$button(
        class = "route-action-btn",
        onclick = sprintf(
          "event.stopPropagation(); copyItinFromList('%s')",
          itin$itin_id
        ),
        title = tr("itin_copy_title", lang),
        htmltools::HTML('<i class="fa-solid fa-clone"></i>')
      ),
      tags$button(
        class = "route-action-btn delete-btn",
        onclick = sprintf(
          "event.stopPropagation(); deleteItinFromList('%s')",
          itin$itin_id
        ),
        title = tr("itin_delete_title", lang),
        htmltools::HTML('<i class="fa-solid fa-trash"></i>')
      )
    )
  )
}
