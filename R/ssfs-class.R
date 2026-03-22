#' Create a new ssfs object
#'
#' Low-level constructor. Assembles the 8 component tables into a list and
#' assigns the `"ssfs"` class. Performs no validation — use [validate_ssfs()]
#' or the user-facing [ssfs()] constructor for safety checks.
#'
#' @param agency A data.frame with columns: agency_id, agency_name,
#'   agency_url, agency_timezone.
#' @param routes A data.frame with columns: route_id, agency_id,
#'   route_short_name, route_long_name, route_type, route_color,
#'   route_text_color.
#' @param stops An sf data.frame (POINT, CRS 4326) with columns:
#'   stop_id, stop_name, geometry.
#' @param itin An sf data.frame (LINESTRING, CRS 4326) with columns:
#'   itin_id, route_id, direction_id, trip_headsign, geometry.
#' @param stop_seq A data.frame with columns: itin_id, stop_id,
#'   stop_sequence, speed_factor.
#' @param span A data.frame with columns: itin_id, service_id,
#'   service_window, first_dep, last_dep.
#' @param hsh A data.frame with columns: itin_id, service_id, hour_dep,
#'   headway, speed.
#' @param calendar A data.frame with columns: service_id, monday, tuesday,
#'   wednesday, thursday, friday, saturday, sunday, start_date, end_date.
#'
#' @return An object of class `"ssfs"`, which is a named list of the 8
#'   component tables.
#'
#' @keywords internal
new_ssfs <- function(
  agency,
  routes,
  stops,
  itin,
  stop_seq,
  span,
  hsh,
  calendar
) {
  #this is a low-level internal constructor that is used within ssfs() and at the end of gtfs_to_ssfs()

  ssfs <- list(
    agency = agency,
    routes = routes,
    stops = stops,
    itin = itin,
    stop_seq = stop_seq,
    span = span,
    hsh = hsh,
    calendar = calendar
  )

  class(ssfs) <- c("ssfs", "list")

  ssfs
}

#' Validate an ssfs object
#'
#' Checks that an ssfs object contains the 8 required tables and that each
#' table contains its required columns. Returns the object invisibly if
#' valid; throws an informative error otherwise.
#'
#' @param x An object to validate, typically created by [new_ssfs()] or
#'   [gtfs_to_ssfs()].
#'
#' @return `x`, invisibly, if validation passes.
#'
#' @export
#' @examples
#' # Run validation. Console messages will indicate missing tables, missing vectors (columns), or incorrect class / data type
#' validate_ssfs(ssfs)
validate_ssfs <- function(x) {
  # FUTURE IMPROVEMENT : include individual field validations, for example
  #validate that route_type is an integer within the valid range, that
  #monday through sunday in calendar are integers of either 0 or 1

  # Must be a list
  if (!is.list(x)) {
    stop("An ssfs object must be a list.", call. = FALSE)
  }

  # Required tables
  required_tables <- c(
    "agency",
    "routes",
    "stops",
    "itin",
    "stop_seq",
    "span",
    "hsh",
    "calendar"
  )

  missing_tables <- setdiff(required_tables, names(x))

  if (length(missing_tables) > 0) {
    stop(
      "ssfs is missing required table(s): ",
      paste(missing_tables, collapse = ", "),
      call. = FALSE
    )
  }

  # Required columns per table
  required_cols <- list(
    agency = c("agency_id", "agency_name", "agency_url", "agency_timezone"),
    routes = c(
      "route_id",
      "agency_id",
      "route_short_name",
      "route_long_name",
      "route_type",
      "route_color",
      "route_text_color"
    ),
    stops = c("stop_id", "stop_name"),
    itin = c("itin_id", "route_id", "direction_id", "trip_headsign"),
    stop_seq = c("itin_id", "stop_id", "stop_sequence", "speed_factor"),
    span = c(
      "itin_id",
      "service_id",
      "service_window",
      "first_dep",
      "last_dep"
    ),
    hsh = c("itin_id", "service_id", "hour_dep", "headway", "speed"),
    calendar = c(
      "service_id",
      "monday",
      "tuesday",
      "wednesday",
      "thursday",
      "friday",
      "saturday",
      "sunday",
      "start_date",
      "end_date"
    )
  )

  for (tbl_name in required_tables) {
    tbl <- x[[tbl_name]]
    missing_cols <- setdiff(required_cols[[tbl_name]], colnames(tbl))
    if (length(missing_cols) > 0) {
      stop(
        "ssfs$",
        tbl_name,
        " is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Spatial checks
  if (!inherits(x$stops, "sf")) {
    stop("ssfs$stops must be an sf object.", call. = FALSE)
  }
  if (!inherits(x$itin, "sf")) {
    stop("ssfs$itin must be an sf object.", call. = FALSE)
  }

  invisible(x)
}

#' Create an ssfs object
#'
#' User-facing constructor that assembles the 8 component tables into a
#' validated ssfs object. This is the recommended way to create an ssfs
#' from scratch (as opposed to [gtfs_to_ssfs()], which converts from GTFS).
#'
#' @inheritParams new_ssfs
#'
#' @return A validated object of class `"ssfs"`.
#'
#' @export
ssfs <- function(agency, routes, stops, itin, stop_seq, span, hsh, calendar) {
  # this is a user-facing constructor
  #needs an example at some point

  ssfs <- new_ssfs(
    agency = agency,
    routes = routes,
    stops = stops,
    itin = itin,
    stop_seq = stop_seq,
    span = span,
    hsh = hsh,
    calendar = calendar
  )

  validate_ssfs(ssfs)

  ssfs
}

#' Print an ssfs object
#'
#' Displays a compact summary of an ssfs object, including the number of
#' agencies, routes, stops, itineraries, service IDs, and the date range.
#'
#' @param x An ssfs object.
#' @param ... Additional arguments (ignored).
#'
#' @return `x`, invisibly.
#'
#' @export
print.ssfs <- function(x, ...) {
  n_agencies <- nrow(x$agency)
  n_routes <- nrow(x$routes)
  n_stops <- nrow(x$stops)
  n_itin <- nrow(x$itin)
  n_services <- nrow(x$calendar)

  cat("<ssfs> Simplified Speed and Frequency Structure\n")
  cat("  Agencies:     ", n_agencies, "\n")
  cat("  Routes:       ", n_routes, "\n")
  cat("  Itineraries:  ", n_itin, "\n")
  cat("  Stops:        ", n_stops, "\n")
  cat("  Service IDs:  ", n_services, "\n")

  if (n_routes > 0) {
    route_types <- unique(x$routes$route_type)
    type_labels <- vapply(
      route_types,
      function(rt) {
        switch(
          as.character(rt),
          "0" = "Tram/LRT",
          "1" = "Metro",
          "2" = "Rail",
          "3" = "Bus",
          "4" = "Ferry",
          "5" = "Cable tram",
          "6" = "Gondola",
          "7" = "Funicular",
          "11" = "Trolleybus",
          "12" = "Monorail",
          paste("Type", rt)
        )
      },
      character(1)
    )
    cat("  Route types:  ", paste(type_labels, collapse = ", "), "\n")
  }

  if (n_services > 0 && "start_date" %in% colnames(x$calendar)) {
    start <- min(as.character(x$calendar$start_date))
    end <- max(as.character(x$calendar$end_date))
    cat("  Date range:   ", start, "to", end, "\n")
  }

  invisible(x)
}
