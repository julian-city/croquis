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
#' @param verbose When TRUE, prints a message if the SSFS is valid.
#'
#' @return `x`, invisibly, if validation passes.
#'
#' @export
#' @examples
#' # Run validation. Console messages will indicate missing tables, missing vectors (columns), or incorrect class / data type
#' \dontrun{
#' bad <- ligne_jaune
#' bad$hsh <- NULL
#' validate_ssfs(bad)
#' }
#' # "ssfs is valid." message appears if ssfs is valid
#' validate_ssfs(ligne_jaune,verbose=TRUE)
validate_ssfs <- function(x, verbose = TRUE) {
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

  # Display message that ssfs is valid if verbose = TRUE
  if (verbose) {
    cli::cli_alert_success("ssfs is valid.")
  }
  invisible(x)
}

#' Create an ssfs object
#'
#' User-facing constructor for SSFS objects. Called with no arguments, it
#' returns a valid empty skeleton with the correct column types - the
#' simplest way to start building a transit network programmatically.
#' Called with one or more table arguments, it assembles and validates
#' them into a complete ssfs object.
#'
#' @param agency A data.frame with columns: agency_id, agency_name,
#'   agency_url, agency_timezone. Defaults to an empty data.frame with
#'   these columns.
#' @param routes A data.frame with columns: route_id, agency_id,
#'   route_short_name, route_long_name, route_type, route_color,
#'   route_text_color. Defaults to an empty data.frame with these columns.
#' @param stops An sf data.frame (POINT, CRS 4326) with columns:
#'   stop_id, stop_name, geometry. Defaults to an empty sf with these
#'   columns.
#' @param itin An sf data.frame (LINESTRING, CRS 4326) with columns:
#'   itin_id, route_id, direction_id, trip_headsign, geometry. Defaults
#'   to an empty sf with these columns.
#' @param stop_seq A data.frame with columns: itin_id, stop_id,
#'   stop_sequence, speed_factor. Defaults to an empty data.frame with
#'   these columns.
#' @param span A data.frame with columns: itin_id, service_id,
#'   service_window, first_dep, last_dep. Defaults to an empty data.frame
#'   with these columns.
#' @param hsh A data.frame with columns: itin_id, service_id, hour_dep,
#'   headway, speed. Defaults to an empty data.frame with these columns.
#' @param calendar A data.frame with columns: service_id, monday, tuesday,
#'   wednesday, thursday, friday, saturday, sunday, start_date, end_date.
#'   Defaults to an empty data.frame with these columns.
#'
#' @return A validated object of class `"ssfs"`.
#'
#' @export
#' @examples
#' # Empty skeleton - start from scratch
#' my_ssfs <- ssfs()
#' my_ssfs
#'
#' # Assemble from pre-built tables
#' my_ssfs <- ssfs(
#'   agency = ligne_jaune$agency,
#'   routes = ligne_jaune$routes,
#'   stops = ligne_jaune$stops,
#'   itin = ligne_jaune$itin,
#'   stop_seq = ligne_jaune$stop_seq,
#'   span = ligne_jaune$span,
#'   hsh = ligne_jaune$hsh,
#'   calendar = ligne_jaune$calendar
#' )
ssfs <- function(
  agency = NULL,
  routes = NULL,
  stops = NULL,
  itin = NULL,
  stop_seq = NULL,
  span = NULL,
  hsh = NULL,
  calendar = NULL
) {
  if (is.null(agency)) {
    agency <- data.frame(
      agency_id = character(),
      agency_name = character(),
      agency_url = character(),
      agency_timezone = character(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(routes)) {
    routes <- data.frame(
      route_id = character(),
      agency_id = character(),
      route_short_name = character(),
      route_long_name = character(),
      route_type = integer(),
      route_color = character(),
      route_text_color = character(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(stops)) {
    stops <- sf::st_sf(
      stop_id = character(),
      stop_name = character(),
      geometry = sf::st_sfc(crs = 4326)
    )
  }

  if (is.null(itin)) {
    itin <- sf::st_sf(
      itin_id = character(),
      route_id = character(),
      direction_id = integer(),
      trip_headsign = character(),
      geometry = sf::st_sfc(crs = 4326)
    )
  }

  if (is.null(stop_seq)) {
    stop_seq <- data.frame(
      itin_id = character(),
      stop_id = character(),
      stop_sequence = integer(),
      speed_factor = double(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(span)) {
    span <- data.frame(
      itin_id = character(),
      service_id = character(),
      service_window = integer(),
      first_dep = character(),
      last_dep = character(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(hsh)) {
    hsh <- data.frame(
      itin_id = character(),
      service_id = character(),
      hour_dep = character(),
      headway = integer(),
      speed = double(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(calendar)) {
    calendar <- data.frame(
      service_id = character(),
      monday = integer(),
      tuesday = integer(),
      wednesday = integer(),
      thursday = integer(),
      friday = integer(),
      saturday = integer(),
      sunday = integer(),
      start_date = character(),
      end_date = character(),
      stringsAsFactors = FALSE
    )
  }

  obj <- new_ssfs(
    agency = agency,
    routes = routes,
    stops = stops,
    itin = itin,
    stop_seq = stop_seq,
    span = span,
    hsh = hsh,
    calendar = calendar
  )

  validate_ssfs(obj, verbose = FALSE)

  obj
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
