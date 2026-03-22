#' Retain routes from GTFS
#'
#' @param gtfs An object of class 'gtfs'
#' @param retain_routes A character vector indicating the route_ids you wish to retain in the GTFS
#'
#' @returns An object of class 'gtfs'
#'
#' @export
#' @examples
#' \dontrun{
#' # Import GTFS
#' gtfs <- gtfstools::read_gtfs("path/to/gtfs.zip")
#'
#' # filter to retain specified routes
#' gtfs_filtered <- gtfs_retain_routes(gtfs,retain_routes = c("route_1","route_2"))
#' }
gtfs_retain_routes <- function(gtfs, retain_routes) {
  #filter out modified routes
  gtfs$routes <-
    gtfs$routes |>
    filter(route_id %in% retain_routes)

  #identify agency_ids to keep
  filter_agency_id <-
    gtfs$routes |>
    pull(agency_id) |>
    unique()

  #identify trip_ids to keep
  filter_trip_id <-
    gtfs$trips |>
    filter(route_id %in% retain_routes) |>
    pull(trip_id) |>
    unique()

  #identify shape_ids to filter out
  filter_shape_id <-
    gtfs$trips |>
    filter(route_id %in% retain_routes) |>
    pull(shape_id) |>
    unique()

  #filter service id

  filter_service_id <-
    gtfs$trips |>
    filter(route_id %in% retain_routes) |>
    pull(service_id) |>
    unique()

  #keep the good trips
  gtfs$trips <-
    gtfs$trips |>
    filter(route_id %in% retain_routes)

  #keep the good stop times
  gtfs$stop_times <-
    gtfs$stop_times |>
    filter(trip_id %in% filter_trip_id)

  #filter stop id

  filter_stop_ids <-
    gtfs$stop_times |>
    pull(stop_id) |>
    unique()

  #keep the good shapes

  gtfs$shapes <-
    gtfs$shapes |>
    filter(shape_id %in% filter_shape_id)

  #keep the good services

  gtfs$calendar <-
    gtfs$calendar |>
    filter(service_id %in% filter_service_id)

  #keep good agencies

  gtfs$agency <-
    gtfs$agency |>
    filter(agency_id %in% filter_agency_id)

  #keep the good stops

  gtfs$stops <-
    gtfs$stops |>
    filter(stop_id %in% filter_stop_ids)

  if ("calendar_dates" %in% names(gtfs)) {
    gtfs$calendar_dates <-
      gtfs$calendar_dates |>
      filter(service_id %in% filter_service_id)
  }

  return(gtfs)
}

#' Remove routes from GTFS
#'
#' @param gtfs An object of class 'gtfs'
#' @param remove_routes A character vector of the route_ids that you wish to remove from the GTFS
#'
#' @returns An object of class 'gtfs'
#'
#' @export
#' @examples
#' \dontrun{
#' # Import GTFS
#' gtfs <- gtfstools::read_gtfs("path/to/gtfs.zip")
#'
#' # filter to remove specified routes
#' gtfs_filtered <- gtfs_remove_routes(gtfs,remove_routes = c("route_3","route_4"))
#' }
gtfs_remove_routes <- function(gtfs, remove_routes) {
  #filter out modified routes
  gtfs$routes <-
    gtfs$routes |>
    filter(!route_id %in% remove_routes)

  #identify agency_ids to keep
  filter_agency_id <-
    gtfs$routes |>
    pull(agency_id) |>
    unique()

  #identify trip_ids to keep
  filter_trip_id <-
    gtfs$trips |>
    filter(!route_id %in% remove_routes) |>
    pull(trip_id) |>
    unique()

  #identify shape_ids to keep
  filter_shape_id <-
    gtfs$trips |>
    filter(!route_id %in% remove_routes) |>
    pull(shape_id) |>
    unique()

  #identify service ids to keep
  filter_service_id <-
    gtfs$trips |>
    filter(!route_id %in% remove_routes) |>
    pull(service_id) |>
    unique()

  #remove trips
  gtfs$trips <-
    gtfs$trips |>
    filter(!route_id %in% remove_routes)

  #keep the good stop times
  gtfs$stop_times <-
    gtfs$stop_times |>
    filter(trip_id %in% filter_trip_id)

  #identify stops to keep
  filter_stop_ids <-
    gtfs$stop_times |>
    pull(stop_id) |>
    unique()

  #keep the good shapes
  gtfs$shapes <-
    gtfs$shapes |>
    filter(shape_id %in% filter_shape_id)

  #keep the good services
  gtfs$calendar <-
    gtfs$calendar |>
    filter(service_id %in% filter_service_id)

  #keep good agencies

  gtfs$agency <-
    gtfs$agency |>
    filter(agency_id %in% filter_agency_id)

  #keep the good stops
  gtfs$stops <-
    gtfs$stops |>
    filter(stop_id %in% filter_stop_ids)

  if ("calendar_dates" %in% names(gtfs)) {
    gtfs$calendar_dates <-
      gtfs$calendar_dates |>
      filter(service_id %in% filter_service_id)
  }

  return(gtfs)
}
