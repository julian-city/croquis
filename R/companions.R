#' Retain routes from GTFS
#'
#' Reads a GTFS and returns a subset GTFS including only the data relevant to specified routes.
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

  gtfs
}

#' Remove routes from GTFS
#'
#' Reads a GTFS and returns a subset GTFS excluding the data relevant to specified routes.
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

  gtfs
}

#' Subset SSFS
#'
#' A handy function that outputs a subset of an input ssfs.
#' You can specify whether you want to remove or retain a based on a set of itin_ids or route_ids.
#'
#' @param ssfs An input SSFS
#' @param subset_id The set of itin_ids or route_ids that you would like to remove or retain
#' @param operation Specify whether you would like the output ssfs to retain (default) or remove the data associated with subset_id
#' @param id_type Specify whether subset_id is itin_id (default) or route_id
#'
#' @returns A SSFS
#'
#' @export
#' @examples
#' #Create a subset SSFS that only includes data pertaining to route 160 of the mileend network
#' ssfs_160 <- ssfs_subset(mileend,subset_id="160",id_type="route_id")
#'
#' #create a subset SSFS that excludes secondary itineraries for various routes in of the mileend network
#' ssfs_mileend_clean <- ssfs_subset(mileend,subset_id=c("160_0_2","161_1_2"),operation="remove")
ssfs_subset <- function(
  ssfs,
  subset_id,
  operation = c("retain", "remove"),
  id_type = c("itin_id", "route_id")
) {
  operation <- match.arg(operation)

  id_type <- match.arg(id_type)

  if (operation == "retain") {
    if (id_type == "route_id") {
      itin_ids_retain <-
        ssfs$itin |> filter(route_id %in% subset_id) |> pull(itin_id)

      route_ids_retain <- subset_id
    } else {
      itin_ids_retain <- subset_id
      route_ids_retain <-
        ssfs$itin |> filter(itin_id %in% subset_id) |> pull(route_id)
    }
  } else {
    if (id_type == "route_id") {
      itin_ids_retain <-
        ssfs$itin |> filter(!route_id %in% subset_id) |> pull(itin_id)

      route_ids_retain <-
        ssfs$itin |> filter(!route_id %in% subset_id) |> pull(route_id)
    } else {
      itin_ids_retain <-
        ssfs$itin |> filter(!itin_id %in% subset_id) |> pull(itin_id)

      route_ids_retain <-
        ssfs$itin |> filter(!itin_id %in% itin_ids_retain) |> pull(route_id)
    }
  }

  ssfs$routes <-
    ssfs$routes |>
    filter(route_id %in% route_ids_retain)

  retain_agency_ids <-
    ssfs$routes |> pull(agency_id) |> unique()

  ssfs$agency <-
    ssfs$agency |> filter(agency_id %in% retain_agency_ids)

  ssfs$itin <-
    ssfs$itin |>
    filter(itin_id %in% itin_ids_retain)

  ssfs$stop_seq <-
    ssfs$stop_seq |>
    filter(itin_id %in% itin_ids_retain)

  retain_stop_ids <-
    ssfs$stop_seq |> pull(stop_id) |> unique()

  ssfs$stops <-
    ssfs$stops |>
    filter(stop_id %in% retain_stop_ids)

  ssfs$span <-
    ssfs$span |>
    filter(itin_id %in% itin_ids_retain)

  ssfs$hsh <-
    ssfs$hsh |>
    filter(itin_id %in% itin_ids_retain)

  retain_service_ids <-
    ssfs$span |>
    pull(service_id) |>
    unique()

  ssfs$calendar <-
    ssfs$calendar |>
    filter(service_id %in% retain_service_ids)

  ssfs
}
