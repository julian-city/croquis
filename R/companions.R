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

#' Generate trips, distance and runtime by hour (TDRH)
#'
#' Function that generates a tibble that can be used to calculate service cost
#' for specified routes or individual route itineraries in terms of service hours and
#' service kilometers.
#'
#' @param ssfs A list of class SSFS
#' @param id_type Either "route_id" or "itin_id"
#' @param id A character vector of one or more route_ids or itin_ids
#' @param service An individual string or vector representing one or several service_ids
#'
#' @export
#' @examples
#' # Generate table for the 99 B line to view runtimes by hour for all itin ids
#' b_line_route_id <- translink$routes |> filter(route_short_name=="099") |> pull(route_id)
#' generate_tdrh(ssfs=translink, id_type="route_id", id = b_line_route_id, service="mon-fri")
generate_tdrh <- function(
  ssfs,
  id_type = c("route_id", "itin_id"),
  id,
  service
) {
  id_type <- match.arg(id_type)

  if (id_type == "route_id") {
    itin_filtid <-
      ssfs$itin |>
      filter(route_id %in% id)
  } else if (id_type == "itin_id") {
    itin_filtid <-
      ssfs$itin |>
      filter(itin_id %in% id)
  }

  itin_len <-
    itin_filtid |>
    mutate(len_m = round(as.numeric(st_length(geometry)))) |>
    as_tibble() |>
    select(itin_id, len_m)

  # generate service cost

  #first, calculate trips by itin_id and service_id using hsh

  itin_ids <- itin_len$itin_id |> unique()

  trips_ph <-
    data.frame(
      itin_id = character(),
      service_id = character(),
      hour_dep = character(),
      n_trips = integer()
    )

  spans <-
    ssfs$span |>
    filter(itin_id %in% itin_ids, service_id %in% service)

  for (i in seq_len(nrow(spans))) {
    itin_id_i <- spans[i, ]$itin_id

    service_id_i <- spans[i, ]$service_id

    service_window_i <- spans[i, ]$service_window

    #initialize cli progress bar
    #cli::cli_progress_update()

    first_dep <- spans[i, ]$first_dep

    last_dep <- spans[i, ]$last_dep

    trip_dep_i <-
      trip_dep_generator(
        ssfs = ssfs,
        first_dep = first_dep,
        last_dep = last_dep,
        itin_id_i = itin_id_i,
        service_id_i = service_id_i
      )

    trips_ph_i <-
      data.frame(
        itin_id = itin_id_i,
        service_id = service_id_i,
        trip_dep = trip_dep_i
      )

    trips_ph_i <-
      trips_ph_i |>
      mutate(
        hour_dep = paste0(
          stringr::str_sub(trip_dep_i, 1, 2),
          ":00:00"
        )
      ) |>
      group_by(itin_id, service_id, hour_dep) |>
      summarise(n_trips = n()) |>
      ungroup()

    trips_ph <- rbind(trips_ph, trips_ph_i)
  }

  # combine with speed from hsh

  trips_ph_hsh <-
    trips_ph |>
    left_join(ssfs$hsh, by = c("itin_id", "service_id", "hour_dep")) |>
    select(-headway)

  # combine with itin_len to generate runtimes

  tdrh <-
    trips_ph_hsh |>
    left_join(itin_len, by = "itin_id") |>
    mutate(runtime = round(((len_m / 1000) / speed) * 60, 1)) |>
    select(-speed)

  tdrh
}

#' Generate total daily costs in service hours and service kilometers
#'
#' Outputs a small tibble of costs for the specified service(s) and itin_id(s) or route_id(s)
#'
#' @param ssfs A list of class SSFS
#' @param id_type Either "route_id" or "itin_id"
#' @param id A character vector of one or more route_ids or itin_ids
#' @param service An individual string or vector representing one or several service_ids
#'
#' @returns A tibble
#'
#' @export
#' @examples
#' # Calculate weekday daily service kilometers and service hours of the 99 B-Line in Vancouver
#' b_line_route_id <- translink$routes |> filter(route_short_name=="099") |> pull(route_id)
#' generate_service_cost(ssfs=translink,id_type="route_id",id=b_line_route_id,service="mon-fri")
generate_service_cost <- function(
  ssfs,
  id_type = c("route_id", "itin_id"),
  id,
  service
) {
  id_type <- match.arg(id_type)

  tdrh <- generate_tdrh(
    ssfs = ssfs,
    id_type = id_type,
    id = id,
    service = service
  )

  itin_ids <- tdrh$itin_id |> unique()

  itin_id_to_route_id <-
    ssfs$itin |>
    as_tibble() |>
    filter(itin_id %in% itin_ids) |>
    select(itin_id, route_id)

  route_ids <- itin_id_to_route_id$route_id |> unique()

  route_id_to_agency_id <-
    ssfs$routes |>
    filter(route_id %in% route_ids) |>
    select(route_id, agency_id)

  itin_id_to_agency_id <-
    itin_id_to_route_id |>
    left_join(route_id_to_agency_id, by = "route_id") |>
    select(-route_id)

  # output : tibble of total daily cost in service km and service hours by agency id
  tdrh |>
    left_join(itin_id_to_agency_id, by = "itin_id") |>
    group_by(agency_id) |>
    #na.rm = TRUE added in summarise in case tdrh returns NA runtimes due to missing speeds in hsh
    #(this risk has not totally been evaluated)
    summarise(
      total_km = round(sum(len_m * n_trips, na.rm = TRUE) / 1000, 1),
      total_h = round(sum(runtime * n_trips, na.rm = TRUE) / 60, 1)
    )
}
