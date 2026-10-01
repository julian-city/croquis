#' Convert a SSFS to a GTFS
#'
#' Reads a SSFS (Simplified Speed and Frequency Specification) and
#' converts it into a GTFS.
#'
#' @param ssfs An ssfs list
#' @param dist_traveled When TRUE, adds shape_dist_traveled field to shapes and stop_times tables in output GTFS
#'
#' @returns an object of class 'gtfs'
#'
#' @export
#' @examples
#' # Convert the sample Ligne Jaune SSFS to GTFS
#' gtfs <- ssfs_to_gtfs(ligne_jaune)
#'
#' \donttest{
#' # Include shape_dist_traveled (increases processing time)
#' gtfs_with_dist <- ssfs_to_gtfs(ligne_jaune, dist_traveled = TRUE)
#' }
ssfs_to_gtfs <- function(
  ssfs,
  dist_traveled = FALSE
) {
  #agency and routes can simply be carried over to the final GTFS

  #TRIPS and trip start times-----------

  #initialize trip ids
  trips <-
    tibble(
      itin_id = as.character(),
      trip_id = as.character(),
      route_id = as.character(),
      service_id = as.character(),
      trip_headsign = as.character(),
      direction_id = as.integer(),
      trip_dep = as.character()
    )

  trip_parts <- vector("list", nrow(ssfs$span))

  # initialize progress bar
  cli::cli_progress_bar(
    total = nrow(ssfs$span),
    format = "Calculating trips for route {route_id_i} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = FALSE
  )

  for (i in seq_len(nrow(ssfs$span))) {
    itin_id_i <- ssfs$span[i, ]$itin_id

    service_id_i <- ssfs$span[i, ]$service_id

    service_window_i <- ssfs$span[i, ]$service_window

    route_id_i <-
      ssfs$itin |>
      filter(itin_id == itin_id_i) |>
      pull(route_id)

    #initialize cli progress bar
    cli::cli_progress_update()

    trip_headsign_i <-
      ssfs$itin |>
      filter(itin_id == itin_id_i) |>
      pull(trip_headsign)

    direction_id_i <-
      ssfs$itin |>
      filter(itin_id == itin_id_i) |>
      pull(direction_id)

    first_dep <- ssfs$span[i, ]$first_dep

    last_dep <- ssfs$span[i, ]$last_dep

    trip_dep <-
      trip_dep_generator(
        ssfs = ssfs,
        first_dep = first_dep,
        last_dep = last_dep,
        itin_id_i = itin_id_i,
        service_id_i = service_id_i
      )

    #build out trip ids

    trips_i <-
      tibble(
        itin_id = itin_id_i,
        route_id = route_id_i,
        service_id = service_id_i,
        trip_headsign = trip_headsign_i,
        direction_id = direction_id_i,
        service_window = service_window_i,
        trip_dep = trip_dep
      )

    trips_i <-
      trips_i |>
      mutate(trip_id = row_number(), .before = route_id) |>
      mutate(trip_id = sprintf("%04d", trip_id)) |> #to have all trip ids the same length... might be pertinent?
      mutate(
        trip_id = str_c(
          itin_id,
          "_",
          service_id,
          "_",
          as.character(service_window),
          "_",
          trip_id
        )
      )

    trip_parts[[i]] <- trips_i
  }

  trips <- bind_rows(c(list(trips), trip_parts))

  #STOP TIMES-----------------------

  #calculate interstop distances

  shapes_points <-
    ssfs$itin |>
    select(itin_id, geometry) |>
    st_cast("POINT") |>
    group_by(itin_id) |>
    mutate(shape_pt_sequence = row_number(), .before = geometry) |>
    ungroup()

  stop_seq <-
    ssfs$stop_seq |>
    mutate(
      stop_seq_id = str_c(
        itin_id,
        "_",
        as.character(stop_id),
        "_",
        as.character(stop_sequence)
      )
    )

  interstop_distances <- lapply(
    unique(stop_seq$itin_id),
    function(itin_id_i) {
      compute_interstop_distances_for_itin(
        itin_id = itin_id_i,
        stop_seq_proto = stop_seq,
        shapes_points = shapes_points,
        stops = ssfs$stops
      )
    }
  ) |>
    bind_rows()

  stop_seq <-
    stop_seq |>
    left_join(interstop_distances, by = "stop_seq_id") |>
    mutate(interstop_dist = round(interstop_dist, 2))

  if (dist_traveled) {
    shapes_points <- shapes_points |>
      arrange(itin_id, shape_pt_sequence) |>
      group_by(itin_id) |>
      mutate(
        shape_dist_traveled = {
          n_pts <- n()
          if (n_pts <= 1) {
            0
          } else {
            seg <- as.numeric(st_distance(
              geometry[-n_pts],
              geometry[-1],
              by_element = TRUE
            ))
            round(c(0, cumsum(seg)), 2)
          }
        }
      ) |>
      ungroup()

    # calculate shape_dist_traveled for stop_seq using shapes_points (instead of interstop_dist)

    stop_seq <-
      stop_seq |>
      group_by(itin_id) |>
      mutate(
        shape_dist_traveled = {
          sp_itin <- shapes_points[shapes_points$itin_id == itin_id[1], ]
          stop_pts <- ssfs$stops[match(stop_id, ssfs$stops$stop_id), ]
          nearest_idx <- st_nearest_feature(stop_pts, sp_itin)
          sp_itin$shape_dist_traveled[nearest_idx]
        }
      ) |>
      ungroup()
  }

  #write stop times

  #initialize stop times (with shape dist traveled if the business is TRUE)

  if (dist_traveled) {
    stop_times <-
      tibble(
        stop_id = as.character(),
        departure_time = as.character(),
        trip_id = as.character(),
        stop_sequence = as.integer(),
        shape_dist_traveled = as.numeric()
      )
  } else {
    stop_times <-
      tibble(
        stop_id = as.character(),
        departure_time = as.character(),
        trip_id = as.character(),
        stop_sequence = as.integer()
      )
  }

  stop_time_parts <- vector("list", nrow(trips))
  stop_time_index <- 0L

  #use $span for the loop as each row represents a unique itin_id * service id * service window combo

  #initialize progress bar
  cli::cli_progress_bar(
    total = length(ssfs$span$itin_id),
    format = "Calculating stop times for itin_id {itin_id_i},service_id {service_id_i} and service_window {service_window_i} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = FALSE
  )

  for (i in seq_along(ssfs$span$itin_id)) {
    itin_id_i <- ssfs$span[i, ]$itin_id

    service_id_i <- ssfs$span[i, ]$service_id

    service_window_i <- ssfs$span[i, ]$service_window

    cli::cli_progress_update()

    #hsh for rvar_id and service_id combo

    hsh_i <-
      ssfs$hsh |>
      filter(itin_id == itin_id_i, service_id == service_id_i) |>
      select(hour_dep, headway, speed)

    #identify the trips

    trips_i <-
      trips |>
      filter(
        itin_id == itin_id_i,
        service_id == service_id_i,
        service_window == service_window_i
      ) |>
      select(trip_id, trip_dep)

    #establish template for stop_times based on stop_seq

    if (dist_traveled) {
      stop_times_template <-
        stop_seq |>
        filter(itin_id == itin_id_i) |>
        select(
          stop_id,
          stop_sequence,
          speed_factor,
          interstop_dist,
          shape_dist_traveled
        )
    } else {
      stop_times_template <-
        stop_seq |>
        filter(itin_id == itin_id_i) |>
        select(stop_id, stop_sequence, speed_factor, interstop_dist)
    }

    for (i in seq_len(nrow(trips_i))) {
      trip_id_i <- trips_i[i, ]$trip_id

      trip_dep_i <- trips_i[i, ]$trip_dep

      trip_dep_dur <- as.duration(hms(trip_dep_i))

      stop_times_i <- stop_times_template

      stop_times_i$departure_time <- NA

      stop_times_i$trip_id <- trip_id_i

      #set speed for the trip based on initial departure time
      hour_dep_i <- sprintf(
        "%02d:00:00",
        as.numeric(floor(as.numeric(trip_dep_dur) / 3600))
      )

      #determine what the commercial speed is for that hour, based on the hsh table
      speed_i <- hsh_i |> filter(hour_dep == hour_dep_i) |> pull(speed)

      stop_times_i$departure_time[1] <- trip_dep_i
      #NB IN BRACKETS IS ALWAYS 1 NEVER i because it's for initializing

      n_stops <- nrow(stop_times_i)

      if (n_stops > 1L) {
        # Each segment uses the distance and speed factor of its starting stop.
        segment_rows <- seq_len(n_stops - 1L)

        segment_speed_ms <-
          speed_i *
          stop_times_i$speed_factor[segment_rows] *
          (1000 / 3600)

        segment_seconds <-
          stop_times_i$interstop_dist[segment_rows] / segment_speed_ms

        # Discard fractional seconds for each segment
        stop_offsets <- c(0, cumsum(floor(segment_seconds)))

        # trip_dep_dur was already parsed once earlier in this trip's loop.
        departure_seconds <- numeric(n_stops)
        departure_seconds[1L] <- as.numeric(trip_dep_dur)

        for (stop_index in seq.int(2L, n_stops)) {
          departure_seconds[stop_index] <- floor(
            departure_seconds[stop_index - 1L] +
              segment_seconds[stop_index - 1L]
          )
        }

        # Keep the original first departure string. Format all remaining
        # stop times together, allowing hours beyond 23.
        remaining_seconds <- departure_seconds[-1L]

        stop_times_i$departure_time[-1L] <- sprintf(
          "%02d:%02d:%02d",
          as.integer(floor(remaining_seconds / 3600)),
          as.integer(floor(remaining_seconds / 60) %% 60),
          as.integer(floor(remaining_seconds %% 60))
        )
      }

      if (dist_traveled) {
        stop_times_i <-
          stop_times_i |>
          select(
            trip_id,
            departure_time,
            stop_id,
            stop_sequence,
            shape_dist_traveled
          )
      } else {
        stop_times_i <-
          stop_times_i |>
          select(trip_id, departure_time, stop_id, stop_sequence)
      }

      stop_time_index <- stop_time_index + 1L
      stop_time_parts[[stop_time_index]] <- stop_times_i
    }
  }

  stop_times <- bind_rows(c(list(stop_times), stop_time_parts))

  #modifications to gtfs_to_ssfs:

  if (dist_traveled) {
    stop_times <-
      stop_times |>
      mutate(arrival_time = departure_time) |>
      select(
        trip_id,
        arrival_time,
        departure_time,
        stop_id,
        stop_sequence,
        shape_dist_traveled
      )
  } else {
    stop_times <-
      stop_times |>
      mutate(arrival_time = departure_time) |>
      select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)
  }

  #SHAPES---------

  if (dist_traveled) {
    shapes <-
      shapes_points |>
      mutate(
        coords = st_coordinates(geometry),
        shape_pt_lat = coords[, "Y"],
        shape_pt_lon = coords[, "X"]
      ) |>
      as_tibble() |>
      rename(shape_id = itin_id) |>
      select(
        shape_id,
        shape_pt_sequence,
        shape_pt_lat,
        shape_pt_lon,
        shape_dist_traveled
      )
  } else {
    shapes <-
      shapes_points |>
      mutate(
        coords = st_coordinates(geometry),
        shape_pt_lat = coords[, "Y"],
        shape_pt_lon = coords[, "X"]
      ) |>
      as_tibble() |>
      rename(shape_id = itin_id) |>
      select(shape_id, shape_pt_sequence, shape_pt_lat, shape_pt_lon)
  }

  #STOPS----------

  stops <-
    ssfs$stops |>
    mutate(
      coords = st_coordinates(geometry),
      stop_lat = coords[, "Y"],
      stop_lon = coords[, "X"]
    ) |>
    as_tibble() |>
    select(stop_id, stop_name, stop_lat, stop_lon)

  #modify trips to drop the rvar_id and trip_dep columns

  trips <-
    trips |>
    rename(shape_id = itin_id) |>
    select(trip_id, route_id, service_id, trip_headsign, direction_id, shape_id)

  #write the gtfs with data tables

  #calendar : modify data format of start_date and end_date (as date)--------

  ssfs$calendar <-
    ssfs$calendar |>
    mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))

  #compile gtfs---------

  gtfs <- list(
    agency = as.data.table(ssfs$agency),
    calendar = as.data.table(ssfs$calendar),
    routes = as.data.table(ssfs$routes),
    shapes = as.data.table(shapes),
    stop_times = as.data.table(stop_times),
    stops = as.data.table(stops),
    trips = as.data.table(trips)
  )

  class(gtfs) <- c("gtfs", "dt_gtfs", class(gtfs))

  gtfs
}
