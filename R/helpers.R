# Helper functions used in conversion and calibration functions such as gtfs_to_ssfs.R

#' Generate trip departure times for a time range
#'
#' Internal function that generates trip departure times for a specified range,
#' based on a first departure and a last departure (for example the bounds of a service window)
#' and based on the hsh values of the ssfs, for a specific itin_id and service_id.
#' Used within ssfs_to_gtfs() as well as within the cost calculator function
#'
#' @param ssfs A list of class SSFS
#' @param first_dep A string indicating first departure time in HH:MM:SS format
#' @param last_dep A string indicating last departure time in HH:MM:SS format
#' @param itin_id_i A string indicating a specific itin_id
#' @param service_id_i A string indicating a specific service_id
#'
#' @returns A vector of strings of trip departure times in HH:MM:SS format
#'
#' @keywords internal
trip_dep_generator <- function(
  ssfs,
  first_dep,
  last_dep,
  itin_id_i,
  service_id_i
) {
  if (first_dep == last_dep) {
    #if first dep and last dep are the same,
    #then there is only one trip

    trip_dep <- first_dep
  } else {
    headways <-
      ssfs$hsh |>
      filter(itin_id == itin_id_i, service_id == service_id_i) |>
      select(hour_dep, headway)

    #initialize the while loop to build out list of trips (departure times)
    trip_dep <- first_dep
    next_dep_duration <- as.duration(minutes(0)) #this refreshes the condition on the below loop

    while (next_dep_duration < as.duration(hms(last_dep))) {
      #takes the last / latest departure in the vector of departures trip_dep
      prev_dep <- as.duration(hms(trip_dep[length(trip_dep)]))
      #identify the hour of departure of this trip
      hour_prev_dep <- sprintf(
        "%02d:00:00",
        as.numeric(floor(as.numeric(prev_dep) / 3600))
      )
      #identify based on the ssfs what the headway is at this hour
      headway <- headways |>
        filter(hour_dep == hour_prev_dep) |>
        pull(headway)

      #IF there is no headway value associated with the hour of the previous departure
      #AND there is no hour specified in the headways table beyond the hour of the previous departure
      #THEN end the loop
      #ELSE IF no headway value associated with the hour of the previous departure
      #AND there is an hour that is specified in the headways table beyond the hour of the previous departure
      #THEN set the next_dep_duration to that hour
      #ELSE calculate the next departure based on the headway and the previous hour

      if (
        is.na(headway) &&
          all(
            as.duration(hms(hour_prev_dep)) >=
              as.duration(hms(headways$hour_dep))
          )
      ) {
        break
      } else if (is.na(headway)) {
        length_hours_prior <- #index of the TRUE value furthest along the result of this logical statement
          max(which(
            as.duration(hms(hour_prev_dep)) >=
              as.duration(hms(headways$hour_dep))
          ))
        next_dep_duration <- as.duration(hms(headways$hour_dep[
          length_hours_prior + 1
        ]))
      } else {
        #determine the time of the next departure, encoded as duration
        next_dep_duration <- prev_dep + as.duration(seconds(headway * 60))
        #the duration coding enables us to write departure times beyond 24:00:00 and to
        #set the condition that ends this while loop
        #identify what the hour of the subsequent departure would be
        hour_next_dep <- sprintf(
          "%02d:00:00",
          as.numeric(floor(as.numeric(next_dep_duration) / 3600))
        )

        #If that hour is NOT within the list of hours specified in the headways table
        #AND there is no hour beyond the that one listed
        #THEN break the loop
        #ELSE IF that hour is NOT within the list of hours specified in the headways table
        #AND there is a subsequent hour listed in the headways table
        #THEN overwrite next_dep_duration to that hour

        if (
          !hour_next_dep %in% headways$hour_dep &&
            all(
              as.duration(hms(hour_next_dep)) >
                as.duration(hms(headways$hour_dep))
            )
        ) {
          break
        } else if (!hour_next_dep %in% headways$hour_dep) {
          length_hours_prior <- #index of the TRUE value furthest along the result of this logical statement
            max(which(
              as.duration(hms(hour_next_dep)) >=
                as.duration(hms(headways$hour_dep))
            ))
          next_dep_duration <- as.duration(hms(headways$hour_dep[
            length_hours_prior + 1
          ]))
        }
      }

      #hours minutes days calculated separately to encode times up to 32:00:00
      next_dep_h <- round(
        as.numeric(floor(as.numeric(next_dep_duration) / 3600)),
        0
      ) #REMOVED the %% that was here previously
      next_dep_m <- round(
        as.numeric(floor(as.numeric(next_dep_duration) / 60)) %% 60,
        0
      )
      next_dep_s <- round(as.numeric(next_dep_duration) %% 60, 0)

      next_dep <- sprintf(
        "%02d:%02d:%02d",
        next_dep_h,
        next_dep_m,
        next_dep_s
      )

      trip_dep <- c(trip_dep, next_dep)
    }
  }

  trip_dep
}

# helpers used in gtfs_to_ssfs()

gtfs_parallel_workers <- function(workers, task_count) {
  if (length(workers) != 1 || is.na(workers)) {
    cli::cli_abort("{.arg workers} must be a single positive integer.")
  }

  workers <- as.integer(workers)

  if (workers < 1) {
    cli::cli_abort("{.arg workers} must be at least 1.")
  }

  min(workers, task_count)
}

croquis_parallel_lapply <- function(x, fun, workers) {
  workers <- gtfs_parallel_workers(workers, length(x))

  if (workers <= 1 || length(x) <= 1) {
    return(lapply(x, fun))
  }

  if (.Platform$OS.type == "windows") {
    cli::cli_warn(
      "Parallel GTFS conversion is not supported on Windows; falling back to a single worker."
    )
    return(lapply(x, fun))
  }

  result <- parallel::mclapply(
    x,
    fun,
    mc.cores = workers,
    mc.preschedule = FALSE
  )

  error_result <- purrr::keep(result, inherits, "try-error")

  if (length(error_result) > 0) {
    cli::cli_abort(conditionMessage(attr(error_result[[1]], "condition")))
  }

  result
}

build_shape_points_for_itin <- function(
  itin_id,
  stop_seq_proto,
  stops,
  itin_to_stop_seq,
  route_info,
  routing_server
) {
  itin_stop_seq <-
    stop_seq_proto[stop_seq_proto$itin_id == itin_id, , drop = FALSE] |>
    arrange(stop_sequence)

  stops_itin <- stops[
    match(itin_stop_seq$stop_id, stops$stop_id),
    ,
    drop = FALSE
  ]

  route_id <- unique(itin_to_stop_seq$route_id[
    itin_to_stop_seq$itin_id == itin_id
  ])[1]
  route_type <- unique(route_info$route_type[route_info$route_id == route_id])[
    1
  ]

  if (route_type %in% c(3, 5, 11)) {
    if (routing_server == "OSRM") {
      shape <- osrm::osrmRoute(loc = stops_itin, overview = "full")
    } else {
      shape <- valh::vl_route(loc = stops_itin)
    }

    shape |>
      select(geometry) |>
      st_cast("POINT") |>
      mutate(coords = st_coordinates(geometry)) |>
      mutate(
        shape_pt_lon = coords[, "X"],
        shape_pt_lat = coords[, "Y"],
        shape_pt_sequence = row_number(),
        shape_id = itin_id
      ) |>
      as.data.table() |>
      select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence)
  } else {
    stops_itin |>
      select(geometry) |>
      mutate(coords = st_coordinates(geometry)) |>
      mutate(
        shape_pt_lon = coords[, "X"],
        shape_pt_lat = coords[, "Y"],
        shape_pt_sequence = row_number(),
        shape_id = itin_id
      ) |>
      as.data.table() |>
      select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence)
  }
}

compute_interstop_distances_for_itin <- function(
  itin_id,
  stop_seq_proto,
  shapes_points,
  stops
) {
  itin_stop_seq <-
    stop_seq_proto[stop_seq_proto$itin_id == itin_id, , drop = FALSE] |>
    arrange(stop_sequence)

  if (nrow(itin_stop_seq) == 0) {
    return(tibble(stop_seq_id = integer(), interstop_dist = numeric()))
  }

  shapes_points_itin <-
    shapes_points[shapes_points$itin_id == itin_id, , drop = FALSE] |>
    arrange(shape_pt_sequence)

  stop_points_itin <- stops[
    match(itin_stop_seq$stop_id, stops$stop_id),
    ,
    drop = FALSE
  ]

  interstop_distances <- rep(NA_real_, nrow(itin_stop_seq))

  if (nrow(shapes_points_itin) > 0) {
    nearest_shape_indexes <- st_nearest_feature(
      stop_points_itin,
      shapes_points_itin
    )

    # Fix nearest_shape_indexes to ensure strictly ascending order (identical values in nearest_shape_indexes OK)
    for (i in 2:length(nearest_shape_indexes)) {
      if (nearest_shape_indexes[i] < nearest_shape_indexes[i - 1]) {
        lower_bound <- nearest_shape_indexes[i - 1]

        # Upper bound: find next element that's already valid, or use end of shape
        upper_bound <- nrow(shapes_points_itin)
        if (i < length(nearest_shape_indexes)) {
          remaining <- nearest_shape_indexes[
            (i + 1):length(nearest_shape_indexes)
          ]
          valid <- remaining[remaining >= lower_bound]
          if (length(valid) > 0) upper_bound <- valid[1]
        }

        # Re-find nearest shape point within the constrained window
        candidate_shapes <- shapes_points_itin[lower_bound:upper_bound, ]
        distances <- st_distance(stop_points_itin[i, ], candidate_shapes)
        nearest_in_window <- which.min(distances[1, ])
        nearest_shape_indexes[i] <- lower_bound + nearest_in_window - 1
      }
    }

    if (nrow(shapes_points_itin) > 1) {
      segment_lengths <- as.numeric(
        st_distance(
          shapes_points_itin[-nrow(shapes_points_itin), , drop = FALSE],
          shapes_points_itin[-1, , drop = FALSE],
          by_element = TRUE
        )
      )
      cumulative_lengths <- c(0, cumsum(segment_lengths))
    } else {
      cumulative_lengths <- 0
    }

    for (index in seq_len(nrow(itin_stop_seq) - 1)) {
      current_index <- nearest_shape_indexes[index]
      next_index <- nearest_shape_indexes[index + 1]

      if (current_index == next_index) {
        cli::cli_warn(
          "Calculated interstop distance {itin_stop_seq$stop_id[index]} -> {itin_stop_seq$stop_id[index + 1]} (itin {itin_id}) directly between both stops."
        )
        interstop_distances[index] <- as.numeric(
          st_distance(
            stop_points_itin[index, , drop = FALSE],
            stop_points_itin[index + 1, , drop = FALSE]
          )
        )
      } else {
        interstop_distances[index] <- cumulative_lengths[max(
          current_index,
          next_index
        )] -
          cumulative_lengths[min(current_index, next_index)]
      }
    }
  }

  tibble(
    stop_seq_id = itin_stop_seq$stop_seq_id,
    interstop_dist = interstop_distances
  )
}
