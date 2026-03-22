#' Convert a SSFS to a GTFS
#'
#' @param ssfs An ssfs list
#' @param dist_traveled When TRUE, adds shape_dist_traveled field to shapes and stop_times tables in output GTFS
#'
#' @returns an object of class 'gtfs'
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a GTFS based on a SSFS
#' gtfs <- ssfs_to_gtfs(ssfs)
#' # Create shape_dist_traveled fields in GTFS stop_times and shapes tables
#' # This increases processing time
#' gtfs <- ssfs_to_gtfs(ssfs, dist_traveled = TRUE)
#' }
ssfs_to_gtfs <- function(ssfs, dist_traveled = FALSE) {
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

  # initialize progress bar
  cli::cli_progress_bar(
    total = nrow(ssfs$span),
    format = "Calculating trips for route {route_id_i} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = FALSE
  )

  for (i in 1:nrow(ssfs$span)) {
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
          is.na(headway) &
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
            !hour_next_dep %in% headways$hour_dep &
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

    trips <- bind_rows(trips, trips_i)
  }

  #STOP TIMES-----------------------

  #calculate interstop distances

  #with dist traveled, calculating interstop distances takes 5 times longer

  stop_seq <-
    ssfs$stop_seq #|>
  #  left_join(ssfs$rvar |> select(rvar_id,shape_id),
  #            by="rvar_id")
  #vestigal from V1

  #initialize

  stop_seq$interstop_dist <- NA_real_

  #convert itin into points

  shapes_points <-
    ssfs$itin |>
    select(itin_id, geometry) |>
    st_cast("POINT") |>
    distinct() |>
    #could add a distinct here to remove the duplicates. But perhaps it should come earlier in the process ?
    group_by(itin_id) |>
    mutate(shape_pt_sequence = row_number(), .before = geometry) |>
    ungroup()

  #if dist_traveled is TRUE, then we need to create shape_dist_traveled attribute in shapes_points
  #In this case, we use this to calculate interstop_dist

  if (dist_traveled) {
    shapes_points$shape_dist_traveled <- NA_real_

    #first one is always 0
    shapes_points$shape_dist_traveled[1] <- 0

    cli::cli_progress_bar(
      "Calculating shape_dist_traveled.",
      total = nrow(shapes_points)
    )

    for (i in 2:(nrow(shapes_points))) {
      cli::cli_progress_update()

      if (
        (shapes_points$shape_pt_sequence[i] - 1 ==
          shapes_points$shape_pt_sequence[i - 1]) &
          (shapes_points$itin_id[i] == shapes_points$itin_id[i - 1])
      ) {
        point_before <-
          shapes_points[i - 1, ]

        point <-
          shapes_points[i, ]

        shape_dist_traveled_before <- point_before$shape_dist_traveled

        shapes_points$shape_dist_traveled[i] <-
          round(
            as.numeric(st_distance(point_before, point)) +
              shape_dist_traveled_before,
            2
          )
      } else {
        shapes_points$shape_dist_traveled[i] <- 0
      }
    }

    cli::cli_progress_bar(
      "Calculating interstop distance.",
      total = nrow(stop_seq) - 1
    )

    for (i in 1:(nrow(stop_seq) - 1)) {
      cli::cli_progress_update()

      #CONDITIONS
      #next stop needs to be part of the same sequence AND
      #part of the same rvar_id (just another way of verifying the same stop sequence)
      #ELSE the NA assignment remains

      if (
        (stop_seq$stop_sequence[i] + 1 == stop_seq$stop_sequence[i + 1]) &
          (stop_seq$itin_id[i] == stop_seq$itin_id[i + 1])
      ) {
        itin_id_i <- stop_seq$itin_id[i]

        #shapes points for only the shape_id associated with the rvar_id associated with stop i
        shapes_points_i <-
          shapes_points |>
          filter(itin_id == itin_id_i)

        current_stop_id <- stop_seq$stop_id[i]
        next_stop_id <- stop_seq$stop_id[i + 1]

        current_stop <-
          ssfs$stops |>
          filter(stop_id == current_stop_id)

        next_stop <-
          ssfs$stops |>
          filter(stop_id == next_stop_id)

        #nearest points along shapes_points to current and next stops

        shape_dist_traveled_current <-
          shapes_points_i[
            st_nearest_feature(current_stop, shapes_points_i),
          ]$shape_dist_traveled

        shape_dist_traveled_next <-
          shapes_points_i[
            st_nearest_feature(next_stop, shapes_points_i),
          ]$shape_dist_traveled

        interstop_dist_i <-
          shape_dist_traveled_next - shape_dist_traveled_current

        stop_seq$interstop_dist[i] <- interstop_dist_i
      } else {
        stop_seq$interstop_dist[i] <- NA_real_
      }
    }
  } else {
    #else no shape_dist_traveled, calculate interstop distance directly from
    #shapes and stop seq

    cli::cli_progress_bar(
      "Calculating interstop distance.",
      total = nrow(stop_seq) - 1
    )

    for (i in 1:(nrow(stop_seq) - 1)) {
      cli::cli_progress_update()
      #CONDITIONS
      #next stop needs to be part of the same sequence AND
      #part of the same rvar_id (just another way of verifying the same stop sequence)
      #ELSE the NA assignment remains

      if (
        (stop_seq$stop_sequence[i] + 1 == stop_seq$stop_sequence[i + 1]) &
          (stop_seq$itin_id[i] == stop_seq$itin_id[i + 1])
      ) {
        itin_id_i <- stop_seq$itin_id[i]

        #shapes points for only the shape_id associated with the rvar_id associated with stop i
        shapes_points_i <-
          shapes_points |>
          filter(itin_id == itin_id_i)

        current_stop_id <- stop_seq$stop_id[i]
        next_stop_id <- stop_seq$stop_id[i + 1]

        current_stop <-
          ssfs$stops |>
          filter(stop_id == current_stop_id)

        next_stop <-
          ssfs$stops |>
          filter(stop_id == next_stop_id)

        #nearest points along shapes_points to current and next stops

        interstop_segment_points <-
          shapes_points_i[
            st_nearest_feature(
              current_stop,
              shapes_points_i
            ):st_nearest_feature(next_stop, shapes_points_i),
          ]

        interstop_dist_i <-
          as.numeric(
            interstop_segment_points |>
              summarise(do_union = FALSE) |> #do_union retains the order of the points
              st_cast("LINESTRING") |>
              st_length()
          )

        stop_seq$interstop_dist[i] <- interstop_dist_i
      } else {
        stop_seq$interstop_dist[i] <- NA_real_
      }
    }
  }

  #write stop times

  #add shape_dist_traveled to stop_seq if shape dist traveled is TRUE

  if (dist_traveled) {
    stop_seq <-
      stop_seq |>
      group_by(itin_id) |>
      mutate(cumsum_interdist = cumsum(interstop_dist)) |>
      mutate(
        interstop_dist = replace_na(interstop_dist, 0),
        cumsum_interdist = if_else(
          is.na(cumsum_interdist),
          lag(cumsum_interdist),
          cumsum_interdist
        )
      ) |>
      mutate(shape_dist_traveled = cumsum_interdist - interstop_dist) |>
      select(-cumsum_interdist) |>
      ungroup()
  }

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

  #use $span for the loop as each row represents a unique itin_id * service id * service window combo

  #initialize progress bar
  cli::cli_progress_bar(
    total = length(ssfs$span$itin_id),
    format = "Calculating stop times for itin_id {itin_id_i},service_id {service_id_i} and service_window {service_window_i} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
    clear = FALSE
  )

  for (i in 1:length(ssfs$span$itin_id)) {
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

    for (i in 1:nrow(trips_i)) {
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

      for (i in 2:nrow(stop_times_i)) {
        # Convert previous departure time to POSIXct
        prev_dep <- as.duration(hms(stop_times_i$departure_time[i - 1]))
        #and the speed factor associated with the previous stop (within the template)
        speed_factor <- stop_times_i$speed_factor[i - 1]
        #adjust the speed based on the speed factor
        speed <- speed_i * speed_factor
        #speed in meters per second
        speed_ms <- speed * (1000 / 3600)

        dist_to_next_stop <- stop_times_i$interstop_dist[i - 1]

        current_dep_dur <- prev_dep +
          as.duration(seconds(dist_to_next_stop / speed_ms))

        current_dep_h <- as.numeric(floor(as.numeric(current_dep_dur) / 3600)) #REMOVED the %% that was here previously
        current_dep_m <- as.numeric(floor(as.numeric(current_dep_dur) / 60)) %%
          60
        current_dep_s <- round(
          as.numeric(floor(as.numeric(current_dep_dur) %% 60)),
          0
        ) #necessary to add rounding to have sprintf work

        # Convert current departure time to "hh:mm:ss" format
        stop_times_i$departure_time[i] <- sprintf(
          "%02d:%02d:%02d",
          current_dep_h,
          current_dep_m,
          current_dep_s
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

      stop_times <-
        bind_rows(
          stop_times,
          stop_times_i
        )
    }
  }

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

  return(gtfs)
}
