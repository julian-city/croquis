#' Speeds calibration engine
#'
#' Reads a reference GTFS and a target SSFS and returns that SSFS with speeds adjusted
#' to those documented in the GTFS based on shapes and stop_times data.
#'
#' @param gtfs An object of class 'gtfs'. Must include a shapes table.
#' @param ssfs A ssfs list
#' @param max_date A date within the range of gtfs$calendar$end_date representing the maximum of a 7 day range used to build the reference speed matrix from the reference GTFS. Leave as NULL to use the last 7 days of the reference GTFS
#' @param buffer_dist A distance in meters used to define the radius of interstop speed matrix points. Defaults to 10.
#' @param dist_factor A value between 0.1 and 0.9 used to ensure that interstops are only applied speeds from reference interstops of a similar length. Defaults to 0.5, which means that for a given interstop being calibrated, reference interstops with a distance of 50% to 150% can be used to calibrate.
#' @param stop_time An integer in seconds, representing the amount of time added per stop made for runtimes calculated using OSRM
#' @param osrm_speed_adj_factor A coefficient used to adjust output OSRM runtimes to make them more representative of bus runtimes.
#' @param accepted_route_types Route types that can be used to build the reference speed matrix. By default, 0 (tramways) and 3 (buses).
#'
#' @returns A ssfs list
#'
#' @export
#' @examples
#' # Calibrate Railway City Transit redesign SSFS speeds using reference GTFS
#' ssfs_calibrated <- apply_gtfs_speeds_to_ssfs(gtfs = gtfs_rct, ssfs = ssfs_rct2)
apply_gtfs_speeds_to_ssfs <- function(
  gtfs,
  ssfs,
  max_date = NULL,
  buffer_dist = 10,
  dist_factor = 0.5,
  stop_time = 10,
  osrm_speed_adj_factor = 0.72,
  accepted_route_types = c(0, 3)
) {
  #check tables in the GTFS
  gtfs_table_names <- names(gtfs)

  #identify routes with the right route type

  route_ids <-
    gtfs$routes |>
    filter(route_type %in% accepted_route_types) |>
    pull(route_id) |>
    unique()

  #identify the service ids associated with these acceptable routes

  route_service_ids <-
    gtfs$trips |>
    filter(route_id %in% route_ids) |>
    pull(service_id) |>
    unique()

  #identify relevant service_ids in the date range

  #min date one week before the max date specified in the function arguments

  if (!is.null(max_date)) {
    min_date <- max_date - days(7)
  } else {
    #max date is null, use either max of calendar_dates or calendar as max date
    if (!"calendar" %in% gtfs_table_names) {
      #use max of calendar dates
      max_date <-
        gtfs$calendar_dates |>
        filter(exception_type == 1, service_id %in% route_service_ids) |>
        summarise(max_date = max(date)) |>
        pull(max_date)
    } else {
      #gtfs uses calendar table, determine max date normally
      max_date <-
        gtfs$calendar |>
        filter(service_id %in% route_service_ids) |>
        summarise(max_date = max(end_date)) |>
        pull(max_date)
    }
    min_date <- max_date - days(7)
  }

  day <- c(
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday"
  )

  #create route_calendar (standard whether input GTFS has calendar or calendar_dates)
  if (!"calendar" %in% gtfs_table_names) {
    #calendar is not in gtfs table names, create route calendar using calendar_dates
    route_calendar <-
      gtfs$calendar_dates |>
      filter(service_id %in% route_service_ids) |>
      filter(
        date >= min_date &
          date <= max_date
      ) |>
      filter(exception_type == 1) |>
      #day of week of each date
      mutate(
        day = tolower(lubridate::wday(
          date,
          label = TRUE,
          abbr = FALSE,
          week_start = 1
        ))
      ) |>
      select(service_id, day) |>
      distinct() |>
      mutate(
        monday = if_else(day == "monday", 1, 0),
        tuesday = if_else(day == "tuesday", 1, 0),
        wednesday = if_else(day == "wednesday", 1, 0),
        thursday = if_else(day == "thursday", 1, 0),
        friday = if_else(day == "friday", 1, 0),
        saturday = if_else(day == "saturday", 1, 0),
        sunday = if_else(day == "sunday", 1, 0)
      ) |>
      group_by(service_id) |>
      summarise(
        monday = sum(monday),
        tuesday = sum(tuesday),
        wednesday = sum(wednesday),
        thursday = sum(thursday),
        friday = sum(friday),
        saturday = sum(saturday),
        sunday = sum(sunday)
      )

    #initialize start and end date

    route_calendar$start_date <- as.Date(NA)
    route_calendar$end_date <- as.Date(NA)

    #lookup start and end date for each service

    for (i in seq_len(nrow(route_calendar))) {
      service_id_i <- route_calendar$service_id[i]

      calendar_dates_i <-
        gtfs$calendar_dates |>
        filter(service_id == service_id_i)

      start_date_i <- min(calendar_dates_i$date)
      end_date_i <- max(calendar_dates_i$date)
      route_calendar$start_date[i] <- start_date_i
      route_calendar$end_date[i] <- end_date_i
    }
  } else {
    #gtfs has calendar table, create route calendar normally
    route_calendar <-
      gtfs$calendar |>
      #only include service ids associated with the route(s) of interest
      filter(service_id %in% route_service_ids) |>
      filter(
        start_date < max_date &
          end_date > min_date
      ) |>
      #filter out any services that are totally inactive
      filter(
        monday == 1 |
          tuesday == 1 |
          wednesday == 1 |
          thursday == 1 |
          friday == 1 |
          saturday == 1 |
          sunday == 1
      )
  }

  service_ids <-
    route_calendar |>
    pull(service_id) |>
    unique()

  #for every service_id in ssfs$calendar,
  #write interstops and interstop_speeds and embed into a nested table

  #necessary for the checks within the loop below
  days_of_week <- c(
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday"
  )

  #initiate the nested table

  interstop_matrices_by_service <-
    tibble(
      service_id = as.character(),
      interstops = as.list(NULL),
      interstop_speeds = as.list(NULL)
    )

  message("calculating interstop distances for GTFS")

  for (i in seq_len(nrow(ssfs$calendar))) {
    service_id_i <- ssfs$calendar[i, ]$service_id

    #FILTER TRIPS BASED ON RELEVANT DAYS OF THE WEEK
    #identify the relevant reference gtfs service_ids for the speed matrix

    ssfs_calendar_days_i <- ssfs$calendar[i, ] |>
      select(all_of(days_of_week)) |>
      unlist()

    #filtered service ids from gtfs : excluding those that ONLY have service on days of the week
    #for which the ssfs$calendar service_id being processed does NOT run service
    service_ids_i <-
      route_calendar |>
      filter(service_id %in% service_ids) |>
      filter(if_any(
        all_of(days_of_week[ssfs_calendar_days_i == 1]),
        ~ . == 1
      )) |>
      pull(service_id) |>
      unique()

    #identify the trip ids that correspond with this
    trip_ids_i <-
      gtfs$trips |>
      filter(service_id %in% service_ids_i) |>
      filter(route_id %in% route_ids) |>
      pull(trip_id) |>
      unique()

    #IDENTIFY ALL UNIQUE INTERSTOPS
    #based on origin stop, dest stop, shape_id

    stop_times <- #calculating this first as we need it for the next step
      gtfs$stop_times |>
      filter(trip_id %in% trip_ids_i) |>
      left_join(gtfs$trips |> select(trip_id, shape_id), by = "trip_id")

    interstops <-
      stop_times |>
      select(stop_id, stop_sequence, shape_id) |>
      distinct() |>
      arrange(shape_id, stop_sequence)

    shapes_points <-
      gtfs$shapes |>
      filter(shape_id %in% unique(interstops$shape_id)) |>
      as_tibble() |>
      st_as_sf(
        coords = c("shape_pt_lon", "shape_pt_lat"),
        crs = 4326
      ) |>
      arrange(shape_id, shape_pt_sequence)

    stops <-
      gtfs$stops |>
      as_tibble() |>
      select(stop_id, stop_lat, stop_lon) |>
      st_as_sf(
        coords = c("stop_lon", "stop_lat"),
        crs = 4326
      )

    #FOR ALL INTERSTOPS, CALCULATE CENTER POINT AND DISTANCE

    #initialize interstops with center points

    interstops_detailed <-
      data.frame(
        stop_id = character(),
        stop_sequence = integer(),
        shape_id = character(),
        nearest_shp_pt = numeric(), # could probably be changed to integer, if data type downstream is forced
        dist = numeric(),
        lead_stop_id = character(),
        lead_nearest_shp_pt = numeric(),
        cntr_shp_pt = numeric(),
        cntr_pt_lat = numeric(),
        cntr_pt_lon = numeric()
      )

    unique_shape_ids <-
      interstops |> pull(shape_id) |> unique()

    for (shape_id_i in unique_shape_ids) {
      interstops_i <- interstops |>
        filter(shape_id == shape_id_i) |>
        arrange(stop_sequence)

      shapes_points_i <- shapes_points |>
        filter(shape_id == shape_id_i) |>
        arrange(shape_pt_sequence)

      unique_stops_i <-
        unique(c(interstops_i$stop_id, interstops_i$lead_stop_id))

      stops_i <- stops |>
        filter(stop_id %in% unique_stops_i) |>
        left_join(
          interstops_i |> select(stop_id, stop_sequence),
          by = "stop_id"
        ) |>
        arrange(stop_sequence)

      #create indexes for nearest shape point to each stop

      nearest_shape_indexes <- st_nearest_feature(
        stops_i,
        shapes_points_i
      )

      # Fix nearest_shape_indexes to enforce ascending order (identical values in nearest_shape_indexes OK)
      # Same method used in compute_interstop_distances_for_itin()
      for (i in 2:length(nearest_shape_indexes)) {
        if (nearest_shape_indexes[i] < nearest_shape_indexes[i - 1]) {
          lower_bound <- nearest_shape_indexes[i - 1]

          # Upper bound: find next element that's already valid, or use end of shape
          upper_bound <- nrow(shapes_points_i)
          if (i < length(nearest_shape_indexes)) {
            remaining <- nearest_shape_indexes[
              (i + 1):length(nearest_shape_indexes)
            ]
            valid <- remaining[remaining >= lower_bound]
            if (length(valid) > 0) upper_bound <- valid[1]
          }

          # Re-find nearest shape point within the constrained window
          candidate_shapes <- shapes_points_i[lower_bound:upper_bound, ]
          distances <- st_distance(stops_i[i, ], candidate_shapes)
          nearest_in_window <- which.min(distances[1, ])
          nearest_shape_indexes[i] <- lower_bound + nearest_in_window - 1
        }
      }

      #calculate interstop dist, interstop cntr lat and interstop cntr lon for each

      interstop_distances <- rep(NA_real_, nrow(interstops_i))

      if (nrow(shapes_points_i) > 1) {
        segment_lengths <- as.numeric(
          st_distance(
            shapes_points_i[-nrow(shapes_points_i), , drop = FALSE],
            shapes_points_i[-1, , drop = FALSE],
            by_element = TRUE
          )
        )
        cumulative_lengths <- c(0, cumsum(segment_lengths))
      } else {
        cumulative_lengths <- 0
      }

      for (index in seq_len(nrow(interstops_i) - 1)) {
        current_index <- nearest_shape_indexes[index]
        next_index <- nearest_shape_indexes[index + 1]

        if (current_index == next_index) {
          cli::cli_warn(
            "Calculated interstop distance {itin_stop_seq$stop_id[index]} -> {itin_stop_seq$stop_id[index + 1]} (itin {itin_id}) directly between both stops."
          )
          interstop_distances[index] <- as.numeric(
            st_distance(
              stops_i[index, , drop = FALSE],
              stops_i[index + 1, , drop = FALSE]
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

      interstops_i$nearest_shp_pt <- nearest_shape_indexes

      interstops_i$dist <- interstop_distances

      interstops_i <-
        interstops_i |>
        mutate(
          lead_stop_id = lead(stop_id),
          lead_nearest_shp_pt = lead(nearest_shp_pt)
        )

      interstops_i <-
        interstops_i[1:(nrow(interstops_i) - 1), ]

      shapes_points_idx <-
        shapes_points_i |>
        mutate(
          cntr_shp_pt = row_number(),
          cntr_pt_lat = st_coordinates(geometry)[, 2],
          cntr_pt_lon = st_coordinates(geometry)[, 1]
        ) |>
        as.data.frame() |>
        select(cntr_shp_pt, cntr_pt_lat, cntr_pt_lon)

      interstops_i <-
        interstops_i |>
        mutate(
          cntr_shp_pt = nearest_shp_pt +
            ceiling((lead_nearest_shp_pt - nearest_shp_pt) / 2)
        ) |>
        left_join(shapes_points_idx, by = "cntr_shp_pt")

      interstops_detailed <-
        rbind(interstops_detailed, interstops_i)
    }

    #SIMPLIFY OUTPUTS AND INTERSTOP_ID

    #if two interstops have the same origin, destination,
    #distance and centrepoint, and the only difference
    #is shape_id, then this should be ignored
    #in order for aggregations of speeds in the subsequent function
    #can consider them the same

    #identify the interstops with variances of distance or centrepoint

    interstops_detailed <-
      interstops_detailed |>
      mutate(
        interstop_id = str_c(
          stop_id,
          "-",
          lead_stop_id,
          "-",
          shape_id
        ),
        .before = stop_id
      )

    varied_interstops <-
      interstops_detailed |>
      select(stop_id, lead_stop_id, dist, cntr_pt_lat, cntr_pt_lon) |>
      distinct() |>
      group_by(stop_id, lead_stop_id) |>
      mutate(vars = n()) |>
      filter(vars > 1) |>
      mutate(
        interstop_id_simpl = str_c(
          stop_id,
          "-",
          lead_stop_id
        )
      ) |>
      pull(interstop_id_simpl) |>
      unique()

    #this will be use to simplify the interstop_id identifier in the case where
    #the shape_id distinction is not meaningful

    #CALCULATE SPEED FOR EVERY INTERSTOP FOR EVERY TRIP

    #use the revise_stop_times() function used for gtfs_to_ssfs(),
    #which typically required itin_id but we will sub in shape_id to play the same role (maybe risky)

    #make a proxy stop_seq_proto from stop_times and interstops_detailed
    stop_seq_proto_proxy <-
      stop_times |>
      select(shape_id, stop_id, stop_sequence) |>
      distinct() |>
      arrange(shape_id, stop_sequence) |>
      left_join(
        interstops_detailed |> select(shape_id, stop_id, stop_sequence, dist),
        by = c("shape_id", "stop_id", "stop_sequence")
      ) |>
      rename(itin_id = shape_id, interstop_dist = dist)

    stop_times_revised <-
      revise_stop_times(
        stop_times = stop_times,
        trips = gtfs$trips |> mutate(itin_id = shape_id), #use shape_id to fill itin_id need
        stop_seq_proto = stop_seq_proto_proxy
      )

    interstop_times <-
      stop_times_revised |>
      rename(shape_id = itin_id) |>
      mutate(
        lead_stop_seq = lead(stop_sequence),
        lead_stop_id = lead(stop_id),
        lead_departure_time = lead(departure_time)
      ) |>
      filter(lead_stop_seq == stop_sequence + 1)

    interstop_speeds <-
      interstop_times |>
      as_tibble() |>
      mutate(
        duration_s = lead_departure_time - departure_time
      ) |>
      mutate(
        interstop_id = str_c(
          stop_id,
          "-",
          lead_stop_id,
          "-",
          shape_id
        ),
        .before = departure_time
      ) |>
      left_join(
        interstops_detailed |> select(interstop_id, dist),
        by = "interstop_id"
      ) |>
      mutate(speed = (dist / duration_s) * 3.6) |>
      mutate(
        interstop_id_simpl = str_c(
          stop_id,
          "-",
          lead_stop_id
        )
      ) |>
      mutate(
        interstop_id = if_else(
          interstop_id_simpl %in% varied_interstops,
          interstop_id,
          interstop_id_simpl
        )
      ) |>
      select(interstop_id, departure_time, speed)

    #if the interstop_id is within those that have been identified as having varying distances,
    #or centre points based on shape_id, then retain interstop_id with the shape_id code,
    #and if not then apply the simplified code containing info on only origin and dest
    #stop_code

    #SIMPLIFY INTERSTOPS FOR ONLY UNIQUE ONES

    interstops <-
      interstops_detailed |>
      as_tibble() |>
      mutate(
        interstop_id_simpl = str_c(
          stop_id,
          "-",
          lead_stop_id
        )
      ) |>
      mutate(
        interstop_id = if_else(
          interstop_id_simpl %in% varied_interstops,
          interstop_id,
          interstop_id_simpl
        )
      ) |>
      select(
        interstop_id,
        stop_id,
        lead_stop_id,
        shape_id,
        dist,
        cntr_pt_lat,
        cntr_pt_lon
      ) |>
      distinct()

    #APPEND INTERSTOP DATA TO NESTED TABLE

    interstop_matrix_by_service_i <-
      tibble(
        service_id = service_id_i,
        interstops = list(interstops),
        interstop_speeds = list(interstop_speeds)
      )

    interstop_matrices_by_service <-
      bind_rows(
        interstop_matrices_by_service,
        interstop_matrix_by_service_i,
      )
  }

  #Create interstop points (basis for the speed matrix)

  interstop_points <-
    tibble(
      interstop_id = as.character(),
      geometry = st_sfc(NA, crs = 4326)
    )

  for (i in seq_len(nrow(interstop_matrices_by_service))) {
    interstop_points_i <-
      interstop_matrices_by_service$interstops[[i]] |>
      st_as_sf(coords = c("cntr_pt_lon", "cntr_pt_lat"), crs = 4326) |>
      select(interstop_id, geometry) |>
      as_tibble()

    interstop_points <-
      bind_rows(
        interstop_points,
        interstop_points_i
      )
  }

  interstop_points <-
    interstop_points |>
    distinct() |>
    st_as_sf()

  message("Generating interstop point buffers")

  interstop_buffers <- st_buffer(interstop_points, dist = buffer_dist)

  #takes 1 minute 12 seconds for stm gtfs interstops

  #   #   #
  #
  #SSFS INTERSTOPS
  #
  #   #   #

  #add lead stop id to ssfs$stop_seq

  stop_seq_interstops <-
    ssfs$stop_seq |>
    mutate(
      lead_stop_id = if_else(
        stop_sequence == lead(stop_sequence) - 1,
        lead(stop_id),
        NA
      )
    )

  #initialize ssfs_interstops

  ssfs_interstops <-
    stop_seq_interstops |>
    select(stop_id, lead_stop_id, stop_sequence, itin_id) |>
    filter(!is.na(lead_stop_id)) |>
    distinct()

  # SSFS INTERSTOP DISTANCES
  # reuse compute_interstop_distances_for_itin() — same helper used in
  # gtfs_to_ssfs() and ssfs_to_gtfs()

  # prepare shapes_points in the format expected by the helper
  shapes_points <-
    ssfs$itin |>
    select(itin_id, geometry) |>
    st_cast("POINT") |>
    distinct() |>
    group_by(itin_id) |>
    mutate(shape_pt_sequence = row_number(), .before = geometry) |>
    ungroup()

  # prepare stop_seq_proto with stop_seq_id (mirroring ssfs_to_gtfs pattern)
  ssfs_stop_seq <-
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

  cli::cli_alert_info(
    "Computing SSFS interstop distances ({nrow(ssfs_interstops)} interstops across {length(unique(ssfs_interstops$itin_id))} itineraries)"
  )

  ssfs_interstop_distances <- lapply(
    unique(ssfs_stop_seq$itin_id),
    function(itin_id_i) {
      compute_interstop_distances_for_itin(
        itin_id = itin_id_i,
        stop_seq_proto = ssfs_stop_seq,
        shapes_points = shapes_points,
        stops = ssfs$stops
      )
    }
  ) |>
    bind_rows()

  # join distances to ssfs_interstops via stop_seq_id
  ssfs_interstops <-
    ssfs_interstops |>
    mutate(
      stop_seq_id = str_c(
        itin_id,
        "_",
        as.character(stop_id),
        "_",
        as.character(stop_sequence)
      )
    ) |>
    left_join(ssfs_interstop_distances, by = "stop_seq_id") |>
    mutate(dist = round(interstop_dist, 0)) |>
    select(-interstop_dist, -stop_seq_id)

  # SSFS INTERSTOP GEOMETRIES
  # build LINESTRING paths per itin_id for downstream st_join with GTFS buffers
  # nearest-feature indexes are computed once per itin (not per row)

  cli::cli_alert_info("Building SSFS interstop path geometries")

  ssfs_interstop_geom_list <- lapply(
    unique(ssfs_interstops$itin_id),
    function(itin_id_i) {
      interstops_i <- ssfs_interstops[
        ssfs_interstops$itin_id == itin_id_i,
        ,
        drop = FALSE
      ]
      shapes_pts_i <- shapes_points[
        shapes_points$itin_id == itin_id_i,
        ,
        drop = FALSE
      ]

      n_shape_pts <- nrow(shapes_pts_i)

      if (n_shape_pts == 0) {
        return(tibble(
          stop_id = interstops_i$stop_id,
          lead_stop_id = interstops_i$lead_stop_id,
          stop_sequence = interstops_i$stop_sequence,
          itin_id = itin_id_i,
          geometry = st_sfc(
            rep(list(st_linestring()), nrow(interstops_i)),
            crs = 4326
          )
        ))
      }

      # reconstruct traversal order of unique stops from interstop pairs
      all_pair_stops <- c(rbind(
        interstops_i$stop_id,
        interstops_i$lead_stop_id
      ))
      ordered_unique_stops <- unique(all_pair_stops)

      stops_ordered <- ssfs$stops[
        match(ordered_unique_stops, ssfs$stops$stop_id),
        ,
        drop = FALSE
      ]

      # one st_nearest_feature call for all stops on this itin
      nearest_shape_indexes <- st_nearest_feature(stops_ordered, shapes_pts_i)

      # fix nearest_shape_indexes to ensure strictly ascending order
      # (identical values OK)
      if (length(nearest_shape_indexes) > 1) {
        for (k in 2:length(nearest_shape_indexes)) {
          if (nearest_shape_indexes[k] < nearest_shape_indexes[k - 1]) {
            lower_bound <- nearest_shape_indexes[k - 1]

            upper_bound <- n_shape_pts
            if (k < length(nearest_shape_indexes)) {
              remaining <- nearest_shape_indexes[
                (k + 1):length(nearest_shape_indexes)
              ]
              valid <- remaining[remaining >= lower_bound]
              if (length(valid) > 0) upper_bound <- valid[1]
            }

            candidate_shapes <- shapes_pts_i[lower_bound:upper_bound, ]
            distances <- st_distance(stops_ordered[k, ], candidate_shapes)
            nearest_in_window <- which.min(distances[1, ])
            nearest_shape_indexes[k] <- lower_bound + nearest_in_window - 1
          }
        }
      }

      nearest_idx <- setNames(nearest_shape_indexes, ordered_unique_stops)

      # pre-extract coordinates for LINESTRING construction
      shape_coords <- st_coordinates(shapes_pts_i)

      # build a LINESTRING for each interstop pair
      geom_list <- vector("list", nrow(interstops_i))

      for (j in seq_len(nrow(interstops_i))) {
        idx_a <- nearest_idx[interstops_i$stop_id[j]]
        idx_b <- nearest_idx[interstops_i$lead_stop_id[j]]

        lo <- min(idx_a, idx_b)
        hi <- max(idx_a, idx_b)

        if (lo == hi) {
          # both stops snap to the same shape point — build a line from
          # that point to the next (or previous) to create a valid LINESTRING
          if (lo < n_shape_pts) {
            geom_list[[j]] <- st_linestring(shape_coords[lo:(lo + 1), 1:2])
          } else {
            geom_list[[j]] <- st_linestring(shape_coords[(lo - 1):lo, 1:2])
          }
        } else {
          geom_list[[j]] <- st_linestring(shape_coords[lo:hi, 1:2])
        }
      }

      tibble(
        stop_id = interstops_i$stop_id,
        lead_stop_id = interstops_i$lead_stop_id,
        stop_sequence = interstops_i$stop_sequence,
        itin_id = itin_id_i,
        geometry = st_sfc(geom_list, crs = 4326)
      )
    }
  ) |>
    bind_rows()

  # join geometries back to ssfs_interstops
  ssfs_interstops <-
    ssfs_interstops |>
    left_join(
      ssfs_interstop_geom_list |>
        select(stop_id, lead_stop_id, stop_sequence, itin_id, geometry),
      by = c("stop_id", "lead_stop_id", "stop_sequence", "itin_id")
    ) |>
    st_as_sf(crs = 4326)

  #     #     #
  #
  #SSFS INTERSTOP SPEEDS
  #
  #     #     #

  unique_service_ids <- interstop_matrices_by_service$service_id |> unique()
  #this should be identical to the unique_service_ids in ssfs$calendar,
  #might be more straightforward to derive these from there

  ssfs_interstop_speeds <-
    tibble(
      stop_id = as.character(),
      lead_stop_id = as.character(),
      itin_id = as.character(),
      stop_sequence = as.integer(),
      dist = as.double(),
      service_id = as.character(),
      hour_dep = as.character(),
      speed = as.double(),
      method = as.character()
    )

  for (service_id_i in unique_service_ids) {
    interstops_and_speeds_i <-
      interstop_matrices_by_service |>
      filter(service_id == service_id_i)

    #interstops for the service_id

    interstops_i <-
      interstops_and_speeds_i |>
      pull(interstops)

    interstops_i <- interstops_i[[1]]

    #interstop speeds for the service id

    interstop_speeds_i <-
      interstops_and_speeds_i |>
      pull(interstop_speeds)

    interstop_speeds_i <- interstop_speeds_i[[1]]

    #how much time does it take to run a st_join of all the ssfs interstop paths
    #with all the gtfs interstop_buffers ?

    interstop_ids_i <- interstops_i |> pull(interstop_id)

    interstop_buffers_i <-
      interstop_buffers |>
      filter(interstop_id %in% interstop_ids_i)

    itin_ids_i <-
      ssfs$hsh |>
      filter(service_id %in% service_id_i) |>
      pull(itin_id) |>
      unique()

    message(paste(
      "\rIntersecting ssfs interstops with gtfs matrix for service_id",
      service_id_i
    ))

    ssfs_interstops_j <-
      ssfs_interstops |>
      filter(itin_id %in% itin_ids_i) |>
      st_join(interstop_buffers_i) |>
      as_tibble() |>
      select(-geometry)

    #summarise by same information in initial ssfs_interstops

    ssfs_interstops_j <-
      ssfs_interstops_j |>
      group_by(stop_id, lead_stop_id, stop_sequence, itin_id, dist) |>
      summarise(interstop_ids = list(interstop_id)) |>
      ungroup()
    #this is now the table that will be referenced in method b to identify
    #which gtfs interstop ids intersect with which ssfs interstop path

    #for every interstop in the ssfs, identify at which hours it would be realized.
    #use itin_id to accomplish this.
    #first, write hours by itin_id for the relevant service_id

    hours_by_itin_id_i <-
      ssfs$hsh |>
      filter(service_id == service_id_i) |>
      select(itin_id, hour_dep) |>
      group_by(itin_id) |>
      summarise(hour_dep = list(hour_dep))

    #then, join the list of hours to the ssfs_interstops and unnest

    ssfs_interstops_h <-
      ssfs_interstops |>
      left_join(hours_by_itin_id_i, by = "itin_id") |>
      #the next line removes any row where hour_dep is NULL
      #due to the itin_id not being active at the service_id_i
      unnest(hour_dep)

    ssfs_interstops_h$service_id <- service_id_i

    ssfs_interstops_h$speed <- NA_real_

    ssfs_interstops_h$method <- "NA"

    #now, launch the algorithm

    # initialize progress bar
    cli::cli_progress_bar(
      total = nrow(ssfs_interstops_h),
      format = "Calculating interstop speeds (service_id {service_id_i}) {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      clear = FALSE
    )

    for (i in seq_len(nrow(ssfs_interstops_h))) {
      cli::cli_progress_update()

      stop_a <- ssfs_interstops_h[i, ]$stop_id

      stop_b <- ssfs_interstops_h[i, ]$lead_stop_id

      hour_dep_i <- ssfs_interstops_h[i, ]$hour_dep
      #with the time window upgrade, incorporate

      #condition A : if there is an identical pair of stops to stop_a, stop_b, then
      #use the speeds for trips at this hour
      #there could be multiple interstop_ids that correspond
      #for example in the case where there are distinct shape_ids in the gtfs
      #that generated the speed matrix

      interstop_ids_i <-
        interstops_i |>
        filter(
          stop_id == stop_a &
            lead_stop_id == stop_b
        ) |>
        pull(interstop_id)

      interstop_speeds_i_h <-
        interstop_speeds_i |>
        filter(interstop_id %in% interstop_ids_i) |>
        rename(departure_time_s = departure_time) |>
        mutate(
          hour_dep = sprintf(
            "%02d:00:00",
            as.numeric(floor(departure_time_s / 3600))
          )
        ) |>
        filter(hour_dep == hour_dep_i)

      #IF THERE ARE NO IDENTICAL INTERSTOPS IN THE REFERENCE GTFS, THEN METHOD B
      if (nrow(interstop_speeds_i_h) == 0) {
        #NEW METHOD B : using ssfs_interstops_j, identify :
        #(1) if there was an intersecting gtfs interstop found and listed in this table
        #(2) if yes, which of those reference interstops are of similar distance AND have listed speeds at that hour

        #first, were any intersecting gtfs interstops found ?
        interstop_ids_b <- ssfs_interstops_j |>
          filter(stop_id == stop_a, lead_stop_id == stop_b) |>
          unnest(interstop_ids) |>
          pull(interstop_ids)

        #if the length of the above is 0 (no results found), then move to method C, otherwise
        #continue method B

        if (length(interstop_ids_b) != 0) {
          dist_i <- ssfs_interstops_h[i, ]$dist

          #among the interstop ids that intersect with the ssfs_interstop path,
          #which have a similar distance ?
          interstop_ids_b <-
            interstops_i |>
            filter(
              interstop_id %in% interstop_ids_b,
              dist <= ((1 + dist_factor) * dist_i) &
                dist > ((1 - dist_factor) * dist_i)
            ) |>
            pull(interstop_id)

          #if the length of the above is 0 (no results found), then move to method C, otherwise
          #continue method B

          if (length(interstop_ids_b) != 0) {
            #of the remaining reference gtfs interstops that intersect with the ssfs_interstop path,
            #which recorded speeds at the hour being calculated ?

            #useful further below if method B is used
            interstop_speeds_i_h_d <-
              interstop_speeds_i |>
              filter(interstop_id %in% interstop_ids_b) |>
              rename(departure_time_s = departure_time) |>
              mutate(
                hour_dep = sprintf(
                  "%02d:00:00",
                  as.numeric(floor(departure_time_s / 3600))
                )
              ) |>
              filter(hour_dep == hour_dep_i)

            #interstops that meet this condition : intersects, similar distance, & speed observed at hour i
            interstop_ids_b <-
              interstop_speeds_i_h_d |>
              pull(interstop_id) |>
              unique()
          }
        }

        #IF THERE ARE NO SPATIAL MATCHES, THEN METHOD C
        if (length(interstop_ids_b) == 0) {
          #METHOD C : calculate speed based on osrm

          stop_a_geom <-
            ssfs$stops |>
            filter(stop_id == stop_a)

          stop_b_geom <-
            ssfs$stops |>
            filter(stop_id == stop_b)

          is_an_error <- FALSE
          tryCatch(
            {
              osrm_result_i <- osrmRoute(
                src = stop_a_geom,
                dst = stop_b_geom,
                overview = FALSE
              )
            },
            error = function(e) {
              is_an_error <<- TRUE
            }
          )

          if (is_an_error) {
            #FALLBACK IF OSRM FAILS, FOR INTERNET REASONS FOR EXAMPLE
            #METHOD D : apply interstop speed based on what is already described in the ssfs

            itin_id_i <- ssfs_interstops_h[i, ]$itin_id

            speed_i <-
              ssfs$hsh |>
              filter(
                service_id == service_id_i,
                itin_id == itin_id_i,
                hour_dep == hour_dep_i
              ) |>
              pull(speed)

            #might be useful to add a warning message here if speed_i is longer than 1

            speed_i <- speed_i[1] #not sure if this is necessary but just in case....

            speed_factor_i <-
              stop_seq_interstops |>
              filter(
                itin_id == itin_id_i,
                stop_id == stop_a,
                lead_stop_id == stop_b
              ) |>
              pull(speed_factor)

            #might be useful to add a warning message here if speed_i is longer than 1

            speed_factor_i <- speed_factor_i[1] #not sure if this is necessary but just in case....

            speed_i <- speed_i * speed_factor_i

            ssfs_interstops_h[i, ]$speed <- speed_i

            ssfs_interstops_h[i, ]$method <- "D"
          } else {
            #METHOD C

            #adjust the OSRM output by the speed adjustment factor specified in the function arguments
            duration_i_mins <- round(
              unname(osrm_result_i["duration"]) * osrm_speed_adj_factor,
              1
            )

            #add bus stop time to distance to
            duration_i_mins <- duration_i_mins + (stop_time / 60)

            distance_i_kms <- unname(osrm_result_i["distance"])

            speed_i <- round(distance_i_kms / (duration_i_mins / 60), 1)

            ssfs_interstops_h[i, ]$speed <- speed_i

            ssfs_interstops_h[i, ]$method <- "C"

            #write a final else for the cases that aren't caught....?
          }
        } else {
          #METHOD B

          speed_i <-
            interstop_speeds_i_h_d |>
            filter(interstop_id %in% interstop_ids_b) |>
            pull(speed) |>
            mean() |>
            round(digits = 1)

          ssfs_interstops_h[i, ]$speed <- speed_i

          ssfs_interstops_h[i, ]$method <- "B"
        }
      } else {
        #method A :

        #this section would be written differently and more easily with a time window parameter
        speed_i <-
          interstop_speeds_i_h |>
          pull(speed) |>
          mean() |>
          round(digits = 1)

        ssfs_interstops_h[i, ]$speed <- speed_i

        ssfs_interstops_h[i, ]$method <- "A"
      }
    }

    #compile information on interstop speeds by time period into ssfs_interstop_speeds

    ssfs_interstops_h_i <-
      ssfs_interstops_h |>
      as_tibble() |>
      select(
        stop_id,
        lead_stop_id,
        itin_id,
        stop_sequence,
        dist,
        service_id,
        hour_dep,
        speed,
        method
      )

    ssfs_interstop_speeds <-
      bind_rows(
        ssfs_interstop_speeds,
        ssfs_interstops_h_i
      )
  }

  print(
    ssfs_interstop_speeds |>
      select(method) |>
      group_by(method) |>
      summarise(n = n()) |>
      mutate(perc = round((n / sum(n)) * 100, 1)) |>
      select(-n)
  )

  #   #   #
  #
  #OVERWRITE SSFS$HSH AND SSFS$STOP_SEQ WITH SSFS_INTERSTOP_SPEEDS
  #
  #   #   #

  #overwrite speeds in ssfs$hsh$speed

  ssfs_hsh_new_speeds <-
    ssfs_interstop_speeds |>
    mutate(speed_ms = speed * (1000 / 3600)) |>
    mutate(duration_s = dist / speed_ms) |>
    group_by(itin_id, service_id, hour_dep) |>
    summarise(total_dist = sum(dist), total_duration = sum(duration_s)) |>
    #write speeds in km / h
    mutate(
      speed_overwrite = round((total_dist / total_duration) * (3600 / 1000), 1)
    ) |>
    select(-c(total_dist, total_duration))

  ssfs$hsh <-
    ssfs$hsh |>
    left_join(
      ssfs_hsh_new_speeds,
      by = c("itin_id", "service_id", "hour_dep")
    ) |>
    select(-speed) |>
    rename(speed = speed_overwrite)

  #overwrite ssfs$stop_seq$speed_factor

  #first, calculate average speed per itin_id
  #we can use the revised hsh

  #with converting to periods, this will need to be modified
  #such that the numerator for n_trips varies based on the duration of each time window
  #instead of being a constant 60 minutes

  speed_by_itin_id <-
    ssfs$hsh |>
    mutate(n_trips = 60 / headway) |>
    mutate(n_trips = if_else(is.na(n_trips), 1, n_trips)) |>
    mutate(speed_total = speed * n_trips) |>
    group_by(itin_id) |>
    summarise(speed_total = sum(speed_total), sum_trips = sum(n_trips)) |>
    mutate(speed_avg = speed_total / sum_trips) |>
    select(-c(speed_total, sum_trips))

  ssfs_stop_seq_new_speed_factor <-
    ssfs_interstop_speeds |>
    #join number of trips by hour, itin_id and service_id
    #same method as above
    left_join(
      ssfs$hsh |>
        mutate(n_trips = 60 / headway) |>
        select(itin_id, service_id, hour_dep, n_trips),
      by = c("itin_id", "service_id", "hour_dep")
    ) |>
    mutate(n_trips = if_else(is.na(n_trips), 1, n_trips)) |>
    mutate(speed_total = speed * n_trips) |>
    group_by(stop_id, stop_sequence, itin_id) |>
    summarise(speed_total = sum(speed_total), sum_trips = sum(n_trips)) |>
    mutate(speed_avg_interstop = speed_total / sum_trips) |>
    select(itin_id, stop_id, stop_sequence, speed_avg_interstop) |>
    ungroup() |>
    left_join(speed_by_itin_id, by = "itin_id") |>
    mutate(
      speed_factor_overwrite = round(speed_avg_interstop / speed_avg, 1)
    ) |>
    select(-c(speed_avg_interstop, speed_avg))

  #TO DEAL WITH SAME STOP (INTERSTOP) TWICE IN SAME ITIN_ID, it might be necessary
  #to import and retain stop_sequence attribute in ssfs_interstop_speeds
  #and use that to facilitate a join

  ssfs$stop_seq <-
    ssfs$stop_seq |>
    left_join(
      ssfs_stop_seq_new_speed_factor,
      by = c("itin_id", "stop_id", "stop_sequence")
    ) |>
    select(-speed_factor) |>
    rename(speed_factor = speed_factor_overwrite)

  ssfs
}
