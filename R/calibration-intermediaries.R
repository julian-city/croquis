#' Turn GTFS into interstop speed matrix
#'
#' @param gtfs An object with class 'gtfs'
#' @param ssfs A ssfs list
#' @param max_date A date representing the maximum date range of services retained from the reference GTFS
#'
#' @returns An interstop speed matrix table
#'
#' @export
#' @examples
gtfs_to_interstop_matrix <- function(
  gtfs,
  ssfs,
  max_date
) {
  #identify relevant service_ids in the date range

  #min date one week before the max date specified in the function arguments
  min_date <- max_date - days(7)

  service_ids <-
    gtfs$calendar %>%
    filter(
      start_date <= min_date &
        end_date >= max_date
    ) %>%
    pull(service_id) %>%
    unique()

  #identify routes with the right route type (buses only, route type 3)

  route_ids <-
    gtfs$routes %>% filter(route_type == 3) %>% pull(route_id) %>% unique()

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

  for (i in c(1:nrow(ssfs$calendar))) {
    service_id_i <- ssfs$calendar[i, ]$service_id

    #FILTER TRIPS BASED ON RELEVANT DAYS OF THE WEEK
    #identify the relevant reference gtfs service_ids for the speed matrix

    #from chatgpt
    ssfs_calendar_days_i <- ssfs$calendar[i, ] %>%
      select(all_of(days_of_week)) %>%
      unlist()

    #filtered service ids from gtfs : excluding those that ONLY have service on days of the week
    #for which the ssfs$calendar service_id being processed does NOT run service
    #from chatgpt
    service_ids_i <-
      gtfs$calendar %>%
      filter(service_id %in% service_ids) %>%
      filter(
        apply(select(., all_of(days_of_week)), 1, function(row) {
          any(row == 1 & ssfs_calendar_days_i == 1)
        })
      ) %>%
      pull(service_id) %>%
      unique()

    #identify the trip ids that correspond with this
    trip_ids_i <-
      gtfs$trips %>%
      filter(service_id %in% service_ids_i) %>%
      filter(route_id %in% route_ids) %>%
      pull(trip_id) %>%
      unique()

    #IDENTIFY ALL UNIQUE INTERSTOPS
    #based on origin stop, dest stop, shape_id

    interstop_times <- #calculating this first as we need it for the next step
      gtfs$stop_times %>%
      filter(trip_id %in% trip_ids_i) %>%
      left_join(gtfs$trips %>% select(trip_id, shape_id), by = "trip_id") %>%
      mutate(
        lead_stop_seq = lead(stop_sequence),
        lead_stop_id = lead(stop_id),
        lead_arrival_time = lead(arrival_time)
      ) %>%
      filter(lead_stop_seq == stop_sequence + 1)

    interstops <-
      interstop_times %>%
      select(stop_id, lead_stop_id, shape_id) %>%
      distinct() %>%
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

    shapes_points <-
      gtfs$shapes %>%
      filter(shape_id %in% unique(interstops$shape_id)) %>%
      as_tibble() %>%
      st_as_sf(
        coords = c("shape_pt_lon", "shape_pt_lat"),
        crs = 4269
      ) %>%
      arrange(shape_id, shape_pt_sequence)

    stops <-
      gtfs$stops %>%
      as_tibble() %>%
      select(stop_id, stop_lat, stop_lon) %>%
      st_as_sf(
        coords = c("stop_lon", "stop_lat"),
        crs = 4269
      )

    #FOR ALL INTERSTOPS, CALCULATE CENTER POINT AND DISTANCE

    #initialize

    interstops$dist <- NA
    interstops$cntr_pt_lat <- NA
    interstops$cntr_pt_lon <- NA

    cli::cli_progress_bar(
      total = nrow(interstops),
      format = "Producing interstop matrix points for {service_id_i} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}",
      clear = FALSE
    )

    for (i in c(1:nrow(interstops))) {
      cli::cli_progress_update()

      shape_id_i <- interstops$shape_id[i]

      stop_id_a <- interstops$stop_id[i]
      stop_id_b <- interstops$lead_stop_id[i]

      shapes_points_i <-
        shapes_points %>%
        filter(shape_id == shape_id_i)

      stop_a <-
        stops %>%
        filter(stop_id == stop_id_a)

      stop_b <-
        stops %>%
        filter(stop_id == stop_id_b)

      interstop_segment_points <-
        shapes_points_i[
          st_nearest_feature(stop_a, shapes_points_i):st_nearest_feature(
            stop_b,
            shapes_points_i
          ),
        ]

      #calculate distance (to the closest meter)

      distance <-
        interstop_segment_points %>%
        summarise(do_union = FALSE) %>%
        st_cast("LINESTRING") %>%
        st_length() %>%
        as.numeric() %>%
        round()

      interstops$dist[i] <- distance

      #it is necessary to take the middle point along the interstop_segment_points
      #taking st_centroid does not work for example

      cntr_point_coords <-
        interstop_segment_points[
          ceiling(nrow(interstop_segment_points) / 2),
        ] %>%
        #ceiling is the best possible function : returns something useful and representative
        #in every case : if nrow = 1, then it returns 1. For nrow = 5, it will return 3,
        #which would indicate the exact middle of the route segment
        #as opposed to round() which would return 2 for 5 due to the parameters of the
        #round function in base R.
        mutate(
          cntr_pt_lat = st_coordinates(geometry)[, 2],
          cntr_pt_lon = st_coordinates(geometry)[, 1]
        ) %>%
        as_tibble() %>%
        select(cntr_pt_lat, cntr_pt_lon)

      interstops$cntr_pt_lat[i] <- cntr_point_coords$cntr_pt_lat[1]

      interstops$cntr_pt_lon[i] <- cntr_point_coords$cntr_pt_lon[1]
    }

    #SIMPLIFY OUTPUTS AND INTERSTOP_ID

    #if two interstops have the same origin, destination,
    #distance and centrepoint, and the only difference
    #is shape_id, then this should be ignored
    #in order for aggregations of speeds in the subsequent function
    #can consider them the same

    #identify the interstops with variances of distance or centrepoint

    varied_interstops <-
      interstops %>%
      select(stop_id, lead_stop_id, dist, cntr_pt_lat, cntr_pt_lon) %>%
      distinct() %>%
      group_by(stop_id, lead_stop_id) %>%
      mutate(vars = n()) %>%
      filter(vars > 1) %>%
      mutate(
        interstop_id_simpl = str_c(
          stop_id,
          "-",
          lead_stop_id
        )
      ) %>%
      pull(interstop_id_simpl) %>%
      unique()

    #this will be use to simplify the interstop_id identifier in the case where
    #the shape_id distinction is not meaningful

    #CALCULATE SPEED FOR EVERY INTERSTOP FOR EVERY TRIP

    interstop_speeds <-
      interstop_times %>%
      as_tibble() %>%
      mutate(
        duration_s = as.numeric(
          as.duration(hms(lead_arrival_time)) -
            as.duration(hms(departure_time))
        )
      ) %>%
      mutate(
        interstop_id = str_c(
          stop_id,
          "-",
          lead_stop_id,
          "-",
          shape_id
        ),
        .before = departure_time
      ) %>%
      left_join(
        interstops %>% select(interstop_id, dist),
        by = "interstop_id"
      ) %>%
      mutate(speed = (dist / duration_s) * 3.6) %>%
      mutate(
        interstop_id_simpl = str_c(
          stop_id,
          "-",
          lead_stop_id
        )
      ) %>%
      mutate(
        interstop_id = if_else(
          interstop_id_simpl %in% varied_interstops,
          interstop_id,
          interstop_id_simpl
        )
      ) %>%
      select(interstop_id, departure_time, speed)

    #if the interstop_id is within those that have been identified as having varying distances,
    #or centre points based on shape_id, then retain interstop_id with the shape_id code,
    #and if not the apply the simplified code containing info on only origin and dest
    #stop_code

    #SIMPLIFY INTERSTOPS FOR ONLY UNIQUE ONES

    interstops <-
      interstops %>%
      as_tibble() %>%
      mutate(
        interstop_id_simpl = str_c(
          stop_id,
          "-",
          lead_stop_id
        )
      ) %>%
      mutate(
        interstop_id = if_else(
          interstop_id_simpl %in% varied_interstops,
          interstop_id,
          interstop_id_simpl
        )
      ) %>%
      select(-c(shape_id, interstop_id_simpl)) %>%
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

  return(interstop_matrices_by_service)
}

#' Apply interstop matrixx to ssfs
#'
#' @param ssfs A ssfs list
#' @param interstop_matrices_by_service An interstop speed matrix table
#' @param buffer_dist A distance in meters used to define the radius of interstop speed matrix points. Defaults to 10.
#' @param dist_factor A value between 0.1 and 0.9 used to ensure that interstops are only applied speeds from reference interstops of a similar length. Defaults to 0.5, which means that for a given interstop being calibrated, reference interstops with a distance of 50% to 150% can be used to calibrate.
#' @param stop_time An integer in seconds, representing the amount of time added per stop made for runtimes calculated using OSRM
#' @param osrm_speed_adj_factor A coefficient used to adjust output OSRM runtimes to make them more representative of bus runtimes.
#'
#' @returns A ssfs list
#'
#' @export
#' @examples
apply_interstop_matrix_to_ssfs <- function(
  ssfs,
  interstop_matrices_by_service,
  buffer_dist = 10,
  dist_factor = 0.5,
  stop_time = 10,
  osrm_speed_adj_factor = 0.72
) {
  interstop_points <-
    tibble(
      interstop_id = as.character(),
      geometry = st_sfc(NA, crs = 4269)
    )

  for (i in 1:nrow(interstop_matrices_by_service)) {
    interstop_points_i <-
      interstop_matrices_by_service$interstops[[i]] %>%
      st_as_sf(coords = c("cntr_pt_lon", "cntr_pt_lat"), crs = 4269) %>%
      select(interstop_id, geometry) %>%
      as_tibble()

    interstop_points <-
      bind_rows(
        interstop_points,
        interstop_points_i
      )
  }

  interstop_points <-
    interstop_points %>%
    distinct() %>%
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
    ssfs$stop_seq %>%
    mutate(
      lead_stop_id = if_else(
        stop_sequence == lead(stop_sequence) - 1,
        lead(stop_id),
        NA
      )
    )

  #initialize ssfs_interstops

  ssfs_interstops <-
    stop_seq_interstops %>%
    select(stop_id, lead_stop_id, stop_sequence, itin_id) %>%
    filter(!is.na(lead_stop_id)) |>
    distinct()

  #initialize vectors to fill

  ssfs_interstops$dist <- NA_real_

  ssfs_interstops$geometry <- st_sfc(NA)

  shapes_points <-
    ssfs$itin %>% select(itin_id, geometry) |> st_cast("POINT")

  cli::cli_progress_bar(
    "Writing ssfs interstops",
    total = nrow(ssfs$span)
  )

  for (i in 1:nrow(ssfs_interstops)) {
    cli::cli_progress_update()

    stop_id_a <-
      ssfs_interstops$stop_id[i]

    stop_id_b <-
      ssfs_interstops$lead_stop_id[i]

    stop_a <-
      ssfs$stops %>%
      filter(stop_id == stop_id_a)

    stop_b <-
      ssfs$stops %>%
      filter(stop_id == stop_id_b)

    shape_id_i <-
      ssfs_interstops$itin_id[i]

    shapes_points_i <-
      shapes_points %>%
      filter(itin_id == shape_id_i)

    interstop_segment_points <-
      shapes_points_i[
        st_nearest_feature(stop_a, shapes_points_i):st_nearest_feature(
          stop_b,
          shapes_points_i
        ),
      ]

    path_i <-
      interstop_segment_points %>%
      group_by(itin_id) %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING") %>%
      as_tibble()

    ssfs_interstops$dist[i] <- round(
      as.numeric(st_length(path_i$geometry[1])),
      0
    )

    ssfs_interstops$geometry[i] <- path_i$geometry[1]
  }

  ssfs_interstops <-
    st_as_sf(ssfs_interstops, crs = 4269)

  #     #     #
  #
  #SSFS INTERSTOP SPEEDS
  #
  #     #     #

  unique_service_ids <- interstop_matrices_by_service$service_id %>% unique()
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
      interstop_matrices_by_service %>%
      filter(service_id == service_id_i)

    #interstops for the service_id

    interstops_i <-
      interstops_and_speeds_i %>%
      pull(interstops)

    interstops_i <- interstops_i[[1]]

    #interstop speeds for the service id

    interstop_speeds_i <-
      interstops_and_speeds_i %>%
      pull(interstop_speeds)

    interstop_speeds_i <- interstop_speeds_i[[1]]

    #how much time does it take to run a st_join of all the ssfs interstop paths
    #with all the gtfs interstop_buffers ?

    interstop_ids_i <- interstops_i %>% pull(interstop_id)

    interstop_buffers_i <-
      interstop_buffers %>%
      filter(interstop_id %in% interstop_ids_i)

    itin_ids_i <-
      ssfs$hsh %>%
      filter(service_id %in% service_id_i) %>%
      pull(itin_id) %>%
      unique()

    message(paste(
      "\rIntersecting ssfs interstops with gtfs matrix for service_id",
      service_id_i
    ))

    ssfs_interstops_j <-
      ssfs_interstops %>%
      filter(itin_id %in% itin_ids_i) %>%
      st_join(interstop_buffers_i) %>%
      as_tibble() %>%
      select(-geometry)

    #summarise by same information in initial ssfs_interstops

    ssfs_interstops_j <-
      ssfs_interstops_j %>%
      group_by(stop_id, lead_stop_id, stop_sequence, itin_id, dist) %>%
      summarise(interstop_ids = list(interstop_id)) %>%
      ungroup()
    #this is now the table that will be referenced in method b to identify
    #which gtfs interstop ids intersect with which ssfs interstop path

    #for every interstop in the ssfs, identify at which hours it would be realized.
    #use itin_id to accomplish this.
    #first, write hours by itin_id for the relevant service_id

    hours_by_itin_id_i <-
      ssfs$hsh %>%
      filter(service_id == service_id_i) %>%
      select(itin_id, hour_dep) %>%
      group_by(itin_id) %>%
      summarise(hour_dep = list(hour_dep))

    #then, join the list of hours to the ssfs_interstops and unnest

    ssfs_interstops_h <-
      ssfs_interstops %>%
      left_join(hours_by_itin_id_i, by = "itin_id") %>%
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

    for (i in c(1:nrow(ssfs_interstops_h))) {
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
        interstops_i %>%
        filter(
          stop_id == stop_a &
            lead_stop_id == stop_b
        ) %>%
        pull(interstop_id)

      interstop_speeds_i_h <-
        interstop_speeds_i %>%
        filter(interstop_id %in% interstop_ids_i) %>%
        mutate(
          departure_time_s = as.numeric(as.duration(hms(departure_time)))
        ) %>% #converts departure time to duration in seconds
        mutate(
          hour_dep = sprintf(
            "%02d:00:00",
            as.numeric(floor(departure_time_s / 3600))
          )
        ) %>%
        filter(hour_dep == hour_dep_i)

      #IF THERE ARE NO IDENTICAL INTERSTOPS IN THE REFERENCE GTFS, THEN METHOD B
      if (nrow(interstop_speeds_i_h) == 0) {
        #NEW METHOD B : using ssfs_interstops_j, identify :
        #(1) if there was an intersecting gtfs interstop found and listed in this table
        #(2) if yes, which of those reference interstops are of similar distance AND have listed speeds at that hour

        #first, were any intersecting gtfs interstops found ?
        interstop_ids_b <- ssfs_interstops_j %>%
          filter(stop_id == stop_a, lead_stop_id == stop_b) %>%
          unnest(interstop_ids) %>%
          pull(interstop_ids)

        #if the length of the above is 0 (no results found), then move to method C, otherwise
        #continue method B

        if (length(interstop_ids_b) != 0) {
          dist_i <- ssfs_interstops_h[i, ]$dist

          #among the interstop ids that intersect with the ssfs_interstop path,
          #which have a similar distance ?
          interstop_ids_b <-
            interstops_i %>%
            filter(
              interstop_id %in% interstop_ids_b,
              dist <= ((1 + dist_factor) * dist_i) &
                dist > ((1 - dist_factor) * dist_i)
            ) %>%
            pull(interstop_id)

          #if the length of the above is 0 (no results found), then move to method C, otherwise
          #continue method B

          if (length(interstop_ids_b) != 0) {
            #of the remaining reference gtfs interstops that intersect with the ssfs_interstop path,
            #which recorded speeds at the hour being calculated ?

            #useful further below if method B is used
            interstop_speeds_i_h_d <-
              interstop_speeds_i %>%
              filter(interstop_id %in% interstop_ids_b) %>%
              mutate(
                departure_time_s = as.numeric(as.duration(hms(departure_time)))
              ) %>% #converts departure time to duration in seconds
              mutate(
                hour_dep = sprintf(
                  "%02d:00:00",
                  as.numeric(floor(departure_time_s / 3600))
                )
              ) %>%
              filter(hour_dep == hour_dep_i)

            #interstops that meet this condition : intersects, similar distance, & speed observed at hour i
            interstop_ids_b <-
              interstop_speeds_i_h_d %>%
              pull(interstop_id) %>%
              unique()
          }
        }

        #IF THERE ARE NO SPATIAL MATCHES, THEN METHOD C
        if (length(interstop_ids_b) == 0) {
          #METHOD C : calculate speed based on osrm

          stop_a_geom <-
            ssfs$stops %>%
            filter(stop_id == stop_a)

          stop_b_geom <-
            ssfs$stops %>%
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
              ssfs$hsh %>%
              filter(
                service_id == service_id_i,
                itin_id == itin_id_i,
                hour_dep == hour_dep_i
              ) %>%
              pull(speed)

            #might be useful to add a warning message here if speed_i is longer than 1

            speed_i <- speed_i[1] #not sure if this is necessary but just in case....

            speed_factor_i <-
              stop_seq_interstops %>%
              filter(
                itin_id == itin_id_i,
                stop_id == stop_a,
                lead_stop_id == stop_b
              ) %>%
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
            interstop_speeds_i_h_d %>%
            filter(interstop_id %in% interstop_ids_b) %>%
            pull(speed) %>%
            mean() %>%
            round(digits = 1)

          ssfs_interstops_h[i, ]$speed <- speed_i

          ssfs_interstops_h[i, ]$method <- "B"
        }
      } else {
        #method A :

        #this section would be written differently and more easily with a time window parameter
        speed_i <-
          interstop_speeds_i_h %>%
          pull(speed) %>%
          mean() %>%
          round(digits = 1)

        ssfs_interstops_h[i, ]$speed <- speed_i

        ssfs_interstops_h[i, ]$method <- "A"
      }
    }

    #compile information on interstop speeds by time period into ssfs_interstop_speeds

    ssfs_interstops_h_i <-
      ssfs_interstops_h %>%
      as_tibble() %>%
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
    ssfs_interstop_speeds %>%
      select(method) %>%
      group_by(method) %>%
      summarise(n = n()) %>%
      mutate(perc = round((n / sum(n)) * 100, 1)) %>%
      select(-n)
  )

  #   #   #
  #
  #OVERWRITE SSFS$HSH AND SSFS$STOP_SEQ WITH SSFS_INTERSTOP_SPEEDS
  #
  #   #   #

  #overwrite speeds in ssfs$hsh$speed

  ssfs_hsh_new_speeds <-
    ssfs_interstop_speeds %>%
    mutate(speed_ms = speed * (1000 / 3600)) %>%
    mutate(duration_s = dist / speed_ms) %>%
    group_by(itin_id, service_id, hour_dep) %>%
    summarise(total_dist = sum(dist), total_duration = sum(duration_s)) %>%
    #write speeds in km / h
    mutate(
      speed_overwrite = round((total_dist / total_duration) * (3600 / 1000), 1)
    ) %>%
    select(-c(total_dist, total_duration))

  ssfs$hsh <-
    ssfs$hsh %>%
    left_join(
      ssfs_hsh_new_speeds,
      by = c("itin_id", "service_id", "hour_dep")
    ) %>%
    select(-speed) %>%
    rename(speed = speed_overwrite)

  #overwrite ssfs$stop_seq$speed_factor

  #first, calculate average speed per itin_id
  #we can use the revised hsh

  #with converting to periods, this will need to be modified
  #such that the numerator for n_trips varies based on the duration of each time window
  #instead of being a constant 60 minutes

  speed_by_itin_id <-
    ssfs$hsh %>%
    mutate(n_trips = 60 / headway) %>%
    mutate(n_trips = if_else(is.na(n_trips), 1, n_trips)) %>%
    mutate(speed_total = speed * n_trips) %>%
    group_by(itin_id) %>%
    summarise(speed_total = sum(speed_total), sum_trips = sum(n_trips)) %>%
    mutate(speed_avg = speed_total / sum_trips) %>%
    select(-c(speed_total, sum_trips))

  ssfs_stop_seq_new_speed_factor <-
    ssfs_interstop_speeds %>%
    #join number of trips by hour, itin_id and service_id
    #same method as above
    left_join(
      ssfs$hsh %>%
        mutate(n_trips = 60 / headway) %>%
        select(itin_id, service_id, hour_dep, n_trips),
      by = c("itin_id", "service_id", "hour_dep")
    ) %>%
    mutate(n_trips = if_else(is.na(n_trips), 1, n_trips)) %>%
    mutate(speed_total = speed * n_trips) %>%
    group_by(stop_id, stop_sequence, itin_id) %>%
    summarise(speed_total = sum(speed_total), sum_trips = sum(n_trips)) %>%
    mutate(speed_avg_interstop = speed_total / sum_trips) %>%
    select(itin_id, stop_id, stop_sequence, speed_avg_interstop) %>%
    ungroup() %>%
    left_join(speed_by_itin_id, by = "itin_id") %>%
    mutate(
      speed_factor_overwrite = round(speed_avg_interstop / speed_avg, 1)
    ) %>%
    select(-c(speed_avg_interstop, speed_avg))

  #TO DEAL WITH SAME STOP (INTERSTOP) TWICE IN SAME ITIN_ID, it might be necessary
  #to import and retain stop_sequence attribute in ssfs_interstop_speeds
  #and use that to facilitate a join

  ssfs$stop_seq <-
    ssfs$stop_seq %>%
    left_join(
      ssfs_stop_seq_new_speed_factor,
      by = c("itin_id", "stop_id", "stop_sequence")
    ) %>%
    select(-speed_factor) %>%
    rename(speed_factor = speed_factor_overwrite)

  return(ssfs)
}
