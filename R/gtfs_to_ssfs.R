#' Convert a GTFS to a SSFS
#'
#' @param gtfs An object of class 'gtfs'. Must contain required tables and calendar table.
#' @param routes A character vector of the route id(s) you wish to convert to ssfs. Leave as NULL to convert all routes to SSFS
#' @param max_date A date within the range of gtfs$calendar$end_date representing the maximum of a 7 day range used to build the SSFS. Leave as NULL to use the last 7 days specified in gtfs$calendar to build the SSFS
#'
#' @returns A SSFS list
#'
#' @export
#' @examples
gtfs_to_ssfs <- function(gtfs, routes = NULL, max_date = NULL) {
  #THREE parameters
  #gtfs must be a gtfs imported by gtfstools : an object with class "dt_gtfs","gtfs","list"
  #it must contain the required tables of gtfs and the required fields

  #route must be a character string or vector. Can include any number of specified routes.

  #max_date, if specified, will return a ssfs which describes the service in the week
  #PRECEEDING the specified date. Must be date (example : max_date=as.date("2024-10-16"))

  #1. VALIDATIONS and initial transformations---------------------

  #goal is to filter routes, trips, and service ids based on max_date argument
  #OR based on the last week of service, so as to reduce the complexity of the ssfs

  if (is.null(routes)) {
    route_service_ids <-
      gtfs$trips |>
      pull(service_id) |>
      unique()

    #routes is frequently used in the script, so assigning this with all the routes
    #is the simplest thing to do
    routes <- gtfs$routes |> pull(route_id)
  } else {
    route_service_ids <-
      gtfs$trips |>
      filter(route_id %in% routes) |>
      pull(service_id) |>
      unique()
  }

  #we only want the trips associated with service ids that are associated with the last states of service
  #for each day of the week

  day <- c(
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday"
  )

  if (!is.null(max_date)) {
    min_date <- max_date - days(7)

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
    #these conditions could be questioned and revised,
    #but at first glance they seem solid enough
  } else {
    #if max_date is NULL, then we assign the last day of service described in the gtfs
    #as the max date
    max_date <-
      gtfs$calendar |>
      summarise(max_date = max(end_date)) |>
      pull(max_date)

    min_date <- max_date - days(7)

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

  #we want to create a table specifying, for each day of the week, what service ids
  #are active on the last day of the service period
  #(OR of the last day of the specified date range if this argument is provided
  #this is something that will need to be added in another instance)

  #initialize the table

  service_ids_byday <- list()

  for (i in day) {
    service_ids_byday[[i]] <-
      route_calendar |>
      filter(!!sym(i) == 1) |> # this enables us to index the column / vector based on i (day of week)
      select(service_id)
  }

  #we can now override route_service_ids with another one based on the above
  #which only includes the latest by day of week

  route_service_ids <- service_ids_byday |>
    purrr::map_df(~.x) |> # Combine all data frames into one
    distinct(service_id) |> # Get distinct service_id
    pull(service_id) # Convert to a vector

  #the trips that interest us, based on service_id and route_id

  trips <-
    gtfs$trips |>
    filter(route_id %in% routes, service_id %in% route_service_ids)

  #force add direction_id if it is absent from trips

  trips_colnames <- trips |> colnames()

  if (!"direction_id" %in% trips_colnames) {
    trips$direction_id <- 0
  }

  #stop times object, validation

  #important to rewrite stop_sequence in case
  #there are gaps of >1 between the stop_sequence values of two sequential stops
  #(for example, King County Metro gtfs September 2025)

  stop_times <-
    gtfs$stop_times |>
    filter(trip_id %in% trips$trip_id) |>
    arrange(trip_id, stop_sequence) |>
    group_by(trip_id) |>
    mutate(stop_sequence = row_number()) |>
    ungroup()

  # 2. Define carried-over tables----------------

  #including calendar, routes, agency and stops

  # CALENDAR

  # Service id consolidation

  #consolidating services that occur on the same day of the week
  service_combos <-
    service_ids_byday |>
    map(~ .x$service_id) |> # Extract service_ids for each day
    enframe(name = "day", value = "service_combo") |> # Convert to a tibble with day and combination
    group_by(service_combo) |>
    summarise(days = list(day)) |> #identify days of week associated with each service combo
    mutate(service_combo_id = paste0("S", row_number())) #defining the service combo id

  service_combo_to_service_id <-
    service_combos |>
    select(service_combo, service_combo_id) |>
    unnest(service_combo) |>
    rename(service_id = service_combo)
  #this can be used for rewriting service ids

  #service_combo_to_day <-
  #  service_combos |>
  #  select(service_combo_id,days) |>
  #  unnest(days) |>
  #  rename(day=days)
  #this could be used for writing the calendar

  calendar <-
    route_calendar |>
    left_join(
      service_combo_to_service_id,
      by = "service_id"
    ) |>
    select(service_combo_id, start_date, end_date) |>
    group_by(service_combo_id) |>
    summarise(start_date = min(start_date), end_date = max(end_date)) |>
    left_join(service_combos |> select(-service_combo)) |>
    rename(service_id = service_combo_id)

  calendar <-
    calendar |>
    #convert days (list) into a single string for each entry
    mutate(days = sapply(days, function(x) paste(x, collapse = ","))) |>
    mutate(
      monday = if_else(str_detect(days, "monday"), 1, 0),
      tuesday = if_else(str_detect(days, "tuesday"), 1, 0),
      wednesday = if_else(str_detect(days, "wednesday"), 1, 0),
      thursday = if_else(str_detect(days, "thursday"), 1, 0),
      friday = if_else(str_detect(days, "friday"), 1, 0),
      saturday = if_else(str_detect(days, "saturday"), 1, 0),
      sunday = if_else(str_detect(days, "sunday"), 1, 0)
    ) |>
    mutate(across(monday:sunday, as.integer)) |>
    mutate(
      start_date = as.character(start_date),
      end_date = as.character(end_date)
    ) |>
    select(
      service_id,
      monday,
      tuesday,
      wednesday,
      thursday,
      friday,
      saturday,
      sunday,
      start_date,
      end_date
    )

  # ROUTES

  #conserving the term "route_info" to distinguish from routes, which is
  #a function argument including the route_ids to conserve

  routes_colnames <- colnames(gtfs$routes)

  #if there is no agency_id in routes_colnames, then add one to the original table and assign
  #the value to 1
  if (!"agency_id" %in% routes_colnames) {
    gtfs$routes <-
      gtfs$routes |>
      mutate(agency_id = "1")
  }

  #if there is no route short name in colnames, then give route_short_name route_id
  if (!"route_short_name" %in% routes_colnames) {
    gtfs$routes <-
      gtfs$routes |>
      mutate(route_short_name = route_id)
  }

  #if there is no route long name in colnames, then give route_long_name route_short_name
  #it is required that either route_short_name OR route_long_name be present, by GTFS standards
  # in any case, this code will fix GTFS files where both are missing
  if (!"route_long_name" %in% routes_colnames) {
    gtfs$routes <-
      gtfs$routes |>
      mutate(route_long_name = route_short_name)
  }

  #if route color and route text color exist in the gtfs$routes,
  #then pass those on to route info
  if (
    "route_color" %in% routes_colnames & "route_text_color" %in% routes_colnames
  ) {
    route_info <-
      gtfs$routes |>
      filter(route_id %in% routes) |>
      select(
        route_id,
        agency_id,
        route_short_name,
        route_long_name,
        route_type,
        route_color,
        route_text_color
      ) |>
      mutate(
        route_color = case_when(
          #handle for when route color column exists but it's blank
          # subways, metros and monorails, use darkest blue
          route_color == "" & route_type %in% c(1, 12) ~ "2166AC",
          # tramways, cable trams, and funiculars : use lighter blue
          route_color == "" & route_type %in% c(0, 5, 7) ~ "4393C3",
          # buses and trolleybuses, use lightest blue and black text
          route_color == "" & route_type %in% c(3, 11) ~ "92C5DE",
          # inter city rail, use dark grey
          route_color == "" & route_type == 2 ~ "5E5E5E",
          # ferries, use darker orange (contrasts with blue of water !)
          route_color == "" & route_type == 4 ~ "D6604D",
          # gondolas / metrocable, use green
          route_color == "" & route_type == 6 ~ "2B5D2C",
          #else, identify as bus
          TRUE ~ route_color
        ),
        route_text_color = case_when(
          #use black text for buses, otherwise white
          route_text_color == "" & route_type %in% c(3, 11) ~ "000000",
          route_text_color == "" & !route_type %in% c(3, 11) ~ "F7F7F7",
          TRUE ~ route_text_color
        )
      )

    #else, choose route_color and route_text_color based on route_type
  } else {
    route_info <-
      gtfs$routes |>
      filter(route_id %in% routes) |>
      mutate(route_type = as.integer(route_type)) |> #just in case
      mutate(
        route_color = case_when(
          # subways, metros and monorails, use darkest blue
          route_type %in% c(1, 12) ~ "2166AC",
          # tramways, cable trams, and funiculars : use lighter blue
          route_type %in% c(0, 5, 7) ~ "4393C3",
          # buses and trolleybuses, use lightest blue and black text
          route_type %in% c(3, 11) ~ "92C5DE",
          # inter city rail, use dark grey
          route_type == 2 ~ "5E5E5E",
          # ferries, use darker orange (contrasts with blue of water !)
          route_type == 4 ~ "D6604D",
          # gondolas / metrocable, use green
          route_type == 6 ~ "2B5D2C",
          #else, identify as bus
          TRUE ~ "92C5DE"
        ),
        route_text_color = case_when(
          #use black text for buses, otherwise white
          route_type %in% c(3, 11) ~ "000000",
          TRUE ~ "F7F7F7"
        )
      ) |>
      select(
        route_id,
        agency_id,
        route_short_name,
        route_long_name,
        route_type,
        route_color,
        route_text_color
      )
  }

  # AGENCY

  agency_colnames <- colnames(gtfs$agency)

  #if agency_id is missing from the agency table, then add it and assign "1"
  if (!"agency_id" %in% agency_colnames) {
    agency <-
      gtfs$agency |>
      mutate(agency_id = "1") |>
      select(agency_id, agency_name, agency_url, agency_timezone)

    #else, identify the agency_ids retained in route_info
    #and filter agency based on this
  } else {
    agency_ids <-
      route_info |> pull(agency_id) |> unique()

    agency <-
      gtfs$agency |>
      filter(agency_id %in% agency_ids) |>
      select(agency_id, agency_name, agency_url, agency_timezone)
  }

  # STOPS

  stop_ids <-
    stop_times |>
    pull(stop_id) |>
    unique()

  stops <-
    gtfs$stops |>
    filter(stop_id %in% stop_ids) |>
    select(stop_id, stop_name, stop_lat, stop_lon) |>
    st_as_sf(
      coords = c("stop_lon", "stop_lat"),
      crs = 4269
    )

  #3. Identify unique route itineraries (itin)---------------------------------

  #identifying distinct variant types (itineraries) within the trips of interest

  #first step towards writing itin and stop_seq. Necessary for writing shapes

  itin_to_stop_seq <-
    stop_times |>
    select(trip_id, stop_id, stop_sequence) |>
    left_join(
      trips |>
        select(route_id, trip_id, direction_id),
      by = "trip_id"
    ) |>
    group_by(route_id, trip_id, direction_id) |>
    summarise(stop_pattern = list(data.frame(stop_id, stop_sequence))) |> #stop_id & stop_sequence pattern of each trip
    ungroup() |>
    group_by(route_id, direction_id, stop_pattern) |>
    summarise(
      trip_ids = list(c(trip_id)), #trip ids by unique stop_id & stop_sequence pattern
      count = n()
    ) |>
    ungroup() |>
    group_by(route_id, direction_id) |>
    arrange(-count) |> #so that the primary itin_id by direction receives suffix _1
    #row number takes into account position within the group (direction_id) so it is possible to define distinct itin_id
    mutate(
      itin_id = paste0(
        as.character(route_id),
        "_",
        as.character(direction_id),
        "_",
        row_number()
      )
    ) |>
    ungroup()

  trip_id_to_itin_id <-
    itin_to_stop_seq |>
    select(trip_ids, itin_id) |>
    unnest(trip_ids) |> #lengthens the tibble for a 1 to 1 association of trip_id to rvar_id
    rename(trip_id = trip_ids)

  #will be used later to calculate interstop distances and create stop_seq
  stop_seq_proto <-
    itin_to_stop_seq |>
    select(itin_id, stop_pattern) |>
    arrange(itin_id) |>
    unnest(stop_pattern) |>
    mutate(
      stop_seq_id = str_c(
        itin_id,
        "_",
        as.character(stop_id),
        "_",
        as.character(stop_sequence)
      ),
      .before = itin_id
    )

  #3. GTFS shapes manipulation------------------------

  #if gtfs shapes are missing, we need to create them to calculate speeds
  #else, we just need them ordred such that speeds can be calculated

  #Can add a step to create shapes information if it's missing

  gtfs_table_names <- names(gtfs)

  #if there are no shapes in gtfs table names, then create it using osrm
  #ONLY WHEN route type is 3,5,11 (bus, cable tram and trolleybus)

  if (!"shapes" %in% gtfs_table_names) {
    message("shapes missing from GTFS, created them.")

    #IDENTIFY UNIQUE ITIN IDS FOR WHICH TO GENERATE SHAPES
    unique_itin_ids <-
      stop_seq_proto |> pull(itin_id) |> unique()

    #CREATE EMPTY SHAPES
    shapes <- data.table(
      shape_id = character(),
      shape_pt_lat = numeric(),
      shape_pt_lon = numeric(),
      shape_pt_sequence = integer()
    )

    #
    for (itin_id_i in unique_itin_ids) {
      stops_itin_i <-
        stop_seq_proto |>
        filter(itin_id == itin_id_i) |>
        arrange(stop_sequence) |>
        select(stop_id) |>
        left_join(stops |> as_tibble(), by = "stop_id") |>
        st_as_sf()

      #return route type of itin_id_i

      route_id_i <-
        itin_to_stop_seq |>
        filter(itin_id == itin_id_i) |>
        pull(route_id) |>
        unique()

      route_type_i <-
        route_info |>
        filter(route_id == route_id_i) |>
        pull(route_type) |>
        unique()

      if (route_type_i %in% c(3, 5, 11)) {
        shape_i <-
          osrm::osrmRoute(loc = stops_itin_i, overview = "full")

        shapes_i <-
          shape_i |>
          select(geometry) |>
          st_cast("POINT") |>
          mutate(coords = st_coordinates(geometry)) |>
          mutate(
            shape_pt_lon = coords[, "X"],
            shape_pt_lat = coords[, "Y"],
            shape_pt_sequence = row_number(),
            shape_id = itin_id_i
          ) |>
          as.data.table() |>
          select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence)
      } else {
        #else (for example, if metro or ferry or light rail),
        #shapes is only made up of points that are also stops

        shapes_i <-
          stops_itin_i |>
          select(geometry) |>
          mutate(coords = st_coordinates(geometry)) |>
          mutate(
            shape_pt_lon = coords[, "X"],
            shape_pt_lat = coords[, "Y"],
            shape_pt_sequence = row_number(),
            shape_id = itin_id_i
          ) |>
          as.data.table() |>
          select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence)
      }

      shapes <-
        rbind(shapes, shapes_i)
    }

    #create shapes within the gtfs

    gtfs$shapes <- shapes

    #add shape id to trip_id and trips

    gtfs$trips <-
      gtfs$trips |>
      left_join(trip_id_to_itin_id, by = "trip_id") |>
      mutate(shape_id = itin_id) |>
      select(-itin_id)

    trips <-
      trips |>
      left_join(trip_id_to_itin_id, by = "trip_id") |>
      mutate(shape_id = itin_id) |>
      select(-itin_id)
  } else {
    #else shapes exist, we just need to ensure that they are properly ordered

    #Necessary to get get_trip_speed to work properly
    gtfs$shapes <-
      gtfs$shapes |>
      arrange(shape_id, shape_pt_sequence)
  }

  #refining trip details : based on new service ids------

  trips_colnames <- trips |> colnames()

  if ("trip_headsign" %in% trips_colnames) {
    trips <- trips |>
      left_join(
        service_combo_to_service_id,
        by = "service_id",

        #NOTE : many-to-many relationship set as native service ids
        #can be associated with multiple service combo ids (ssfs service ids)
        #and trips need to be duplicated in this case

        relationship = "many-to-many"
      ) |>
      select(-service_id) |>
      rename(service_id = service_combo_id) |>
      left_join(trip_id_to_itin_id, by = "trip_id") |>
      select(
        trip_id,
        route_id,
        direction_id,
        itin_id,
        trip_headsign,
        service_id,
        shape_id
      ) |>
      arrange(service_id, itin_id)
  } else {
    #if no trip headsign, use route_long_name from route_info to add one
    trips <- trips |>
      left_join(
        service_combo_to_service_id,

        #NOTE : many-to-many relationship set as native service ids
        #can be associated with multiple service combo ids (ssfs service ids)
        #and trips need to be duplicated in this case

        relationship = "many-to-many"
      ) |>
      select(-service_id) |>
      rename(service_id = service_combo_id) |>
      left_join(trip_id_to_itin_id, by = "trip_id") |>
      left_join(
        route_info |>
          select(route_id, route_long_name) |>
          rename(trip_headsign = route_long_name),
        by = "route_id"
      ) |>
      select(
        trip_id,
        route_id,
        direction_id,
        itin_id,
        trip_headsign,
        service_id,
        shape_id
      ) |>
      arrange(service_id, itin_id)
  }

  trips_speed <-
    gtfstools::get_trip_speed(gtfs = gtfs, trip_id = trips$trip_id) |>
    as.data.frame() |>
    select(trip_id, speed)

  trips <-
    trips |>
    left_join(trips_speed, by = "trip_id")

  #defining shapes------------------------------------------

  #defining one shape id per itin_id
  #in most cases this should be a one-to-one association but this is for good measure
  #the criterion applied is the SHORTEST shape id per rvar_id is retained
  #and applied to all associated rvar_id

  unique_shape_id <-
    trips |>
    pull(shape_id) |>
    unique()

  shapes <-
    gtfs$shapes |>
    as_tibble() |>
    filter(shape_id %in% unique_shape_id) |>
    st_as_sf(
      coords = c("shape_pt_lon", "shape_pt_lat"),
      crs = 4269
    ) |>
    #arrange(shape_id,shape_pt_sequence) |> #already arranged above
    group_by(shape_id) |>
    summarise(do_union = FALSE) |> #ensures that point geometries are not merged
    st_cast("LINESTRING") |>
    mutate(length = st_length(geometry)) |>
    mutate(length = as.numeric(length)) #length in meters

  itin_to_shortest_shape_id <-
    trips |>
    select(itin_id, shape_id) |>
    distinct() |>
    left_join(shapes |> as_tibble() |> select(-geometry), by = "shape_id") |>
    group_by(itin_id) |>
    mutate(min_length = min(length)) |>
    ungroup() |>
    filter(length == min_length) |>
    select(itin_id, shape_id) |>
    rename(shape_id_shortest = shape_id) |>
    #critical in case these are shapes with identical lengths
    distinct(itin_id, .keep_all = TRUE)

  #overwrite shape id with shortest shape id associated with each itin_id
  trips <-
    trips |>
    left_join(itin_to_shortest_shape_id, by = "itin_id") |>
    select(-shape_id) |> #remove existing shape_id column
    rename(shape_id = shape_id_shortest) #define the new shape_id column as the current

  #unique shape_id may have changed now
  unique_shape_id <-
    trips |>
    pull(shape_id) |>
    unique()

  #limit the extent of shapes to the ones that we are retaining
  shapes <-
    shapes |>
    filter(shape_id %in% unique_shape_id) |>
    select(-length)

  #create shape points, which we will need later to calculate interstop distances
  #replace shape_id with itin_id

  shapes_points <-
    gtfs$shapes |>
    as_tibble() |>
    filter(shape_id %in% unique_shape_id) |>
    left_join(
      itin_to_shortest_shape_id |>
        select(shape_id_shortest, itin_id) |>
        rename(shape_id = shape_id_shortest),
      by = "shape_id"
    ) |>
    st_as_sf(
      coords = c("shape_pt_lon", "shape_pt_lat"),
      crs = 4269
    ) |>
    arrange(shape_id, shape_pt_sequence) |>
    select(itin_id, shape_pt_sequence, geometry) #INCLUDE shape_dist_traveled eventually

  #define itin------------------------------

  itin <-
    trips |>
    select(itin_id, route_id, direction_id, trip_headsign, shape_id) |>
    left_join(shapes |> as_tibble(), by = "shape_id") |>
    distinct() |>
    select(itin_id, route_id, direction_id, trip_headsign, geometry) |>
    st_as_sf()

  #define stop_seq by itin_id-----------------------------

  #calculate interstop distances

  #initialize

  stop_seq_proto$interstop_dist <- NA

  #initialize cli
  # Before the loop:
  cli::cli_progress_bar(
    "Calculating interstop distance",
    total = length(stop_seq_proto$stop_seq_id) - 1
  )

  for (i in c(1:(length(stop_seq_proto$stop_seq_id) - 1))) {
    #Cat message must eventually be converted to cli_progress_bar or similar!

    #old cat() call for progress
    #cat(
    #  "\rCalculating interstop distance",
    #  i,
    #  "of",
    #  length(stop_seq_proto$stop_seq_id) - 1
    #)

    #replaced with new cli functions
    cli::cli_progress_update()

    #CONDITIONS
    #next stop needs to be part of the same sequence AND
    #part of the same rvar_id (just another way of verifying the same stop sequence)
    #ELSE the NA assignment remains

    if (
      (stop_seq_proto$stop_sequence[i] + 1 ==
        stop_seq_proto$stop_sequence[i + 1]) &
        (stop_seq_proto$itin_id[i] == stop_seq_proto$itin_id[i + 1])
    ) {
      itin_id_i <- stop_seq_proto$itin_id[i]

      #shape_id_i <-
      #  itin |>
      #  filter(itin_id==itin_id_i) |>
      #  pull(shape_id)

      #shapes points for only the shape_id associated with the rvar_id associated with stop i
      shapes_points_i <-
        shapes_points |>
        filter(itin_id == itin_id_i)

      current_stop_id <- stop_seq_proto$stop_id[i]
      next_stop_id <- stop_seq_proto$stop_id[i + 1]

      current_stop <-
        stops |>
        filter(stop_id == current_stop_id)

      next_stop <-
        stops |>
        filter(stop_id == next_stop_id)

      #nearest points along shapes_points to current and next stops

      interstop_segment_points <-
        shapes_points_i[
          st_nearest_feature(current_stop, shapes_points_i):st_nearest_feature(
            next_stop,
            shapes_points_i
          ),
        ]

      interstop_dist_i <-
        as.numeric(
          interstop_segment_points |>
            summarise(do_union = FALSE) |> #do_union retains the order of the points
            st_cast("LINESTRING") |>
            st_length()
        )

      stop_seq_proto$interstop_dist[i] <- interstop_dist_i
    } else {
      stop_seq_proto$interstop_dist[i] <- NA
    }
  }

  #REWRITE STOP TIMES to handle sequential stops with the same stop time

  stop_times_revised <-
    stop_times |>
    left_join(
      trips |> select(trip_id, itin_id, service_id),
      by = "trip_id"
    ) |>
    select(
      itin_id,
      service_id,
      trip_id,
      departure_time,
      stop_id,
      stop_sequence
    ) |>
    left_join(
      stop_seq_proto |>
        select(itin_id, stop_id, stop_sequence, interstop_dist),
      by = c("itin_id", "stop_id", "stop_sequence")
    ) |>
    mutate(
      departure_time = as.numeric(as.duration(hms(departure_time))),
      lag_interstop_dist = lag(interstop_dist)
    ) |> #necessary input for adjustment of last stop times
    #if the last stop times of a trip are identical and need to be adjusted backward
    #as opposed to forward
    #identify groups of stops within the same trip that are made at the same time
    #in the GTFS that need to be adjusted based on distance covered within that same minute

    #WINDSOR TESTS
    #filter(trip_id=="1261767") |>
    #filter(trip_id%in%c("1261875","1261876","1261877")) |>

    group_by(trip_id) |>
    mutate(trip_max_stop_seq = max(stop_sequence)) |>
    group_by(itin_id, trip_id, departure_time) |>
    mutate(
      ord = row_number(),
      group_n = n(),
      group_dist = sum(interstop_dist),
      group_dist_back = sum(lag_interstop_dist),
      group_dist_cov = lag(cumsum(interstop_dist), default = 0),
      group_dist_cov_back = cumsum(lag_interstop_dist),
      group_max_stop_seq = max(stop_sequence)
    ) |>
    ungroup() |>
    mutate(
      next_departure_time = case_when(
        #in reality, the next departure time or the last one of the trip
        stop_sequence == trip_max_stop_seq ~ departure_time + 60,
        #^ Adding 60 seconds to final departure time. This will only be used for
        #rewriting departure times in the case that there is two stops that occur at the same time
        #and they are both at the end. Justification : if for example the second last stop is at 7:45 and
        #the last stop is at 7:45 in the stop times, it makes sense to delay the last stop in the schedule as the
        #one before it is already made at 7:45 (so the one after will logically be made afterwards...).
        #Adding this buffer avoids a potential non chronological
        #sequence of revised departure times if the the last stop shares the same departure time
        #as one or more before it which are revised backward and before that is another set of stops
        #that share the same departure time that need to be adjusted forward.
        #Now we no longer need to revise times backwards at all.
        ord == group_n ~ lead(departure_time),
        TRUE ~ NA_real_
      )
    ) |>
    fill(next_departure_time, .direction = "up") |>
    mutate(
      departure_time = case_when(
        #Backward cases : several stops with same time at end of trip
        #if it's the very last stop, then apply next departure time (add 60 seconds)
        #otherwise calculate departure time forward but use alternate group_dist (_back) variables
        (group_max_stop_seq == trip_max_stop_seq) &
          (group_n > 1) &
          (ord == group_n) ~ next_departure_time,
        (group_max_stop_seq == trip_max_stop_seq) &
          (group_n > 1) &
          (ord < group_n) ~
          round(
            departure_time +
              ((next_departure_time - departure_time) *
                (group_dist_cov_back / group_dist_back)),
            0
          ),
        #normal case : if in a group with several identical stop times, and ord > 1,
        #then adjust departure time based on A*B, where A is the difference between next_departure_time
        #and current departure_time and B is the proportion of distance covered in the group
        #relative to total distance
        (group_n > 1) & (ord > 1) ~
          round(
            departure_time +
              ((next_departure_time - departure_time) *
                (group_dist_cov / group_dist)),
            0
          ),
        TRUE ~ departure_time #otherwise, departure_time unchanged.
      )
    ) |>
    select(itin_id, trip_id, service_id, stop_id, stop_sequence, departure_time)

  #Calculate interstop times

  interstop_times <-
    stop_times_revised |>
    arrange(itin_id, trip_id, stop_sequence) |>
    mutate(
      interstop_time = if_else(
        lead(stop_sequence) == stop_sequence + 1,
        lead(departure_time) - departure_time, #interstop_time in seconds
        NA_real_ #NA in numeric format
      )
    ) |>
    select(itin_id, stop_id, stop_sequence, interstop_time) |>
    group_by(itin_id, stop_id, stop_sequence) |>
    summarise(mean_interstop_time = mean(interstop_time)) |>
    ungroup() |>
    arrange(itin_id, stop_sequence) |>
    mutate(
      stop_seq_id = str_c(
        itin_id,
        "_",
        as.character(stop_id),
        "_",
        as.character(stop_sequence)
      ),
      .before = itin_id
    )

  #calculate interstop factor

  #overall average speed by itin

  itin_speeds_overall <-
    trips |>
    group_by(itin_id) |>
    summarise(itin_speed = mean(speed))

  stop_seq <-
    stop_seq_proto |>
    left_join(
      interstop_times |> select(stop_seq_id, mean_interstop_time),
      by = "stop_seq_id"
    ) |>
    left_join(
      itin_speeds_overall,
      by = "itin_id"
    ) |>
    mutate(interstop_speed = (interstop_dist / mean_interstop_time) * 3.6) |>
    mutate(speed_factor = round((interstop_speed / itin_speed), 1)) |>
    select(itin_id, stop_id, stop_sequence, speed_factor)

  #this writes a gtfs where
  #VARIATIONS IN INTERSTOP TIME ARE DISTRIBUTED THE SAME THROUGHOUT THE DAY
  #LES VARIATIONS DES VITESSES INTER-ARRÊT SONT RÉPARTIES DE LA MÊME FAÇON DURANT TOUTE LA JOURNÉE
  #important to communicate this.

  #define spans-----------

  #now includes service_windows

  span <-
    stop_times_revised |>
    filter(stop_sequence == 1) |>
    arrange(service_id, itin_id, departure_time) |>
    group_by(itin_id, service_id) |>
    #7200 : if gap is more than two hours, then define new service window
    #Every time there's a gap larger than 7200, we increment the group number.
    #The first observation always starts group 1 (the first TRUE)
    #and we stay in the same group until we hit a gap > 7200.
    mutate(service_window = cumsum(c(TRUE, diff(departure_time) > 7200))) |>
    group_by(itin_id, service_id, service_window) |>
    summarise(
      first_dep = min(departure_time),
      last_dep = max(departure_time)
    ) |>
    mutate(
      #sprintf in order to handle times past 24:00:00
      first_dep = sprintf(
        "%02d:%02d:%02d",
        first_dep %/% 3600,
        (first_dep %% 3600) %/% 60,
        first_dep %% 60
      ),
      last_dep = sprintf(
        "%02d:%02d:%02d",
        last_dep %/% 3600,
        (last_dep %% 3600) %/% 60,
        last_dep %% 60
      ) #,
      #service_window=as.character(service_window)
    ) |>
    ungroup()

  #define hsh--------------------------

  #headways and speeds by hour by service and rvar_id
  #we just want the first stop time per trip to define this

  hsh <-
    stop_times_revised |>
    filter(stop_sequence == 1) |>
    left_join(trips |> select(trip_id, speed), by = "trip_id") |>
    mutate(
      hour_dep = sprintf("%02d:00:00", as.numeric(floor(departure_time / 3600)))
    ) |>
    select(itin_id, service_id, hour_dep, departure_time, speed) |>
    arrange(itin_id, service_id, departure_time) |>
    mutate(
      interval_to_next = if_else(
        #in minutes
        (service_id == lead(service_id)) & (itin_id == lead(itin_id)),
        as.numeric(floor(lead(departure_time) - departure_time) / 60),
        NA_real_
      )
    ) |> #turn interval_to_next values above 60 to NA
    #this will mean that if there is an hour_dep associated with the
    #itin_id to service_id combo but interval to next is NA
    #then either only one trip will be created starting at the hour OR the only
    #trip that will be generated will be the one that departs at the time that equals
    #previous departure + the previous hour's interval
    mutate(
      interval_to_next = if_else(
        interval_to_next >= 120,
        NA_real_,
        interval_to_next
      )
    ) |>
    group_by(itin_id, service_id, hour_dep) |>
    arrange(interval_to_next) |>
    summarise(
      headway = interval_to_next[ceiling((length(interval_to_next) / 2) + 0.5)],
      #old way of creating headway, problematic when trying to generate accurate
      #stop times for trips where headways are larger than 60 minutes.
      #new way of generating headway (above) handles this case by
      #looking at the median or above median value of the series

      # MAY STILL BE PROBLEMATIC, FOR EXAMPLE WHEN interval_to_next
      # = c(3,3,3,75) .... the current thing will return 3 not 75

      #headway=round(median(interval_to_next,na.rm=TRUE),0),
      speed = round(mean(speed), 1)
    ) |>
    mutate(headway = as.integer(headway)) |>
    ungroup()

  #return ssfs-----------------------

  ssfs <- new_ssfs(
    agency = agency |> as.data.frame(),
    routes = route_info |> as.data.frame(),
    stops = stops,
    itin = itin,
    stop_seq = stop_seq |> as.data.frame(),
    span = span |> as.data.frame(),
    hsh = hsh |> as.data.frame(),
    calendar = calendar |> as.data.frame()
  )

  return(ssfs)
}
