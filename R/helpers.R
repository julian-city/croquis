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

# Check whether the current environment supports forked parallelism.
# Returns FALSE on Windows and in IDEs that block fork() (e.g. Positron).
# Kept as a standalone helper so the check list is easy to extend and test.
croquis_can_fork <- function() {
  if (.Platform$OS.type == "windows") {
    return(FALSE)
  }
  if (nzchar(Sys.getenv("POSITRON"))) {
    return(FALSE)
  }
  TRUE
}

croquis_parallel_lapply <- function(x, fun, workers) {
  workers <- gtfs_parallel_workers(workers, length(x))

  if (workers <= 1 || length(x) <= 1) {
    return(lapply(x, fun))
  }

  if (!croquis_can_fork()) {
    cli::cli_warn(
      "Forked parallelism is not available in this environment; falling back to a single worker."
    )
    return(lapply(x, fun))
  }

  # Safety net: catch fork failures from environments not yet listed
  # in croquis_can_fork()
  result <- tryCatch(
    parallel::mclapply(
      x,
      fun,
      mc.cores = workers,
      mc.preschedule = FALSE
    ),
    error = function(e) {
      cli::cli_warn(
        "Parallel execution failed ({conditionMessage(e)}); falling back to a single worker."
      )
      lapply(x, fun)
    }
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

# FUNCTIONS FOR ADDING FOOT POINTS (ANCHORS) TO ITIN SHAPES----------------

#' Build a local metric CRS centred on a geometry
#'
#' Azimuthal equidistant projection centred on the bounding box of `x`.
#' Handles itineraries that cross the antimeridian.
#'
#' @param x An `sf` or `sfc` object.
#' @returns A PROJ string.
#' @keywords internal
local_metric_crs <- function(x) {
  coords <- sf::st_coordinates(sf::st_transform(x, 4326))
  lon <- coords[, "X"]
  lat <- coords[, "Y"]

  # antimeridian: if the longitude span is implausibly wide, work in 0-360
  if (diff(range(lon)) > 180) {
    lon <- lon %% 360
  }

  lon0 <- mean(range(lon))
  lon0 <- ((lon0 + 180) %% 360) - 180
  lat0 <- mean(range(lat))

  sprintf(
    "+proj=aeqd +lat_0=%.6f +lon_0=%.6f +datum=WGS84 +units=m +no_defs",
    lat0,
    lon0
  )
}

#' Locate an ordered set of stops along a polyline
#'
#' Projects every stop onto every segment of the line, keeps a few candidate
#' projections per stop (one per "pass" of the line near the stop), then picks
#' the combination that minimises the summed stop-to-line offsets under the
#' constraint that measures never decrease with stop order (dynamic
#' programming). Unlike the greedy ascending-index fix, early stops can be
#' revised when later stops demand it.
#'
#' @param line_xy Numeric matrix (n_vertices x 2), metric coordinates.
#' @param stop_xy Numeric matrix (n_stops x 2), metric coordinates, in
#'   stop_sequence order.
#' @param n_candidates Integer. Candidate projections kept per stop.
#' @param tol Numeric. Tolerance (m) when comparing measures.
#'
#' @returns A data.frame with one row per stop: `seg` (segment index),
#'   `t` (position on segment, 0-1), `measure` (m from line start),
#'   `offset` (m from stop to line), `foot_x`, `foot_y`, and `backtrack`
#'   (TRUE when no monotone assignment existed for that stop).
#' @keywords internal
locate_stops_on_line <- function(
  line_xy,
  stop_xy,
  n_candidates = 3L,
  tol = 1e-6
) {
  n_stops <- nrow(stop_xy)
  n_seg <- nrow(line_xy) - 1L

  if (n_seg < 1L) {
    cli::cli_abort("Itinerary geometry must contain at least two vertices.")
  }

  # segment geometry ------------------------------------------------------------

  ax <- line_xy[-nrow(line_xy), 1]
  ay <- line_xy[-nrow(line_xy), 2]
  dx <- diff(line_xy[, 1])
  dy <- diff(line_xy[, 2])
  seg_len <- sqrt(dx^2 + dy^2)
  cum_len <- c(0, cumsum(seg_len))
  len2 <- ifelse(seg_len > 0, seg_len^2, 1)

  # stop x segment matrices ------------------------------------------------------

  row_mat <- function(v) matrix(v, n_stops, n_seg, byrow = TRUE)
  col_mat <- function(v) matrix(v, n_stops, n_seg)

  AX <- row_mat(ax)
  AY <- row_mat(ay)
  DX <- row_mat(dx)
  DY <- row_mat(dy)
  SX <- col_mat(stop_xy[, 1])
  SY <- col_mat(stop_xy[, 2])

  t <- ((SX - AX) * DX + (SY - AY) * DY) / row_mat(len2)
  t <- pmin(pmax(t, 0), 1)

  FX <- AX + t * DX
  FY <- AY + t * DY
  off <- sqrt((SX - FX)^2 + (SY - FY)^2)
  meas <- row_mat(cum_len[-length(cum_len)]) + t * row_mat(seg_len)

  # candidates: local minima of offset along the line, best first --------------
  # projections closer than 1 m in measure are the same candidate (e.g. a
  # foot point sitting exactly on a shared vertex of two segments)

  cand <- lapply(seq_len(n_stops), function(j) {
    o <- off[j, ]
    prev <- c(Inf, o[-n_seg])
    nxt <- c(o[-1], Inf)
    idx <- which(o <= prev & o <= nxt)
    idx <- idx[order(o[idx])]
    idx <- idx[!duplicated(round(meas[j, idx]))]
    idx[seq_len(min(n_candidates, length(idx)))]
  })

  # dynamic programming over candidates ----------------------------------------

  backtrack_penalty <- 1e9

  cost <- off[1, cand[[1]]]
  back <- vector("list", n_stops)

  if (n_stops > 1) {
    for (j in 2:n_stops) {
      m_prev <- meas[j - 1, cand[[j - 1]]]
      m_cur <- meas[j, cand[[j]]]

      pen <- outer(
        m_prev,
        m_cur,
        function(a, b) ifelse(a <= b + tol, 0, backtrack_penalty)
      )

      total <- pen + cost # cost recycles down each column (one row per prev)
      back[[j]] <- apply(total, 2, which.min)
      cost <- total[cbind(back[[j]], seq_along(m_cur))] + off[j, cand[[j]]]
    }
  }

  # backtrack -------------------------------------------------------------------

  sel <- integer(n_stops)
  q <- which.min(cost)
  for (j in n_stops:1) {
    sel[j] <- cand[[j]][q]
    if (j > 1) q <- back[[j]][q]
  }

  ij <- cbind(seq_len(n_stops), sel)
  measure <- meas[ij]

  data.frame(
    seg = sel,
    t = t[ij],
    measure = measure,
    offset = off[ij],
    foot_x = FX[ij],
    foot_y = FY[ij],
    backtrack = c(FALSE, diff(measure) < -tol)
  )
}

#' Insert located stop positions as vertices of a line
#'
#' Positions within `snap_tol` metres of an existing vertex reuse that vertex,
#' so re-running on an already anchored line is idempotent (GTFS round trips
#' do not keep adding points).
#'
#' @param line_xy Numeric matrix of the line in its OUTPUT coordinates.
#' @param loc Output of locate_stops_on_line()
#' @param foot_xy Numeric matrix of foot points in OUTPUT coordinates.
#' @param seg_len Numeric vector of segment lengths in metres.
#' @param snap_tol Numeric. Metres.
#'
#' @returns A list with `xy` (densified coordinates) and `anchor` (vertex
#'   index of each stop in `xy`).
#' @keywords internal
densify_line_with_stops <- function(
  line_xy,
  loc,
  foot_xy,
  seg_len,
  snap_tol = 0.5
) {
  along <- loc$t * seg_len[loc$seg]
  at_start <- along <= snap_tol
  at_end <- !at_start & (seg_len[loc$seg] - along) <= snap_tol
  inserted <- !(at_start | at_end)

  # fractional "position" in vertex index space: vertex i sits at i,
  # a foot point on segment s at s + t
  pos_stop <- loc$seg + loc$t
  pos_stop[at_start] <- loc$seg[at_start]
  pos_stop[at_end] <- loc$seg[at_end] + 1

  new_pos <- pos_stop[inserted]
  new_xy <- foot_xy[inserted, , drop = FALSE]
  keep <- !duplicated(new_pos)

  all_pos <- c(seq_len(nrow(line_xy)), new_pos[keep])
  all_xy <- rbind(line_xy[, 1:2, drop = FALSE], new_xy[keep, , drop = FALSE])

  ord <- order(all_pos)

  list(
    xy = all_xy[ord, , drop = FALSE],
    anchor = match(pos_stop, all_pos[ord])
  )
}

#' Anchor the stops of an itinerary on its geometry
#'
#' @param line An `sfc` holding one LINESTRING (the itin geometry).
#' @param stop_points An `sfc` of POINTs in stop_sequence order.
#' @param densify Logical. Insert projected stop positions as vertices.
#' @param max_offset Numeric. Metres. Stops further than this from the line
#'   trigger a warning (likely wrong shape or a bad stop location).
#' @param itin_id Character, only used in messages.
#'
#' @returns A list with `geometry` (sfc LINESTRING, same CRS as `line`),
#'   `measure` (m along the line for each stop), `offset` (m), `anchor`
#'   (vertex index, NULL when `densify = FALSE`) and `interstop_dist`
#'   (m, NA for the last stop).
#' @keywords internal
anchor_stops_to_itin <- function(
  line,
  stop_points,
  densify = TRUE,
  max_offset = 150,
  itin_id = NA_character_
) {
  crs_out <- sf::st_crs(line)
  crs_local <- local_metric_crs(line)

  line_ll <- sf::st_coordinates(line)[, 1:2, drop = FALSE]
  line_m <- sf::st_coordinates(sf::st_transform(line, crs_local))[,
    1:2,
    drop = FALSE
  ]
  stop_m <- sf::st_coordinates(sf::st_transform(stop_points, crs_local))[,
    1:2,
    drop = FALSE
  ]

  loc <- locate_stops_on_line(line_m, stop_m)

  if (any(loc$backtrack)) {
    cli::cli_warn(
      "Itin {itin_id}: stop order could not be matched monotonically to its geometry."
    )
  }

  if (any(loc$offset > max_offset)) {
    cli::cli_warn(
      "Itin {itin_id}: {sum(loc$offset > max_offset)} stop{?s} more than {max_offset} m from the itinerary geometry."
    )
  }

  geometry <- line
  anchor <- NULL

  if (densify) {
    foot_ll <- sf::st_as_sf(
      data.frame(x = loc$foot_x, y = loc$foot_y),
      coords = c("x", "y"),
      crs = crs_local
    ) |>
      sf::st_transform(crs_out) |>
      sf::st_coordinates()

    seg_len <- sqrt(rowSums(diff(line_m)^2))

    dens <- densify_line_with_stops(
      line_xy = line_ll,
      loc = loc,
      foot_xy = foot_ll[, 1:2, drop = FALSE],
      seg_len = seg_len
    )

    geometry <- sf::st_sfc(sf::st_linestring(dens$xy), crs = crs_out)
    anchor <- dens$anchor
  }

  list(
    geometry = geometry,
    measure = loc$measure,
    offset = loc$offset,
    anchor = anchor,
    interstop_dist = c(diff(loc$measure), NA_real_)
  )
}

#Compute interstop distances for itineraries---------------

compute_interstop_distances_for_itin <- function(
  itin_id,
  stop_seq_proto,
  itin,
  stops
) {
  itin_stop_seq <-
    stop_seq_proto[stop_seq_proto$itin_id == itin_id, , drop = FALSE] |>
    arrange(stop_sequence)

  line <- sf::st_geometry(itin)[itin$itin_id == itin_id]

  if (nrow(itin_stop_seq) == 0 || length(line) == 0) {
    return(tibble(
      stop_seq_id = character(),
      interstop_dist = numeric()
    ))
  }

  stop_points <- sf::st_geometry(stops)[
    match(itin_stop_seq$stop_id, stops$stop_id)
  ]

  anchored <- anchor_stops_to_itin(
    line = line[1],
    stop_points = stop_points,
    densify = FALSE,
    itin_id = itin_id
  )

  tibble(
    stop_seq_id = itin_stop_seq$stop_seq_id,
    interstop_dist = anchored$interstop_dist
  )
}

#REVISE STOP TIMES--------------------
#to handle sequential stops with the same stop time
#fixes instances of stop times with the same stop time as the previous (e.g. when rounded to the minute)

revise_stop_times <- function(stop_times, trips, stop_seq_proto) {
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

    group_by(trip_id, service_id) |>
    mutate(trip_max_stop_seq = max(stop_sequence)) |>
    group_by(itin_id, trip_id, service_id, departure_time) |>
    mutate(
      ord = row_number(),
      group_n = n(),
      group_dist = sum(interstop_dist, na.rm = TRUE),
      group_dist_back = sum(lag_interstop_dist, na.rm = TRUE),
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

  stop_times_revised
}
