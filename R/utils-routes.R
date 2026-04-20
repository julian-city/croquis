# Generate a route segment between two points using routing or straight line
#
# @param from_point Numeric vector c(lng, lat)
# @param to_point Numeric vector c(lng, lat)
# @param drawing_mode "network" or "free"
# @param routing_server "OSRM" or "Valhalla"
generateRouteSegment <- function(
  from_point,
  to_point,
  drawing_mode = c("network", "free"),
  routing_server = c("Valhalla", "OSRM")
) {
  drawing_mode <- match.arg(drawing_mode)
  routing_server <- match.arg(routing_server)

  straight_line <- function() {
    data.frame(
      lng = c(from_point[1], to_point[1]),
      lat = c(from_point[2], to_point[2])
    )
  }

  if (drawing_mode != "network") {
    return(straight_line())
  }

  from_sf <- st_sf(geometry = st_sfc(st_point(from_point), crs = 4326))
  to_sf <- st_sf(geometry = st_sfc(st_point(to_point), crs = 4326))

  call_routing <- function() {
    if (routing_server == "OSRM") {
      osrm::osrmRoute(src = from_sf, dst = to_sf, overview = "full")
    } else {
      valh::vl_route(src = from_sf, dst = to_sf)
    }
  }

  # Try twice: transient failures on public routing servers are common.
  route <- NULL
  for (attempt in seq_len(2)) {
    route <- tryCatch(call_routing(), error = function(e) NULL)
    if (!is.null(route)) {
      break
    }
    if (attempt < 2) Sys.sleep(0.3)
  }

  if (is.null(route)) {
    cli::cli_warn(
      "Routing server ({routing_server}) did not respond; falling back to straight line."
    )
    return(straight_line())
  }

  route_coords <- sf::st_coordinates(route$geometry)
  if (is.null(route_coords) || nrow(route_coords) < 2) {
    cli::cli_warn(
      "Routing server ({routing_server}) returned an empty route; falling back to straight line."
    )
    return(straight_line())
  }

  data.frame(lng = route_coords[, 1], lat = route_coords[, 2])
}

# Convert route nodes to a stop sequence data frame
#
# @param nodes Data frame of route nodes (must include is_stop, stop_id, stop_name, speed_factor)
# @param itin_id Character string, the itinerary ID
generateStopSequenceFromNodes <- function(nodes, itin_id) {
  stop_nodes <- nodes[nodes$is_stop, ]

  if (nrow(stop_nodes) == 0) {
    return(data.frame())
  }

  stop_seq <- data.frame(
    itin_id = rep(itin_id, nrow(stop_nodes)),
    stop_id = stop_nodes$stop_id,
    stop_sequence = seq_len(nrow(stop_nodes)),
    speed_factor = stop_nodes$speed_factor,
    stop_name = stop_nodes$stop_name,
    stringsAsFactors = FALSE
  )

  stop_seq <-
    stop_seq |>
    mutate(speed_factor = replace_na(speed_factor, 1)) |>
    mutate(
      speed_factor = if_else(
        stop_sequence == max(stop_sequence),
        NA_real_,
        speed_factor
      )
    )

  stop_seq
}
