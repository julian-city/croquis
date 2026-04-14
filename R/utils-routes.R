# Generate a route segment between two points using routing or straight line
#
# @param from_point Numeric vector c(lng, lat)
# @param to_point Numeric vector c(lng, lat)
# @param drawing_mode "network" or "free"
# @param routing_server "OSRM" or "Valhalla"
generateRouteSegment <- function(
  from_point,
  to_point,
  drawing_mode = "network",
  routing_server = "OSRM"
) {
  result_points <- data.frame(lng = numeric(), lat = numeric())

  if (drawing_mode == "network") {
    tryCatch(
      {
        from_sf <- st_sf(
          geometry = st_sfc(st_point(from_point), crs = 4326)
        )
        to_sf <- st_sf(geometry = st_sfc(st_point(to_point), crs = 4326))

        if (routing_server == "OSRM") {
          route <- osrm::osrmRoute(
            src = from_sf,
            dst = to_sf,
            overview = "full"
          )
        } else {
          route <- valh::vl_route(src = from_sf, dst = to_sf)
        }
        route_coords <- st_coordinates(route$geometry)

        for (j in 1:nrow(route_coords)) {
          result_points <- rbind(
            result_points,
            data.frame(
              lng = route_coords[j, 1],
              lat = route_coords[j, 2]
            )
          )
        }
      },
      error = function(e) {
        result_points <- rbind(
          data.frame(lng = from_point[1], lat = from_point[2]),
          data.frame(lng = to_point[1], lat = to_point[2])
        )
      }
    )
  } else {
    result_points <- rbind(
      data.frame(lng = from_point[1], lat = from_point[2]),
      data.frame(lng = to_point[1], lat = to_point[2])
    )
  }

  result_points
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
    stop_sequence = 1:nrow(stop_nodes),
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
