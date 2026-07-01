#' Reroute segments around a moved node
#'
#' Recalculates the route point geometry when a node (stop or waypoint) is
#' moved to new coordinates. Handles all positional cases: single node, first
#' node, last node, and middle node.
#'
#' All existing node attributes (node_id, is_stop, stop_id, etc.) are
#' preserved on the moved node. Callers that need to change attributes (e.g.,
#' converting a waypoint to a stop) should modify the returned `nodes` data
#' frame at row `node_idx` after the call.
#'
#' @param nodes Data frame of route nodes with at minimum: `node_id`, `lng`,
#'   `lat`, `is_stop`, `stop_id`, `stop_name`, `speed_factor`, `index`.
#' @param points Data frame of route points with columns: `index`, `lng`,
#'   `lat`.
#' @param node_idx Integer. Row index (not `node_id`) of the node being moved.
#' @param new_lng Numeric. New longitude for the node.
#' @param new_lat Numeric. New latitude for the node.
#' @param drawing_mode Character. Either `"network"` or `"free"`.
#' @param routing_server Character. The routing server to use (e.g. `"Valhalla"`, `"OSRM"`).
#' @return A list with elements `nodes` and `points`, each a data frame with
#'   updated coordinates and indices. Row names are reset to
#'   `1:nrow(...)`.
#' @keywords internal
rerouteNodeSegments <- function(
  nodes,
  points,
  node_idx,
  new_lng,
  new_lat,
  drawing_mode,
  routing_server
) {
  # --- Single node ---
  if (nrow(nodes) == 1) {
    nodes$lng <- new_lng
    nodes$lat <- new_lat
    points$lng <- new_lng
    points$lat <- new_lat

    return(list(nodes = nodes, points = points))
  }

  # --- First node ---
  if (node_idx == 1) {
    nb_points_before <- nodes[2, ]$index - 1

    new_segment <- generateRouteSegment(
      c(new_lng, new_lat),
      c(nodes[2, ]$lng, nodes[2, ]$lat),
      drawing_mode = drawing_mode,
      routing_server = routing_server
    )

    new_points <-
      new_segment[1:(nrow(new_segment) - 1), ] |>
      mutate(index = row_number(), .before = "lng")

    adj_index <- nrow(new_points) - nb_points_before

    points <-
      rbind(
        new_points,
        points[(nb_points_before + 1):nrow(points), ] |>
          mutate(index = index + adj_index)
      )

    nodes[1, ]$lng <- new_lng
    nodes[1, ]$lat <- new_lat

    nodes <-
      rbind(
        nodes[1, ],
        nodes[2:nrow(nodes), ] |>
          mutate(index = index + adj_index)
      )

    # --- Last node ---
  } else if (node_idx == nrow(nodes)) {
    prev_idx <- node_idx - 1

    new_segment <- generateRouteSegment(
      c(nodes[prev_idx, ]$lng, nodes[prev_idx, ]$lat),
      c(new_lng, new_lat),
      drawing_mode = drawing_mode,
      routing_server = routing_server
    )

    nb_points_retained <- nodes[prev_idx, ]$index

    new_points <-
      new_segment[2:nrow(new_segment), ] |>
      mutate(index = row_number() + nb_points_retained, .before = "lng")

    points <-
      rbind(
        points[1:nb_points_retained, ],
        new_points
      )

    nodes[node_idx, ]$lng <- new_lng
    nodes[node_idx, ]$lat <- new_lat
    nodes[node_idx, ]$index <- max(points$index)

    # --- Middle node ---
  } else {
    before_idx <- node_idx - 1
    after_idx <- node_idx + 1

    nodes_a <- nodes[1:before_idx, ]
    nodes_a_idx_max <- max(nodes_a$index)
    points_a <- points[1:nodes_a_idx_max, ]

    nodes_d <- nodes[after_idx:nrow(nodes), ]
    nodes_d_idx_min <- min(nodes_d$index)
    points_d <- points[nodes_d_idx_min:nrow(points), ]

    nb_points_bc_before <-
      min(points_d$index) - max(points_a$index) - 1

    segment_b <- generateRouteSegment(
      c(nodes[before_idx, ]$lng, nodes[before_idx, ]$lat),
      c(new_lng, new_lat),
      drawing_mode = drawing_mode,
      routing_server = routing_server
    )

    points_b <-
      segment_b[2:nrow(segment_b), ] |>
      mutate(index = row_number() + nodes_a_idx_max, .before = "lng")

    points_b_idx_max <- max(points_b$index)

    segment_c <- generateRouteSegment(
      c(new_lng, new_lat),
      c(nodes[after_idx, ]$lng, nodes[after_idx, ]$lat),
      drawing_mode = drawing_mode,
      routing_server = routing_server
    )

    points_c <-
      segment_c[2:(nrow(segment_c) - 1), ] |>
      mutate(index = row_number() + points_b_idx_max, .before = "lng")

    points_bc <- rbind(points_b, points_c)

    nb_points_bc_after <- nrow(points_bc)
    adj_index_d <- nb_points_bc_after - nb_points_bc_before

    points_d <-
      points_d |>
      mutate(index = index + adj_index_d)

    nodes_d <-
      nodes_d |>
      mutate(index = index + adj_index_d)

    # Preserve all existing attributes; update only coordinates and index
    node_moved <- nodes[node_idx, , drop = FALSE]
    node_moved$lng <- new_lng
    node_moved$lat <- new_lat
    node_moved$index <- points_b_idx_max

    points <- rbind(points_a, points_b, points_c, points_d)
    nodes <- rbind(nodes_a, node_moved, nodes_d)
  }

  row.names(points) <- 1:nrow(points)
  row.names(nodes) <- 1:nrow(nodes)

  list(nodes = nodes, points = points)
}
