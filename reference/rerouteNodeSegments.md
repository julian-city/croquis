# Reroute segments around a moved node

Recalculates the route point geometry when a node (stop or waypoint) is
moved to new coordinates. Handles all positional cases: single node,
first node, last node, and middle node.

## Usage

``` r
rerouteNodeSegments(
  nodes,
  points,
  node_idx,
  new_lng,
  new_lat,
  drawing_mode,
  routing_server
)
```

## Arguments

- nodes:

  Data frame of route nodes with at minimum: `node_id`, `lng`, `lat`,
  `is_stop`, `stop_id`, `stop_name`, `speed_factor`, `index`.

- points:

  Data frame of route points with columns: `index`, `lng`, `lat`.

- node_idx:

  Integer. Row index (not `node_id`) of the node being moved.

- new_lng:

  Numeric. New longitude for the node.

- new_lat:

  Numeric. New latitude for the node.

- drawing_mode:

  Character. Either `"network"` or `"free"`.

- routing_server:

  Character. The routing server to use (e.g. `"Valhalla"`, `"OSRM"`).

## Value

A list with elements `nodes` and `points`, each a data frame with
updated coordinates and indices. Row names are reset to `1:nrow(...)`.

## Details

All existing node attributes (node_id, is_stop, stop_id, etc.) are
preserved on the moved node. Callers that need to change attributes
(e.g., converting a waypoint to a stop) should modify the returned
`nodes` data frame at row `node_idx` after the call.
