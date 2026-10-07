# Anchor the stops of an itinerary on its geometry

Anchor the stops of an itinerary on its geometry

## Usage

``` r
anchor_stops_to_itin(
  line,
  stop_points,
  densify = TRUE,
  max_offset = 150,
  itin_id = NA_character_
)
```

## Arguments

- line:

  An `sfc` holding one LINESTRING (the itin geometry).

- stop_points:

  An `sfc` of POINTs in stop_sequence order.

- densify:

  Logical. Insert projected stop positions as vertices.

- max_offset:

  Numeric. Metres. Stops further than this from the line trigger a
  warning (likely wrong shape or a bad stop location).

- itin_id:

  Character, only used in messages.

## Value

A list with `geometry` (sfc LINESTRING, same CRS as `line`), `measure`
(m along the line for each stop), `offset` (m), `anchor` (vertex index,
NULL when `densify = FALSE`) and `interstop_dist` (m, NA for the last
stop).
