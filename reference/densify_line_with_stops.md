# Insert located stop positions as vertices of a line

Positions within `snap_tol` metres of an existing vertex reuse that
vertex, so re-running on an already anchored line is idempotent (GTFS
round trips do not keep adding points).

## Usage

``` r
densify_line_with_stops(line_xy, loc, foot_xy, seg_len, snap_tol = 0.5)
```

## Arguments

- line_xy:

  Numeric matrix of the line in its OUTPUT coordinates.

- loc:

  Output of locate_stops_on_line()

- foot_xy:

  Numeric matrix of foot points in OUTPUT coordinates.

- seg_len:

  Numeric vector of segment lengths in metres.

- snap_tol:

  Numeric. Metres.

## Value

A list with `xy` (densified coordinates) and `anchor` (vertex index of
each stop in `xy`).
