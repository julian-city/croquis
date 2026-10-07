# Locate an ordered set of stops along a polyline

Projects every stop onto every segment of the line, keeps a few
candidate projections per stop (one per "pass" of the line near the
stop), then picks the combination that minimises the summed stop-to-line
offsets under the constraint that measures never decrease with stop
order (dynamic programming). Unlike the greedy ascending-index fix,
early stops can be revised when later stops demand it.

## Usage

``` r
locate_stops_on_line(line_xy, stop_xy, n_candidates = 3L, tol = 1e-06)
```

## Arguments

- line_xy:

  Numeric matrix (n_vertices x 2), metric coordinates.

- stop_xy:

  Numeric matrix (n_stops x 2), metric coordinates, in stop_sequence
  order.

- n_candidates:

  Integer. Candidate projections kept per stop.

- tol:

  Numeric. Tolerance (m) when comparing measures.

## Value

A data.frame with one row per stop: `seg` (segment index), `t` (position
on segment, 0-1), `measure` (m from line start), `offset` (m from stop
to line), `foot_x`, `foot_y`, and `backtrack` (TRUE when no monotone
assignment existed for that stop).
