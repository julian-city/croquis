# Build a local metric CRS centred on a geometry

Azimuthal equidistant projection centred on the bounding box of `x`.
Handles itineraries that cross the antimeridian.

## Usage

``` r
local_metric_crs(x)
```

## Arguments

- x:

  An `sf` or `sfc` object.

## Value

A PROJ string.
