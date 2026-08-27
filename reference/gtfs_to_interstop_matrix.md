# Turn GTFS into interstop speed matrix

Creates a table record of every interstop (including geometry) and the
speed of every trip along every interstop in a reference GTFS, for every
service detailed in a target SSFS. This table (the interstop speed
matrix) contains the data necessary to calibrate the speeds of the
target SSFS. This function consists of the first half of the process in
apply_gtfs_speeds_to_ssfs and is included for development purposes

## Usage

``` r
gtfs_to_interstop_matrix(
  gtfs,
  ssfs,
  max_date = NULL,
  accepted_route_types = c(0, 3)
)
```

## Arguments

- gtfs:

  An object with class 'gtfs'

- ssfs:

  A ssfs list

- max_date:

  A date representing the maximum date range of services retained from
  the reference GTFS

- accepted_route_types:

  Route types that can be used to build the reference speed matrix. By
  default, 0 (tramways) and 3 (buses).

## Value

An interstop speed matrix table

## Examples

``` r
# Create interstop speed matrix using a GTFS
interstop_matrix <- gtfs_to_interstop_matrix(
  gtfs = gtfs_rct,
  ssfs = ssfs_rct2
)
#> calculating interstop distances for GTFS
```
