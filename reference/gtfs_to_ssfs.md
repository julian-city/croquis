# Convert a GTFS to a SSFS

Reads a GTFS object and transforms it into a Simplified Speed and
Frequency Specification (SSFS), extracting route geometries, stop
sequences, and service patterns into a more editable format for sketch
planning workflows.

## Usage

``` r
gtfs_to_ssfs(
  gtfs,
  routes = NULL,
  max_date = NULL,
  routing_server = c("OSRM", "Valhalla"),
  workers = 1L
)
```

## Arguments

- gtfs:

  An object of class 'gtfs'. Must contain required tables and calendar
  table.

- routes:

  A character vector of the route id(s) you wish to convert to ssfs.
  Leave as NULL to convert all routes to SSFS

- max_date:

  A date within the range of gtfs\$calendar\$end_date representing the
  maximum of a 7 day range used to build the SSFS. Leave as NULL to use
  the last 7 days specified in gtfs\$calendar to build the SSFS

- routing_server:

  Routing server used to draw shapes in the case where none are provided
  in the input GTFS

- workers:

  Number of worker processes to use for itinerary-scoped shape and
  distance calculations. Values above 1 use parallel workers on
  Unix-like systems and fall back to serial execution on Windows.

## Value

An SSFS list

## Examples

``` r
# \donttest{
# First, create a GTFS from the sample Ligne Jaune network
gtfs <- ssfs_to_gtfs(stm_metro)
#> Calculating trips for route 1 ■■■■                              11% | ETA:  9s
#> Calculating trips for route 1 ■■■■■■                            18% | ETA: 10s
#> Calculating trips for route 2 ■■■■■■■■■■■■■■■■                  50% | ETA:  5s
#> Calculating trips for route 4 ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA:  2s
#> Calculating trips for route 5 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
#> Calculating stop times for itin_id 1_0_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 1_0_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 1_1_1,service_id mon-fri and service_window …
#> Calculating stop times for itin_id 1_1_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 1_1_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 2_0_1,service_id mon-fri and service_window …
#> Calculating stop times for itin_id 2_0_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 2_0_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 2_0_2,service_id mon-fri and service_window …
#> Calculating stop times for itin_id 2_1_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 2_1_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 2_1_2,service_id mon-fri and service_window …
#> Calculating stop times for itin_id 4_0_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 5_0_1,service_id mon-fri and service_window …
#> Calculating stop times for itin_id 5_0_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 5_0_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 5_1_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 5_1_1,service_id sun and service_window 1 ■■…
#> 

# Convert the GTFS back to SSFS
ssfs <- gtfs_to_ssfs(gtfs)
#> Calculating interstop distance

# Convert specific routes only
ssfs <- gtfs_to_ssfs(gtfs, routes = c("1","2"))
#> Calculating interstop distance
# }
```
