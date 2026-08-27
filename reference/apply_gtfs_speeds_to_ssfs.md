# Speeds calibration engine

Reads a reference GTFS and a target SSFS and returns that SSFS with
speeds adjusted to those documented in the GTFS based on shapes and
stop_times data.

## Usage

``` r
apply_gtfs_speeds_to_ssfs(
  gtfs,
  ssfs,
  max_date = NULL,
  buffer_dist = 10,
  dist_factor = 0.5,
  stop_time = 10,
  osrm_speed_adj_factor = 0.72,
  accepted_route_types = c(0, 3)
)
```

## Arguments

- gtfs:

  An object of class 'gtfs'. Must include a shapes table.

- ssfs:

  A ssfs list

- max_date:

  A date within the range of gtfs\$calendar\$end_date representing the
  maximum of a 7 day range used to build the reference speed matrix from
  the reference GTFS. Leave as NULL to use the last 7 days of the
  reference GTFS

- buffer_dist:

  A distance in meters used to define the radius of interstop speed
  matrix points. Defaults to 10.

- dist_factor:

  A value between 0.1 and 0.9 used to ensure that interstops are only
  applied speeds from reference interstops of a similar length. Defaults
  to 0.5, which means that for a given interstop being calibrated,
  reference interstops with a distance of 50% to 150% can be used to
  calibrate.

- stop_time:

  An integer in seconds, representing the amount of time added per stop
  made for runtimes calculated using OSRM

- osrm_speed_adj_factor:

  A coefficient used to adjust output OSRM runtimes to make them more
  representative of bus runtimes.

- accepted_route_types:

  Route types that can be used to build the reference speed matrix. By
  default, 0 (tramways) and 3 (buses).

## Value

A ssfs list

## Examples

``` r
# Calibrate Railway City Transit redesign SSFS speeds using reference GTFS
ssfs_calibrated <- apply_gtfs_speeds_to_ssfs(gtfs = gtfs_rct, ssfs = ssfs_rct2)
#> calculating interstop distances for GTFS
#> Generating interstop point buffers
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
#> ℹ Computing SSFS interstop distances (243 interstops across 8 itineraries)
#> ℹ Building SSFS interstop path geometries
#> Intersecting ssfs interstops with gtfs matrix for service_id mon-fri
#> Calculating interstop speeds (service_id mon-fri) ■■                           …
#> Calculating interstop speeds (service_id mon-fri) ■■■                          …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■                       …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■                     …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■                  …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■                …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■              …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■            …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■          …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■       …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■     …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■   …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■ …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■…
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■…
#> 
#> Intersecting ssfs interstops with gtfs matrix for service_id sat
#> Calculating interstop speeds (service_id sat) ■■■■■                            …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■                        …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■                   …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■               …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■■■■          …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■     …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  …
#> 
#> Intersecting ssfs interstops with gtfs matrix for service_id sun
#> Calculating interstop speeds (service_id sun) ■■                               …
#> Calculating interstop speeds (service_id sun) ■■■■■■■                          …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■                      …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■                 …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■            …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■■■■■■       …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  …
#> 
#> # A tibble: 3 × 2
#>   method  perc
#>   <chr>  <dbl>
#> 1 A       50.4
#> 2 B       11.2
#> 3 D       38.4
```
