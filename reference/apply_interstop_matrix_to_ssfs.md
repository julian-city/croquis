# Apply interstop matrix to ssfs

Using the interstop matrix produced by gtfs_to_interstop_matrix, revises
the speeds detailed in a target ssfs to reflect the speeds detailed in a
reference GTFS. This function represents the second half of
apply_gtfs_speeds_to_ssfs and is included for development purposes.

## Usage

``` r
apply_interstop_matrix_to_ssfs(
  ssfs,
  interstop_matrices_by_service,
  buffer_dist = 10,
  dist_factor = 0.5,
  stop_time = 10,
  osrm_speed_adj_factor = 0.72
)
```

## Arguments

- ssfs:

  A ssfs list

- interstop_matrices_by_service:

  An interstop speed matrix table

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

## Value

A ssfs list

## Examples

``` r
# First, create an interstop speed matrix from a reference GTFS for the Railway City Transit redesign target ssfs
interstop_matrix_rct <- gtfs_to_interstop_matrix(
  gtfs = gtfs_rct,
  ssfs = ssfs_rct2
)
#> calculating interstop distances for GTFS

# Apply the interstop matrix to calibrate ssfs speeds
ssfs_calibrated <- apply_interstop_matrix_to_ssfs(
ssfs = ssfs_rct2,
interstop_matrices_by_service = interstop_matrix_rct
)
#> Generating interstop point buffers
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
#> ℹ Computing SSFS interstop distances (243 interstops across 8 itineraries)
#> ℹ Building SSFS interstop path geometries
#> Intersecting ssfs interstops with gtfs matrix for service_id mon-fri
#> Calculating interstop speeds (service_id mon-fri) ■■                           …
#> Calculating interstop speeds (service_id mon-fri) ■■                           …
#> Calculating interstop speeds (service_id mon-fri) ■■■■                         …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■                      …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■                    …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■                  …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■                …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■               …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■             …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■           …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■        …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■      …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■     …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■   …
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■…
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■…
#> Calculating interstop speeds (service_id mon-fri) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■…
#> 
#> Intersecting ssfs interstops with gtfs matrix for service_id sat
#> Calculating interstop speeds (service_id sat) ■■■■                             …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■                        …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■                     …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■                 …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■             …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■■■■■■        …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    …
#> Calculating interstop speeds (service_id sat) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  …
#> 
#> Intersecting ssfs interstops with gtfs matrix for service_id sun
#> Calculating interstop speeds (service_id sun) ■■                               …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■                         …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■                      …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■                  …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■              …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■■■■         …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■     …
#> Calculating interstop speeds (service_id sun) ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  …
#> 
#> # A tibble: 3 × 2
#>   method  perc
#>   <chr>  <dbl>
#> 1 A       50.4
#> 2 B       11.2
#> 3 D       38.4
```
