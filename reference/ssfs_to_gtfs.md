# Convert a SSFS to a GTFS

Reads a SSFS (Simplified Speed and Frequency Specification) and converts
it into a GTFS.

## Usage

``` r
ssfs_to_gtfs(ssfs, dist_traveled = FALSE)
```

## Arguments

- ssfs:

  An ssfs list

- dist_traveled:

  When TRUE, adds shape_dist_traveled field to shapes and stop_times
  tables in output GTFS

## Value

an object of class 'gtfs'

## Examples

``` r
# Convert the sample Ligne Jaune SSFS to GTFS
gtfs <- ssfs_to_gtfs(ligne_jaune)
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
#> Calculating stop times for itin_id 4_0_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 4_1_1,service_id sat and service_window 1 ■■…
#> Calculating stop times for itin_id 4_1_1,service_id sun and service_window 1 ■■…
#> 

# \donttest{
# Include shape_dist_traveled (increases processing time)
gtfs_with_dist <- ssfs_to_gtfs(ligne_jaune, dist_traveled = TRUE)
#> Warning: repeating attributes for all sub-geometries for which they may not be constant
#> Calculating stop times for itin_id 4_0_1,service_id sun and service_window 1 ■■…
#> Calculating stop times for itin_id 4_1_1,service_id sun and service_window 1 ■■…
#> 
# }
```
