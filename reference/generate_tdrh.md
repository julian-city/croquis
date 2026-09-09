# Generate trips, distance and runtime by hour (TDRH)

Function that generates a tibble that can be used to calculate service
cost for specified routes or individual route itineraries in terms of
service hours and service kilometers.

## Usage

``` r
generate_tdrh(ssfs, id_type = c("route_id", "itin_id"), id, service)
```

## Arguments

- ssfs:

  A list of class SSFS

- id_type:

  Either "route_id" or "itin_id"

- id:

  A character vector of one or more route_ids or itin_ids

- service:

  An individual string or vector representing one or several service_ids

## Examples

``` r
# Generate table for the 99 B line to view runtimes by hour for all itin ids
b_line_route_id <- translink$routes |> dplyr::filter(route_short_name=="099") |> dplyr::pull(route_id)
generate_tdrh(ssfs=translink, id_type="route_id", id = b_line_route_id, service="mon-fri")
#> # A tibble: 65 × 6
#>    itin_id  service_id hour_dep n_trips len_m runtime
#>    <chr>    <chr>      <chr>      <int> <dbl>   <dbl>
#>  1 6641_0_1 mon-fri    06:00:00       7 13871    38.9
#>  2 6641_0_1 mon-fri    07:00:00      19 13871    42  
#>  3 6641_0_1 mon-fri    08:00:00      15 13871    46.2
#>  4 6641_0_1 mon-fri    09:00:00      15 13871    44.5
#>  5 6641_0_1 mon-fri    10:00:00      12 13871    46.5
#>  6 6641_0_1 mon-fri    11:00:00      10 13871    49  
#>  7 6641_0_1 mon-fri    12:00:00      10 13871    50.4
#>  8 6641_0_1 mon-fri    13:00:00       9 13871    51.1
#>  9 6641_0_1 mon-fri    14:00:00      14 13871    50.4
#> 10 6641_0_1 mon-fri    15:00:00      20 13871    53.4
#> # ℹ 55 more rows
```
