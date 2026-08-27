# Generate total daily costs in service hours and service kilometers

Outputs a small tibble of costs for the specified service(s) and
itin_id(s) or route_id(s)

## Usage

``` r
generate_service_cost(ssfs, id_type = c("route_id", "itin_id"), id, service)
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

## Value

A tibble

## Examples

``` r
# Calculate weekday daily service kilometers and service hours of the 99 B-Line in Vancouver
b_line_route_id <- translink$routes |> dplyr::filter(route_short_name=="099") |> dplyr::pull(route_id)
generate_service_cost(ssfs=translink,id_type="route_id",id=b_line_route_id,service="mon-fri")
#> # A tibble: 1 × 3
#>   agency_id total_km total_h
#>   <chr>        <dbl>   <dbl>
#> 1 TL           7352.    393.
```
