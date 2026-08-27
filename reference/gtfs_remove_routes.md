# Remove routes from GTFS

Reads a GTFS and returns a subset GTFS excluding the data relevant to
specified routes.

## Usage

``` r
gtfs_remove_routes(gtfs, remove_routes)
```

## Arguments

- gtfs:

  An object of class 'gtfs'

- remove_routes:

  A character vector of the route_ids that you wish to remove from the
  GTFS

## Value

An object of class 'gtfs'

## Examples

``` r
if (FALSE) { # \dontrun{
# Import GTFS
gtfs <- gtfstools::read_gtfs("path/to/gtfs.zip")

# filter to remove specified routes
gtfs_filtered <- gtfs_remove_routes(gtfs,remove_routes = c("route_3","route_4"))
} # }
```
