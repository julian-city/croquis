# Retain routes from GTFS

Reads a GTFS and returns a subset GTFS including only the data relevant
to specified routes.

## Usage

``` r
gtfs_retain_routes(gtfs, retain_routes)
```

## Arguments

- gtfs:

  An object of class 'gtfs'

- retain_routes:

  A character vector indicating the route_ids you wish to retain in the
  GTFS

## Value

An object of class 'gtfs'

## Examples

``` r
if (FALSE) { # \dontrun{
# Import GTFS
gtfs <- gtfstools::read_gtfs("path/to/gtfs.zip")

# filter to retain specified routes
gtfs_filtered <- gtfs_retain_routes(gtfs,retain_routes = c("route_1","route_2"))
} # }
```
