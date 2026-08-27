# Create an ssfs object

User-facing constructor for SSFS objects. Called with no arguments, it
returns a valid empty skeleton with the correct column types - the
simplest way to start building a transit network programmatically.
Called with one or more table arguments, it assembles and validates them
into a complete ssfs object.

## Usage

``` r
ssfs(
  agency = NULL,
  routes = NULL,
  stops = NULL,
  itin = NULL,
  stop_seq = NULL,
  span = NULL,
  hsh = NULL,
  calendar = NULL
)
```

## Arguments

- agency:

  A data.frame with columns: agency_id, agency_name, agency_url,
  agency_timezone. Defaults to an empty data.frame with these columns.

- routes:

  A data.frame with columns: route_id, agency_id, route_short_name,
  route_long_name, route_type, route_color, route_text_color. Defaults
  to an empty data.frame with these columns.

- stops:

  An sf data.frame (POINT, CRS 4326) with columns: stop_id, stop_name,
  geometry. Defaults to an empty sf with these columns.

- itin:

  An sf data.frame (LINESTRING, CRS 4326) with columns: itin_id,
  route_id, direction_id, trip_headsign, geometry. Defaults to an empty
  sf with these columns.

- stop_seq:

  A data.frame with columns: itin_id, stop_id, stop_sequence,
  speed_factor. Defaults to an empty data.frame with these columns.

- span:

  A data.frame with columns: itin_id, service_id, service_window,
  first_dep, last_dep. Defaults to an empty data.frame with these
  columns.

- hsh:

  A data.frame with columns: itin_id, service_id, hour_dep, headway,
  speed. Defaults to an empty data.frame with these columns.

- calendar:

  A data.frame with columns: service_id, monday, tuesday, wednesday,
  thursday, friday, saturday, sunday, start_date, end_date. Defaults to
  an empty data.frame with these columns.

## Value

A validated object of class `"ssfs"`.

## Examples

``` r
# Empty skeleton - start from scratch
my_ssfs <- ssfs()
my_ssfs
#> <ssfs> Simplified Speed and Frequency Specification
#>   Agencies:      0 
#>   Routes:        0 
#>   Itineraries:   0 
#>   Stops:         0 
#>   Service IDs:   0 

# Assemble from pre-built tables
my_ssfs <- ssfs(
  agency = ligne_jaune$agency,
  routes = ligne_jaune$routes,
  stops = ligne_jaune$stops,
  itin = ligne_jaune$itin,
  stop_seq = ligne_jaune$stop_seq,
  span = ligne_jaune$span,
  hsh = ligne_jaune$hsh,
  calendar = ligne_jaune$calendar
)
```
