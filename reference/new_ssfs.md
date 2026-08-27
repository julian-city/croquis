# Create a new ssfs object

Low-level constructor. Assembles the 8 component tables into a list and
assigns the `"ssfs"` class. Performs no validation — use
[`validate_ssfs()`](https://croquis.comotive.net/reference/validate_ssfs.md)
or the user-facing
[`ssfs()`](https://croquis.comotive.net/reference/ssfs.md) constructor
for safety checks.

## Usage

``` r
new_ssfs(agency, routes, stops, itin, stop_seq, span, hsh, calendar)
```

## Arguments

- agency:

  A data.frame with columns: agency_id, agency_name, agency_url,
  agency_timezone.

- routes:

  A data.frame with columns: route_id, agency_id, route_short_name,
  route_long_name, route_type, route_color, route_text_color.

- stops:

  An sf data.frame (POINT, CRS 4326) with columns: stop_id, stop_name,
  geometry.

- itin:

  An sf data.frame (LINESTRING, CRS 4326) with columns: itin_id,
  route_id, direction_id, trip_headsign, geometry.

- stop_seq:

  A data.frame with columns: itin_id, stop_id, stop_sequence,
  speed_factor.

- span:

  A data.frame with columns: itin_id, service_id, service_window,
  first_dep, last_dep.

- hsh:

  A data.frame with columns: itin_id, service_id, hour_dep, headway,
  speed.

- calendar:

  A data.frame with columns: service_id, monday, tuesday, wednesday,
  thursday, friday, saturday, sunday, start_date, end_date.

## Value

An object of class `"ssfs"`, which is a named list of the 8 component
tables.
