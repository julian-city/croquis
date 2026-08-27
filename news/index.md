# Changelog

## croquis 0.1-0

Initial public beta release.

### Core features

- Introduced the Simplified Speed and Frequency Specification (SSFS), a
  data structure for simplified transit network creation and editing.
  SSFS objects contain 8 component tables (agency, routes, stops, itin,
  stop_seq, span, hsh, calendar) and can be constructed with
  [`ssfs()`](https://croquis.comotive.net/reference/ssfs.md), validated
  with
  [`validate_ssfs()`](https://croquis.comotive.net/reference/validate_ssfs.md),
  and printed with a compact
  [`print.ssfs()`](https://croquis.comotive.net/reference/print.ssfs.md)
  summary.

- Added
  [`ssfs_to_gtfs()`](https://croquis.comotive.net/reference/ssfs_to_gtfs.md)
  for converting SSFS objects to valid GTFS feeds, with optional
  `shape_dist_traveled` output. Output is compatible with downstream
  analysis tools such as r5r and gtfstools.

- Added
  [`gtfs_to_ssfs()`](https://croquis.comotive.net/reference/gtfs_to_ssfs.md)
  for converting existing GTFS feeds into SSFS format for editing.
  Supports route filtering, date windowing, parallel processing on
  Unix-like systems, and both OSRM and Valhalla routing servers for
  shape generation.

- Added
  [`ssfs_subset()`](https://croquis.comotive.net/reference/ssfs_subset.md)
  for filtering SSFS objects by route or itinerary, with retain or
  remove operations.

### Shiny application

- Launched the Croquis interactive Shiny application via
  [`croquis()`](https://croquis.comotive.net/reference/croquis.md). The
  app provides a complete workflow for transit sketch planning: create
  or import a transit network, manage stops, draw routes along streets
  or in free mode, define schedules with headway-based service patterns,
  and export to GTFS or save as a Croquis project file (.rds).

- Modules include: Home (project setup, agency management, GTFS/project
  import), Stops (create, edit, import/export, auto-generate from OSM),
  Routes (route and itinerary management, stop sequences, network or
  free drawing mode), Schedule (service calendars, service windows,
  headways and speeds by hour, speed recalculation), and Export/Save.

- Internationalization (i18n) support for English, French, and Spanish
  across the full interface, with live language switching.

### Calibration engine

- Added
  [`apply_gtfs_speeds_to_ssfs()`](https://croquis.comotive.net/reference/apply_gtfs_speeds_to_ssfs.md)
  for calibrating SSFS speeds using a reference GTFS. Builds a
  temporal-spatial interstop speed matrix and applies observed speeds by
  hour and segment to a target SSFS. Falls back to routing-based speed
  estimates for segments not covered by the reference GTFS.

- Added
  [`gtfs_to_interstop_matrix()`](https://croquis.comotive.net/reference/gtfs_to_interstop_matrix.md)
  and
  [`apply_interstop_matrix_to_ssfs()`](https://croquis.comotive.net/reference/apply_interstop_matrix_to_ssfs.md)
  as lower-level alternatives to
  [`apply_gtfs_speeds_to_ssfs()`](https://croquis.comotive.net/reference/apply_gtfs_speeds_to_ssfs.md).

### Service planning utilities

- Added
  [`generate_tdrh()`](https://croquis.comotive.net/reference/generate_tdrh.md)
  for computing trips, distance, and runtime by hour for specified
  routes or itineraries and services.

- Added
  [`generate_service_cost()`](https://croquis.comotive.net/reference/generate_service_cost.md)
  for computing total daily service hours and service kilometers by
  agency.

### GTFS and SSFS utilities

- Added
  [`gtfs_remove_routes()`](https://croquis.comotive.net/reference/gtfs_remove_routes.md)
  and
  [`gtfs_retain_routes()`](https://croquis.comotive.net/reference/gtfs_retain_routes.md)
  for filtering GTFS objects by route.
- Added
  [`ssfs_subset()`](https://croquis.comotive.net/reference/ssfs_subset.md)
  for filtering SSFS objects by route.

### Bundled datasets

- Included 7 sample datasets: `ligne_jaune` (STM Yellow Line),
  `stm_metro` (full STM metro), `mileend` (STM Mile-End bus subset),
  `ttc_subway` (TTC subway), `translink` (Translink Vancouver),
  `gtfs_rct` (Railway City Transit GTFS), and `ssfs_rct2` (Railway City
  Transit redesign scenario).

### Documentation

- Added a “Produce a GTFS” vignette with a full walkthrough using the
  Toronto Island Ferries as a worked example.
- Added a “SSFS data structure” vignette that provides an overview of
  the data structure introduced by this package designed to facilitate
  rapid network and schedule production and conversion to GTFS.
