# tests/testthat/test-ssfs-to-gtfs.R

test_that("ssfs_to_gtfs returns an object of class gtfs", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  expect_true(inherits(gtfs, "gtfs"))
})

test_that("ssfs_to_gtfs output contains all required GTFS tables", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  required_tables <- c(
    "agency",
    "calendar",
    "routes",
    "shapes",
    "stop_times",
    "stops",
    "trips"
  )
  for (tbl in required_tables) {
    expect_true(tbl %in% names(gtfs), info = paste("Missing table:", tbl))
  }
})

test_that("ssfs_to_gtfs produces non-empty trips and stop_times", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  expect_gt(nrow(gtfs$trips), 0)
  expect_gt(nrow(gtfs$stop_times), 0)
})

test_that("stop_times has required columns", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  required_cols <- c(
    "trip_id",
    "arrival_time",
    "departure_time",
    "stop_id",
    "stop_sequence"
  )
  for (col in required_cols) {
    expect_true(
      col %in% colnames(gtfs$stop_times),
      info = paste("Missing column in stop_times:", col)
    )
  }
})

test_that("trips has required columns", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  required_cols <- c(
    "trip_id",
    "route_id",
    "service_id",
    "trip_headsign",
    "direction_id",
    "shape_id"
  )
  for (col in required_cols) {
    expect_true(
      col %in% colnames(gtfs$trips),
      info = paste("Missing column in trips:", col)
    )
  }
})

test_that("all trip_ids in stop_times exist in trips", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  stop_time_trip_ids <- unique(gtfs$stop_times$trip_id)
  trip_ids <- unique(gtfs$trips$trip_id)
  expect_true(all(stop_time_trip_ids %in% trip_ids))
})

test_that("all stop_ids in stop_times exist in stops", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  stop_time_stop_ids <- unique(gtfs$stop_times$stop_id)
  stop_ids <- unique(gtfs$stops$stop_id)
  expect_true(all(stop_time_stop_ids %in% stop_ids))
})

test_that("all route_ids in trips exist in routes", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  trip_route_ids <- unique(gtfs$trips$route_id)
  route_ids <- unique(gtfs$routes$route_id)
  expect_true(all(trip_route_ids %in% route_ids))
})

test_that("departure times are in HH:MM:SS format", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  # GTFS allows hours > 23 for service past midnight
  time_pattern <- "^\\d{2}:\\d{2}:\\d{2}$"
  sample_times <- head(gtfs$stop_times$departure_time, 50)
  expect_true(all(grepl(time_pattern, sample_times)))
})

test_that("stop_sequence is monotonically increasing within each trip", {
  gtfs <- ssfs_to_gtfs(mileend)
  st <- as.data.frame(gtfs$stop_times)
  # Check a few trips
  trip_ids <- unique(st$trip_id)[1:min(5, length(unique(st$trip_id)))]
  for (tid in trip_ids) {
    seqs <- st$stop_sequence[st$trip_id == tid]
    expect_equal(
      seqs,
      sort(seqs),
      info = paste("Non-monotonic stop_sequence in trip:", tid)
    )
  }
})

test_that("dist_traveled = TRUE adds shape_dist_traveled columns", {
  gtfs <- ssfs_to_gtfs(ligne_jaune, dist_traveled = TRUE)
  expect_true("shape_dist_traveled" %in% colnames(gtfs$stop_times))
  expect_true("shape_dist_traveled" %in% colnames(gtfs$shapes))
})

#can also add a test for testing that dist_traveled = FALSE
#omits shape_dist_traveled but this seems unnecessary

test_that("ssfs_to_gtfs works with stm_metro (multi-route ssfs)", {
  gtfs <- ssfs_to_gtfs(stm_metro)
  expect_true(inherits(gtfs, "gtfs"))
  expect_gt(nrow(gtfs$trips), 0)
  # Should have multiple routes
  expect_gt(length(unique(gtfs$routes$route_id)), 1)
})
