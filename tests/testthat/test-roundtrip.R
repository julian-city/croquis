# tests/testthat/test-roundtrip.R

# Round-trip test: ssfs -> gtfs -> ssfs
# Verifies that key structural properties survive the conversion cycle.

test_that("round-trip preserves number of routes", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  result <- gtfs_to_ssfs(gtfs)
  expect_equal(nrow(result$routes), nrow(ligne_jaune$routes))
})

test_that("round-trip preserves route IDs", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  result <- gtfs_to_ssfs(gtfs)
  expect_equal(
    sort(unique(result$routes$route_id)),
    sort(unique(ligne_jaune$routes$route_id))
  )
})

test_that("round-trip preserves stop IDs", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  result <- gtfs_to_ssfs(gtfs)
  expect_equal(
    sort(unique(result$stop_seq$stop_id)),
    sort(unique(ligne_jaune$stop_seq$stop_id))
  )
})

test_that("round-trip produces a valid ssfs", {
  gtfs <- ssfs_to_gtfs(ligne_jaune)
  result <- gtfs_to_ssfs(gtfs)
  expect_no_error(validate_ssfs(result))
})

test_that("round-trip works for multi-route network", {
  gtfs <- ssfs_to_gtfs(stm_metro)
  result <- gtfs_to_ssfs(gtfs)
  expect_no_error(validate_ssfs(result))
  expect_equal(
    sort(unique(result$routes$route_id)),
    sort(unique(stm_metro$routes$route_id))
  )
})
