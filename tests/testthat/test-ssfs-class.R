# tests/testthat/test-ssfs-class.R

# --- validate_ssfs ---

test_that("validate_ssfs passes on valid bundled ssfs objects", {
  expect_invisible(validate_ssfs(ligne_jaune))
  expect_invisible(validate_ssfs(stm_metro))
  expect_invisible(validate_ssfs(mileend))
  expect_invisible(validate_ssfs(ttc_subway))
})

test_that("validate_ssfs rejects non-list input", {
  expect_error(validate_ssfs("not a list"), "must be a list")
})

test_that("validate_ssfs catches missing tables", {
  bad <- ligne_jaune
  bad$hsh <- NULL
  expect_error(validate_ssfs(bad), "missing required table")

  bad2 <- ligne_jaune
  bad2$agency <- NULL
  bad2$stops <- NULL
  expect_error(validate_ssfs(bad2), "missing required table")
})

test_that("validate_ssfs catches missing columns in agency", {
  bad <- ligne_jaune
  bad$agency$agency_name <- NULL
  expect_error(validate_ssfs(bad), "missing required column")
})

test_that("validate_ssfs catches missing columns in routes", {
  bad <- ligne_jaune
  bad$routes$route_type <- NULL
  expect_error(validate_ssfs(bad), "missing required column")
})

test_that("validate_ssfs catches missing columns in stop_seq", {
  bad <- ligne_jaune
  bad$stop_seq$speed_factor <- NULL
  expect_error(validate_ssfs(bad), "missing required column")
})

test_that("validate_ssfs catches missing columns in hsh", {
  bad <- ligne_jaune
  bad$hsh$headway <- NULL
  expect_error(validate_ssfs(bad), "missing required column")
})

test_that("validate_ssfs catches missing columns in calendar", {
  bad <- ligne_jaune
  bad$calendar$monday <- NULL
  expect_error(validate_ssfs(bad), "missing required column")
})

test_that("validate_ssfs catches missing columns in span", {
  bad <- ligne_jaune
  bad$span$first_dep <- NULL
  expect_error(validate_ssfs(bad), "missing required column")
})

test_that("validate_ssfs catches non-sf stops", {
  bad <- ligne_jaune
  bad$stops <- as.data.frame(bad$stops)
  expect_error(validate_ssfs(bad), "must be an sf object")
})

test_that("validate_ssfs catches non-sf itin", {
  bad <- ligne_jaune
  bad$itin <- as.data.frame(bad$itin)
  expect_error(validate_ssfs(bad), "must be an sf object")
})

# --- new_ssfs ---

test_that("new_ssfs assigns the correct class", {
  obj <- new_ssfs(
    agency = ligne_jaune$agency,
    routes = ligne_jaune$routes,
    stops = ligne_jaune$stops,
    itin = ligne_jaune$itin,
    stop_seq = ligne_jaune$stop_seq,
    span = ligne_jaune$span,
    hsh = ligne_jaune$hsh,
    calendar = ligne_jaune$calendar
  )
  expect_s3_class(obj, "ssfs")
  expect_true(inherits(obj, "list"))
})

test_that("new_ssfs produces an object with all 8 tables", {
  obj <- new_ssfs(
    agency = ligne_jaune$agency,
    routes = ligne_jaune$routes,
    stops = ligne_jaune$stops,
    itin = ligne_jaune$itin,
    stop_seq = ligne_jaune$stop_seq,
    span = ligne_jaune$span,
    hsh = ligne_jaune$hsh,
    calendar = ligne_jaune$calendar
  )
  expected_names <- c(
    "agency",
    "routes",
    "stops",
    "itin",
    "stop_seq",
    "span",
    "hsh",
    "calendar"
  )
  expect_equal(names(obj), expected_names)
})

# --- ssfs() user-facing constructor ---

test_that("ssfs() returns a valid ssfs object", {
  obj <- ssfs(
    agency = ligne_jaune$agency,
    routes = ligne_jaune$routes,
    stops = ligne_jaune$stops,
    itin = ligne_jaune$itin,
    stop_seq = ligne_jaune$stop_seq,
    span = ligne_jaune$span,
    hsh = ligne_jaune$hsh,
    calendar = ligne_jaune$calendar
  )
  expect_s3_class(obj, "ssfs")
})

test_that("ssfs() rejects invalid input", {
  expect_error(
    ssfs(
      agency = ligne_jaune$agency,
      routes = ligne_jaune$routes,
      stops = as.data.frame(ligne_jaune$stops), # not sf
      itin = ligne_jaune$itin,
      stop_seq = ligne_jaune$stop_seq,
      span = ligne_jaune$span,
      hsh = ligne_jaune$hsh,
      calendar = ligne_jaune$calendar
    ),
    "must be an sf object"
  )
})

# --- print.ssfs ---

test_that("print.ssfs produces output without error", {
  expect_output(print(ligne_jaune), "agency")
  expect_output(print(ligne_jaune), "routes")
  expect_output(print(ligne_jaune), "stops")
})

#not sure about this test yet
#test_that("print.ssfs returns the object invisibly", {
#  out <- withr::with_output_sink(tempfile(), print(ligne_jaune))
#  expect_s3_class(out, "ssfs")
#})
