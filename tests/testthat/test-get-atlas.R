test_that("get_atlas(extrapolate_to_tract) points to the new functions", {
  local_mocked_bindings(.request_atlas_all = function(payloads) stop("CDC was called"))

  expect_snapshot(
    error = TRUE,
    get_atlas("hiv", "county", 2022, extrapolate_to_tract = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    get_atlas("hiv", "county", 2022, extrapolate_to_tract = FALSE)
  )
})

test_that("get_atlas() only downloads", {
  local_mocked_bindings(
    .request_atlas_all = function(payloads) fake_county_atlas()
  )
  expect_equal(get_atlas("hiv", "county", 2022), fake_county_atlas())
})

test_that("an old positional extrapolate_to_tract still stops before any request", {
  local_mocked_bindings(.request_atlas_all = function(payloads) stop("CDC was called"))
  expect_snapshot(get_atlas("hiv", "county", 2022, NULL, TRUE), error = TRUE)
  expect_snapshot(get_atlas("hiv", "county", 2022, NULL, FALSE), error = TRUE)
})

test_that("geographies CDC doesn't report for a disease are refused", {
  local_mocked_bindings(.request_atlas_all = function(p) stop("CDC was called"))
  expect_snapshot(error = TRUE, get_atlas("hepatitis", "county", 2022))
  expect_snapshot(error = TRUE, get_atlas("congenital syphilis", "county", 2022))
  expect_snapshot(error = TRUE, get_atlas("chlamydia", "msa", 2022))
})

test_that("geographies CDC reports for a disease are requested", {
  requested <- 0
  local_mocked_bindings(.request_atlas_all = function(p) {
    requested <<- requested + 1
    fake_county_atlas()
  })
  get_atlas("hiv", "msa", 2022)
  get_atlas("estimate", "county", 2022)
  get_atlas("tuberculosis", "county", 2022)
  get_atlas("hepatitis", "state", 2022)
  expect_equal(requested, 4)
})
