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
