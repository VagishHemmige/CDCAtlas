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

# Years (#16) ---------------------------------------------------------------------

test_that("the latest year comes from CDC's code table", {
  names <- varvals$name[varvals$vtid == 2]
  years <- as.numeric(names[grepl("^[0-9]{4}$", names)])
  expect_equal(.latest_atlas_year(), 2025)
  expect_equal(.latest_atlas_year(), max(years))
})

test_that("years up to the latest one in the code table are accepted", {
  payloads <- NULL
  local_mocked_bindings(.request_atlas_all = function(p) {
    payloads <<- p
    fake_county_atlas()
  })
  get_atlas("hiv", "state", .latest_atlas_year())
  ids <- as.numeric(strsplit(payloads[[1]]$VariableIDs, ",")[[1]])
  expect_contains(ids, .latest_atlas_year() - 1478)
})

test_that("years outside the code table are refused, naming the range", {
  local_mocked_bindings(.latest_atlas_year = function() 2025)
  local_mocked_bindings(.request_atlas_all = function(p) stop("CDC was called"))
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 2026))
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 1999))
})
