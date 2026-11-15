test_that("get_atlas(extrapolate_to_tract) points to the new functions", {
  local_mocked_bindings(.request_atlas_all = function(payloads) {
    stop("CDC was called")
  })

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

test_that("an old positional extrapolate_to_tract still stops before any request", {
  local_mocked_bindings(.request_atlas_all = function(payloads) {
    stop("CDC was called")
  })
  expect_snapshot(get_atlas("hiv", "county", 2022, NULL, TRUE), error = TRUE)
  expect_snapshot(get_atlas("hiv", "county", 2022, NULL, FALSE), error = TRUE)
})

test_that("geographies CDC doesn't report for a disease are refused", {
  local_mocked_bindings(.request_atlas_all = function(p) stop("CDC was called"))
  expect_snapshot(error = TRUE, get_atlas("hepatitis", "county", 2022))
  expect_snapshot(
    error = TRUE,
    get_atlas("congenital syphilis", "county", 2022)
  )
  expect_snapshot(error = TRUE, get_atlas("chlamydia", "msa", 2022))
})

test_that("geographies CDC reports for a disease are requested", {
  requested <- 0
  local_mocked_bindings(.request_atlas_all = function(p) {
    requested <<- requested + 1
    fake_county_atlas()
  })
  suppressWarnings({
    get_atlas("hiv", "msa", 2022)
    get_atlas("estimate", "county", 2022)
    get_atlas("tuberculosis", "county", 2022)
    get_atlas("hepatitis", "state", 2022)
  })
  expect_equal(requested, 4)
})

test_that("congenital syphilis can't be stratified", {
  local_mocked_bindings(.request_atlas_all = function(p) stop("CDC was called"))
  expect_snapshot(
    error = TRUE,
    get_atlas("congenital syphilis", "state", 2022, stratify_by = "age")
  )
  expect_snapshot(
    error = TRUE,
    get_atlas("congenital syphilis", "state", 2022, stratify_by = "sex")
  )
})

# Missing indicators (#116) -------------------------------------------------------

test_that("a response missing a requested indicator warns, naming it", {
  # Like HIV in 2025: CDC returned diagnoses but not prevalence.
  local_mocked_bindings(
    .request_atlas_all = function(payloads) {
      fake_county_atlas(indicators = "HIV diagnoses", years = 2024:2025) |>
        rbind(fake_county_atlas(indicators = "HIV prevalence", years = 2024))
    }
  )
  expect_warning(get_atlas("hiv", "county", 2024:2025), "HIV prevalence")
  expect_snapshot(out <- get_atlas("hiv", "county", 2024:2025))
  expect_equal(nrow(out), 6)
})

test_that("a complete response doesn't warn", {
  local_mocked_bindings(
    .request_atlas_all = function(payloads) fake_county_atlas()
  )
  expect_no_warning(get_atlas("hiv", "county", 2022))
})
