test_that("the remainder is the state total minus published and imputed counts", {
  local_fake_sources(
    state = fake_state_atlas(no_county = 500, suppressed = character(0))
  )
  county <- impute_suppressed(
    fake_county_atlas(suppressed = "01003"),
    cap = Inf
  )
  report <- no_county_cases(county)

  expect_equal(report$state, c("Alabama", "Alabama"))
  expect_equal(report$state_cases, c(32, 232) + 500)
  expect_equal(report$published_cases, c(11, 111))
  # Imputed: the state rate includes the no-county cases.
  expect_equal(report$imputed_cases, (c(32, 232) + 500) / 20)
  expect_equal(
    report$no_county_cases,
    report$state_cases - report$published_cases - report$imputed_cases
  )
})

test_that("the report has one row per state, indicator, year and stratum", {
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "race", years = 2021:2022)
  )
  county <- impute_suppressed(
    fake_county_atlas("race", years = 2021:2022),
    method = "zero"
  )
  report <- no_county_cases(county)

  expect_equal(nrow(report), 2 * 2 * 7)
  expect_equal(report$no_county_cases, rep(0, nrow(report)))
  expect_named(report, c(
    "state_fips", "state", "indicator", "year", "race_ethnicity", "sex",
    "age", "state_cases", "published_cases", "unpublished_cases",
    "imputed_cases", "no_county_cases"
  ))
})

test_that("a suppressed state total gives NA", {
  state <- fake_state_atlas()
  state$cases[1] <- NA
  local_fake_sources(state = state)
  report <- no_county_cases(impute_suppressed(fake_county_atlas()))

  expect_equal(report$no_county_cases, c(NA, 0))
})

test_that("each state's remainder uses its own total", {
  two <- two_state_atlas()
  local_fake_sources(state = two$state)
  report <- no_county_cases(impute_suppressed(two$county, method = "zero"))

  expect_equal(report$state, rep(c("Alabama", "Alaska"), each = 2))
  expect_equal(report$no_county_cases, c(21, 121, 321, 421))
})

test_that("before imputation, only the published gap is reported", {
  local_fake_sources(
    state = fake_state_atlas(no_county = 500, suppressed = character(0))
  )
  report <- no_county_cases(fake_county_atlas(suppressed = "01003"))

  expect_equal(report$unpublished_cases, c(32, 232) + 500 - c(11, 111))
  expect_equal(report$imputed_cases, c(NA_real_, NA_real_))
  expect_equal(report$no_county_cases, c(NA_real_, NA_real_))
})

test_that("rows impute_suppressed() couldn't estimate count towards the remainder", {
  county <- fake_county_atlas(suppressed = "01003")
  county$population <- NA
  local_fake_sources(state = fake_state_atlas(no_county = 500))
  not_estimated <- suppressWarnings(impute_suppressed(county))

  expect_snapshot(report <- no_county_cases(not_estimated))
  expect_equal(report$imputed_cases, c(0, 0))
  expect_equal(report$no_county_cases, c(32, 232) + 500 - c(11, 111))
})

test_that("no_county_cases() refuses published rows with no count and state data", {
  local_fake_sources()
  county <- impute_suppressed(fake_county_atlas())
  county$cases[1] <- NA
  expect_snapshot(error = TRUE, no_county_cases(county))
  expect_snapshot(error = TRUE, no_county_cases(fake_state_atlas()))
})

test_that("kept 'Data not available' rows count towards the remainder", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"
  local_fake_sources()

  kept <- no_county_cases(impute_suppressed(county, not_available = "keep"))
  dropped <- no_county_cases(suppressMessages(impute_suppressed(county)))

  expect_equal(kept, dropped)
  expect_equal(kept$no_county_cases, c(21, 121))
})
