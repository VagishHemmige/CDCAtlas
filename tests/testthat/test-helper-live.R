# The live tests share downloads through helper-live.R. These offline checks
# make sure sharing keeps each download's warnings and failures visible to
# every test that uses it, whichever test ran first.

local_fresh_live_cache <- function(env = parent.frame()) {
  withr::defer(
    {
      rm(list = ls(live_counties), envir = live_counties)
      rm(list = ls(live_runs), envir = live_runs)
    },
    envir = env
  )
}

test_that("live_county() downloads once and repeats its warnings to each caller", {
  local_fresh_live_cache()
  calls <- 0
  local_mocked_bindings(get_atlas = function(...) {
    calls <<- calls + 1
    warning("download warning")
    data.frame(fips = "01001")
  })

  first <- capture_warnings(out_first <- live_county(1999))
  second <- capture_warnings(out_second <- live_county(1999))

  expect_equal(calls, 1)
  expect_equal(first, "download warning")
  expect_equal(second, "download warning")
  expect_equal(out_second, out_first)
})

test_that("live_county() remembers a failed download instead of retrying", {
  local_fresh_live_cache()
  calls <- 0
  local_mocked_bindings(get_atlas = function(...) {
    calls <<- calls + 1
    stop("CDC is down")
  })

  expect_error(live_county(1999), "CDC is down")
  expect_error(live_county(1999), "CDC is down")
  expect_equal(calls, 1)
})

test_that("live_pipeline() reports warnings from a download made earlier", {
  skip_if_not_installed("tidycensus")
  withr::local_envvar(CENSUS_API_KEY = "not-a-real-key")
  local_fresh_live_cache()
  local_mocked_bindings(
    get_atlas = function(...) {
      warning("download warning")
      data.frame(fips = "01001")
    },
    impute_suppressed = function(x, ...) x,
    extrapolate_from_county = function(x, ...) x
  )

  suppressWarnings(live_county(1999))
  run <- live_pipeline(1999)

  expect_contains(run$warnings, "download warning")
})

# The check of CDC's rates on suppressed rows (#157) -----------------------------

test_that("expect_no_rate_on_suppressed() passes when suppressed rows have no rate", {
  county <- with_fake_rates(fake_county_atlas("sex", suppressed = "01003"))

  expect_success(expect_no_rate_on_suppressed(county))
  # The number of rows checked: both sexes and both indicators in 01003.
  expect_equal(expect_no_rate_on_suppressed(county), 4)
})

test_that("expect_no_rate_on_suppressed() names each suppressed row with a rate", {
  county <- with_fake_rates(fake_county_atlas("sex", suppressed = "01003"))
  first <- county$fips == "01003" &
    county$sex == "Male" &
    county$indicator == "HIV diagnoses"
  second <- county$fips == "01003" &
    county$sex == "Female" &
    county$indicator == "HIV prevalence"
  county$rate100000[first] <- 12.5
  county$upperci_rate[second] <- 3

  stratum <- function(sex) {
    paste0("All races/ethnicities, ", sex, ", Ages 13 years and older")
  }
  expected <- c(
    "made-up 2022: 2 of 4 suppressed rows with no count have a rate",
    paste0(
      "* 01003 | HIV diagnoses | 2022 | ",
      stratum("Male"),
      ": rate100000 = 12.5"
    ),
    paste0(
      "* 01003 | HIV prevalence | 2022 | ",
      stratum("Female"),
      ": upperci_rate = 3"
    )
  )
  for (line in expected) {
    expect_failure(
      expect_no_rate_on_suppressed(county, "made-up 2022"),
      line,
      fixed = TRUE
    )
  }
  expect_failure(
    expect_no_rate_on_suppressed(county, "made-up 2022", max_shown = 1),
    "rate100000 = 12.5\n... and 1 more",
    fixed = TRUE
  )
})

test_that("expect_no_rate_on_suppressed() ignores rates on rows it isn't about", {
  # A rate with a published count, and a rate on a row with no count that
  # isn't marked suppressed, are not this check's business.
  county <- with_fake_rates(
    fake_county_atlas(suppressed = "01003"),
    rate_without_count = TRUE
  )
  not_available <- county$fips == "01003" &
    county$indicator == "HIV prevalence"
  county$data_status[not_available] <- "Data not available"
  county[county$fips == "01003" & !not_available, rate_columns] <- NA

  expect_success(expect_no_rate_on_suppressed(county))
})

test_that("expect_no_rate_on_suppressed() fails when there is nothing to check", {
  county <- with_fake_rates(fake_county_atlas())

  expect_failure(
    expect_no_rate_on_suppressed(county),
    "nothing was checked"
  )
  expect_failure(
    expect_no_rate_on_suppressed(county[setdiff(
      names(county),
      "lowerci_rate"
    )]),
    "no column lowerci_rate"
  )
})

test_that("live_county_if_released() gives NULL for a year CDC has no data for", {
  local_fresh_live_cache()
  local_mocked_bindings(get_atlas = function(disease, geography, year, ...) {
    if (year == 1999) {
      # The message .request_atlas_internal() stops with.
      cli::cli_abort("CDC AtlasPlus returned no data for this request.")
    }
    if (year == 1998) {
      stop("CDC is down")
    }
    cli::cli_warn("CDC returned no rows for 1 requested indicator-year:")
    warning("another warning")
    data.frame(fips = "01001")
  })

  expect_null(live_county_if_released(1999))
  expect_error(live_county_if_released(1998), "CDC is down")
  expect_warning(out <- live_county_if_released(1997), "another warning")
  expect_equal(out, data.frame(fips = "01001"))
})

test_that("latest_year_with_rows() steps back to the latest year with rows", {
  rows <- function(indicators) data.frame(indicator = factor(indicators))
  responses <- list(
    "2025" = rows("HIV diagnoses"),
    "2024" = rows(c("HIV diagnoses", "HIV prevalence")),
    "2023" = NULL
  )

  expect_equal(
    latest_year_with_rows(
      responses,
      c("HIV diagnoses", "HIV prevalence", "Chlamydia")
    ),
    c("HIV diagnoses" = 2025, "HIV prevalence" = 2024, "Chlamydia" = NA)
  )
  expect_equal(
    latest_year_with_rows(list(), "HIV diagnoses"),
    c("HIV diagnoses" = NA_real_)
  )
})
