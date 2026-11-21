# The live tests share downloads through helper-live.R. These offline checks
# make sure sharing keeps each download's warnings and failures visible to
# every test that uses it, whichever test ran first.

local_fresh_live_cache <- function(env = parent.frame()) {
  withr::defer(
    {
      rm(list = ls(live_counties), envir = live_counties)
      rm(list = ls(live_states), envir = live_states)
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

# What .request_atlas_internal() would return for one request, made up: one
# row per indicator asked for, in the state asked for (county requests) or in
# Alabama (state requests). Stands in for CDC below the real get_atlas(), so
# the county download is still split by state and combined as usual.
fake_cdc_rows <- function(payload, indicators = c(203, 205)) {
  ids <- as.numeric(strsplit(payload$VariableIDs, ",")[[1]])
  indicators <- intersect(indicators, ids)
  if (length(indicators) == 0) {
    cli::cli_abort(
      "CDC AtlasPlus returned no data for this request.",
      call = NULL
    )
  }
  state <- live_payload_state(payload)
  fips <- if (is.na(state)) "01" else paste0(gsub("\\D", "", state), "001")
  data.frame(
    indicator = factor(varvals$name[match(indicators, varvals$id)]),
    year = intersect(ids, varvals$id[varvals$vtid == 2]) + 1478,
    geography = factor(paste("Place", fips)),
    data_status = factor("Data suppressed"),
    race_ethnicity = factor("All races/ethnicities"),
    sex = factor("Both sexes"),
    age = factor("Ages 13 years and older"),
    transmission = factor("All transmission categories"),
    rate100000 = NA_real_,
    cases = NA_real_,
    population = 50000,
    lowerci_rate = NA_real_,
    upperci_rate = NA_real_,
    rse = NA_real_,
    lowerci_cases = NA_real_,
    upperci_cases = NA_real_,
    fips = fips
  )
}

test_that("a county download that fails in one state is not read as an unreleased year (#157)", {
  # CDC has released the year (the state request has rows) and Alabama's
  # counties download, but Alaska's request returns no data. The real
  # get_atlas() stops at the first failing state. That must not look like a
  # year CDC hasn't released, or the latest-years check would quietly move to
  # an earlier year.
  local_fresh_live_cache()
  requests <- character()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    state <- live_payload_state(payload)
    requests <<- c(requests, state)
    if (state %in% "Alaska (02)") {
      cli::cli_abort(
        "CDC AtlasPlus returned no data for this request.",
        call = NULL
      )
    }
    fake_cdc_rows(payload)
  })

  expect_snapshot(error = TRUE, live_county_if_released(2025, "sex"))
  # The state request, then Alabama's and Alaska's counties.
  expect_equal(requests, c(NA, "Alabama (01)", "Alaska (02)"))
})

test_that("live_county_if_released() gives NULL only when CDC has no state rows for the year", {
  local_fresh_live_cache()
  requests <- character()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    requests <<- c(requests, live_payload_state(payload))
    cli::cli_abort(
      "CDC AtlasPlus returned no data for this request.",
      call = NULL
    )
  })

  expect_null(live_county_if_released(2025))
  # One state request; the counties are not asked for.
  expect_equal(requests, NA_character_)
})

test_that("live_county_if_released() returns a partly released year without a warning", {
  # Diagnoses but not prevalence, for states and counties alike (#149).
  local_fresh_live_cache()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    fake_cdc_rows(payload, indicators = 203)
  })

  expect_no_warning(county <- live_county_if_released(2025))
  expect_equal(as.character(unique(county$indicator)), "HIV diagnoses")
  expect_equal(length(unique(substr(county$fips, 1, 2))), 52)
  expect_equal(live_released_indicators(2025), "HIV diagnoses")
})

test_that("live_county_if_released() stops when counties lack an indicator states have", {
  local_fresh_live_cache()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    county_request <- !is.na(live_payload_state(payload))
    fake_cdc_rows(
      payload,
      indicators = if (county_request) 203 else c(203, 205)
    )
  })

  expect_snapshot(error = TRUE, live_county_if_released(2025))
})

test_that("live_county_if_released() passes on other failures of the state request", {
  local_fresh_live_cache()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    stop("CDC is down")
  })

  expect_error(live_county_if_released(2025), "CDC is down")
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
