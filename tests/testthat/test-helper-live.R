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

# CDC's reply to one request as the package receives it, before decoding: an
# httr response holding `json`. Built by hand; nothing in httr is replaced.
fake_cdc_reply <- function(json) {
  structure(
    list(
      url = "https://cdc.invalid",
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      content = charToRaw(as.character(json))
    ),
    class = "response"
  )
}

# CDC's JSON for one request, made up: a suppressed row with no count or rate
# for each of `indicators`, in the first place and the year asked for.
fake_cdc_json <- function(payload, indicators) {
  ids <- as.numeric(strsplit(payload$VariableIDs, ",")[[1]])
  places <- c(lookup[["state"]]$geo_id, lookup[["counties"]]$geo_id)
  rows <- lapply(indicators, function(indicator) {
    list(
      indicator,
      intersect(ids, varvals$id[varvals$vtid == 2]),
      intersect(ids, places)[1],
      405,
      551,
      601,
      650,
      801,
      NA,
      NA,
      50000,
      NA,
      NA,
      NA,
      NA,
      NA
    )
  })
  jsonlite::toJSON(list(sourcedata = rows), auto_unbox = TRUE, na = "null")
}

# Replace CDC at .atlas_call(), the package's own wrapper round the HTTP call
# and the lowest point the package owns. Everything after it is real: the
# status check, the JSON decoding, the missing-`sourcedata` check and the
# decoding of CDC's codes in .request_atlas_internal(), and get_atlas() above
# it. Not run: the HTTP request itself, .atlas_call()'s own handling of
# connection failures, and the wait between requests. `reply_for(payload)`
# gives the JSON for a request; the requests made are returned as they come.
local_fake_cdc_replies <- function(reply_for, env = parent.frame()) {
  seen <- new.env()
  seen$requests <- character()
  local_mocked_bindings(
    client_rate = function() invisible(NULL),
    .atlas_call = function(request) {
      # `request` is the unevaluated HTTP call and is never run. The payload
      # is the one .request_atlas_internal() is sending.
      payload <- get("payload", envir = parent.frame())
      ids <- as.numeric(strsplit(payload$VariableIDs, ",")[[1]])
      state <- live_payload_state(payload)
      seen$requests <- c(
        seen$requests,
        paste(
          intersect(ids, varvals$id[varvals$vtid == 2]) + 1478,
          if (is.na(state)) "states" else state
        )
      )
      fake_cdc_reply(reply_for(payload))
    },
    .env = env
  )
  seen
}

test_that("a state response with no data is not read as an unreleased year (#157)", {
  # CDC answers the state request with valid JSON that has no `sourcedata`.
  # The package reports that as "returned no data" whatever the reason, so it
  # can't establish that the year is unreleased: stop, ask for no counties and
  # don't return NULL, which would send the latest-years check to an earlier
  # year.
  local_fresh_live_cache()
  seen <- local_fake_cdc_replies(function(payload) "{}")

  expect_snapshot(error = TRUE, live_county_if_released(2025, "sex"))
  expect_snapshot(error = TRUE, live_released_indicators(2025, "sex"))
  # One state request, made once; no county request.
  expect_equal(seen$requests, "2025 states")
})

test_that("an indicator steps back a year only when the state response has rows without it (#157)", {
  # As CDC answered in November 2026: 2025 has diagnoses but not prevalence
  # (#149), for states and counties; 2024 has both.
  local_fresh_live_cache()
  seen <- local_fake_cdc_replies(function(payload) {
    ids <- as.numeric(strsplit(payload$VariableIDs, ",")[[1]])
    fake_cdc_json(payload, if ((2025 - 1478) %in% ids) 203 else c(203, 205))
  })
  both <- c("HIV diagnoses", "HIV prevalence")

  expect_equal(live_released_indicators(2025), "HIV diagnoses")
  expect_equal(live_released_indicators(2024), both)
  expect_no_warning(
    responses <- list(
      "2025" = live_county_if_released(2025),
      "2024" = live_county_if_released(2024)
    )
  )
  expect_equal(
    as.character(unique(responses[["2025"]]$indicator)),
    "HIV diagnoses"
  )
  expect_equal(
    latest_year_with_rows(responses, both),
    c("HIV diagnoses" = 2025, "HIV prevalence" = 2024)
  )
  # Each year: one state request, then every state's counties.
  expect_equal(seen$requests[1:2], c("2025 states", "2024 states"))
  expect_equal(
    sum(grepl("^2025", seen$requests)),
    sum(grepl("^2024", seen$requests))
  )
  expect_gt(sum(grepl("^2025", seen$requests)), 50)
})

test_that("a state response for another disease is not read as an unreleased year (#157)", {
  # An HIV request answered with chlamydia rows is a wrong response. It says
  # nothing about HIV, so: stop, ask for no counties, and don't give up on
  # the year.
  local_fresh_live_cache()
  seen <- local_fake_cdc_replies(function(payload) fake_cdc_json(payload, 206))

  expect_snapshot(error = TRUE, live_county_if_released(2025))
  expect_equal(seen$requests, "2025 states")
})

test_that("a state response with another disease beside the one asked for stops too (#157)", {
  local_fresh_live_cache()
  seen <- local_fake_cdc_replies(function(payload) {
    fake_cdc_json(payload, c(203, 206))
  })

  expect_snapshot(error = TRUE, live_county_if_released(2025))
  expect_equal(seen$requests, "2025 states")
})

test_that("a state response with no rows is not read as an unreleased year (#157)", {
  local_fresh_live_cache()
  requests <- character()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    requests <<- c(requests, live_payload_state(payload))
    fake_cdc_rows(payload)[0, ]
  })

  expect_snapshot(error = TRUE, live_county_if_released(2025))
  # One state request; the counties are not asked for.
  expect_equal(requests, NA_character_)
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

test_that("live_county_if_released() stops on any other failure of the state request", {
  local_fresh_live_cache()
  local_mocked_bindings(.request_atlas_internal = function(payload) {
    cli::cli_abort("CDC is down", call = NULL)
  })

  expect_snapshot(error = TRUE, live_county_if_released(2025))
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

test_that("unplaced_warning_names() looks for the state in one indicator's entry (#164)", {
  # Made-up warnings in the shape the pipeline gives.
  warning <- function(diagnoses, prevalence) {
    paste(
      "2 states have more than 25% of their published state total in no county row of `x`.",
      paste0("* HIV diagnoses 2022: ", diagnoses),
      paste0("* HIV prevalence 2022: ", prevalence),
      "i Percentages are of the state totals CDC published for the strata in `x`.",
      sep = "\n"
    )
  }
  only_diagnoses <- warning("Alaska (87%)", "Idaho (47%)")
  in_prevalence <- warning("Idaho (47%)", "Rhode Island (73%) and Alaska (87%)")

  # Alaska is somewhere in both warnings.
  expect_match(only_diagnoses, "Alaska \\(")
  expect_match(in_prevalence, "Alaska \\(")
  expect_false(
    unplaced_warning_names(only_diagnoses, "HIV prevalence 2022", "Alaska")
  )
  expect_true(
    unplaced_warning_names(in_prevalence, "HIV prevalence 2022", "Alaska")
  )
  expect_true(
    unplaced_warning_names(only_diagnoses, "HIV diagnoses 2022", "Alaska")
  )
  # Another year's entry, or no entry at all, is not a match.
  expect_false(
    unplaced_warning_names(in_prevalence, "HIV prevalence 2021", "Alaska")
  )
  # A wrapped entry and a different bullet are read the same way.
  wrapped <- sub(
    " and Alaska",
    " and\n  Alaska",
    gsub("* ", "\u2022 ", in_prevalence, fixed = TRUE),
    fixed = TRUE
  )
  expect_true(unplaced_warning_names(wrapped, "HIV prevalence 2022", "Alaska"))
})
