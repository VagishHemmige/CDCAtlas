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

# The counts of suppressed stratum cells (#164) ----------------------------------

# Made-up county rows for the counts below, in get_atlas()'s columns: one row
# per count in `cases`, which is named by county FIPS code. NA is a suppressed
# count unless the county is in `not_available`.
fake_count_rows <- function(
  cases,
  age = "Ages 13 years and older",
  indicator = "HIV prevalence",
  not_available = character()
) {
  data.frame(
    indicator = factor(indicator),
    year = 2022,
    data_status = factor(
      ifelse(
        !is.na(cases),
        "Not Suppressed",
        ifelse(
          names(cases) %in% not_available,
          "Data not available",
          "Data suppressed"
        )
      ),
      levels = c("Not Suppressed", "Data suppressed", "Data not available")
    ),
    race_ethnicity = factor("All races/ethnicities"),
    sex = factor("Both sexes"),
    age = age,
    cases = unname(cases),
    fips = names(cases)
  )
}

# Two made-up responses for one year: unstratified, and by three made-up age
# groups. S is a suppressed count, n/a "Data not available", - no row.
#
# HIV prevalence
# | County | Total | 13-24 | 25-44 | 45+ | What it is                         |
# |--------|-------|-------|-------|-----|------------------------------------|
# | 01001  | 30    | 5     | 10    | 15  | all published, sum equal           |
# | 01003  | 40    | 5     | 10    | 15  | all published, sum less            |
# | 01005  | 20    | 12    | S     | S   | two suppressed cells               |
# | 01007  | 20    | 5     | 9     | S   | exactly one suppressed cell        |
# | 01009  | S     | 5     | S     | S   | total suppressed: left out         |
# | 01011  | 25    | 5     | 10    | -   | a stratum row is absent            |
# | 01013  | 50    | -     | -     | -   | only in the unstratified response  |
# | 01015  | -     | 5     | 10    | 15  | only in the stratified response    |
# | 01017  | 10    | 6     | 7     | 8   | all published, sum more            |
# | 01019  | 10    | 12    | S     | S   | two suppressed; published exceed   |
# | 01021  | 15    | 5     | n/a   | 5   | a cell with no count, not suppressed |
# | 01023  | n/a   | 5     | 10    | 15  | total not available: left out      |
#
# HIV diagnoses
# | 01001  | 3     | 1     | 1     | 1   | all published, sum equal           |
# | 01003  | 9     | S     | S     | 2   | two suppressed cells               |
fake_count_responses <- function() {
  s <- NA_real_
  prevalence <- list(
    total = c(
      "01001" = 30,
      "01003" = 40,
      "01005" = 20,
      "01007" = 20,
      "01009" = s,
      "01011" = 25,
      "01013" = 50,
      "01017" = 10,
      "01019" = 10,
      "01021" = 15,
      "01023" = s
    ),
    "13-24" = c(
      "01001" = 5,
      "01003" = 5,
      "01005" = 12,
      "01007" = 5,
      "01009" = 5,
      "01011" = 5,
      "01015" = 5,
      "01017" = 6,
      "01019" = 12,
      "01021" = 5,
      "01023" = 5
    ),
    "25-44" = c(
      "01001" = 10,
      "01003" = 10,
      "01005" = s,
      "01007" = 9,
      "01009" = s,
      "01011" = 10,
      "01015" = 10,
      "01017" = 7,
      "01019" = s,
      "01021" = s,
      "01023" = 10
    ),
    "45+" = c(
      "01001" = 15,
      "01003" = 15,
      "01005" = s,
      "01007" = s,
      "01009" = s,
      "01015" = 15,
      "01017" = 8,
      "01019" = s,
      "01021" = 5,
      "01023" = 15
    )
  )
  diagnoses <- list(
    total = c("01001" = 3, "01003" = 9),
    "13-24" = c("01001" = 1, "01003" = s),
    "25-44" = c("01001" = 1, "01003" = s),
    "45+" = c("01001" = 1, "01003" = 2)
  )
  ages <- c("13-24", "25-44", "45+")
  strata <- function(counts, ...) {
    do.call(
      rbind,
      lapply(ages, \(age) fake_count_rows(counts[[age]], age, ...))
    )
  }
  list(
    unstratified = rbind(
      fake_count_rows(prevalence$total, not_available = "01023"),
      fake_count_rows(diagnoses$total, indicator = "HIV diagnoses")
    ),
    stratified = rbind(
      strata(prevalence, not_available = "01021"),
      strata(diagnoses, indicator = "HIV diagnoses")
    )
  )
}

# count_suppressed_strata() on the made-up responses, asking for what they
# hold: age, in the three made-up groups.
fake_ages <- c("13-24", "25-44", "45+")
count_fake_strata <- function(
  stratified,
  unstratified,
  stratify_by = "age",
  strata = fake_ages
) {
  count_suppressed_strata(stratified, unstratified, stratify_by, strata)
}

test_that("count_suppressed_strata() counts counties by their suppressed stratum cells", {
  fake <- fake_count_responses()

  counts <- count_fake_strata(fake$stratified, fake$unstratified)

  expect_equal(
    counts,
    data.frame(
      indicator = c("HIV diagnoses", "HIV prevalence"),
      year = "2022",
      stratified_by = "age",
      strata = 3L,
      # Counties in one response only are counted and left out.
      counties_stratified_only = c(0L, 1L),
      counties_unstratified_only = c(0L, 1L),
      counties_in_both = c(2L, 10L),
      # The unstratified count: suppressed, other (not available), published.
      total_suppressed = c(0L, 1L),
      total_other = c(0L, 1L),
      total_published = c(2L, 8L),
      # Among counties with a published unstratified count.
      none_suppressed = c(1L, 5L),
      one_suppressed = c(0L, 1L),
      two_or_more_suppressed = c(1L, 2L),
      incomplete_strata = c(0L, 1L),
      other_cells = c(0L, 1L),
      all_published = c(1L, 3L),
      sum_equal = c(1L, 1L),
      sum_less = c(0L, 1L),
      sum_more = c(0L, 1L),
      published_exceed = c(0L, 2L)
    )
  )
})

test_that("count_suppressed_strata() doesn't depend on row order or factors", {
  fake <- fake_count_responses()
  shuffled <- lapply(fake, function(rows) {
    rows <- rows[rev(seq_len(nrow(rows))), ]
    rows$indicator <- as.character(rows$indicator)
    rows$data_status <- as.character(rows$data_status)
    rows$year <- factor(rows$year)
    rows
  })

  expect_equal(
    count_fake_strata(shuffled$stratified, shuffled$unstratified),
    count_fake_strata(fake$stratified, fake$unstratified)
  )
})

test_that("count_suppressed_strata() names no county, in what it returns or says", {
  fake <- fake_count_responses()
  fips <- unique(c(fake$stratified$fips, fake$unstratified$fips))
  names_a_county <- function(text) {
    any(vapply(fips, grepl, logical(1), x = paste(text, collapse = "\n")))
  }

  expect_no_message(
    counts <- count_fake_strata(fake$stratified, fake$unstratified)
  )
  # Counts only: one row per indicator and year, whole numbers, no county
  # column, and no county's code anywhere in it.
  expect_equal(nrow(counts), 2)
  expect_false(any(c("fips", "geography") %in% names(counts)))
  numbers <- counts[setdiff(
    names(counts),
    c("indicator", "year", "stratified_by")
  )]
  expect_true(all(vapply(numbers, is.integer, logical(1))))
  expect_null(attributes(counts)$counties)
  expect_setequal(names(attributes(counts)), c("names", "class", "row.names"))
  expect_false(names_a_county(utils::capture.output(print(counts))))
  expect_false(names_a_county(utils::capture.output(dput(counts))))

  lines <- describe_suppressed_strata(counts, "made-up")
  expect_length(lines, 2)
  expect_false(names_a_county(lines))
  # The fixture's own check that the search would find a county's code.
  expect_true(names_a_county(fake$unstratified$fips[1]))
})

test_that("describe_suppressed_strata() reports every count, one line per indicator", {
  fake <- fake_count_responses()
  counts <- count_fake_strata(fake$stratified, fake$unstratified)

  expect_snapshot(writeLines(describe_suppressed_strata(counts, "made-up")))
})

test_that("count_suppressed_strata() keeps an indicator that only one response has", {
  fake <- fake_count_responses()
  no_diagnoses <- fake$unstratified[
    fake$unstratified$indicator != "HIV diagnoses",
  ]

  counts <- count_fake_strata(fake$stratified, no_diagnoses)

  diagnoses <- counts[counts$indicator == "HIV diagnoses", ]
  expect_equal(diagnoses$counties_stratified_only, 2)
  expect_equal(diagnoses$counties_in_both, 0)
  expect_equal(diagnoses$total_published, 0)
  expect_equal(diagnoses$two_or_more_suppressed, 0)
  # And the other way round.
  other_way <- count_fake_strata(
    fake$stratified[fake$stratified$indicator != "HIV diagnoses", ],
    fake$unstratified
  )
  diagnoses <- other_way[other_way$indicator == "HIV diagnoses", ]
  expect_equal(diagnoses$counties_unstratified_only, 2)
  expect_equal(diagnoses$counties_in_both, 0)
  # The other indicator's counts are unchanged.
  expect_equal(
    counts[counts$indicator == "HIV prevalence", ],
    count_fake_strata(fake$stratified, fake$unstratified)[2, ],
    ignore_attr = "row.names"
  )
})

test_that("count_suppressed_strata() treats a count marked suppressed as not published", {
  # A count beside "Data suppressed" is neither a published count nor a
  # suppressed cell with no count: it is counted with the other cells.
  fake <- fake_count_responses()
  total <- fake$unstratified$fips == "01001" &
    fake$unstratified$indicator == "HIV prevalence"
  fake$unstratified$data_status[total] <- "Data suppressed"
  cell <- fake$stratified$fips == "01003" &
    fake$stratified$age == "13-24" &
    fake$stratified$indicator == "HIV prevalence"
  fake$stratified$data_status[cell] <- "Data suppressed"

  counts <- count_fake_strata(fake$stratified, fake$unstratified)
  prevalence <- counts[counts$indicator == "HIV prevalence", ]

  expect_equal(prevalence$total_other, 2)
  expect_equal(prevalence$total_published, 7)
  expect_equal(prevalence$other_cells, 2)
  # 01003 has no suppressed cell with no count, and not every cell published.
  expect_equal(prevalence$none_suppressed, 4)
  expect_equal(prevalence$one_suppressed, 1)
  expect_equal(prevalence$all_published, 1)
})

test_that("count_suppressed_strata() stops rather than count a county twice", {
  fake <- fake_count_responses()
  twice <- rbind(fake$unstratified, fake$unstratified[1:2, ])
  cell_twice <- rbind(fake$stratified, fake$stratified[1:3, ])

  expect_snapshot(error = TRUE, {
    count_fake_strata(fake$stratified, twice)
    count_fake_strata(cell_twice, fake$unstratified)
  })
})

test_that("count_suppressed_strata() stops unless given one stratified and one unstratified response", {
  fake <- fake_count_responses()
  by_sex_too <- fake$stratified
  by_sex_too$sex <- factor("Male")

  expect_snapshot(error = TRUE, {
    count_fake_strata(fake$unstratified, fake$unstratified)
    count_fake_strata(fake$stratified, fake$stratified)
    count_fake_strata(by_sex_too, fake$unstratified)
    count_fake_strata(
      fake$stratified[c("fips", "cases")],
      fake$unstratified
    )
    count_fake_strata(
      fake$stratified,
      fake$unstratified[c("fips", "age")]
    )
  })
})

# Every call in `fn`, nested functions included, as function names.
calls_in <- function(fn) {
  unique(all.names(body(fn), functions = TRUE, unique = TRUE))
}

test_that("the counting helpers never subtract one count from another (#164)", {
  # The boundary is on the calculation, not only on what is returned: a
  # county's (or state's) total minus its published parts must not exist even
  # as an intermediate value. No test of what the helpers return can show
  # that, so this reads their code: no `-` (binary or unary) and no diff()
  # anywhere in them. Sums of published cells are compared with `==`, `<`
  # and `>` only.
  subtracting <- c("-", "diff", "Reduce", "outer", "sweep")
  expect_equal(
    intersect(calls_in(count_suppressed_strata), subtracting),
    character()
  )
  expect_equal(
    intersect(calls_in(count_suppressed_counties), subtracting),
    character()
  )
  # The search does find a subtraction where there is one.
  expect_contains(calls_in(function(total, parts) total - sum(parts)), "-")
  expect_contains(calls_in(function(x) lapply(x, function(y) -y)), "-")
})

test_that("count_suppressed_strata() compares sums only for fully published counties", {
  # With every county's published strata equal to its total except the
  # county with exactly one suppressed cell and the counties with two, the
  # three-way comparison must report equality throughout: counties with a
  # suppressed cell are not part of it.
  fake <- fake_count_responses()
  full <- c("01001", "01003", "01017")
  fake$unstratified$cases[
    fake$unstratified$fips %in%
      full &
      fake$unstratified$indicator == "HIV prevalence"
  ] <- c(30, 30, 21)

  counts <- count_fake_strata(fake$stratified, fake$unstratified)
  prevalence <- counts[counts$indicator == "HIV prevalence", ]

  expect_equal(prevalence$all_published, 3)
  expect_equal(prevalence$sum_equal, 3)
  expect_equal(prevalence$sum_less, 0)
  expect_equal(prevalence$sum_more, 0)
  # 01019's published cell alone (12) is still above its total (10).
  expect_equal(prevalence$published_exceed, 1)
  expect_equal(prevalence$one_suppressed, 1)
})

test_that("count_suppressed_strata() stops when the response is by another variable than asked for", {
  # A response by sex must not pass for the one by age that was asked for.
  fake <- fake_count_responses()
  by_sex <- fake$stratified[fake$stratified$age != "45+", ]
  by_sex$sex <- factor(ifelse(by_sex$age == "13-24", "Male", "Female"))
  by_sex$age <- "Ages 13 years and older"

  expect_no_error(
    count_fake_strata(by_sex, fake$unstratified, "sex", c("Male", "Female"))
  )
  expect_snapshot(error = TRUE, {
    count_fake_strata(by_sex, fake$unstratified, "age", fake_ages)
    count_fake_strata(by_sex, fake$unstratified, "age", c("Male", "Female"))
    count_fake_strata(fake$stratified, fake$unstratified, "sex", fake_ages)
    count_fake_strata(fake$stratified, fake$unstratified, "ages", fake_ages)
  })
})

test_that("count_suppressed_strata() stops when the strata are not the ones asked for", {
  fake <- fake_count_responses()
  # A stratum absent from every county: without the check, the counties
  # would look complete with two strata.
  none_45 <- fake$stratified[fake$stratified$age != "45+", ]
  # Absent from every county of one indicator only.
  none_45_diagnoses <- fake$stratified[
    !(fake$stratified$age == "45+" &
      fake$stratified$indicator == "HIV diagnoses"),
  ]
  # A stratum that wasn't asked for.
  extra <- fake$stratified
  extra$age[extra$fips == "01001" & extra$age == "45+"] <- "Unknown"

  expect_snapshot(error = TRUE, {
    count_fake_strata(none_45, fake$unstratified)
    count_fake_strata(none_45_diagnoses, fake$unstratified)
    count_fake_strata(extra, fake$unstratified)
    count_fake_strata(fake$stratified, fake$unstratified, strata = character())
  })
  # The same rows are fine when those two strata are what was asked for.
  expect_equal(
    count_fake_strata(
      none_45,
      fake$unstratified,
      strata = c("13-24", "25-44")
    )$strata,
    c(2, 2)
  )
})

# The counts of suppressed county rows in each state stratum (#164) --------------

# Made-up county and state rows. S is a suppressed count, n/a "Data not
# available", - no row.
#
# Unstratified, HIV prevalence
# | State | County rows              | State total | Suppressed county rows |
# |-------|--------------------------|-------------|------------------------|
# | 01    | 30, S, S                 | 100         | two                    |
# | 02    | 10, S                    | 20          | exactly one            |
# | 04    | 5, 7                     | 12          | none                   |
# | 05    | S                        | S           | exactly one; total suppressed |
# | 06    | S, n/a, 3                | 9           | exactly one            |
# | 08    | S, 4                     | -           | exactly one; no state row |
# | 09    | -                        | 50          | no county rows: not a state stratum here |
# Unstratified, HIV diagnoses
# | 01    | S, S                     | 8           | two                    |
# | 02    | 1, 2                     | 3           | none                   |
#
# By two made-up age groups, HIV prevalence
# | State | Age   | County rows | State total | Suppressed county rows        |
# |-------|-------|-------------|-------------|-------------------------------|
# | 01    | 13-24 | S, 5        | 9           | exactly one                   |
# | 01    | 25+   | S, S        | S           | two; total suppressed         |
# | 02    | 13-24 | 3, 4        | 7           | none                          |
# | 02    | 25+   | S, 6        | 10          | exactly one                   |
fake_state_strata <- function() {
  s <- NA_real_
  list(
    county = rbind(
      fake_count_rows(
        c(
          "01001" = 30,
          "01003" = s,
          "01005" = s,
          "02001" = 10,
          "02003" = s,
          "04001" = 5,
          "04003" = 7,
          "05001" = s,
          "06001" = s,
          "06003" = s,
          "06005" = 3,
          "08001" = s,
          "08003" = 4
        ),
        not_available = "06003"
      ),
      fake_count_rows(
        c("01001" = s, "01003" = s, "02001" = 1, "02003" = 2),
        indicator = "HIV diagnoses"
      )
    ),
    state = rbind(
      fake_count_rows(
        c("01" = 100, "02" = 20, "04" = 12, "05" = s, "06" = 9, "09" = 50)
      ),
      fake_count_rows(c("01" = 8, "02" = 3), indicator = "HIV diagnoses")
    ),
    county_by_age = rbind(
      fake_count_rows(
        c("01001" = s, "01003" = 5, "02001" = 3, "02003" = 4),
        age = "13-24"
      ),
      fake_count_rows(
        c("01001" = s, "01003" = s, "02001" = s, "02003" = 6),
        age = "25+"
      )
    ),
    state_by_age = rbind(
      fake_count_rows(c("01" = 9, "02" = 7), age = "13-24"),
      fake_count_rows(c("01" = s, "02" = 10), age = "25+")
    )
  )
}

# The fixtures above are for 2022; unstratified unless `stratify_by` is
# given, and then by the two made-up age groups.
fake_state_ages <- c("13-24", "25+")
count_fake_counties <- function(
  county,
  state = NULL,
  year = 2022,
  stratify_by = NULL,
  strata = if (!is.null(stratify_by)) fake_state_ages
) {
  count_suppressed_counties(county, state, year, stratify_by, strata)
}

test_that("count_suppressed_counties() counts state strata by their suppressed county rows", {
  fake <- fake_state_strata()

  expect_equal(
    count_fake_counties(fake$county, fake$state),
    data.frame(
      indicator = c("HIV diagnoses", "HIV prevalence"),
      year = "2022",
      stratified_by = "none",
      state_strata = c(2L, 6L),
      # Only state strata with a published state total are counted.
      state_total_published = c(2L, 4L),
      state_total_other = c(0L, 1L),
      no_state_row = c(0L, 1L),
      counted = c(2L, 4L),
      none_suppressed = c(1L, 1L),
      one_suppressed = c(0L, 2L),
      two_or_more_suppressed = c(1L, 1L)
    )
  )
  expect_equal(
    count_fake_counties(
      fake$county_by_age,
      fake$state_by_age,
      stratify_by = "age"
    ),
    data.frame(
      indicator = "HIV prevalence",
      year = "2022",
      stratified_by = "age",
      state_strata = 4L,
      state_total_published = 3L,
      state_total_other = 1L,
      no_state_row = 0L,
      counted = 3L,
      none_suppressed = 1L,
      one_suppressed = 2L,
      two_or_more_suppressed = 0L
    )
  )
})

test_that("count_suppressed_counties() counts every state stratum when given no state totals", {
  fake <- fake_state_strata()

  expect_equal(
    count_fake_counties(fake$county),
    data.frame(
      indicator = c("HIV diagnoses", "HIV prevalence"),
      year = "2022",
      stratified_by = "none",
      state_strata = c(2L, 6L),
      state_total_published = NA_integer_,
      state_total_other = NA_integer_,
      no_state_row = NA_integer_,
      counted = c(2L, 6L),
      none_suppressed = c(1L, 1L),
      one_suppressed = c(0L, 4L),
      two_or_more_suppressed = c(1L, 1L)
    )
  )
  by_age <- count_fake_counties(fake$county_by_age, stratify_by = "age")
  expect_equal(by_age$counted, 4)
  expect_equal(by_age$none_suppressed, 1)
  expect_equal(by_age$one_suppressed, 2)
  expect_equal(by_age$two_or_more_suppressed, 1)
})

test_that("count_suppressed_counties() names no state or county, in what it returns or says", {
  fake <- fake_state_strata()
  fips <- unique(fake$county$fips)
  names_a_county <- function(text) {
    any(vapply(fips, grepl, logical(1), x = paste(text, collapse = "\n")))
  }

  expect_no_message(
    counts <- count_fake_counties(fake$county, fake$state)
  )
  # Counts only: one row per indicator and year, whole numbers, and no
  # column that could hold a place.
  expect_equal(nrow(counts), 2)
  expect_equal(
    names(counts),
    c(
      "indicator",
      "year",
      "stratified_by",
      "state_strata",
      "state_total_published",
      "state_total_other",
      "no_state_row",
      "counted",
      "none_suppressed",
      "one_suppressed",
      "two_or_more_suppressed"
    )
  )
  expect_true(all(vapply(counts[-(1:3)], is.integer, logical(1))))
  expect_setequal(names(attributes(counts)), c("names", "class", "row.names"))
  expect_false(names_a_county(utils::capture.output(dput(counts))))
  expect_false(names_a_county(describe_suppressed_counties(counts, "made-up")))
})

test_that("describe_suppressed_counties() reports every count, one line per indicator", {
  fake <- fake_state_strata()

  expect_snapshot({
    writeLines(describe_suppressed_counties(
      count_fake_counties(fake$county, fake$state),
      "made-up"
    ))
    writeLines(describe_suppressed_counties(
      count_fake_counties(fake$county_by_age, stratify_by = "age"),
      "made-up"
    ))
  })
})

test_that("count_suppressed_counties() stops rather than count a row twice", {
  fake <- fake_state_strata()

  expect_snapshot(error = TRUE, {
    count_fake_counties(rbind(fake$county, fake$county[1:2, ]))
    count_fake_counties(fake$county, rbind(fake$state, fake$state[1, ]))
    count_fake_counties(fake$state)
    count_fake_counties(fake$county, fake$county)
    count_fake_counties(fake$county[c("fips", "cases")])
  })
})

test_that("count_suppressed_counties() stops when a response is not for the year or stratification asked for", {
  # A response for another year, or by another variable, must not pass for
  # the one asked for, even when its counts would meet every expectation.
  fake <- fake_state_strata()
  in_2021 <- function(x) transform(x, year = 2021)
  one_row_2021 <- fake$county
  one_row_2021$year[1] <- 2021
  # One state row has lost its year; the rest are for the year asked for.
  one_state_year_missing <- fake$state
  one_state_year_missing$year[1] <- NA
  # One row of a response by age carries the all-ages label: the response
  # still looks stratified by age, but that row is a total.
  one_state_row_total <- fake$state_by_age
  one_state_row_total$age[1] <- "Ages 13 years and older"
  one_county_row_total <- fake$county_by_age
  one_county_row_total$age[1] <- "Ages 13 years and older"
  # One row of an unstratified response carries an age group.
  one_county_row_by_age <- fake$county
  one_county_row_by_age$age[1] <- "13-24"
  # One state row has an age group the request didn't ask for: without the
  # check its county stratum would find no state row and drop out unseen.
  one_state_row_unrequested <- fake$state_by_age
  one_state_row_unrequested$age[1] <- "13-99"

  expect_no_error(count_fake_counties(fake$county, fake$state))
  expect_no_error(
    count_fake_counties(fake$county_by_age, fake$state_by_age, 2022, "age")
  )
  expect_snapshot(error = TRUE, {
    count_fake_counties(in_2021(fake$county), fake$state)
    count_fake_counties(fake$county, in_2021(fake$state))
    count_fake_counties(one_row_2021)
    count_fake_counties(fake$county, year = 2021)
    count_fake_counties(fake$county_by_age)
    count_fake_counties(fake$county, stratify_by = "age")
    count_fake_counties(fake$county_by_age, fake$state, stratify_by = "age")
    count_fake_counties(fake$county, year = c(2021, 2022))
    count_fake_counties(fake$county, stratify_by = "ages")
    count_fake_counties(fake$county, one_state_year_missing)
    count_fake_counties(fake$county_by_age, one_state_row_total, 2022, "age")
    count_fake_counties(one_county_row_total, fake$state_by_age, 2022, "age")
    count_fake_counties(one_county_row_by_age, fake$state)
    count_fake_counties(
      fake$county_by_age,
      one_state_row_unrequested,
      2022,
      "age"
    )
    count_fake_counties(fake$county_by_age, NULL, 2022, "age", strata = NULL)
  })
})

# The live expectation built on those counts -------------------------------------

test_that("expect_no_single_suppressed_county() fails on one suppressed county row in an unstratified state stratum with a published total", {
  fake <- fake_state_strata()

  # States 02 and 06: exactly one suppressed county row, state total published.
  expect_failure(
    suppressMessages(expect_no_single_suppressed_county(
      fake$county,
      fake$state,
      "HIV prevalence",
      2022
    )),
    "HIV prevalence 2022, unstratified: 2 of 4 state strata have exactly one suppressed county row",
    fixed = TRUE
  )
  # HIV diagnoses has none.
  expect_success(suppressMessages(expect_no_single_suppressed_county(
    fake$county,
    fake$state,
    "HIV diagnoses",
    2022
  )))
})

test_that("expect_no_single_suppressed_county() leaves out a state stratum whose state total is not published", {
  # State 05: one county row, suppressed, and the state total suppressed too.
  # State 08: one suppressed county row and no state row. State 04: none.
  # Only state 04 is counted, so nothing fails.
  fake <- fake_state_strata()
  county <- fake$county[
    substr(fake$county$fips, 1, 2) %in% c("04", "05", "08"),
  ]

  expect_success(suppressMessages(expect_no_single_suppressed_county(
    county,
    fake$state,
    "HIV prevalence",
    2022
  )))
  expect_message(
    expect_no_single_suppressed_county(
      county,
      fake$state,
      "HIV prevalence",
      2022
    ),
    "state total published in 1, not published in 1, no state row for 1",
    fixed = TRUE
  )
})

test_that("expect_no_single_suppressed_county() only reports stratified counts and counts without state totals", {
  fake <- fake_state_strata()

  # By age: two state strata with exactly one suppressed county row, reported.
  expect_success(suppressMessages(expect_no_single_suppressed_county(
    fake$county_by_age,
    fake$state_by_age,
    "HIV prevalence",
    2022,
    "age",
    fake_state_ages
  )))
  expect_message(
    expect_no_single_suppressed_county(
      fake$county_by_age,
      fake$state_by_age,
      "HIV prevalence",
      2022,
      "age",
      fake_state_ages
    ),
    "exactly one 2",
    fixed = TRUE
  )
  # No state totals: four, reported.
  expect_success(suppressMessages(expect_no_single_suppressed_county(
    fake$county,
    NULL,
    "HIV prevalence",
    2022
  )))
  # Prevalence rows only, so the line checked is the only one said.
  prevalence <- fake$county[fake$county$indicator == "HIV prevalence", ]
  expect_message(
    expect_no_single_suppressed_county(
      prevalence,
      NULL,
      "HIV prevalence",
      2022
    ),
    "exactly one 4",
    fixed = TRUE
  )
})

test_that("expect_no_single_suppressed_county() fails when nothing is counted", {
  fake <- fake_state_strata()
  # State 05 only: its state total is suppressed, so no state stratum counts.
  county <- fake$county[fake$county$fips == "05001", ]

  expect_failure(
    suppressMessages(expect_no_single_suppressed_county(
      county,
      fake$state,
      "HIV prevalence",
      2022
    )),
    "no state stratum with county rows and a published state total",
    fixed = TRUE
  )
  expect_failure(
    suppressMessages(expect_no_single_suppressed_county(
      county,
      fake$state,
      "HIV diagnoses",
      2022
    )),
    "HIV diagnoses 2022, unstratified: no state stratum",
    fixed = TRUE
  )
})

test_that("expect_no_single_suppressed_county() stops on a response for another year, whatever its counts", {
  # States 01 and 04 have no state stratum with exactly one suppressed county
  # row, so only the year can make this fail.
  fake <- fake_state_strata()
  county <- fake$county[substr(fake$county$fips, 1, 2) %in% c("01", "04"), ]

  expect_success(suppressMessages(expect_no_single_suppressed_county(
    county,
    fake$state,
    "HIV prevalence",
    2022
  )))
  expect_error(
    suppressMessages(expect_no_single_suppressed_county(
      transform(county, year = 2021),
      transform(fake$state, year = 2021),
      "HIV prevalence",
      2022
    )),
    "must be for 2022 only"
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

# The fit's live checks log counts only (#179): a county or area named in a
# download warning or an error must not reach the log.

test_that("live_collect() keeps a replayed download warning's text out of the log", {
  local_fresh_live_cache()
  local_mocked_bindings(get_atlas = function(...) {
    warning("Sentinel County 987654")
    data.frame(fips = "01001")
  })
  suppressWarnings(live_county(1999))

  log <- capture_messages(capture_warnings(
    run <- live_collect(live_county(1999), "downloading")
  ))
  expect_length(log, 0)
  expect_length(run$warnings, 1)
  counts <- live_warning_counts(run$warnings)
  expect_equal(counts, "unclassified: 1")
})

test_that("live_collect() fails with the stage and class only", {
  sentinel <- function() {
    cli::cli_abort(
      "Duplicate rows for area {.val 987654}.",
      class = "cdcatlas_sentinel_error"
    )
  }
  error <- expect_error(
    live_collect(sentinel(), "allocating"),
    class = "cdcatlas_live_redacted_error"
  )
  expect_equal(
    conditionMessage(error),
    "Live check failed while allocating (cdcatlas_sentinel_error); details are left out of the log."
  )
  expect_null(error$call)
  expect_null(error$parent)
  expect_no_match(paste(format(error), collapse = "\n"), "987654")
})
