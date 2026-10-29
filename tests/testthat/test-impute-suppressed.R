test_that("state_rate fills suppressed counties with state rate x county population", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"), cap = Inf)
  filled <- out[out$fips == "01003", ]

  # State diagnoses: 11 + 21 = 32 per 1e6, times 50,000 = 1.6.
  # State prevalence: 111 + 121 = 232 per 1e6, times 50,000 = 11.6.
  expect_equal(filled$cases, c(1.6, 11.6))
  expect_equal(unique(filled$cases_source), "state_rate")
  expect_equal(unique(out$cases_source[out$fips == "01001"]), "published")
})

test_that("state_rate is capped at `cap`", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")
  filled <- function(out) out$cases[out$fips == "01003"]

  # Uncapped: diagnoses 1.6, prevalence 11.6.
  expect_equal(filled(impute_suppressed(county)), c(1.6, 4))
  expect_equal(filled(impute_suppressed(county, cap = 1)), c(1, 1))
})

test_that("the cap never changes a published count", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"), cap = 0)
  published <- out[out$cases_source == "published", ]

  expect_equal(published$cases, c(11, 111))
})

test_that("state rates match on stratum as well as indicator", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  out <- impute_suppressed(
    fake_county_atlas("sex", suppressed = "01003"),
    cap = Inf
  )
  filled <- out[out$fips == "01003" & out$indicator == "HIV diagnoses", ]

  # Male: 11 + 21 = 32; female: 12 + 22 = 34 (per 1e6, times 50,000).
  expect_equal(filled$cases[filled$sex == "Male"], 32 / 20)
  expect_equal(filled$cases[filled$sex == "Female"], 34 / 20)
})

test_that("zero fills suppressed counties with 0 and makes no request", {
  local_mocked_bindings(.request_atlas = function(payload) stop("no request"))
  out <- impute_suppressed(
    fake_county_atlas(suppressed = "01003"),
    method = "zero"
  )

  expect_equal(out$cases[out$fips == "01003"], c(0, 0))
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "zero")
})

test_that("rows marked 'Data not available' with no count are dropped", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"

  expect_snapshot(out <- impute_suppressed(county, method = "zero"))
  expect_equal(unique(out$fips), "01001")
})

test_that("a published count is kept even when marked 'Data not available'", {
  # Like Connecticut's planning regions in 2017: a count but no population.
  county <- fake_county_atlas()
  county$data_status[county$fips == "01003"] <- "Data not available"
  county$population[county$fips == "01003"] <- NA

  out <- impute_suppressed(county)
  expect_equal(out$cases, county$cases)
  expect_equal(unique(out$cases_source), "published")
})

test_that("suppressed rows with no county population stay NA, with a warning", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")
  county$population[county$fips == "01003"] <- NA

  expect_snapshot(out <- impute_suppressed(county))
  expect_equal(out$cases[out$fips == "01003"], c(NA_real_, NA_real_))
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "not estimated")
})

test_that("suppressed rows with no state rate stay NA, with a warning", {
  state <- fake_state_atlas()
  state$population <- NA
  local_fake_sources(state = state)

  expect_snapshot(out <- impute_suppressed(fake_county_atlas(suppressed = "01003")))
  expect_equal(sum(is.na(out$cases)), 2)
})

test_that("the input's columns and types are kept", {
  out <- impute_suppressed(fake_county_atlas(), method = "zero")
  county <- fake_county_atlas()

  expect_equal(out[names(county)], county)
  expect_equal(setdiff(names(out), names(county)), "cases_source")
})

test_that("impute_suppressed() rejects data it can't use", {
  county <- fake_county_atlas()
  state <- fake_state_atlas()

  expect_snapshot(error = TRUE, impute_suppressed(state))
  expect_snapshot(error = TRUE, impute_suppressed(county[c("fips", "cases")]))
  expect_snapshot(error = TRUE, impute_suppressed("county"))
  expect_snapshot(
    error = TRUE,
    impute_suppressed(impute_suppressed(county, method = "zero"))
  )
  expect_snapshot(error = TRUE, impute_suppressed(county, method = "mean"))
  expect_snapshot(error = TRUE, impute_suppressed(county, cap = -1))
  expect_snapshot(error = TRUE, impute_suppressed(county, cap = c(1, 2)))
  expect_snapshot(error = TRUE, impute_suppressed(county[0, ]))

  numeric_fips <- county
  numeric_fips$fips <- as.numeric(numeric_fips$fips)
  expect_snapshot(error = TRUE, impute_suppressed(numeric_fips))

  expect_snapshot(error = TRUE, impute_suppressed(rbind(county, county[1, ])))

  mixed <- rbind(fake_county_atlas(), fake_county_atlas("sex"))
  expect_snapshot(error = TRUE, impute_suppressed(mixed))
})

test_that("state rates are matched within each state", {
  local_fake_sources(state = two_state_atlas()$state)
  out <- impute_suppressed(two_state_atlas()$county, cap = Inf)
  filled <- out[out$cases_source == "state_rate" &
    out$indicator == "HIV diagnoses", ]

  # State 01: 32 per 1e6; state 02: (32 + 300) per 2e6. County population
  # 50,000.
  expect_equal(filled$fips, c("01003", "02003"))
  expect_equal(filled$cases, c(1.6, 8.3))
})

test_that("state totals are requested for the same disease, years and strata", {
  received <- NULL
  local_mocked_bindings(.request_atlas_all = function(payloads) {
    received <<- payloads
    fake_state_atlas(stratify_by = "sex", years = 2021:2022)
  })
  .fetch_state_totals(fake_county_atlas("sex", years = 2021:2022))

  expect_length(received, 1)
  expected <- .build_payload("hiv", "state", 2021:2022, stratify_by = "sex")
  expect_equal(received[[1]], expected)
})

test_that("state_rate is floored at `floor`", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")
  out <- impute_suppressed(county, floor = 2)

  # Uncapped estimates are 1.6 and 11.6.
  expect_equal(out$cases[out$fips == "01003"], c(2, 4))
  expect_snapshot(error = TRUE, impute_suppressed(county, floor = 5))
  expect_snapshot(error = TRUE, impute_suppressed(county, floor = NA))
})

test_that("population = 'acs' uses the county's tracts in the same stratum", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  county <- fake_county_atlas("sex", suppressed = "01003")
  out <- impute_suppressed(county, cap = Inf, population = "acs")
  male <- out[out$fips == "01003" & out$sex == "Male" &
    out$indicator == "HIV diagnoses", ]

  pop <- fake_acs_tract("01003000100")
  male_13 <- sum(pop[sprintf("B01001_%03d", 6:25)]) + 0.4 * pop[["B01001_005"]]
  expect_equal(male$cases, 32 / 1e6 * male_13)
  expect_equal(male$cases_source, "state_rate")
})

test_that("population = 'acs' needs a Census key", {
  withr::local_envvar(CENSUS_API_KEY = "")
  local_mocked_bindings(.request_atlas = function(payload) stop("CDC was called"))
  county <- fake_county_atlas(suppressed = "01003")

  expect_snapshot(error = TRUE, impute_suppressed(county, population = "acs"))
})

test_that("unestimated = 'zero' sets rows with no rate to 0", {
  state <- fake_state_atlas()
  state$population <- NA
  local_fake_sources(state = state)
  county <- fake_county_atlas(suppressed = "01003")

  expect_snapshot(out <- impute_suppressed(county, unestimated = "zero"))
  expect_equal(out$cases[out$fips == "01003"], c(0, 0))
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "not estimated")
})

test_that("not_available = 'keep' keeps the rows with no count", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"

  expect_no_message(out <- impute_suppressed(county, not_available = "keep"))
  expect_equal(nrow(out), nrow(county))
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "not available")
  expect_equal(out$cases[out$fips == "01003"], c(NA_real_, NA_real_))
})
