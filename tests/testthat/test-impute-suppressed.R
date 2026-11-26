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

test_that("only rows marked 'Data suppressed' are filled in (#107)", {
  local_fake_sources()
  statuses <- list("Not Suppressed", "No reported cases", "CDC code 999", NA)
  for (status in statuses) {
    county <- fake_county_atlas(suppressed = "01003")
    rows <- county$fips == "01003"
    county$data_status <- factor(
      ifelse(rows, status, as.character(county$data_status))
    )
    for (method in c("state_rate", "zero")) {
      out <- suppressWarnings(impute_suppressed(county, method = method))
      label <- paste(status, method)
      expect_equal(
        out$cases[out$fips == "01003"],
        c(NA_real_, NA),
        label = label
      )
      expect_equal(
        unique(out$cases_source[out$fips == "01003"]),
        "not estimated",
        label = label
      )
      expect_equal(out$cases[out$fips == "01001"], c(11, 111), label = label)
    }
  }
})

test_that("a missing count that isn't suppression gets a warning naming its status", {
  county <- fake_county_atlas(suppressed = "01003")
  county$data_status <- factor(ifelse(
    county$fips == "01003",
    "No reported cases",
    as.character(county$data_status)
  ))
  expect_snapshot(out <- impute_suppressed(county, method = "zero"))
})

test_that("a published zero stays zero whatever its status", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- 0
  county$data_status <- factor(ifelse(
    county$fips == "01003",
    "No reported cases",
    as.character(county$data_status)
  ))
  out <- impute_suppressed(county, method = "zero")
  expect_equal(out$cases[out$fips == "01003"], c(0, 0))
  expect_equal(unique(out$cases_source), "published")
})

test_that("rows marked 'Data not available' with no count are kept by default (#110)", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"

  expect_no_message(out <- impute_suppressed(county, method = "zero"))
  expect_equal(nrow(out), nrow(county))
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "not available")
})

test_that("not_available = 'drop' drops rows marked 'Data not available' with no count", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"

  expect_snapshot(
    out <- impute_suppressed(county, method = "zero", not_available = "drop")
  )
  expect_equal(unique(out$fips), "01001")
})

test_that("not_available = 'drop' can drop every row, leaving the columns", {
  local_mocked_bindings(.request_atlas = function(payload) {
    stop("CDC was called")
  })
  county <- fake_county_atlas()
  county$cases[] <- NA_real_
  county$data_status[] <- "Data not available"
  inputs <- list(data.frame = county, tibble = tibble::as_tibble(county))

  for (class in names(inputs)) {
    for (method in c("zero", "state_rate")) {
      label <- paste(class, method)
      input <- inputs[[class]]
      out <- suppressMessages(suppressWarnings(
        impute_suppressed(input, method = method, not_available = "drop")
      ))

      expect_equal(nrow(out), 0, label = label)
      expect_identical(
        names(out),
        c(names(input), "cases_source", "cases_uncapped", "fit_reason"),
        label = label
      )
      # The input's columns, types and class, with no rows.
      expect_identical(out[names(input)], input[0, ], label = label)
      expect_identical(out$cases_source, character(0), label = label)
      expect_identical(out$cases_uncapped, numeric(0), label = label)
      expect_identical(out$fit_reason, character(0), label = label)
    }
  }
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

  expect_snapshot(
    out <- impute_suppressed(fake_county_atlas(suppressed = "01003"))
  )
  expect_equal(sum(is.na(out$cases)), 2)
})

test_that("the input's columns and types are kept", {
  county <- with_fake_rates(fake_county_atlas())
  out <- impute_suppressed(county, method = "zero")

  expect_equal(out[names(county)], county)
  expect_equal(
    names(out),
    c(names(county), "cases_source", "cases_uncapped", "fit_reason")
  )
  expect_equal(out$fit_reason, rep(NA_character_, nrow(county)))
})

# The rate columns after imputation (#157) ---------------------------------------
# These record what impute_suppressed() does today, before #157 decides what
# it should do: `cases` on a suppressed row becomes the package's estimate,
# while rate100000, lowerci_rate and upperci_rate stay as CDC returned them.

test_that("the rate columns are returned unchanged on every row (#157)", {
  local_fake_sources()
  fixtures <- list(
    "no rate on suppressed rows" = with_fake_rates(
      fake_county_atlas(suppressed = "01003")
    ),
    "a rate on suppressed rows" = with_fake_rates(
      fake_county_atlas(suppressed = "01003"),
      rate_without_count = TRUE
    )
  )
  # The fixtures are what their names say: a rate on every row of the
  # second, and on the published rows only of the first.
  with_rate <- fixtures[["a rate on suppressed rows"]]
  expect_equal(sum(is.na(as.matrix(with_rate[rate_columns]))), 0)
  expect_equal(
    is.na(fixtures[["no rate on suppressed rows"]]$rate100000),
    with_rate$fips == "01003"
  )

  for (fixture in names(fixtures)) {
    county <- fixtures[[fixture]]
    for (method in c("state_rate", "zero")) {
      for (population in c("cdc", "acs")) {
        label <- paste(fixture, method, population, sep = ", ")
        out <- impute_suppressed(
          county,
          method = method,
          population = population
        )

        # The suppressed rows were filled in, so their rates had the chance
        # to change.
        filled <- out$fips == "01003"
        expect_equal(out$cases_source[filled], rep(method, 2), label = label)
        expect_equal(sum(is.na(out$cases[filled])), 0, label = label)
        expect_identical(
          out[rate_columns],
          county[rate_columns],
          label = label
        )
      }
    }
  }
})

test_that("published rows keep CDC's rate exactly, whatever is dropped (#157)", {
  local_fake_sources()
  # Row order: 01001 and 01003 for diagnoses, then for prevalence. Rates are
  # 1001.25 to 1004.25 by row; 01003's prevalence row is not available and
  # is dropped.
  county <- with_fake_rates(fake_county_atlas(suppressed = "01003"))
  not_available <- county$fips == "01003" &
    county$indicator == "HIV prevalence"
  county$data_status[not_available] <- "Data not available"

  for (method in c("state_rate", "zero")) {
    out <- suppressMessages(suppressWarnings(impute_suppressed(
      county,
      method = method,
      not_available = "drop"
    )))
    published <- out[out$cases_source == "published", ]

    expect_equal(published$fips, c("01001", "01001"), label = method)
    expect_identical(published$rate100000, c(1001.25, 1003.25), label = method)
    expect_identical(
      published$lowerci_rate,
      c(1000.75, 1002.75),
      label = method
    )
    expect_identical(
      published$upperci_rate,
      c(1001.75, 1003.75),
      label = method
    )
    expect_identical(out$rate100000[out$fips == "01003"], NA_real_)
  }
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

test_that("negative, infinite or non-numeric counts stop before any request (#115)", {
  local_mocked_bindings(.request_atlas = function(payload) {
    stop("CDC was called")
  })
  county <- fake_county_atlas()

  negative <- county
  negative$cases[1] <- -10
  expect_snapshot(error = TRUE, impute_suppressed(negative, method = "zero"))

  infinite <- county
  infinite$population[2] <- Inf
  expect_snapshot(error = TRUE, impute_suppressed(infinite))

  text <- county
  text$cases <- as.character(text$cases)
  expect_snapshot(error = TRUE, impute_suppressed(text))

  imputed <- impute_suppressed(county, method = "zero")
  imputed$cases <- -imputed$cases
  expect_snapshot(error = TRUE, no_county_cases(imputed))
  expect_snapshot(error = TRUE, extrapolate_from_county(imputed))
})

test_that("NA, zero and fractional counts are allowed", {
  county <- fake_county_atlas(suppressed = "01003")
  county$cases[county$fips == "01001"] <- c(0, 2.5)
  out <- impute_suppressed(county, method = "zero")
  expect_equal(out$cases, c(0, 0, 2.5, 0))
})

test_that("state rates are matched within each state", {
  local_fake_sources(state = two_state_atlas()$state)
  out <- impute_suppressed(two_state_atlas()$county, cap = Inf)
  filled <- out[
    out$cases_source == "state_rate" &
      out$indicator == "HIV diagnoses",
  ]

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

  expected <- list(
    .build_payload("hiv", "state", 2021, stratify_by = "sex"),
    .build_payload("hiv", "state", 2022, stratify_by = "sex")
  )
  expect_equal(received, expected)
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

# The estimate before `cap` and `floor` (#151) ----------------------------------

test_that("cases_uncapped is the estimate before the cap, so it shows when the cap decided", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"))
  filled <- out[out$fips == "01003", ]

  # State rate x county population: diagnoses 1.6, prevalence 11.6. The cap
  # of 4 decides prevalence only; diagnoses lie between floor and cap.
  expect_equal(filled$cases, c(1.6, 4))
  expect_equal(filled$cases_uncapped, c(1.6, 11.6))
  expect_equal(filled$cases_uncapped > 4, c(FALSE, TRUE))
  expect_identical(filled$cases_uncapped[1], filled$cases[1])
  expect_type(out$cases_uncapped, "double")
})

test_that("an estimate exactly equal to the cap is not above it", {
  # State rates 32 / 64 = 0.5 and 232 / 64 = 3.625, times a county population
  # of 8: exactly 4 and 29, with no rounding.
  state <- fake_state_atlas()
  state$population <- 64
  local_fake_sources(state = state)
  county <- fake_county_atlas(suppressed = "01003")
  county$population <- 8
  out <- impute_suppressed(county, cap = 4)
  filled <- out[out$fips == "01003", ]

  expect_identical(filled$cases, c(4, 4))
  expect_identical(filled$cases_uncapped, c(4, 29))
  expect_identical(filled$cases_uncapped > 4, c(FALSE, TRUE))
})

test_that("cases_uncapped shows when a raised floor decided", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"), floor = 2)
  filled <- out[out$fips == "01003", ]

  expect_equal(filled$cases, c(2, 4))
  expect_equal(filled$cases_uncapped, c(1.6, 11.6))
  expect_equal(filled$cases_uncapped < 2, c(TRUE, FALSE))
})

test_that("cases_uncapped equals cases with cap = Inf and floor = 0", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"), cap = Inf)
  filled <- out[out$fips == "01003", ]

  expect_equal(filled$cases_uncapped, c(1.6, 11.6))
  expect_identical(filled$cases_uncapped, filled$cases)
})

test_that("floor = cap gives a fixed value and cases_uncapped keeps the estimate", {
  local_fake_sources()
  out <- impute_suppressed(
    fake_county_atlas(suppressed = "01003"),
    floor = 2.5,
    cap = 2.5
  )
  filled <- out[out$fips == "01003", ]

  expect_equal(filled$cases, c(2.5, 2.5))
  expect_equal(filled$cases_uncapped, c(1.6, 11.6))
})

test_that("cases_uncapped is NA for published rows", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"))
  expect_equal(
    out$cases_uncapped[out$cases_source == "published"],
    c(NA_real_, NA_real_)
  )

  # Nothing suppressed: no estimate is made, and the column is still there.
  none <- impute_suppressed(fake_county_atlas())
  expect_identical(none$cases_uncapped, rep(NA_real_, nrow(none)))
})

test_that("cases_uncapped is NA with method = 'zero'", {
  out <- impute_suppressed(
    fake_county_atlas(suppressed = "01003"),
    method = "zero"
  )
  expect_identical(out$cases_uncapped, rep(NA_real_, nrow(out)))
})

test_that("cases_uncapped is NA for rows that could not be estimated", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")
  county$population[county$fips == "01003"] <- NA
  for (unestimated in c("na", "zero")) {
    out <- suppressWarnings(impute_suppressed(
      county,
      unestimated = unestimated
    ))
    rows <- out$cases_source == "not estimated"
    expect_equal(sum(rows), 2, label = unestimated)
    expect_identical(
      out$cases_uncapped[rows],
      c(NA_real_, NA_real_),
      label = unestimated
    )
  }

  # No state rate.
  state <- fake_state_atlas()
  state$population <- NA
  local_fake_sources(state = state)
  out <- suppressWarnings(
    impute_suppressed(fake_county_atlas(suppressed = "01003"))
  )
  expect_identical(out$cases_uncapped, rep(NA_real_, nrow(out)))
})

test_that("cases_uncapped is NA for rows that are not filled in", {
  local_fake_sources()
  # One county "Data not available", the other with no count and a status that
  # isn't suppression.
  county <- fake_county_atlas()
  county$cases <- NA_real_
  county$data_status <- factor(ifelse(
    county$fips == "01003",
    "Data not available",
    "No reported cases"
  ))
  out <- suppressWarnings(impute_suppressed(county))

  expect_setequal(out$cases_source, c("not available", "not estimated"))
  expect_identical(out$cases_uncapped, rep(NA_real_, nrow(out)))
})

test_that("cases_uncapped is computed in each stratum and state on its own", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  out <- impute_suppressed(fake_county_atlas("sex", suppressed = "01003"))
  filled <- out[out$fips == "01003", ]

  # Per 1e6, times 50,000. Diagnoses: male 32, female 34. Prevalence: male
  # 232, female 234.
  expect_equal(as.character(filled$sex), c("Male", "Female", "Male", "Female"))
  expect_equal(filled$cases_uncapped, c(1.6, 1.7, 11.6, 11.7))
  expect_equal(filled$cases, c(1.6, 1.7, 4, 4))

  # State 01: 32 and 232 per 1e6. State 02: 332 and 532 per 2e6.
  local_fake_sources(state = two_state_atlas()$state)
  two <- impute_suppressed(two_state_atlas()$county)
  two <- two[two$cases_source == "state_rate", ]
  expect_equal(two$fips, c("01003", "01003", "02003", "02003"))
  expect_equal(two$cases_uncapped, c(1.6, 11.6, 8.3, 13.3))
  expect_equal(two$cases, c(1.6, 4, 4, 4))
})

test_that("filled-in counts are fractions, not rounded to whole numbers", {
  local_fake_sources()
  out <- impute_suppressed(fake_county_atlas(suppressed = "01003"))
  filled <- out$cases[out$cases_source == "state_rate"]

  # Diagnoses: 32 per 1e6 times 50,000 = 1.6, under the cap of 4.
  # Prevalence: 11.6, capped at 4.
  expect_equal(filled, c(1.6, 4))
  expect_false(filled[1] == round(filled[1]))
})

test_that("each stratum is capped on its own, so strata needn't add up to the unstratified estimate", {
  # One suppressed county whose two sex strata have half its population each,
  # and a state rate of 240 per million in every stratum: before the cap the
  # strata (6 + 6) add up to the unstratified estimate (12).
  at_rate <- function(state) {
    state$cases <- 240
    state
  }
  county_by_sex <- fake_county_atlas("sex", suppressed = "01003")
  county_by_sex$population[county_by_sex$fips == "01003"] <- 25000
  prevalence_01003 <- function(out) {
    out[out$fips == "01003" & out$indicator == "HIV prevalence", ]
  }

  local_fake_sources(state = at_rate(fake_state_atlas()))
  total <- prevalence_01003(impute_suppressed(
    fake_county_atlas(suppressed = "01003")
  ))
  local_fake_sources(state = at_rate(fake_state_atlas(stratify_by = "sex")))
  by_sex <- prevalence_01003(impute_suppressed(county_by_sex))

  expect_equal(total$cases_uncapped, 12)
  expect_equal(by_sex$cases_uncapped, c(6, 6))
  expect_equal(sum(by_sex$cases_uncapped), total$cases_uncapped)

  # Each is capped at 4 on its own: 4 + 4 by sex against 4 unstratified.
  expect_equal(total$cases, 4)
  expect_equal(by_sex$cases, c(4, 4))
})

test_that("cases_uncapped uses the ACS population with population = 'acs'", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  county <- fake_county_atlas("sex", suppressed = "01003")
  out <- impute_suppressed(county, cap = 0.1, population = "acs")
  male <- out[
    out$fips == "01003" & out$sex == "Male" & out$indicator == "HIV diagnoses",
  ]

  pop <- fake_acs_tract("01003000100")
  male_13 <- sum(pop[sprintf("B01001_%03d", 6:25)]) + 0.4 * pop[["B01001_005"]]
  # CDC's population (50,000) would give 1.6.
  expect_equal(male$cases_uncapped, 32 / 1e6 * male_13)
  expect_false(isTRUE(all.equal(male$cases_uncapped, 1.6)))
  expect_equal(male$cases, 0.1)
})

test_that("cases and cases_source are what they were before cases_uncapped", {
  # Expected values are the output of the version without the column, on the
  # same made-up input.
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")
  unpopulated <- county
  unpopulated$population[unpopulated$fips == "01003"] <- NA
  before <- list(
    list(out = impute_suppressed(county), cases = c(11, 1.6, 111, 4)),
    list(out = impute_suppressed(county, cap = 1), cases = c(11, 1, 111, 1)),
    list(out = impute_suppressed(county, floor = 2), cases = c(11, 2, 111, 4)),
    list(
      out = impute_suppressed(county, cap = Inf),
      cases = c(11, 1.6, 111, 11.6)
    ),
    list(
      out = impute_suppressed(county, cap = Inf, population = "acs"),
      cases = c(11, 0.3512064, 111, 2.5462464)
    ),
    list(
      out = impute_suppressed(county, method = "zero"),
      cases = c(11, 0, 111, 0),
      source = "zero"
    ),
    list(
      out = suppressWarnings(impute_suppressed(unpopulated)),
      cases = c(11, NA, 111, NA),
      source = "not estimated"
    ),
    list(
      out = suppressWarnings(
        impute_suppressed(unpopulated, unestimated = "zero")
      ),
      cases = c(11, 0, 111, 0),
      source = "not estimated"
    )
  )
  for (i in seq_along(before)) {
    source <- before[[i]]$source %||% "state_rate"
    expect_equal(before[[i]]$out$cases, before[[i]]$cases, label = i)
    expect_equal(
      before[[i]]$out$cases_source,
      c("published", source, "published", source),
      label = i
    )
  }
})

test_that("impute_suppressed() stops if `x` already has cases_uncapped", {
  county <- fake_county_atlas(suppressed = "01003")
  county$cases_uncapped <- 1
  expect_snapshot(error = TRUE, impute_suppressed(county, method = "zero"))
})

test_that("population = 'acs' uses the county's tracts in the same stratum", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  county <- fake_county_atlas("sex", suppressed = "01003")
  out <- impute_suppressed(county, cap = Inf, population = "acs")
  male <- out[
    out$fips == "01003" & out$sex == "Male" & out$indicator == "HIV diagnoses",
  ]

  pop <- fake_acs_tract("01003000100")
  male_13 <- sum(pop[sprintf("B01001_%03d", 6:25)]) + 0.4 * pop[["B01001_005"]]
  expect_equal(male$cases, 32 / 1e6 * male_13)
  expect_equal(male$cases_source, "state_rate")
})

test_that("population = 'acs' keeps HIV at ages 13+ beside another disease (#53)", {
  mixed <- function(make, ...) {
    rbind(
      make(indicators = "HIV prevalence", ...),
      make(indicators = "Chlamydia", ...)
    )
  }
  local_fake_sources(state = mixed(fake_state_atlas))
  # One fake response holds both diseases' state rows.
  local_mocked_bindings(.request_atlas_all = function(payloads) {
    mixed(fake_state_atlas)
  })
  combined <- impute_suppressed(
    mixed(fake_county_atlas, suppressed = "01003"),
    cap = Inf,
    population = "acs"
  )
  hiv_alone <- impute_suppressed(
    fake_county_atlas(indicators = "HIV prevalence", suppressed = "01003"),
    cap = Inf,
    population = "acs"
  )

  hiv <- combined[
    combined$indicator == "HIV prevalence" &
      combined$fips == "01003",
  ]
  expect_equal(hiv$cases, hiv_alone$cases[hiv_alone$fips == "01003"])
})

# Strata the ACS populations can't match yet (#55) -------------------------------

sex_by_age <- function(suppressed = "01003") {
  young <- fake_county_atlas("sex", suppressed = suppressed)
  older <- young
  young$age <- factor("13-24")
  older$age <- factor("25-34")
  rbind(young, older)
}

test_that("population = 'acs' stops on two-way strata before any request", {
  local_mocked_bindings(
    .request_atlas = function(payload) stop("CDC was called"),
    .check_census_ready = function(call) invisible(),
    .get_acs_areas = function(...) stop("the Census was called")
  )
  expect_snapshot(
    error = TRUE,
    impute_suppressed(sex_by_age(), population = "acs")
  )
})

test_that("population = 'acs' stops for congenital syphilis", {
  local_mocked_bindings(
    .request_atlas = function(payload) stop("CDC was called"),
    .check_census_ready = function(call) invisible(),
    .get_acs_areas = function(...) stop("the Census was called")
  )
  congenital <- fake_county_atlas(
    suppressed = "01003",
    indicators = "Congenital Syphilis"
  )
  expect_snapshot(
    error = TRUE,
    impute_suppressed(congenital, population = "acs")
  )
})

test_that("Knowledge of Status uses CDC's denominator, people with HIV (#108)", {
  # State: 80 of 100 people with HIV know their status. County: 10 people with
  # HIV, 1,000 residents. The estimate is 0.8 x 10 = 8, not 0.8 x 1,000.
  status <- fake_county_atlas(
    suppressed = "01003",
    indicators = "Knowledge of Status"
  )
  status$population[status$fips == "01003"] <- 10
  state <- fake_state_atlas(indicators = "Knowledge of Status")
  state$cases <- 80
  state$population <- 100
  local_fake_sources(state = state)

  out <- impute_suppressed(status, cap = Inf)
  expect_equal(out$cases[out$fips == "01003"], 8)
})

test_that("population = 'acs' stops for Knowledge of Status before any request (#108)", {
  local_mocked_bindings(
    .request_atlas = function(payload) stop("CDC was called"),
    .check_census_ready = function(call) invisible(),
    .get_acs_areas = function(...) stop("the Census was called")
  )
  status <- fake_county_atlas(
    suppressed = "01003",
    indicators = c("Estimated HIV incidence", "Knowledge of Status")
  )
  expect_snapshot(
    error = TRUE,
    impute_suppressed(status, population = "acs")
  )
})

test_that("population = 'acs' works on age strata for chlamydia", {
  chlamydia <- fake_county_atlas(
    "age",
    suppressed = "01003",
    indicators = "Chlamydia"
  )
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "age", indicators = "Chlamydia")
  )
  out <- suppressWarnings(impute_suppressed(chlamydia, population = "acs"))
  known <- out$fips == "01003" & out$age != "Unknown"
  expect_equal(sum(is.na(out$cases[known])), 0)
})

test_that("two-way strata still work with CDC populations and with zeros", {
  state <- fake_state_atlas(stratify_by = "sex")
  state <- rbind(
    transform(state, age = "13-24"),
    transform(state, age = "25-34")
  )
  local_fake_sources(state = state)

  cdc <- suppressWarnings(impute_suppressed(sex_by_age()))
  zero <- impute_suppressed(sex_by_age(), method = "zero")

  expect_equal(unique(cdc$cases_source[cdc$fips == "01003"]), "state_rate")
  expect_equal(unique(zero$cases[zero$fips == "01003"]), 0)
})

test_that("unsupported strata don't matter when nothing is suppressed", {
  local_fake_sources()
  out <- impute_suppressed(
    sex_by_age(suppressed = character(0)),
    population = "acs"
  )
  expect_equal(unique(out$cases_source), "published")
})

test_that("population = 'acs' needs a Census key", {
  withr::local_envvar(CENSUS_API_KEY = "")
  local_mocked_bindings(.request_atlas = function(payload) {
    stop("CDC was called")
  })
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

test_that("population = 'acs' doesn't treat a missing population as 0 (#52)", {
  acs <- fake_acs()
  acs$estimate[acs$GEOID == "01003000100" & acs$variable == "B01001_030"] <- NA
  local_fake_sources(acs = acs)
  county <- fake_county_atlas(suppressed = "01003")

  out <- suppressWarnings(impute_suppressed(
    county,
    cap = Inf,
    population = "acs"
  ))
  suppressed <- out[out$fips == "01003", ]

  expect_equal(suppressed$cases, c(NA_real_, NA_real_))
  expect_equal(unique(suppressed$cases_source), "not estimated")
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

# How often the cap and floor decided (#164) -------------------------------------

test_that("the message says how many estimates the cap decided (#164)", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")

  # Estimates before the cap: 1.6 and 11.6, so the cap of 4 decides one of
  # the two.
  expect_snapshot(out <- impute_suppressed(county))
  expect_equal(out$cases_uncapped[out$fips == "01003"], c(1.6, 11.6))
})

test_that("an estimate exactly equal to the cap isn't counted as decided by it (#164)", {
  # Estimates are exactly 4 and 29 (see the cases_uncapped test above).
  state <- fake_state_atlas()
  state$population <- 64
  local_fake_sources(state = state)
  county <- fake_county_atlas(suppressed = "01003")
  county$population <- 8

  expect_message(
    impute_suppressed(county, cap = 4),
    "cap of 4 set the value of 1 row (50%)",
    fixed = TRUE
  )
  # Both estimates are above a cap of 3.
  expect_message(
    impute_suppressed(county, cap = 3),
    "cap of 3 set the value of 2 rows (100%)",
    fixed = TRUE
  )
})

test_that("the message says how many estimates a raised floor decided (#164)", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")

  # Estimates 1.6 and 11.6: a floor of 2 decides the first, the cap of 4 the
  # second.
  expect_snapshot(out <- impute_suppressed(county, floor = 2))
})

test_that("an estimate exactly equal to the floor isn't counted as decided by it (#164)", {
  # Estimates are exactly 4 and 29, as above.
  state <- fake_state_atlas()
  state$population <- 64
  local_fake_sources(state = state)
  county <- fake_county_atlas(suppressed = "01003")
  county$population <- 8

  expect_message(
    impute_suppressed(county, floor = 4, cap = 1e6),
    "floor of 4 set the value of 0 rows (0%)",
    fixed = TRUE
  )
  expect_message(
    impute_suppressed(county, floor = 5, cap = 1e6),
    "floor of 5 set the value of 1 row (50%)",
    fixed = TRUE
  )
  # A large cap is written out in full.
  expect_message(
    impute_suppressed(county, floor = 4, cap = 1e6),
    "cap of 1,000,000 set the value of 0 rows (0%)",
    fixed = TRUE
  )
})

test_that("limits that can't decide anything aren't mentioned (#164)", {
  local_fake_sources()
  county <- fake_county_atlas(suppressed = "01003")

  # No cap, and a floor of 0, which no estimate can be below.
  expect_snapshot(out <- impute_suppressed(county, cap = Inf))
})

test_that("method = 'zero' and data with nothing suppressed give no message (#164)", {
  local_fake_sources()

  expect_no_message(
    impute_suppressed(fake_county_atlas(suppressed = "01003"), method = "zero")
  )
  expect_no_message(impute_suppressed(fake_county_atlas()))
})

test_that("there is no message when no suppressed row could be estimated (#164)", {
  county <- fake_county_atlas(suppressed = "01003")
  county$population <- NA
  local_fake_sources()

  expect_no_message(
    out <- suppressWarnings(impute_suppressed(county)),
    class = "rlang_message"
  )
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "not estimated")
})

test_that("the message counts only rows that were estimated (#164)", {
  # Diagnoses can't be estimated (no state total); prevalence is estimated at
  # 11.6 and capped. The percentage is of the one estimated row.
  state <- fake_state_atlas()
  state$cases[state$indicator == "HIV diagnoses"] <- NA
  local_fake_sources(state = state)
  county <- fake_county_atlas(suppressed = "01003")

  expect_message(
    suppressWarnings(impute_suppressed(county)),
    "cap of 4 set the value of 1 row (100%)",
    fixed = TRUE
  )
  expect_message(
    suppressWarnings(impute_suppressed(county)),
    "1 suppressed row was filled in",
    fixed = TRUE
  )
})

# Fitting to county totals (#179) -----------------------------------------------

test_that("fit_to = 'county_total' fits suppressed strata to the county total", {
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "sex"),
    county_unstratified = fake_county_unstratified()
  )
  county <- fake_county_atlas(stratify_by = "sex", suppressed = "01003")

  expect_snapshot(out <- impute_suppressed(county, fit_to = "county_total"))

  # State rate x county population: the state's Male and Female cases are
  # 11 + 21 = 32 and 12 + 22 = 34 (diagnoses), 232 and 234 (prevalence), over
  # 1,000,000 people; each county has 50,000.
  seeds <- list(
    "HIV diagnoses" = c(32, 34) / 20,
    "HIV prevalence" = c(232, 234) / 20
  )
  totals <- c("HIV diagnoses" = 43, "HIV prevalence" = 243)
  for (indicator in names(totals)) {
    rows <- out$fips == "01003" & out$indicator == indicator
    expect_equal(
      out$cases[rows],
      totals[[indicator]] * seeds[[indicator]] / sum(seeds[[indicator]])
    )
    expect_equal(sum(out$cases[rows]), totals[[indicator]])
    expect_equal(out$cases_uncapped[rows], seeds[[indicator]])
  }
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "county_fit")
  expect_equal(
    unique(out$fit_reason[out$fips == "01003"]),
    "fitted to county total"
  )
  expect_equal(out$fit_reason[out$fips == "01001"], rep(NA_character_, 4))
  expect_equal(
    out$cases[out$fips == "01001"],
    county$cases[county$fips == "01001"]
  )
})

test_that("fit_to = 'none' (the default) downloads no county totals", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  county <- fake_county_atlas(stratify_by = "sex", suppressed = "01003")
  out <- suppressMessages(impute_suppressed(county))
  # Diagnoses' state-rate estimates (32 and 34 / 20) are under the cap of 4.
  expect_equal(out$cases[out$fips == "01003"], c(32 / 20, 34 / 20, 4, 4))
  expect_equal(out$fit_reason, rep(NA_character_, 8))
})

test_that("unfitted groups keep the state-rate estimate, and only they count for the cap", {
  # Prevalence's county total is suppressed: it falls back to the capped
  # state rate. Diagnoses are fitted.
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "sex"),
    county_unstratified = fake_county_unstratified(
      list("01001" = c(23, 223), "01003" = c(43, NA))
    )
  )
  county <- fake_county_atlas(stratify_by = "sex", suppressed = "01003")

  expect_snapshot(out <- impute_suppressed(county, fit_to = "county_total"))
  prevalence <- out$fips == "01003" & out$indicator == "HIV prevalence"
  expect_equal(out$cases[prevalence], c(4, 4))
  expect_equal(unique(out$cases_source[prevalence]), "state_rate")
  expect_equal(unique(out$fit_reason[prevalence]), "county total suppressed")
})

test_that("race is never fitted and needs no download", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "race"))
  county <- fake_county_atlas(stratify_by = "race", suppressed = "01003")
  out <- suppressMessages(impute_suppressed(county, fit_to = "county_total"))
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "state_rate")
  expect_equal(unique(out$fit_reason[out$fips == "01003"]), "race not fitted")
})

test_that("a filtered age download is not fitted, with a warning", {
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "age"),
    county_unstratified = fake_county_unstratified()
  )
  county <- fake_county_atlas(stratify_by = "age", suppressed = "01003")
  county <- county[county$age != "65+", ]

  expect_warning(
    out <- suppressMessages(impute_suppressed(county, fit_to = "county_total")),
    class = "cdcatlas_unfittable_county_strata"
  )
  rows <- out$fips == "01003"
  expect_equal(unique(out$cases_source[rows]), "state_rate")
  expect_equal(
    unique(out$fit_reason[rows]),
    "strata incomplete or inconsistent"
  )
})

test_that("eligibility is decided before rows are dropped or nothing is suppressed", {
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "sex"),
    county_unstratified = fake_county_unstratified()
  )
  county <- fake_county_atlas(stratify_by = "sex")
  county$cases[county$fips == "01003" & county$sex == "Male"] <- NA
  county$data_status[county$fips == "01003" & county$sex == "Male"] <-
    "Data not available"

  expect_warning(
    out <- impute_suppressed(county, fit_to = "county_total"),
    class = "cdcatlas_unfittable_county_strata"
  )
  rows <- is.na(out$cases)
  expect_equal(
    unique(out$fit_reason[rows]),
    "strata incomplete or inconsistent"
  )
  expect_equal(out$fit_reason[!rows], rep(NA_character_, 6))

  # Dropping leaves only published rows, but the group is still judged on
  # the rows as given.
  expect_warning(
    out <- suppressMessages(
      impute_suppressed(county, fit_to = "county_total", not_available = "drop")
    ),
    class = "cdcatlas_unfittable_county_strata"
  )
  expect_equal(out$fit_reason, rep(NA_character_, 6))
})

test_that("eligibility survives `not_available = \"drop\"`, in row order", {
  # 01001's Male rows are not available and its Female rows suppressed; both
  # of 01003's are suppressed. 01001 is judged with its dropped rows, so it is
  # not fitted and keeps the state rate. 01003 is fitted.
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "sex"),
    county_unstratified = fake_county_unstratified()
  )
  county <- fake_county_atlas(stratify_by = "sex", suppressed = both_counties)
  dropped <- county$fips == "01001" & county$sex == "Male"
  county$data_status[dropped] <- "Data not available"

  expect_snapshot(
    out <- impute_suppressed(
      county,
      fit_to = "county_total",
      not_available = "drop"
    )
  )
  expect_equal(out$fips, county$fips[!dropped])
  expect_equal(out$sex, county$sex[!dropped])
  kept <- out$fips == "01001"
  expect_equal(
    out$fit_reason[kept],
    rep("strata incomplete or inconsistent", 2)
  )
  expect_equal(out$cases_source[kept], rep("state_rate", 2))
  # Female's state-rate estimates: 34 / 20 for diagnoses, and prevalence's
  # 234 / 20 capped at 4.
  expect_equal(out$cases[kept], c(34 / 20, 4))
  expect_equal(unique(out$cases_source[!kept]), "county_fit")
  expect_equal(sum(out$cases[!kept & out$indicator == "HIV diagnoses"]), 43)
})

test_that("fitting a table with every count published downloads nothing", {
  local_fake_sources(state = fake_state_atlas(stratify_by = "sex"))
  county <- fake_county_atlas(stratify_by = "sex")
  out <- impute_suppressed(county, fit_to = "county_total")
  expect_equal(out$cases, county$cases)
  expect_equal(out$fit_reason, rep(NA_character_, 8))
})

test_that("county counts above the state total are reported, not adjusted", {
  # Both counties suppressed, with county totals far above the state's.
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "sex"),
    county_unstratified = fake_county_unstratified(
      list("01001" = c(100, 300), "01003" = c(100, 300))
    )
  )
  county <- fake_county_atlas(stratify_by = "sex", suppressed = both_counties)
  expect_warning(
    out <- suppressMessages(impute_suppressed(county, fit_to = "county_total")),
    class = "cdcatlas_fit_exceeds_state"
  )
  expect_snapshot(
    out <- suppressMessages(impute_suppressed(county, fit_to = "county_total"))
  )
  expect_equal(unique(out$cases_source), "county_fit")
})

test_that("a withheld state remainder stays out of the state check", {
  # One suppressed county: whether it pushes the state over its total would
  # bound that county's count, so it isn't reported.
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "sex"),
    county_unstratified = fake_county_unstratified(
      list("01001" = c(23, 223), "01003" = c(100, 300))
    )
  )
  county <- fake_county_atlas(stratify_by = "sex", suppressed = "01003")
  expect_no_warning(
    out <- suppressMessages(impute_suppressed(county, fit_to = "county_total"))
  )
  expect_equal(unique(out$cases_source[out$fips == "01003"]), "county_fit")
})

test_that("fit_to is checked before anything is downloaded", {
  local_fake_sources()
  by_sex <- fake_county_atlas(stratify_by = "sex", suppressed = "01003")
  expect_snapshot(error = TRUE, {
    impute_suppressed(by_sex, fit_to = "county")
    impute_suppressed(by_sex, method = "zero", fit_to = "county_total")
    impute_suppressed(
      fake_county_atlas(suppressed = "01003"),
      fit_to = "county_total"
    )
    impute_suppressed(
      fake_county_atlas(
        stratify_by = "sex",
        indicators = "Chlamydia",
        suppressed = "01003"
      ),
      fit_to = "county_total"
    )
  })
  two_way <- by_sex
  two_way$age <- "13-24"
  expect_snapshot(
    error = TRUE,
    impute_suppressed(two_way, fit_to = "county_total")
  )
  with_column <- by_sex
  with_column$fit_reason <- NA
  expect_snapshot(error = TRUE, impute_suppressed(with_column))
})
