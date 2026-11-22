# Tract extrapolation on a small made-up state (see helper-pipeline.R), with CDC
# and the Census replaced. These tests don't need tidycensus.

run_extrapolation <- function(
  stratify_by = NULL,
  county = fake_county_atlas(stratify_by),
  state = fake_state_atlas(stratify_by = stratify_by),
  acs = fake_acs(),
  county_totals = NULL,
  ...,
  env = parent.frame()
) {
  local_fake_sources(
    state = state,
    acs = acs,
    county_totals = county_totals,
    env = env
  )
  if (!"cases_source" %in% names(county)) {
    county <- impute_suppressed(county)
  }
  # Not the subject of the tests that use this runner; see
  # without_unplaced_warning().
  without_unplaced_warning(
    suppressMessages(extrapolate_from_county(county, ...))
  )
}

# Adding up ---------------------------------------------------------------------

for (strat in list(NULL, "sex", "race", "age")) {
  label <- if (is.null(strat)) "unstratified" else paste("by", strat)

  test_that(paste("tract estimates add up to county counts,", label), {
    check <- county_vs_areas(run_extrapolation(strat))
    expect_equal(check$county_values, rep(1, nrow(check)))
    expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-8)
  })

  test_that(paste("every tract appears once per county stratum,", label), {
    check <- county_vs_areas(run_extrapolation(strat))
    expected <- split(fake_tracts$geoid, fake_tracts$county_fips)

    expect_equal(nrow(check), 2 * 2 * nrow(strata_for(strat)))
    expect_equal(check$area_ids, unname(expected[check$county_fips]))
  })

  test_that(paste("tract estimates are never missing or negative,", label), {
    tracts <- run_extrapolation(strat)
    expect_equal(sum(is.na(tracts$area_cases)), 0)
    expect_gte(min(tracts$area_cases), 0)
  })
}

test_that("tract estimates are county count x tract share of the stratum", {
  tracts <- run_extrapolation("sex")
  one <- tracts[
    tracts$county_fips == "01001" &
      tracts$sex == "Male" &
      tracts$indicator == "HIV prevalence",
  ]

  share <- one$area_population_acs / sum(one$area_population_acs)
  expect_equal(one$area_cases, 111 * share)
  expect_equal(unique(one$county_population_acs), sum(one$area_population_acs))
})

test_that("there is no area_noncases column (#117)", {
  # Population minus cases means people without the condition only for
  # prevalence; for diagnoses, infections and Knowledge of Status it doesn't.
  tracts <- run_extrapolation(NULL)
  expect_equal(intersect(names(tracts), "area_noncases"), character(0))
  expect_equal(names(tracts)[ncol(tracts)], "area_cases")
})

test_that("the output has no rate or rate-bound columns (#157)", {
  # Records what extrapolate_from_county() does today, before #157 decides
  # what the rate columns should hold after imputation: CDC's county rates,
  # with or without one on a suppressed row, are not carried to the areas.
  for (rate_without_count in c(FALSE, TRUE)) {
    county <- with_fake_rates(
      fake_county_atlas(suppressed = "01003"),
      rate_without_count = rate_without_count
    )
    expect_contains(names(county), rate_columns)
    for (to in c("tract", "block group")) {
      areas <- run_extrapolation(county = county, to = to)
      label <- paste(to, "rate without count:", rate_without_count)

      expect_gt(nrow(areas), 0, label = label)
      expect_equal(
        intersect(names(areas), rate_columns),
        character(0),
        label = label
      )
    }
  }

  # A county that is left out, carrying rates: 01003 has no CDC data.
  county <- with_fake_rates(fake_county_atlas(), rate_without_count = TRUE)
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"
  left_out <- attr(run_extrapolation(county = county), "not_allocated")

  expect_equal(left_out$county_fips, c("01003", "01003"))
  expect_equal(intersect(names(left_out), rate_columns), character(0))
})

test_that("allocation uses known shares after saving and reordering county rows", {
  county <- data.frame(
    indicator = "Chlamydia",
    year = 2022,
    geography = c("Synthetic A", "Synthetic B"),
    data_status = "Not Suppressed",
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = "All age groups",
    cases = c(12, 20),
    population = c(400, 100),
    fips = c("01001", "01003")
  )
  state <- county[1, ]
  state$fips <- "01"
  state$cases <- 32
  state$population <- 500
  acs <- expand.grid(
    GEOID = c("01001000100", "01001000200", "01003000100"),
    variable = sprintf("B01001_%03d", 1:49),
    stringsAsFactors = FALSE
  )
  acs$NAME <- "Synthetic tract"
  acs$estimate <- 0
  totals <- acs$variable %in% c("B01001_001", "B01001_003")
  acs$estimate[totals] <- rep(c(100, 300, 100), 2)
  local_mocked_bindings(
    .request_atlas = function(payload) state,
    .check_census_ready = function(call) invisible(),
    .get_acs_areas = function(...) acs[nrow(acs):1, ]
  )

  saved <- withr::local_tempfile(fileext = ".rds")
  saveRDS(impute_suppressed(county), saved)
  restored <- readRDS(saved)[2:1, ]
  areas <- suppressMessages(extrapolate_from_county(restored))
  areas <- areas[order(areas$geoid), ]

  # Independently specified shares: 12 * 1/4, 12 * 3/4, and all of county B.
  expect_equal(areas$geoid, c("01001000100", "01001000200", "01003000100"))
  expect_equal(areas$county_fips, c("01001", "01001", "01003"))
  expect_equal(areas$area_population_acs, c(100, 300, 100))
  expect_equal(areas$area_cases, c(3, 9, 20))
  expect_equal(attr(areas, "no_county_cases")$no_county_cases, 0)
  expect_equal(nrow(attr(areas, "not_allocated")), 0)
})

# Census denominators ------------------------------------------------------------

test_that("HIV denominators count ages 13+, with 2/5 of the 10-14 band", {
  tracts <- run_extrapolation(NULL)
  pop <- fake_acs_tract("01001000100")

  bands_15_plus <- sprintf("B01001_%03d", c(6:25, 30:49))
  expected <- sum(pop[bands_15_plus]) +
    0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
  got <- unique(tracts$area_population_acs[tracts$geoid == "01001000100"])

  expect_equal(got, expected)
})

test_that("other diseases use all ages", {
  county <- fake_county_atlas(indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(indicators = "Chlamydia")
  )
  pop <- fake_acs_tract("01001000100")
  all_bands <- sprintf("B01001_%03d", c(3:25, 27:49))
  got <- unique(tracts$area_population_acs[tracts$geoid == "01001000100"])

  expect_equal(got, sum(pop[all_bands]))
  expect_equal(unique(tracts$age), "All age groups")
})

test_that("HIV keeps ages 13+ when combined with another disease (#53)", {
  mixed <- function(make) {
    rbind(make(indicators = "HIV prevalence"), make(indicators = "Chlamydia"))
  }
  # One fake response holds both diseases' state rows.
  local_mocked_bindings(.request_atlas_all = function(payloads) {
    mixed(fake_state_atlas)
  })
  combined <- run_extrapolation(
    county = mixed(fake_county_atlas),
    state = mixed(fake_state_atlas)
  )
  hiv_alone <- run_extrapolation(
    county = fake_county_atlas(indicators = "HIV prevalence"),
    state = fake_state_atlas(indicators = "HIV prevalence")
  )
  chlamydia_alone <- run_extrapolation(
    county = fake_county_atlas(indicators = "Chlamydia"),
    state = fake_state_atlas(indicators = "Chlamydia")
  )

  cols <- c("geoid", "area_population_acs", "area_cases")
  hiv <- combined[combined$indicator == "HIV prevalence", cols]
  chlamydia <- combined[combined$indicator == "Chlamydia", cols]
  expect_equal(hiv, hiv_alone[cols], ignore_attr = TRUE)
  expect_equal(chlamydia, chlamydia_alone[cols], ignore_attr = TRUE)
})

test_that("`ages` overrides the default", {
  tracts <- run_extrapolation(NULL, ages = "all")
  pop <- fake_acs_tract("01001000100")
  all_bands <- sprintf("B01001_%03d", c(3:25, 27:49))
  got <- unique(tracts$area_population_acs[tracts$geoid == "01001000100"])

  expect_equal(got, sum(pop[all_bands]))
})

test_that("age group 13-24 combines the right ACS bands for both sexes", {
  tracts <- run_extrapolation("age")
  pop <- fake_acs_tract("01001000100")

  bands <- sprintf("B01001_%03d", c(6:10, 30:34))
  expected <- sum(pop[bands]) +
    0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
  got <- unique(tracts$area_population_acs[
    tracts$geoid == "01001000100" & tracts$age == "13-24"
  ])

  expect_equal(got, expected)
})

test_that("race denominators use B03002, scaled to the tract's 13+ share", {
  tracts <- run_extrapolation("race")
  pop <- fake_acs_tract("01001000100")

  total_13 <- sum(pop[sprintf("B01001_%03d", c(6:25, 30:49))]) +
    0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
  expected <- pop[["B03002_004"]] * total_13 / pop[["B01001_001"]]
  got <- unique(tracts$area_population_acs[
    tracts$geoid == "01001000100" &
      tracts$race_ethnicity == "Black/African American"
  ])

  expect_equal(got, expected)
})

test_that("race denominators aren't scaled for all ages", {
  county <- fake_county_atlas("race", indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(stratify_by = "race", indicators = "Chlamydia")
  )
  got <- unique(tracts$area_population_acs[
    tracts$geoid == "01001000100" & tracts$race_ethnicity == "White"
  ])

  expect_equal(got, fake_acs_tract("01001000100")[["B03002_003"]])
})

# Empty strata ----------------------------------------------------------------------

# Like Loving County, TX: ages 35-44 have no ACS population in 01003.
acs_without_35_44 <- function() {
  acs <- fake_acs()
  bands <- c("B01001_013", "B01001_014", "B01001_037", "B01001_038")
  acs$estimate[
    substr(acs$GEOID, 1, 5) == "01003" & acs$variable %in% bands
  ] <- 0
  acs
}

test_that("empty strata are shared by total population by default", {
  tracts <- run_extrapolation("age", acs = acs_without_35_44())
  empty <- tracts[tracts$county_fips == "01003" & tracts$age == "35-44", ]

  expect_equal(sum(empty$area_cases), sum(empty$county_cases))
  expect_equal(unique(empty$allocation), "total population")
  expect_equal(
    unique(tracts$allocation[tracts$age != "35-44"]),
    "stratum population"
  )
})

test_that("empty strata can be shared equally", {
  acs <- fake_acs()
  bands <- c("B01001_013", "B01001_014", "B01001_037", "B01001_038")
  acs$estimate[
    substr(acs$GEOID, 1, 5) == "01001" & acs$variable %in% bands
  ] <- 0

  tracts <- run_extrapolation("age", acs = acs, empty_strata = "equal")
  empty <- tracts[tracts$county_fips == "01001" & tracts$age == "35-44", ]

  expect_equal(unique(empty$allocation), "equal shares")
  expect_equal(empty$area_cases, empty$county_cases / 3)
})

test_that("total-population fallback becomes equal shares if the county is empty", {
  acs <- fake_acs()
  acs$estimate[substr(acs$GEOID, 1, 5) == "01003"] <- 0

  tracts <- run_extrapolation(NULL, acs = acs)
  empty <- tracts[tracts$county_fips == "01003", ]

  expect_equal(unique(empty$allocation), "equal shares")
  expect_equal(empty$area_cases, empty$county_cases)
})

test_that("empty strata can be left NA, with a warning", {
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "age"),
    acs = acs_without_35_44()
  )
  county <- impute_suppressed(fake_county_atlas("age"))

  expect_snapshot(
    tracts <- extrapolate_from_county(county, empty_strata = "none")
  )
  empty <- tracts[tracts$county_fips == "01003" & tracts$age == "35-44", ]
  expect_equal(unique(empty$allocation), "none")
  expect_equal(empty$area_cases, c(NA_real_, NA_real_))
  expect_equal(sum(is.na(tracts$area_cases)), 2)
})

test_that("a stratum with no cases gives 0, even with no population", {
  acs <- acs_without_35_44()
  county <- fake_county_atlas("age")
  county$cases[county$fips == "01003" & county$age == "35-44"] <- 0

  tracts <- run_extrapolation(
    "age",
    county = county,
    acs = acs,
    empty_strata = "none"
  )
  empty <- tracts[tracts$county_fips == "01003" & tracts$age == "35-44", ]

  expect_equal(empty$area_cases, c(0, 0))
})

# Suppressed and no-county cases -----------------------------------------------------

test_that("imputed counts are allocated like published ones", {
  county <- impute_suppressed(
    fake_county_atlas(suppressed = "01003"),
    method = "zero"
  )
  tracts <- run_extrapolation(county = county)
  small <- tracts[tracts$county_fips == "01003", ]

  expect_equal(unique(small$cases_source), "zero")
  expect_equal(small$area_cases, c(0, 0))
})

test_that("no-county cases are reported, not allocated, by default", {
  state <- fake_state_atlas(no_county = 500, suppressed = character(0))
  county <- fake_county_atlas(suppressed = "01003")
  tracts <- run_extrapolation(county = county, state = state)
  report <- attr(tracts, "no_county_cases")

  imputed <- c(4, 4)
  expect_equal(report$imputed_cases, imputed)
  expect_equal(report$no_county_cases, c(32, 232) + 500 - c(11, 111) - imputed)
  expect_equal(sum(tracts$area_cases), sum(c(11, 111) + imputed))
  expect_equal(unique(tracts$county_cases_spread), 0)
})

test_that("no-county cases can be spread across counties by population", {
  state <- fake_state_atlas(no_county = 500, suppressed = character(0))
  county <- fake_county_atlas(suppressed = "01003")
  tracts <- run_extrapolation(
    county = county,
    state = state,
    no_county = "spread"
  )
  report <- attr(tracts, "no_county_cases")

  expect_equal(
    sum(tracts$area_cases),
    sum(report$state_cases)
  )
  spread <- unique(tracts[c(
    "county_fips",
    "indicator",
    "county_cases_spread",
    "county_population_acs"
  )])
  diagnoses <- spread[spread$indicator == "HIV diagnoses", ]
  expect_equal(
    diagnoses$county_cases_spread,
    report$no_county_cases[1] *
      diagnoses$county_population_acs /
      sum(diagnoses$county_population_acs)
  )
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases)
})

test_that("each state's remainder is spread within that state", {
  two <- two_state_atlas()
  county <- impute_suppressed(two$county, method = "zero")
  tracts <- run_extrapolation(
    county = county,
    state = two$state,
    acs = two$acs,
    no_county = "spread"
  )
  by_state <- tracts |>
    dplyr::summarize(
      area_cases = sum(area_cases),
      .by = c(state_fips, indicator)
    )
  state <- two$state[order(two$state$fips, two$state$indicator), ]

  expect_equal(by_state$area_cases, state$cases)
})

test_that("remainders with no county to receive them are reported", {
  # State 02's counties have no tracts, so its remainder can't be spread.
  two <- two_state_atlas()
  acs <- two$acs[substr(two$acs$GEOID, 1, 2) == "01", ]
  local_fake_sources(state = two$state, acs = acs)
  county <- impute_suppressed(two$county, method = "zero")

  expect_snapshot(
    invisible(extrapolate_from_county(county, no_county = "spread"))
  )
})

test_that("spread cases that reach no area are reported as not spread (#114)", {
  # Two counties with 10 cases each and a state remainder of 20, spread by
  # cases: 10 to each. County 01001 has no ACS population in the stratum and
  # empty_strata = "none", so its 10 reach no tract.
  x <- fake_county_atlas()[1, ]
  x$cases <- 10
  county <- .county_cases(transform(x, cases_source = "published"))
  areas <- county[rep(1, 2), ]
  areas$county_fips <- c("01001", "01003")
  areas$county_cases <- c(10, 10)
  areas$county_cases_unknown <- 0
  areas$area_population_acs <- c(0, 100)
  areas$area_total_population <- c(0, 100)
  areas$geoid <- c("01001000100", "01003000100")
  report <- data.frame(
    state_fips = "01",
    lapply(as.data.frame(x[.atlas_keys]), as.character),
    no_county_cases = 20
  )
  report$year <- as.numeric(report$year)

  out <- .allocate(.spread_no_county(areas, report, "none", "cases"), "none")
  expect_equal(out$county_cases_spread, c(10, 10))
  expect_equal(out$area_cases, c(NA_real_, 20))
  expect_message(.announce_spread(out, report, "tract"), "could not be spread")
  expect_snapshot(.announce_spread(out, report, "tract"))

  # The warning about the state's remainder (20 of a state total of 40)
  # doesn't claim the cases were placed: half of them reached no tract (#164).
  report$state <- "Alabama"
  report$state_cases <- 40
  # Another warning here says a county stratum had no ACS population.
  caught <- suppressWarnings(catch_class(suppressMessages(
    .announce_extrapolation(
      out,
      report,
      not_allocated = data.frame(
        reason = character(),
        county_cases = numeric()
      ),
      no_county = "spread",
      empty_strata = "none",
      to = "tract"
    )
  )))
  expect_length(caught, 1)
  expect_match(
    conditionMessage(caught[[1]]),
    "any of those cases that reach areas are placed by assumption",
    fixed = TRUE
  )
})

test_that("age strata use 13+ total population whatever `ages` says", {
  default <- run_extrapolation("age", acs = acs_without_35_44())
  all_ages <- run_extrapolation("age", acs = acs_without_35_44(), ages = "all")

  expect_equal(all_ages$area_total_population, default$area_total_population)
  expect_equal(all_ages$area_cases, default$area_cases)
})

test_that("negative remainders are not spread", {
  state <- fake_state_atlas(no_county = -100)
  tracts <- run_extrapolation(state = state, no_county = "spread")

  expect_equal(unique(tracts$county_cases_spread), 0)
})

test_that("spreading stops when `x` lacks some of a state's counties (#54)", {
  full <- fake_county_atlas()
  one_county <- full[full$fips == "01001", ]

  expect_snapshot(
    error = TRUE,
    run_extrapolation(
      county = one_county,
      county_totals = full,
      no_county = "spread"
    )
  )
})

test_that("spreading stops for HIV estimates, whose counties are EHE only (#109)", {
  # State total 100; the EHE counties returned hold 30. The other 70 are
  # partly in counties CDC doesn't estimate, so not simply cases with no county.
  estimate <- fake_county_atlas(indicators = "Estimated HIV incidence")
  local_mocked_bindings(
    .request_atlas = function(payload) stop("CDC was called"),
    .check_census_ready = function(call) invisible(),
    .get_acs_areas = function(...) stop("the Census was called")
  )
  county <- impute_suppressed(estimate, method = "zero")

  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, no_county = "spread")
  )
})

test_that("spreading works on whole states filtered from a larger table (#54)", {
  two <- two_state_atlas()
  state_02 <- two$county[substr(two$county$fips, 1, 2) == "02", ]

  tracts <- run_extrapolation(
    county = state_02,
    state = two$state,
    acs = two$acs,
    county_totals = two$county,
    no_county = "spread"
  )

  expect_equal(unique(tracts$state_fips), "02")
  expect_gt(sum(tracts$county_cases_spread), 0)
})

test_that("counties CDC lists as not available don't count as missing (#54)", {
  county <- fake_county_atlas()
  unavailable <- county[county$fips == "01003", ]
  unavailable$fips <- "01005"
  unavailable$geography <- factor("County 01005")
  unavailable$cases <- NA_real_
  unavailable$data_status[] <- "Data not available"

  tracts <- run_extrapolation(
    county = county,
    county_totals = rbind(county, unavailable),
    no_county = "spread"
  )

  expect_setequal(unique(tracts$county_fips), c("01001", "01003"))
})

test_that("a 'Data not available' county with a count still counts (#54)", {
  county <- fake_county_atlas()
  labelled_unavailable <- county
  labelled_unavailable$data_status[labelled_unavailable$fips == "01003"] <-
    "Data not available"
  one_county <- county[county$fips == "01001", ]

  expect_error(
    run_extrapolation(
      county = one_county,
      county_totals = labelled_unavailable,
      no_county = "spread"
    ),
    class = "rlang_error",
    regexp = "01003"
  )
})

test_that("each state is checked only for its own years (#54)", {
  two <- two_state_atlas()
  y2017 <- two$county
  y2017$year <- 2017
  both_years <- rbind(two$county, y2017)
  state_both <- rbind(two$state, transform(two$state, year = 2017))
  # State 01 in 2022 and state 02 in 2017, each complete.
  x <- both_years[
    (substr(both_years$fips, 1, 2) == "01" & both_years$year == 2022) |
      (substr(both_years$fips, 1, 2) == "02" & both_years$year == 2017),
  ]

  expect_no_error(suppressWarnings(run_extrapolation(
    county = x,
    state = state_both,
    acs = two$acs,
    county_totals = both_years,
    no_county = "spread"
  )))
})

test_that("the default report mode doesn't check for missing counties (#54)", {
  full <- fake_county_atlas()
  one_county <- full[full$fips == "01001", ]

  tracts <- run_extrapolation(county = one_county, county_totals = full)

  expect_equal(unique(tracts$county_fips), "01001")
  expect_equal(unique(tracts$county_cases_spread), 0)
})

test_that("the no-county total is announced", {
  local_fake_sources(state = fake_state_atlas(no_county = 500))
  county <- impute_suppressed(fake_county_atlas())

  expect_snapshot(invisible(extrapolate_from_county(county)))
  expect_snapshot(invisible(extrapolate_from_county(
    county,
    no_county = "spread"
  )))
})

# Missing ACS populations (#52) ------------------------------------------------
# A missing estimate or variable row is not a zero: the county stratum it
# belongs to is left out, not allocated over the remaining areas.

acs_missing <- function(geoid, variables, drop = FALSE) {
  acs <- fake_acs()
  hit <- acs$GEOID %in% geoid & acs$variable %in% variables
  if (drop) {
    acs[!hit, ]
  } else {
    acs$estimate[hit] <- NA
    acs
  }
}

left_out_reasons <- function(tracts) {
  na <- attr(tracts, "not_allocated")
  unique(na[c("county_fips", "reason")])
}

test_that("a missing age band leaves its county out, with a warning", {
  acs <- acs_missing("01001000100", "B01001_005")
  expect_snapshot(tracts <- run_extrapolation(NULL, acs = acs))

  expect_equal(unique(tracts$county_fips), "01003")
  expect_equal(
    left_out_reasons(tracts),
    data.frame(county_fips = "01001", reason = "missing ACS population"),
    ignore_attr = TRUE
  )
})

test_that("an area with every band missing leaves its county out", {
  bands <- sprintf("B01001_%03d", c(1:49))
  acs <- acs_missing("01001000200", bands)
  tracts <- suppressWarnings(run_extrapolation(NULL, acs = acs))

  expect_equal(unique(tracts$county_fips), "01003")
})

test_that("a missing variable row leaves its county out", {
  acs <- acs_missing("01003000100", "B01001_030", drop = TRUE)
  tracts <- suppressWarnings(run_extrapolation(NULL, acs = acs))

  expect_equal(unique(tracts$county_fips), "01001")
  expect_equal(
    unique(left_out_reasons(tracts)$reason),
    "missing ACS population"
  )
})

test_that("a duplicated ACS row can't stand in for a missing one (#111)", {
  acs <- acs_missing("01001000100", "B01001_005", drop = TRUE)
  acs <- rbind(
    acs,
    acs[acs$GEOID == "01001000100" & acs$variable == "B01001_006", ]
  )
  expect_snapshot(error = TRUE, run_extrapolation(NULL, acs = acs))
})

test_that("a duplicated ACS tract stops rather than skewing shares (#111)", {
  acs <- fake_acs()
  acs <- rbind(acs, acs[acs$GEOID == "01001000100", ])
  expect_snapshot(error = TRUE, run_extrapolation(NULL, acs = acs))
})

test_that("a real zero is still a zero, not missing", {
  acs <- fake_acs()
  acs$estimate[acs$GEOID == "01001000100" & acs$variable == "B01001_005"] <- 0

  expect_no_warning(tracts <- run_extrapolation(NULL, acs = acs))
  expect_setequal(unique(tracts$county_fips), c("01001", "01003"))
})

test_that("race scaling needs B01001_001", {
  acs <- acs_missing("01001000100", "B01001_001")
  tracts <- suppressWarnings(run_extrapolation("race", acs = acs))

  expect_equal(unique(tracts$county_fips), "01003")
})

test_that("an empty result is still a result when every county is left out", {
  acs <- acs_missing(fake_tracts$geoid, "B01001_005")
  for (no_county in c("report", "spread")) {
    tracts <- suppressWarnings(
      run_extrapolation(NULL, acs = acs, no_county = no_county)
    )
    expect_equal(nrow(tracts), 0)
    expect_true("area_cases" %in% names(tracts))
    expect_setequal(
      attr(tracts, "not_allocated")$county_fips,
      c("01001", "01003")
    )
  }
})

test_that("missing_population = 'error' stops instead", {
  acs <- acs_missing("01001000100", "B01001_005")
  expect_snapshot(
    error = TRUE,
    run_extrapolation(NULL, acs = acs, missing_population = "error")
  )
})

# Rows that can't be allocated ---------------------------------------------------------

test_that("county counts with no matching tracts are left out with a warning", {
  # Like Connecticut in 2017: AtlasPlus uses planning regions, ACS tracts use
  # old counties.
  county <- fake_county_atlas()
  county$fips <- as.character(county$fips)
  county$fips[county$fips == "01003"] <- "01110"
  local_fake_sources()
  county <- impute_suppressed(county)

  expect_snapshot(tracts <- extrapolate_from_county(county))
  left_out <- attr(tracts, "not_allocated")
  expect_equal(unique(left_out$county_fips), "01110")
  expect_equal(unique(left_out$reason), "no ACS tracts for this county")
  expect_equal(unique(tracts$county_fips), "01001")
})

test_that("race 'Unknown' has no Census population and is reported", {
  county <- county_with_unknown_race()
  local_fake_sources(state = state_with_unknown_race())
  county <- impute_suppressed(county)

  expect_snapshot(tracts <- extrapolate_from_county(county))
  left_out <- attr(tracts, "not_allocated")
  expect_equal(unique(left_out$race_ethnicity), "Unknown")
  expect_equal(sum(left_out$county_cases), 20)
  expect_equal(
    unique(left_out$reason),
    "no Census population for this stratum"
  )
  expect_false("Unknown" %in% tracts$race_ethnicity)
})

# Years ------------------------------------------------------------------------------

test_that("each data year uses its own ACS year", {
  years_seen <- c()
  local_fake_sources(state = fake_state_atlas(years = 2021:2022))
  local_mocked_bindings(.get_acs_areas = function(variables, year, states, to) {
    years_seen <<- c(years_seen, year)
    acs <- fake_acs()
    tibble::as_tibble(acs[acs$variable %in% variables, ])
  })
  county <- impute_suppressed(fake_county_atlas(years = 2021:2022))
  tracts <- suppressMessages(extrapolate_from_county(county))

  expect_equal(years_seen, c(2021, 2022))
  expect_equal(
    unique(tracts[c("year", "acs_year")]),
    tibble::tibble(year = c(2021, 2022), acs_year = c(2021, 2022)),
    ignore_attr = TRUE
  )
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases)
  expect_equal(
    check$county_cases[check$year == 2022] -
      check$county_cases[check$year == 2021],
    rep(1, 4)
  )
})

test_that("`acs_year` sets the ACS year for one data year", {
  tracts <- run_extrapolation(NULL, acs_year = 2021)
  expect_equal(unique(tracts$acs_year), 2021)
  expect_equal(unique(tracts$year), 2022)
})

# Checks ----------------------------------------------------------------------------

test_that("extrapolate_from_county() rejects data and arguments it can't use", {
  local_fake_sources()
  county <- impute_suppressed(fake_county_atlas())

  expect_snapshot(error = TRUE, extrapolate_from_county(fake_county_atlas()))

  with_na <- county
  with_na$cases[1] <- NA
  expect_snapshot(error = TRUE, extrapolate_from_county(with_na))

  two_way <- impute_suppressed(fake_county_atlas("sex"), method = "zero")
  two_way$race_ethnicity <- factor("White")
  expect_snapshot(error = TRUE, extrapolate_from_county(two_way))

  congenital <- impute_suppressed(
    fake_county_atlas(indicators = "Congenital Syphilis"),
    method = "zero"
  )
  expect_snapshot(error = TRUE, extrapolate_from_county(congenital))

  several_years <- impute_suppressed(
    fake_county_atlas(years = 2021:2022),
    method = "zero"
  )
  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(several_years, acs_year = 2021)
  )
  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, acs_year = "2021")
  )
  expect_snapshot(error = TRUE, extrapolate_from_county(county, weights = 1))
  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, no_county = "drop")
  )
})

test_that("a missing Census key is reported before any request", {
  withr::local_envvar(CENSUS_API_KEY = "")
  local_mocked_bindings(
    .request_atlas = function(payload) stop("CDC was called"),
    .get_acs_areas = function(...) stop("Census was called")
  )
  county <- impute_suppressed(fake_county_atlas(), method = "zero")

  expect_snapshot(error = TRUE, extrapolate_from_county(county))
})

test_that("kept 'Data not available' rows are listed as not allocated", {
  county <- fake_county_atlas()
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"
  county <- impute_suppressed(county, not_available = "keep")
  tracts <- run_extrapolation(county = county)

  left_out <- attr(tracts, "not_allocated")
  expect_equal(unique(left_out$reason), "no CDC data for this county")
  expect_equal(unique(tracts$county_fips), "01001")
})

test_that("suppressed rows impute_suppressed() couldn't estimate are left out", {
  # Like Valdez-Cordova (02261) in CDC's 2022 data: abolished, suppressed and
  # with no CDC population.
  county <- fake_county_atlas(suppressed = "01003")
  county$population[county$fips == "01003"] <- NA
  local_fake_sources()
  county <- suppressWarnings(impute_suppressed(county))

  expect_snapshot(tracts <- extrapolate_from_county(county))
  left_out <- attr(tracts, "not_allocated")
  expect_equal(left_out$county_fips, c("01003", "01003"))
  expect_equal(unique(left_out$reason), "suppressed, not estimated")
  expect_equal(unique(tracts$county_fips), "01001")
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-8)
})

test_that("rows with no count that weren't suppressed are left out with their own reason (#107)", {
  county <- fake_county_atlas(suppressed = "01003")
  county$data_status <- factor(ifelse(
    county$fips == "01003",
    "No reported cases",
    as.character(county$data_status)
  ))
  local_fake_sources()
  county <- suppressWarnings(impute_suppressed(county))

  tracts <- without_unplaced_warning(
    suppressMessages(extrapolate_from_county(county))
  )
  left_out <- attr(tracts, "not_allocated")
  expect_equal(left_out$county_fips, c("01003", "01003"))
  expect_equal(unique(left_out$reason), "no count, not marked suppressed")
  expect_equal(unique(tracts$county_fips), "01001")
})

test_that("unestimated = 'error' refuses rows impute_suppressed() couldn't estimate", {
  county <- fake_county_atlas(suppressed = "01003")
  county$population[county$fips == "01003"] <- NA
  local_fake_sources()
  county <- suppressWarnings(impute_suppressed(county))

  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, unestimated = "error")
  )
})

# Block groups ----------------------------------------------------------------------

test_that("block group estimates add up to county counts", {
  for (strat in list(NULL, "sex", "race", "age")) {
    case <- strat %||% "unstratified"
    areas <- run_extrapolation(strat, to = "block group")
    check <- county_vs_areas(areas)

    expect_equal(
      check$area_cases_sum,
      check$county_cases,
      tolerance = 1e-8,
      label = paste("block group sums,", case)
    )
    expect_equal(
      unique(nchar(areas$geoid)),
      12,
      label = paste("GEOID length,", case)
    )
    expect_equal(
      unique(areas$area_type),
      "block group",
      label = paste("area type,", case)
    )
  }
})

test_that("each tract's block groups split its share by population", {
  areas <- run_extrapolation(NULL, to = "block group")
  one <- areas[
    areas$county_fips == "01003" &
      areas$indicator == "HIV prevalence",
  ]

  expect_equal(one$geoid, c("010030001001", "010030001002"))
  expect_equal(
    one$area_cases,
    121 * one$area_population_acs / sum(one$area_population_acs)
  )
  expect_gt(one$area_cases[2], one$area_cases[1])
})

test_that("ACS years before an area's first release are refused", {
  local_fake_sources()
  county <- impute_suppressed(
    fake_county_atlas(years = 2011),
    method = "zero"
  )

  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, to = "block group")
  )
  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, acs_year = 2008)
  )
  expect_no_error(suppressMessages(extrapolate_from_county(county)))
})

# Race table ------------------------------------------------------------------------

test_that("race_table = 'B01001' uses race-by-age tables, unscaled", {
  tracts <- run_extrapolation("race", race_table = "B01001")
  pop <- fake_acs_tract("01001000100")

  bands_15_plus <- sprintf("B01001B_%03d", c(6:16, 21:31))
  expected <- sum(pop[bands_15_plus]) +
    0.4 * (pop[["B01001B_005"]] + pop[["B01001B_020"]])
  got <- unique(tracts$area_population_acs[
    tracts$geoid == "01001000100" &
      tracts$race_ethnicity == "Black/African American"
  ])
  expect_equal(got, expected)

  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases)
})

test_that("race_table = 'B01001' counts all ages for other diseases", {
  county <- fake_county_atlas("race", indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(stratify_by = "race", indicators = "Chlamydia"),
    race_table = "B01001"
  )
  pop <- fake_acs_tract("01001000100")
  got <- unique(tracts$area_population_acs[
    tracts$geoid == "01001000100" & tracts$race_ethnicity == "Hispanic/Latino"
  ])

  expect_equal(got, sum(pop[sprintf("B01001I_%03d", c(3:16, 18:31))]))
})

# Spreading by cases ------------------------------------------------------------------

test_that("spread_by = 'cases' shares the remainder by county counts", {
  state <- fake_state_atlas(no_county = 500)
  tracts <- run_extrapolation(
    state = state,
    no_county = "spread",
    spread_by = "cases"
  )
  spread <- unique(tracts[c("county_fips", "indicator", "county_cases_spread")])
  diagnoses <- spread[spread$indicator == "HIV diagnoses", ]

  # County diagnoses are 11 and 21.
  expect_equal(diagnoses$county_cases_spread, 500 * c(11, 21) / 32)
  expect_equal(sum(tracts$area_cases), sum(state$cases))
})

test_that("spread_by = 'cases' falls back to population with no county cases", {
  county <- fake_county_atlas()
  county$cases <- 0
  state <- fake_state_atlas(no_county = 500)
  tracts <- run_extrapolation(
    county = county,
    state = state,
    no_county = "spread",
    spread_by = "cases"
  )
  by_population <- run_extrapolation(
    county = county,
    state = state,
    no_county = "spread"
  )

  expect_equal(tracts$area_cases, by_population$area_cases)
  expect_equal(sum(tracts$area_cases), sum(state$cases))

  # The warning about the state's remainder doesn't say the cases were
  # shared "by cases", which is not what happened here (#164).
  caught <- catch_class(suppressMessages(
    extrapolate_from_county(
      impute_suppressed(county),
      no_county = "spread",
      spread_by = "cases"
    )
  ))
  expect_length(caught, 1)
  expect_no_match(conditionMessage(caught[[1]]), "by cases")
  expect_no_match(conditionMessage(caught[[1]]), "by population")
  expect_match(
    conditionMessage(caught[[1]]),
    "any of those cases that reach areas are placed by assumption",
    fixed = TRUE
  )
})

# Unknown race ------------------------------------------------------------------------

test_that("unknown race is shared across known races by their counts", {
  county <- county_with_unknown_race()
  local_fake_sources(state = state_with_unknown_race())
  county <- impute_suppressed(county)

  expect_snapshot(
    tracts <- extrapolate_from_county(county, unknown_race = "redistribute")
  )
  expect_equal(nrow(attr(tracts, "not_allocated")), 0)
  expect_equal(sum(tracts$area_cases), sum(county$cases))

  one <- unique(tracts[
    tracts$county_fips == "01001" & tracts$indicator == "HIV diagnoses",
    c("race_ethnicity", "county_cases", "county_cases_unknown")
  ])
  expect_equal(
    one$county_cases_unknown,
    5 * one$county_cases / sum(one$county_cases)
  )
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases)
})

test_that("unknown race stays unallocated in a county with no known cases", {
  county <- county_with_unknown_race()
  county$cases[county$fips == "01003" & county$race_ethnicity != "Unknown"] <- 0
  tracts <- run_extrapolation(
    county = county,
    state = state_with_unknown_race(),
    unknown_race = "redistribute"
  )
  left_out <- attr(tracts, "not_allocated")

  expect_equal(unique(left_out$county_fips), "01003")
  expect_equal(unique(left_out$race_ethnicity), "Unknown")
  expect_equal(sum(tracts$area_cases[tracts$county_fips == "01003"]), 0)
})

# CDC's current county map on 2010 tracts (#30) ---------------------------------

# A made-up crosswalk: tract 01001000300 now lies in county 01003, and tract
# 01001000200 is split evenly between the two counties.
fake_crosswalk <- data.frame(
  tract_2010 = c("01001000200", "01001000200", "01001000300"),
  county_fips = c("01001", "01003", "01003"),
  share = c(0.5, 0.5, 1)
)

run_2017 <- function(stratify_by = NULL, years = 2017, ...) {
  local_mocked_bindings(.tract_crosswalk = function() fake_crosswalk)
  run_extrapolation(
    county = fake_county_atlas(stratify_by, years = years),
    state = fake_state_atlas(stratify_by = stratify_by, years = years),
    ...
  )
}

test_that("2010 tracts are moved to CDC's current counties by the crosswalk", {
  expect_warning(tracts <- run_2017(), class = "cdcatlas_duplicated_geoid")
  pop <- fake_acs_tract("01001000200")
  total <- sum(pop[sprintf("B01001_%03d", c(6:25, 30:49))]) +
    0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])

  in_01003 <- unique(tracts[
    tracts$county_fips == "01003",
    c("geoid", "area_total_population")
  ])
  expect_setequal(
    in_01003$geoid,
    c("01001000200", "01001000300", "01003000100")
  )
  expect_equal(
    in_01003$area_total_population[in_01003$geoid == "01001000200"],
    0.5 * total
  )
  in_01001 <- unique(tracts$geoid[tracts$county_fips == "01001"])
  expect_setequal(in_01001, c("01001000100", "01001000200"))
})

test_that("crosswalked 2010 tracts still add up to county counts", {
  expect_warning(tracts <- run_2017(), class = "cdcatlas_duplicated_geoid")
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-8)
})

# An area split between two current counties has one row in each (#152).

# A call's value, with the first class and the message of every warning it
# gives.
catch_warnings <- function(expr) {
  classes <- character()
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      classes <<- c(classes, class(w)[[1]])
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, classes = classes, messages = messages)
}

# The step the help and the article give for one row per area: sum the area
# columns by `geoid` within year, indicator and stratum.
sum_split_areas <- function(areas) {
  areas |>
    dplyr::summarize(
      area_cases = sum(area_cases),
      area_population_acs = sum(area_population_acs),
      area_total_population = sum(area_total_population),
      .by = c(year, indicator, race_ethnicity, sex, age, geoid, area_name)
    )
}

test_that("a tract split between counties gives one warning (#152)", {
  caught <- catch_warnings(run_2017())
  tracts <- caught$value

  expect_equal(caught$classes, "cdcatlas_duplicated_geoid")
  expect_snapshot(invisible(run_2017()))

  # One row for each county, for each indicator.
  split <- tracts[tracts$geoid == "01001000200", ]
  expect_equal(
    split$county_fips[order(split$indicator, split$county_fips)],
    rep(c("01001", "01003"), 2)
  )
})

test_that("the split-area warning counts areas, not rows (#152)", {
  # One tract, however many indicators, years and strata repeat it.
  caught <- catch_warnings(run_2017("sex", years = 2016:2017))
  expect_equal(caught$classes, "cdcatlas_duplicated_geoid")
  expect_match(caught$messages, "^1 tract is split")

  # The split tract's two block groups follow it into both counties.
  caught <- catch_warnings(run_2017(to = "block group"))
  expect_equal(caught$classes, "cdcatlas_duplicated_geoid")
  expect_snapshot(invisible(run_2017(to = "block group")))
  expect_setequal(
    with(caught$value, geoid[duplicated(data.frame(geoid, indicator))]),
    c("010010002001", "010010002002")
  )
})

test_that("no split-area warning when every area is in one county (#152)", {
  # A whole tract moved to another county is not split.
  local_mocked_bindings(.tract_crosswalk = function() fake_crosswalk[3, ])
  expect_no_warning(
    moved <- run_extrapolation(
      county = fake_county_atlas(years = 2017),
      state = fake_state_atlas(years = 2017)
    )
  )
  expect_false(anyDuplicated(moved[c("geoid", "indicator")]) > 0)

  # The same areas in two years, two indicators and two strata.
  expect_no_warning(
    run_extrapolation(
      "sex",
      county = fake_county_atlas("sex", years = 2021:2022),
      state = fake_state_atlas(stratify_by = "sex", years = 2021:2022)
    )
  )
})

test_that("summing a split tract by geoid gives the whole tract (#152)", {
  expect_warning(tracts <- run_2017(), class = "cdcatlas_duplicated_geoid")
  summed <- sum_split_areas(tracts)

  # Ages 13 and older in each made-up tract, as the unsplit ACS gives them.
  adults <- vapply(
    fake_tracts$geoid,
    function(geoid) {
      pop <- fake_acs_tract(geoid)
      sum(pop[sprintf("B01001_%03d", c(6:25, 30:49))]) +
        0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
    },
    numeric(1)
  )

  expect_equal(anyDuplicated(summed[c("geoid", "indicator")]), 0)
  diagnoses <- summed[summed$indicator == "HIV diagnoses", ]
  diagnoses <- diagnoses[match(fake_tracts$geoid, diagnoses$geoid), ]
  expect_equal(diagnoses$area_population_acs, unname(adults))
  expect_equal(diagnoses$area_total_population, unname(adults))

  # With the made-up crosswalk, county 01001 is tract 1 and half of tract 2;
  # county 01003 is the other half, tract 3 and its own tract. Their 2017
  # diagnoses are 11 and 21.
  county_01001 <- adults[[1]] + 0.5 * adults[[2]]
  county_01003 <- 0.5 * adults[[2]] + adults[[3]] + adults[[4]]
  expect_equal(
    diagnoses$area_cases[2],
    11 *
      0.5 *
      adults[[2]] /
      county_01001 +
      21 * 0.5 * adults[[2]] / county_01003
  )
  expect_equal(sum(diagnoses$area_cases), 11 + 21)

  # Keeping one of the two rows instead would understate the tract.
  first_row <- tracts[
    tracts$geoid == "01001000200" & tracts$indicator == "HIV diagnoses",
  ][1, ]
  expect_equal(first_row$area_population_acs, 0.5 * adults[[2]])
})

test_that("the crosswalk isn't used with 2020 tracts", {
  local_mocked_bindings(.tract_crosswalk = function() fake_crosswalk)
  tracts <- run_extrapolation()
  expect_setequal(
    unique(tracts$geoid[tracts$county_fips == "01003"]),
    "01003000100"
  )
})

test_that("Connecticut with ACS 2020 or 2021 says why it has no tracts (#112)", {
  county <- fake_county_atlas()
  county$fips[county$fips == "01003"] <- "09110"
  county$geography <- factor(paste("County", county$fips))
  for (acs in c(2020, 2021)) {
    expect_warning(
      run_extrapolation(county = county, acs_year = acs),
      "planning regions"
    )
  }
  expect_snapshot(
    invisible(run_extrapolation(county = county, acs_year = 2021))
  )
})

test_that("ACS 2009 says why current Connecticut and Alaska counties have no tracts (#112)", {
  expect_equal(
    .acs_vintage_note(c("09110", "02063", "02066", "01001"), 2009),
    c(
      "09110" = "Connecticut's planning regions",
      "02063" = "Chugach and Copper River, Alaska",
      "02066" = "Chugach and Copper River, Alaska",
      "01001" = NA
    )
  )
  expect_equal(unname(.acs_vintage_note("09110", 2022)), NA_character_)
  expect_equal(unname(.acs_vintage_note("09110", 2015)), NA_character_)
  expect_equal(unname(.acs_vintage_note("02063", 2020)), NA_character_)
})

test_that("counties renamed in 2015 are explained for earlier ACS releases (#112)", {
  notes <- .acs_vintage_note(c("02158", "46102", "02158"), c(2014, 2012, 2015))
  expect_equal(
    unname(notes),
    c(
      rep("Kusilvak, AK and Oglala Lakota, SD (renamed in 2015)", 2),
      NA_character_
    )
  )
})

test_that("the shipped crosswalk gives each 2010 tract shares that sum to 1", {
  crosswalk <- .tract_crosswalk()
  expect_equal(unique(nchar(crosswalk$tract_2010)), 11)
  expect_setequal(unique(substr(crosswalk$tract_2010, 1, 2)), c("02", "09"))
  expect_contains(lookup[["counties"]]$fips, unique(crosswalk$county_fips))
  totals <- tapply(crosswalk$share, crosswalk$tract_2010, sum)
  expect_equal(as.vector(totals), rep(1, length(totals)), tolerance = 1e-5)
})

# Age groups (#10) ------------------------------------------------------------

band_weights <- function(groups, ages = "all") {
  d <- .census_denominators("age", ages, age_groups = groups)
  weights <- tapply(d$weight, d$variable, sum)
  stats::setNames(as.vector(weights), names(weights))
}

test_that("CDC age-group labels are read as ages", {
  expect_equal(.age_group_bounds("13-24"), c(13, 24))
  expect_equal(.age_group_bounds("05-14"), c(5, 14))
  expect_equal(.age_group_bounds(varvals$name[varvals$id == 674]), c(13, 19)) # en dash
  expect_equal(.age_group_bounds("65+"), c(65, Inf))
  expect_equal(.age_group_bounds("Unknown"), c(NA_real_, NA_real_))
})

test_that("STI age groups count every ACS band once, for both sexes", {
  weights <- band_weights(setdiff(sti_age_groups, "Unknown"))
  bands <- sprintf("B01001_%03d", c(3:25, 27:49))
  expect_setequal(names(weights), bands)
  expect_equal(unname(weights), rep(1, length(bands)))
})

test_that("HIV age groups count 2/5 of the 10-14 band and nothing younger", {
  weights <- band_weights(strata_for("age")$age, ages = "13+")
  expect_equal(unname(weights[c("B01001_005", "B01001_029")]), c(0.4, 0.4))
  expect_equal(
    intersect(names(weights), sprintf("B01001_%03d", c(3, 4, 27, 28))),
    character(0)
  )
  rest <- setdiff(names(weights), c("B01001_005", "B01001_029"))
  expect_equal(unname(weights[rest]), rep(1, length(rest)))
})

test_that("every age group in CDC's code table maps to ACS bands", {
  labels <- varvals$name[varvals$vtid == 7]
  # "Age 15 and older" is in the code table, but no indicator offers it.
  labels <- setdiff(labels, c(.atlas_totals$age, "Unknown", "Age 15 and older"))
  for (label in labels) {
    d <- .census_denominators("age", "all", age_groups = label)
    expect_gt(nrow(d), 0)
    expect_equal(all(d$weight > 0 & d$weight <= 1), TRUE, label = label)
  }
})

test_that("chlamydia 0-14 counts all of the under-15 bands", {
  county <- fake_county_atlas("age", indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(stratify_by = "age", indicators = "Chlamydia")
  )
  pop <- fake_acs_tract("01001000100")
  expected <- sum(pop[sprintf("B01001_%03d", c(3:5, 27:29))])
  got <- unique(tracts$area_population_acs[
    tracts$geoid == "01001000100" & tracts$age == "0-14"
  ])
  expect_equal(got, expected)
})

test_that("chlamydia age estimates add up, with all-ages totals", {
  county <- fake_county_atlas("age", indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(stratify_by = "age", indicators = "Chlamydia")
  )
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-8)

  pop <- fake_acs_tract("01001000100")
  all_ages <- sum(pop[sprintf("B01001_%03d", c(3:25, 27:49))])
  expect_equal(
    unique(tracts$area_total_population[tracts$geoid == "01001000100"]),
    all_ages
  )
})

test_that("unknown age is left out and listed by default", {
  county <- fake_county_atlas("age", indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(stratify_by = "age", indicators = "Chlamydia")
  )
  left_out <- attr(tracts, "not_allocated")
  expect_equal(sort(unique(left_out$age)), "Unknown")
  expect_equal(nrow(left_out), 2)
  expect_equal(sum(tracts$age == "Unknown"), 0)
})

test_that("unknown_age = 'redistribute' shares unknown cases across known ages", {
  county <- fake_county_atlas("age", indicators = "Chlamydia")
  tracts <- run_extrapolation(
    county = county,
    state = fake_state_atlas(stratify_by = "age", indicators = "Chlamydia"),
    unknown_age = "redistribute"
  )
  expect_equal(nrow(attr(tracts, "not_allocated")), 0)

  for (fips in c("01001", "01003")) {
    in_county <- county[county$fips == fips, ]
    known <- in_county$age != "Unknown"
    unknown <- in_county$cases[!known]
    rows <- tracts$county_fips == fips
    expect_equal(sum(tracts$area_cases[rows]), sum(in_county$cases))
    in_tracts <- tracts[rows, ]
    one_per_group <- in_tracts[!duplicated(in_tracts$age), ]
    share <- in_county$cases[known] / sum(in_county$cases[known])
    expect_equal(
      one_per_group$county_cases_unknown[
        match(as.character(in_county$age[known]), one_per_group$age)
      ],
      unknown * share
    )
  }
})

test_that("unknown_age is checked", {
  county <- impute_suppressed(
    fake_county_atlas("age", indicators = "Chlamydia"),
    method = "zero"
  )
  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(county, unknown_age = "spread")
  )
})

test_that("tracts are the same with or without cases_uncapped, and don't carry it (#151)", {
  local_fake_sources()
  # County 01003 has no CDC population in one indicator, so one suppressed row
  # is estimated and one is left out.
  county <- fake_county_atlas(suppressed = "01003")
  county$population[
    county$fips == "01003" & county$indicator == "HIV diagnoses"
  ] <- NA
  county <- suppressWarnings(impute_suppressed(county))
  expect_contains(names(county), "cases_uncapped")
  # Data imputed by a version without the column.
  older <- county[setdiff(names(county), "cases_uncapped")]

  tracts <- without_unplaced_warning(suppressMessages(
    extrapolate_from_county(county, unestimated = "leave out")
  ))
  tracts_older <- without_unplaced_warning(suppressMessages(
    extrapolate_from_county(older, unestimated = "leave out")
  ))
  expect_identical(tracts, tracts_older)
  expect_false("cases_uncapped" %in% names(tracts))
  expect_equal(nrow(attr(tracts, "not_allocated")), 1)
  expect_false("cases_uncapped" %in% names(attr(tracts, "not_allocated")))
  # The capped count is the one shared out: 4, not 11.6.
  expect_equal(unique(tracts$county_cases[tracts$county_fips == "01003"]), 4)
})

# States with many cases in no county row (#164) --------------------------------

test_that("one extrapolation warns once about states with many cases in no county row (#164)", {
  data <- placed_atlas(74, 100, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state, acs = data$acs)

  caught <- catch_warnings(suppressMessages(
    tracts <- extrapolate_from_county(data$county)
  ))
  expect_equal(caught$classes, "cdcatlas_state_unplaced_cases")
  expect_snapshot(tracts <- extrapolate_from_county(data$county))
  # Nothing is added to the tracts: they hold the counties' 74 cases.
  expect_equal(sum(tracts$area_cases), 74)
  expect_equal(attr(tracts, "no_county_cases")$no_county_cases, 26)
})

test_that("extrapolation at exactly 25% in no county row gives no warning (#164)", {
  data <- placed_atlas(75, 100, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state, acs = data$acs)

  expect_no_warning(suppressMessages(extrapolate_from_county(data$county)))
})

test_that("extrapolation names every state with many cases in no county row (#164)", {
  data <- bind_atlas(
    placed_atlas(13, 100, "02", "Alaska", indicators = "HIV diagnoses"),
    placed_atlas(27, 100, "44", "Rhode Island", indicators = "HIV diagnoses"),
    placed_atlas(90, 100, "01", "Alabama", indicators = "HIV diagnoses")
  )
  local_fake_sources(state = data$state, acs = data$acs)

  caught <- catch_warnings(suppressMessages(
    extrapolate_from_county(data$county, to = "block group")
  ))
  expect_equal(caught$classes, "cdcatlas_state_unplaced_cases")
  expect_match(caught$messages, "Alaska \\(87%\\) and Rhode Island \\(73%\\)")
  expect_match(caught$messages, "block group estimates")
  expect_no_match(caught$messages, "Alabama")
})

test_that("no_county = 'spread' still warns, and says the cases are placed by assumption (#164)", {
  data <- placed_atlas(74, 100, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state, acs = data$acs)

  caught <- catch_warnings(suppressMessages(
    tracts <- extrapolate_from_county(data$county, no_county = "spread")
  ))
  expect_equal(caught$classes, "cdcatlas_state_unplaced_cases")
  expect_snapshot(
    tracts <- extrapolate_from_county(
      data$county,
      no_county = "spread",
      spread_by = "cases"
    )
  )
  # Spreading is unchanged: the tracts hold the whole state total.
  expect_equal(sum(tracts$area_cases), 100)
})
