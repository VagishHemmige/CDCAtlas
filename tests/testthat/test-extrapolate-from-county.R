# Tract extrapolation on a small made-up state (see helper-pipeline.R), with CDC
# and the Census replaced. These tests don't need tidycensus.

run_extrapolation <- function(
  stratify_by = NULL,
  county = fake_county_atlas(stratify_by),
  state = fake_state_atlas(stratify_by = stratify_by),
  acs = fake_acs(),
  ...,
  env = parent.frame()
) {
  local_fake_sources(state = state, acs = acs, env = env)
  if (!"cases_source" %in% names(county)) {
    county <- impute_suppressed(county)
  }
  suppressMessages(extrapolate_from_county(county, ...))
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
    expect_gte(min(tracts$area_noncases), 0)
  })
}

test_that("tract estimates are county count x tract share of the stratum", {
  tracts <- run_extrapolation("sex")
  one <- tracts[tracts$county_fips == "01001" & tracts$sex == "Male" &
    tracts$indicator == "HIV prevalence", ]

  share <- one$area_population_acs / sum(one$area_population_acs)
  expect_equal(one$area_cases, 111 * share)
  expect_equal(unique(one$county_population_acs), sum(one$area_population_acs))
  expect_equal(
    one$area_noncases,
    one$area_population_acs - one$area_cases
  )
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
  acs$estimate[substr(acs$GEOID, 1, 5) == "01003" & acs$variable %in% bands] <- 0
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
  acs$estimate[substr(acs$GEOID, 1, 5) == "01001" & acs$variable %in% bands] <- 0

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
    "age", county = county, acs = acs, empty_strata = "none"
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
  tracts <- run_extrapolation(county = county, state = state, no_county = "spread")
  report <- attr(tracts, "no_county_cases")

  expect_equal(
    sum(tracts$area_cases),
    sum(report$state_cases)
  )
  spread <- unique(tracts[c("county_fips", "indicator", "county_cases_spread",
                            "county_population_acs")])
  diagnoses <- spread[spread$indicator == "HIV diagnoses", ]
  expect_equal(
    diagnoses$county_cases_spread,
    report$no_county_cases[1] * diagnoses$county_population_acs /
      sum(diagnoses$county_population_acs)
  )
  check <- county_vs_areas(tracts)
  expect_equal(check$area_cases_sum, check$county_cases)
})

test_that("each state's remainder is spread within that state", {
  two <- two_state_atlas()
  county <- impute_suppressed(two$county, method = "zero")
  tracts <- run_extrapolation(
    county = county, state = two$state, acs = two$acs, no_county = "spread"
  )
  by_state <- tracts |>
    dplyr::summarize(area_cases = sum(area_cases), .by = c(state_fips, indicator))
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

test_that("the no-county total is announced", {
  local_fake_sources(state = fake_state_atlas(no_county = 500))
  county <- impute_suppressed(fake_county_atlas())

  expect_snapshot(invisible(extrapolate_from_county(county)))
  expect_snapshot(invisible(extrapolate_from_county(county, no_county = "spread")))
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

  chlamydia <- impute_suppressed(
    fake_county_atlas("age", indicators = "Chlamydia"),
    method = "zero"
  )
  expect_snapshot(error = TRUE, extrapolate_from_county(chlamydia))

  several_years <- impute_suppressed(
    fake_county_atlas(years = 2021:2022),
    method = "zero"
  )
  expect_snapshot(
    error = TRUE,
    extrapolate_from_county(several_years, acs_year = 2021)
  )
  expect_snapshot(error = TRUE, extrapolate_from_county(county, acs_year = "2021"))
  expect_snapshot(error = TRUE, extrapolate_from_county(county, weights = 1))
  expect_snapshot(error = TRUE, extrapolate_from_county(county, no_county = "drop"))
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
    areas <- run_extrapolation(strat, to = "block group")
    check <- county_vs_areas(areas)

    expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-8)
    expect_equal(unique(nchar(areas$geoid)), 12)
    expect_equal(unique(areas$area_type), "block group")
  }
})

test_that("each tract's block groups split its share by population", {
  areas <- run_extrapolation(NULL, to = "block group")
  one <- areas[areas$county_fips == "01003" &
    areas$indicator == "HIV prevalence", ]

  expect_equal(one$geoid, c("010030001001", "010030001002"))
  expect_equal(one$area_cases, 121 * one$area_population_acs /
    sum(one$area_population_acs))
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
    state = state, no_county = "spread", spread_by = "cases"
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
    county = county, state = state, no_county = "spread", spread_by = "cases"
  )
  by_population <- run_extrapolation(
    county = county, state = state, no_county = "spread"
  )

  expect_equal(tracts$area_cases, by_population$area_cases)
  expect_equal(sum(tracts$area_cases), sum(state$cases))
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
