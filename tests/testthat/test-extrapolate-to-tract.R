# Tract extrapolation on a small made-up state, run through the real code with
# tidycensus and the CDC state request replaced. Because everything is made up,
# the right answers are known exactly.

skip_if_not_installed("tidycensus")

# Made-up geography ------------------------------------------------------------
# State 01: county 01001 has three tracts, 01003 has one (like a very small
# county such as Loving County, TX).

fake_tracts <- data.frame(
  tract_fips = c("01001000100", "01001000200", "01001000300", "01003000100"),
  county_fips = c("01001", "01001", "01001", "01003")
)

census_variables <- c(sprintf("B01001_%03d", 1:49), sprintf("B03002_%03d", 1:21))

# A fixed made-up population for every tract and Census variable.
fake_acs <- function() {
  grid <- expand.grid(t = seq_len(nrow(fake_tracts)), v = seq_along(census_variables))
  data.frame(
    GEOID = fake_tracts$tract_fips[grid$t],
    NAME = paste("Census Tract", fake_tracts$tract_fips[grid$t]),
    variable = census_variables[grid$v],
    estimate = (37 * grid$t + 13 * grid$v) %% 400 + 25,
    moe = 0
  )
}

# Stands in for tidycensus::get_acs(): returns the requested variables.
make_get_acs <- function(acs) {
  function(geography, variables, year, survey, state, ...) {
    stopifnot(geography == "tract", survey == "acs5")
    tibble::as_tibble(acs[acs$variable %in% variables, ])
  }
}

# Made-up AtlasPlus data -------------------------------------------------------

strata_for <- function(stratify_by) {
  all <- data.frame(
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = "Ages 13 years and older"
  )
  if (is.null(stratify_by)) {
    return(all)
  }
  values <- switch(stratify_by,
    sex = list(sex = c("Male", "Female")),
    race = list(race_ethnicity = c(
      "American Indian/Alaska Native", "Asian", "Black/African American",
      "Hispanic/Latino", "Multiracial", "Native Hawaiian/Other Pacific Islander", "White"
    )),
    age = list(age = c("13-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  )
  out <- all[rep(1, length(values[[1]])), ]
  out[[names(values)]] <- values[[1]]
  rownames(out) <- NULL
  out
}

# County rows as get_atlas() returns them. `suppressed` lists county fips whose
# counts are suppressed.
fake_county_atlas <- function(stratify_by, suppressed = character(0)) {
  strata <- strata_for(stratify_by)
  grid <- expand.grid(
    county = c("01001", "01003"),
    stratum = seq_len(nrow(strata)),
    indicator = c("HIV diagnoses", "HIV prevalence"),
    stringsAsFactors = FALSE
  )
  cases <- 10 * match(grid$county, c("01001", "01003")) + grid$stratum +
    ifelse(grid$indicator == "HIV prevalence", 100, 0)
  is_suppressed <- grid$county %in% suppressed
  data.frame(
    indicator = grid$indicator,
    year = 2022,
    geography = paste("County", grid$county),
    data_status = ifelse(is_suppressed, "Data suppressed", "Not Suppressed"),
    strata[grid$stratum, ],
    transmission = "All transmission categories",
    rate100000 = NA_real_,
    cases = ifelse(is_suppressed, NA_real_, cases),
    population = 50000,
    fips = grid$county,
    row.names = NULL
  )
}

# State rows: the sum of the counties' true counts plus `no_county` extra cases,
# with a population of 1,000,000 (so the state rate is cases / 1e6).
fake_state_atlas <- function(stratify_by, no_county = 0) {
  full <- fake_county_atlas(stratify_by)
  state <- aggregate(cases ~ indicator + year + race_ethnicity + sex + age, data = full, FUN = sum)
  state$cases <- state$cases + no_county
  state$population <- 1e6
  state$geography <- "State 01"
  state$data_status <- "Not Suppressed"
  state$fips <- "01"
  state
}

run_extrapolation <- function(stratify_by, county = fake_county_atlas(stratify_by),
                              state = fake_state_atlas(stratify_by), acs = fake_acs(),
                              fill_suppressed = "state_rate", env = parent.frame()) {
  local_mocked_bindings(get_acs = make_get_acs(acs), .package = "tidycensus", .env = env)
  local_mocked_bindings(.request_atlas = function(payload) state, .env = env)
  suppressMessages(
    .extrapolate_to_tract(county, payload_state = list(), stratify_by = stratify_by,
                          suppressed = fill_suppressed)
  )
}

county_vs_tracts <- function(tracts) {
  tracts |>
    dplyr::summarize(
      county_cases = dplyr::first(county_cases),
      tract_cases_sum = sum(tract_cases),
      tracts = dplyr::n(),
      .by = c(county_fips, indicator, race_ethnicity, sex, age)
    )
}

# Adding up ---------------------------------------------------------------------

for (strat in list(NULL, "sex", "race", "age")) {
  label <- if (is.null(strat)) "unstratified" else paste("by", strat)

  test_that(paste("tract estimates add up to county counts,", label), {
    check <- county_vs_tracts(run_extrapolation(strat))
    expect_equal(check$tract_cases_sum, check$county_cases, tolerance = 1e-8)
  })

  test_that(paste("every tract appears once per stratum and indicator,", label), {
    check <- county_vs_tracts(run_extrapolation(strat))
    expect_equal(check$tracts, as.vector(table(fake_tracts$county_fips)[check$county_fips]))
    expect_equal(nrow(check), 2 * 2 * nrow(strata_for(strat)))
  })

  test_that(paste("tract estimates are never missing or negative,", label), {
    tracts <- run_extrapolation(strat)
    expect_equal(sum(is.na(tracts$tract_cases)), 0)
    expect_gte(min(tracts$tract_cases), 0)
    expect_gte(min(tracts$tract_noncases), 0)
  })
}

test_that("cases are kept when a stratum has no Census population in the county", {
  # Like Loving County, TX: ages 35-44 have no ACS population in 01003.
  acs <- fake_acs()
  bands_35_44 <- c("B01001_013", "B01001_014", "B01001_037", "B01001_038")
  acs$estimate[substr(acs$GEOID, 1, 5) == "01003" & acs$variable %in% bands_35_44] <- 0

  tracts <- run_extrapolation("age", acs = acs)
  empty <- tracts[tracts$county_fips == "01003" & tracts$age == "35-44", ]

  expect_equal(sum(empty$tract_cases), sum(empty$county_cases))
  expect_equal(unique(empty$allocation), "total population")
  expect_equal(unique(tracts$allocation[tracts$age != "35-44"]), "stratum population")
})

# Census denominators ------------------------------------------------------------

test_that("HIV denominators count ages 13+, with 2/5 of the 10-14 band", {
  tracts <- run_extrapolation(NULL)
  acs <- fake_acs()
  one <- acs[acs$GEOID == "01001000100", ]
  pop <- setNames(one$estimate, one$variable)

  bands_15_plus <- sprintf("B01001_%03d", c(6:25, 30:49))
  expected <- sum(pop[bands_15_plus]) + 0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
  got <- unique(tracts$tract_population_acs[tracts$tract_fips == "01001000100"])

  expect_equal(got, expected)
})

test_that("age group 13-24 combines the right ACS bands for both sexes", {
  tracts <- run_extrapolation("age")
  acs <- fake_acs()
  one <- acs[acs$GEOID == "01001000100", ]
  pop <- setNames(one$estimate, one$variable)

  bands <- sprintf("B01001_%03d", c(6:10, 30:34))
  expected <- sum(pop[bands]) + 0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
  got <- unique(tracts$tract_population_acs[tracts$tract_fips == "01001000100" & tracts$age == "13-24"])

  expect_equal(got, expected)
})

test_that("race denominators use B03002, scaled to the tract's 13+ share", {
  tracts <- run_extrapolation("race")
  acs <- fake_acs()
  one <- acs[acs$GEOID == "01001000100", ]
  pop <- setNames(one$estimate, one$variable)

  total_13 <- sum(pop[sprintf("B01001_%03d", c(6:25, 30:49))]) +
    0.4 * (pop[["B01001_005"]] + pop[["B01001_029"]])
  expected <- pop[["B03002_004"]] * total_13 / pop[["B01001_001"]]
  got <- unique(tracts$tract_population_acs[
    tracts$tract_fips == "01001000100" & tracts$race_ethnicity == "Black/African American"
  ])

  expect_equal(got, expected)
})

# Suppressed counties -------------------------------------------------------------

test_that("state_rate fills suppressed counties with the state rate times county population, capped at 4", {
  tracts <- run_extrapolation(
    NULL,
    county = fake_county_atlas(NULL, suppressed = "01003"),
    state = fake_state_atlas(NULL)
  )
  filled <- unique(tracts[tracts$county_fips == "01003", c("indicator", "county_cases", "county_cases_source")])

  # State diagnoses: 11 + 21 = 32 per 1e6; times county population 50,000 = 1.6.
  expect_equal(filled$county_cases[filled$indicator == "HIV diagnoses"], 32 / 1e6 * 50000)
  # State prevalence: 111 + 121 = 232 per 1e6; times 50,000 = 11.6, capped at 4.
  expect_equal(filled$county_cases[filled$indicator == "HIV prevalence"], 4)
  expect_equal(unique(filled$county_cases_source), "state rate (suppressed)")
})

test_that("zero fills suppressed counties with 0", {
  tracts <- run_extrapolation(
    NULL,
    county = fake_county_atlas(NULL, suppressed = "01003"),
    fill_suppressed = "zero"
  )
  filled <- tracts[tracts$county_fips == "01003", ]

  expect_equal(unique(filled$county_cases), 0)
  expect_equal(unique(filled$county_cases_source), "zero (suppressed)")
  expect_equal(unique(tracts$county_cases_source[tracts$county_fips == "01001"]), "published")
})

test_that("counties marked 'Data not available' without a count are dropped", {
  county <- fake_county_atlas(NULL)
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"

  tracts <- run_extrapolation(NULL, county = county)
  expect_equal(unique(tracts$county_fips), "01001")
})

test_that("a published count is kept even when marked 'Data not available'", {
  # Like Connecticut's planning regions in 2017: a count but no population.
  county <- fake_county_atlas(NULL)
  county$data_status[county$fips == "01003"] <- "Data not available"
  county$population[county$fips == "01003"] <- NA

  tracts <- run_extrapolation(NULL, county = county)
  kept <- tracts[tracts$county_fips == "01003", ]
  expect_equal(sort(unique(kept$county_cases)), c(21, 121))
  expect_equal(unique(kept$county_cases_source), "published")
})

# Cases with no county ------------------------------------------------------------

test_that("cases with no county are reported, not allocated", {
  tracts <- run_extrapolation(NULL, state = fake_state_atlas(NULL, no_county = 500))
  report <- attr(tracts, "no_county_cases")

  expect_equal(report$no_county_cases, c(500, 500))
  expect_equal(sum(tracts$tract_cases), sum(c(11, 21, 111, 121)))
})

test_that("the no-county total is announced", {
  local_mocked_bindings(get_acs = make_get_acs(fake_acs()), .package = "tidycensus")
  local_mocked_bindings(.request_atlas = function(payload) fake_state_atlas(NULL, no_county = 500))
  expect_snapshot(
    invisible(.extrapolate_to_tract(fake_county_atlas(NULL), list(), NULL))
  )
})

test_that("county counts with no matching tracts are reported with a warning", {
  # Like Connecticut in 2017: AtlasPlus uses planning regions, ACS tracts use old counties.
  county <- fake_county_atlas(NULL)
  county$fips[county$fips == "01003"] <- "01110"

  expect_warning(
    tracts <- run_extrapolation(NULL, county = county),
    "no matching ACS tracts"
  )
  expect_equal(unique(attr(tracts, "unmatched_county_cases")$county_fips), "01110")
})

# get_atlas() -----------------------------------------------------------------------

test_that("get_atlas() passes fill_suppressed to the tract step", {
  received <- NULL
  local_mocked_bindings(
    .request_atlas_all = function(payloads) data.frame(source = "county request"),
    .extrapolate_to_tract = function(df, payload_state, stratify_by, suppressed) {
      received <<- suppressed
      data.frame()
    }
  )
  get_atlas("hiv", "county", 2022, extrapolate_to_tract = TRUE)
  expect_equal(received, "state_rate")
  get_atlas("hiv", "county", 2022, extrapolate_to_tract = TRUE, fill_suppressed = "zero")
  expect_equal(received, "zero")
})

test_that("get_atlas() rejects unknown fill_suppressed values", {
  local_mocked_bindings(.request_atlas_all = function(payloads) stop("CDC should not be called"))
  expect_snapshot(error = TRUE, get_atlas("hiv", "county", 2022, fill_suppressed = "mean"))
})
