# Does tract extrapolation make mathematical sense?
#
# The core property: within each county, stratum and indicator, the tract
# estimates must add back up to the county count. These tests run
# .extrapolate_to_tract() end to end on a small made-up state, with tidycensus
# and the CDC state request replaced by made-up data, so the right answer is
# known exactly.

skip_if_not_installed("tidycensus")

# Made-up geography ----------------------------------------------------------
# State 01 with two counties: 01001 has three tracts, 01003 has one (like a
# very small county such as Loving County, TX or Kalawao County, HI).

fake_tracts <- data.frame(
  tract_fips = c("01001000100", "01001000200", "01001000300", "01003000100"),
  county_fips = c("01001", "01001", "01001", "01003")
)

# The Census variables tidycensus would list for a recent ACS year: one
# label format only, as load_variables() returns for a single year.
fake_load_variables <- function(year, dataset, ...) {
  v <- variable_lookup_atlas
  v <- v[(grepl(":", v$label) | v$label == "Estimate!!Total") & v$concept != toupper(v$concept), ]
  v <- unique(v[c("label", "concept")])
  data.frame(
    name = sprintf("FAKE_%03d", seq_len(nrow(v))),
    label = v$label,
    concept = v$concept,
    geography = "tract"
  )
}

# A made-up but fixed population for every tract and Census variable.
fake_tract_population <- function() {
  vars <- fake_load_variables(2022, "acs5")
  grid <- expand.grid(
    tract_index = seq_len(nrow(fake_tracts)),
    var_index = seq_len(nrow(vars))
  )
  data.frame(
    tract_fips = fake_tracts$tract_fips[grid$tract_index],
    county_fips = fake_tracts$county_fips[grid$tract_index],
    variable = vars$name[grid$var_index],
    label = vars$label[grid$var_index],
    estimate = (37 * grid$tract_index + 13 * grid$var_index) %% 400 + 25
  )
}

# Stands in for tidycensus::get_acs(). County populations are the sum of their
# tracts unless `county_factor` says otherwise.
make_fake_get_acs <- function(tract_pop, county_factor = c("01001" = 1, "01003" = 1)) {
  function(geography, variables, year, survey, state, ...) {
    pop <- tract_pop[tract_pop$variable %in% variables, ]
    if (geography == "tract") {
      return(tibble::tibble(
        GEOID = pop$tract_fips,
        NAME = paste("Census Tract", pop$tract_fips),
        variable = pop$variable,
        estimate = pop$estimate,
        moe = 0
      ))
    }
    county <- aggregate(estimate ~ county_fips + variable, data = pop, FUN = sum)
    county$estimate <- county$estimate * county_factor[county$county_fips]
    tibble::tibble(
      GEOID = county$county_fips,
      NAME = paste("County", county$county_fips),
      variable = county$variable,
      estimate = county$estimate,
      moe = 0
    )
  }
}

# Made-up AtlasPlus data -----------------------------------------------------

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

# County rows as .request_atlas() returns them, with no suppression.
fake_county_atlas <- function(stratify_by) {
  strata <- strata_for(stratify_by)
  grid <- expand.grid(
    county = c("01001", "01003"),
    stratum = seq_len(nrow(strata)),
    indicator = c("HIV diagnoses", "HIV prevalence"),
    stringsAsFactors = FALSE
  )
  data.frame(
    indicator = grid$indicator,
    year = 2022,
    geography = paste("County", grid$county),
    data_status = "Not Suppressed",
    strata[grid$stratum, ],
    transmission = "All transmission categories",
    rate100000 = NA_real_,
    cases = 10 * match(grid$county, c("01001", "01003")) + grid$stratum +
      ifelse(grid$indicator == "HIV prevalence", 100, 0),
    population = 1000,
    lowerci_rate = NA_real_, upperci_rate = NA_real_, rse = NA_real_,
    lowerci_cases = NA_real_, upperci_cases = NA_real_,
    fips = grid$county,
    row.names = NULL
  )
}

# State rows: the sum of the counties.
fake_state_atlas <- function(county_atlas) {
  state <- aggregate(
    cases ~ indicator + year + race_ethnicity + sex + age,
    data = county_atlas, FUN = sum
  )
  state$geography <- "State 01"
  state$fips <- "01"
  state
}

run_extrapolation <- function(stratify_by, tract_pop = fake_tract_population(),
                              county_factor = c("01001" = 1, "01003" = 1),
                              env = parent.frame()) {
  county_atlas <- fake_county_atlas(stratify_by)
  local_mocked_bindings(
    load_variables = fake_load_variables,
    get_acs = make_fake_get_acs(tract_pop, county_factor),
    .package = "tidycensus",
    .env = env
  )
  local_mocked_bindings(.request_atlas = function(payload) fake_state_atlas(county_atlas), .env = env)
  suppressMessages(.extrapolate_to_tract(county_atlas, payload_state = list(), stratify_by = stratify_by))
}

# One row per county, stratum and indicator: county count vs sum of tracts.
county_vs_tracts <- function(tracts) {
  tracts |>
    dplyr::group_by(county_fips, indicator, race_ethnicity, sex, age) |>
    dplyr::summarize(
      county_cases = dplyr::first(county_cases),
      tract_cases_sum = sum(tract_cases, na.rm = TRUE),
      tracts = dplyr::n(),
      missing_tract_cases = sum(is.na(tract_cases)),
      .groups = "drop"
    )
}

# Tests ----------------------------------------------------------------------

for (strat in list(NULL, "sex", "race", "age")) {
  label <- if (is.null(strat)) "unstratified" else paste("by", strat)

  test_that(paste("tract estimates add up to county counts,", label), {
    check <- county_vs_tracts(run_extrapolation(strat))
    expect_equal(check$tract_cases_sum, check$county_cases, tolerance = 1e-8)
  })

  test_that(paste("every tract appears once per stratum and indicator,", label), {
    check <- county_vs_tracts(run_extrapolation(strat))
    expected_tracts <- table(fake_tracts$county_fips)[check$county_fips]
    expect_equal(check$tracts, as.vector(expected_tracts))
    expect_equal(nrow(check), 2 * 2 * nrow(strata_for(strat)))
  })

  test_that(paste("tract estimates are never missing or negative,", label), {
    tracts <- run_extrapolation(strat)
    expect_equal(sum(is.na(tracts$tract_cases)), 0)
    expect_gte(min(tracts$tract_cases), 0)
  })
}

test_that("cases are kept when a stratum has no Census population in the county", {
  # Like Loving County, TX on the published page: CDC (after fill-in) gives
  # the county a small count in an age group where the ACS reports nobody.
  tract_pop <- fake_tract_population()
  age_35_44 <- variable_lookup_atlas$label[variable_lookup_atlas$age == "35-44"]
  zero <- tract_pop$county_fips == "01003" & tract_pop$label %in% age_35_44
  tract_pop$estimate[zero] <- 0

  check <- county_vs_tracts(run_extrapolation("age", tract_pop = tract_pop))
  lost <- check[check$county_fips == "01003" & check$age == "35-44", ]

  expect_equal(lost$tract_cases_sum, lost$county_cases, tolerance = 1e-8)
})

test_that("tract estimates add up even if the county Census total differs from its tracts", {
  # ACS county and tract estimates are produced separately. Allocation should
  # not depend on them agreeing exactly.
  check <- county_vs_tracts(run_extrapolation("age", county_factor = c("01001" = 1.1, "01003" = 1)))
  expect_equal(check$tract_cases_sum, check$county_cases, tolerance = 1e-8)
})
