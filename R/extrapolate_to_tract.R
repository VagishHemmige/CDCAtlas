# Tract extrapolation -----------------------------------------------------------
#
# County counts from AtlasPlus are shared across each county's census tracts in
# proportion to the tract's ACS population in the same stratum. Steps:
#
#   1. County counts: keep published counts, estimate suppressed counties
#      (state rate or zero), drop counties CDC marks "Data not available".
#   2. Report cases in the state total that no county accounts for. They are
#      not allocated to tracts.
#   3. Tract populations from the ACS 5-year estimates ending in the AtlasPlus
#      year: ages 13+ for HIV indicators, all ages otherwise.
#   4. Allocation within each county, stratum and indicator, dividing by the sum
#      of the county's tracts so tract estimates add up to the county count.
#      If no tract has population in the stratum, fall back to the tracts'
#      total population.


# Census variables --------------------------------------------------------------

# ACS table B01001 (sex by age) age bands, in table order. Codes 003-025 are
# male and 027-049 female. `hiv_age` is the AtlasPlus HIV age group each band
# belongs to; `hiv_weight` counts 2/5 of the 10-14 band (ages 13 and 14),
# assuming ages are spread evenly within the band.
.acs_age_bands <- function() {
  data.frame(
    band = c(
      "Under 5", "5 to 9", "10 to 14", "15 to 17", "18 and 19", "20", "21",
      "22 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
      "50 to 54", "55 to 59", "60 and 61", "62 to 64", "65 and 66", "67 to 69",
      "70 to 74", "75 to 79", "80 to 84", "85 and over"
    ),
    hiv_age = c(
      NA, NA, rep("13-24", 6), rep("25-34", 2), rep("35-44", 2),
      rep("45-54", 2), rep("55-64", 3), rep("65+", 6)
    ),
    hiv_weight = c(0, 0, 0.4, rep(1, 20)),
    male = sprintf("B01001_%03d", 3:25),
    female = sprintf("B01001_%03d", 27:49)
  )
}

# ACS table B03002 (Hispanic or Latino origin by race), matched to AtlasPlus's
# combined race/ethnicity categories. "Some other race alone" (B03002_008) has
# no AtlasPlus category.
.acs_race_variables <- function() {
  data.frame(
    variable = sprintf("B03002_%03d", c(3, 4, 5, 6, 7, 9, 12)),
    race_ethnicity = c(
      "White", "Black/African American", "American Indian/Alaska Native",
      "Asian", "Native Hawaiian/Other Pacific Islander", "Multiracial",
      "Hispanic/Latino"
    )
  )
}

# One row per Census variable and stratum, with the weight each variable gets.
# `ages` is "13+" (HIV) or "all".
.census_denominators <- function(stratify_by, ages) {
  bands <- .acs_age_bands()
  all_age_label <- if (ages == "13+") "Ages 13 years and older" else "All age groups"
  weight <- if (ages == "13+") bands$hiv_weight else rep(1, nrow(bands))

  by_band <- rbind(
    data.frame(variable = bands$male, sex = "Male", hiv_age = bands$hiv_age, weight = weight),
    data.frame(variable = bands$female, sex = "Female", hiv_age = bands$hiv_age, weight = weight)
  )
  by_band <- by_band[by_band$weight > 0, ]

  if (is.null(stratify_by)) {
    return(data.frame(
      variable = by_band$variable, weight = by_band$weight,
      race_ethnicity = "All races/ethnicities", sex = "Both sexes", age = all_age_label
    ))
  }

  switch(stratify_by,
    sex = data.frame(
      variable = by_band$variable, weight = by_band$weight,
      race_ethnicity = "All races/ethnicities", sex = by_band$sex, age = all_age_label
    ),
    age = data.frame(
      variable = by_band$variable, weight = by_band$weight,
      race_ethnicity = "All races/ethnicities", sex = "Both sexes", age = by_band$hiv_age
    ),
    race = data.frame(
      variable = .acs_race_variables()$variable, weight = 1,
      race_ethnicity = .acs_race_variables()$race_ethnicity,
      sex = "Both sexes", age = all_age_label
    )
  )
}

# Tract populations for one year: one row per tract and stratum, with the
# stratum population and the tract's total population for the same ages.
#
# Race populations come from B03002, which has no ages. For ages 13+, each
# tract's race populations are scaled by the tract's 13+ share of its total
# population (an approximation that assumes the same age structure for every
# group within a tract).
.tract_population <- function(year, states, stratify_by, ages) {
  denominators <- .census_denominators(stratify_by, ages)
  total_13 <- .census_denominators(NULL, "13+")
  variables <- unique(c(denominators$variable, total_13$variable, "B01001_001"))

  acs <- tidycensus::get_acs(
    geography = "tract",
    variables = variables,
    year = year,
    survey = "acs5",
    state = states
  )
  acs <- data.frame(
    tract_fips = acs$GEOID, tract_name = acs$NAME,
    variable = acs$variable, estimate = acs$estimate
  )

  tracts <- unique(acs[c("tract_fips", "tract_name")])
  tracts$county_fips <- substr(tracts$tract_fips, 1, 5)

  totals <- acs |>
    dplyr::left_join(total_13[c("variable", "weight")], by = "variable") |>
    dplyr::summarize(
      total_all = sum(estimate[variable == "B01001_001"], na.rm = TRUE),
      total_13 = sum(estimate * weight, na.rm = TRUE),
      .by = tract_fips
    )
  totals$tract_total_population <- if (ages == "13+") totals$total_13 else totals$total_all

  strata <- acs |>
    dplyr::inner_join(denominators, by = "variable", relationship = "many-to-one") |>
    dplyr::summarize(
      tract_population_acs = sum(estimate * weight, na.rm = TRUE),
      .by = c(tract_fips, race_ethnicity, sex, age)
    )

  # Every tract appears in every stratum, even with no population in it.
  grid <- merge(tracts, unique(denominators[c("race_ethnicity", "sex", "age")]), by = NULL)
  out <- grid |>
    dplyr::left_join(strata, by = c("tract_fips", "race_ethnicity", "sex", "age")) |>
    dplyr::left_join(totals, by = "tract_fips") |>
    dplyr::mutate(tract_population_acs = dplyr::coalesce(tract_population_acs, 0))

  if (identical(stratify_by, "race") && ages == "13+") {
    share_13 <- ifelse(out$total_all > 0, out$total_13 / out$total_all, 0)
    out$tract_population_acs <- out$tract_population_acs * share_13
  }

  out[c("tract_fips", "tract_name", "county_fips", "race_ethnicity", "sex", "age",
        "tract_population_acs", "tract_total_population")]
}


# County counts -----------------------------------------------------------------

# County counts ready for allocation, plus a per-state report of cases that no
# county accounts for.
#
# suppressed = "state_rate": the state's rate in the same stratum and indicator
#   times the county's CDC population, capped at 4 (suppressed counts are
#   normally 1-4). Not floored at 1: cells suppressed for a small population
#   may have no cases.
# suppressed = "zero": suppressed counties get 0.
.prepare_county_cases <- function(county, state, suppressed) {
  keys <- c("indicator", "year", "race_ethnicity", "sex", "age")
  for (k in c("indicator", "race_ethnicity", "sex", "age", "data_status", "geography")) {
    if (k %in% names(county)) county[[k]] <- as.character(county[[k]])
    if (k %in% names(state)) state[[k]] <- as.character(state[[k]])
  }

  county <- county |>
    dplyr::transmute(
      indicator, year, race_ethnicity, sex, age,
      county_fips = fips, county_name = geography, data_status,
      county_cases = cases, county_population_atlas = population,
      state_fips = substr(fips, 1, 2)
    ) |>
    dplyr::filter(!(is.na(county_cases) & data_status == "Data not available"))

  state_rates <- state |>
    dplyr::transmute(
      indicator, year, race_ethnicity, sex, age,
      state_fips = fips, state_cases = cases,
      state_rate = ifelse(!is.na(population) & population > 0, cases / population, NA_real_)
    )

  county <- county |>
    dplyr::left_join(state_rates[c(keys, "state_fips", "state_rate")], by = c(keys, "state_fips")) |>
    dplyr::mutate(
      suppressed_county = is.na(county_cases),
      estimate = if (suppressed == "zero") 0 else
        pmin(4, pmax(0, dplyr::coalesce(state_rate * county_population_atlas, 0))),
      county_cases_source = dplyr::case_when(
        !suppressed_county ~ "published",
        suppressed == "zero" ~ "zero (suppressed)",
        TRUE ~ "state rate (suppressed)"
      ),
      county_cases = dplyr::if_else(suppressed_county, estimate, county_cases)
    ) |>
    dplyr::select(-estimate, -state_rate, -suppressed_county)

  no_county <- county |>
    dplyr::summarize(
      counties = dplyr::n(),
      suppressed_counties = sum(county_cases_source != "published"),
      published_cases = sum(county_cases[county_cases_source == "published"]),
      estimated_cases = sum(county_cases[county_cases_source != "published"]),
      .by = dplyr::all_of(c(keys, "state_fips"))
    ) |>
    dplyr::left_join(state_rates[c(keys, "state_fips", "state_cases")], by = c(keys, "state_fips")) |>
    dplyr::mutate(no_county_cases = state_cases - published_cases - estimated_cases)

  list(county = county, no_county = no_county)
}


# Allocation --------------------------------------------------------------------

.extrapolate_to_tract <- function(df, payload_state, stratify_by, suppressed = "state_rate") {
  hiv_indicators <- c(
    "HIV diagnoses", "HIV prevalence", "Estimated HIV incidence",
    "Estimated HIV prevalence (undiagnosed and diagnosed)", "Knowledge of Status"
  )
  ages <- if (all(as.character(unique(df$indicator)) %in% hiv_indicators)) "13+" else "all"

  state <- .request_atlas_all(list(payload_state))
  prepared <- .prepare_county_cases(df, state, suppressed)
  county <- prepared$county
  states <- sort(unique(county$state_fips))

  tracts <- lapply(sort(unique(county$year)), function(yr) {
    pop <- .tract_population(yr, states, stratify_by, ages)
    cases <- county[county$year == yr, ]
    joined <- dplyr::inner_join(
      pop, cases,
      by = c("county_fips", "race_ethnicity", "sex", "age"),
      relationship = "many-to-many"
    )
    unmatched <- dplyr::anti_join(cases, pop, by = "county_fips")
    list(joined = joined, unmatched = unmatched)
  })

  unmatched <- dplyr::bind_rows(lapply(tracts, `[[`, "unmatched"))
  out <- dplyr::bind_rows(lapply(tracts, `[[`, "joined")) |>
    dplyr::mutate(
      county_population_acs = sum(tract_population_acs),
      county_total_population = sum(tract_total_population),
      allocation = dplyr::case_when(
        county_population_acs > 0 ~ "stratum population",
        county_total_population > 0 ~ "total population",
        TRUE ~ "equal shares"
      ),
      share = dplyr::case_when(
        allocation == "stratum population" ~ tract_population_acs / county_population_acs,
        allocation == "total population" ~ tract_total_population / county_total_population,
        TRUE ~ 1 / dplyr::n()
      ),
      tract_cases = county_cases * share,
      tract_noncases = pmax(0, tract_population_acs - tract_cases),
      .by = c(county_fips, year, indicator, race_ethnicity, sex, age)
    ) |>
    dplyr::select(
      year, tract_fips, tract_name, county_fips, county_name, state_fips,
      indicator, race_ethnicity, sex, age, data_status,
      county_cases, county_cases_source, county_population_atlas,
      tract_population_acs, county_population_acs, tract_total_population,
      allocation, tract_cases, tract_noncases
    ) |>
    tibble::as_tibble()

  attr(out, "no_county_cases") <- tibble::as_tibble(prepared$no_county)
  attr(out, "unmatched_county_cases") <- tibble::as_tibble(unmatched)

  no_county_by_indicator <- prepared$no_county |>
    dplyr::summarize(cases = sum(pmax(0, no_county_cases), na.rm = TRUE), .by = c(indicator, year))
  cli::cli_inform(c(
    "i" = "Cases in state totals with no county are not allocated to tracts:",
    stats::setNames(
      sprintf("%s %s: %s", no_county_by_indicator$indicator, no_county_by_indicator$year,
              format(round(no_county_by_indicator$cases), big.mark = ",")),
      rep("*", nrow(no_county_by_indicator))
    ),
    " " = "See {.code attr(x, \"no_county_cases\")} for each state."
  ))
  if (nrow(unmatched) > 0) {
    cli::cli_warn(c(
      "{nrow(unmatched)} county row{?s} had no matching ACS tracts and {?was/were} not allocated.",
      "i" = "Counties: {.val {unique(unmatched$county_name)}}.",
      "i" = "See {.code attr(x, \"unmatched_county_cases\")}."
    ))
  }

  out
}
