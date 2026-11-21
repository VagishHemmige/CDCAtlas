# Made-up data for test-pipeline-report.R. Every number is invented so that
# each count in the report can be checked by hand.
#
# One state (01), 2022, by sex, six counties. For HIV prevalence, with
# `cap = 4` and `floor = 1`:
#
# | County | Male                         | Female                        |
# |--------|------------------------------|-------------------------------|
# | 01001  | published 40                 | published 30                  |
# | 01003  | suppressed, raw 10 -> 4      | suppressed, raw 0.5 -> 1      |
# | 01005  | suppressed, raw 4 (at cap)   | suppressed, raw 3             |
# | 01007  | suppressed, no population    | published 5                   |
# | 01009  | published 7, no ACS tracts   | published 2, no ACS tracts    |
# | 01011  | published 6                  | published 3, no ACS women     |
#
# State totals: Male 1,000 of 1,000,000 (rate 0.001); Female 10 of 20,000
# (rate 0.0005). So the remainders are 1000 - 53 - 8 = 939 (Male) and
# 10 - 40 - 4 = -34 (Female).
#
# HIV diagnoses has no suppressed rows: 2 (Male) and 1 (Female) in every
# county, with state totals of 10 and 9, so its remainders are -2 and 3. A
# report that mixed the indicators would count two negative strata.

report_counties <- c("01001", "01003", "01005", "01007", "01009", "01011")

report_county_atlas <- function() {
  one <- function(indicator, sex, cases, population) {
    data.frame(
      indicator = indicator,
      year = 2022,
      geography = paste("County", report_counties),
      data_status = ifelse(is.na(cases), "Data suppressed", "Not Suppressed"),
      race_ethnicity = "All races/ethnicities",
      sex = sex,
      age = "Ages 13 years and older",
      transmission = "All transmission categories",
      rate100000 = NA_real_,
      cases = cases,
      population = population,
      fips = report_counties
    )
  }
  male_pop <- c(50000, 10000, 4000, NA, 3000, 800)
  female_pop <- c(50000, 1000, 6000, 2000, 3000, 700)
  out <- rbind(
    one("HIV prevalence", "Male", c(40, NA, NA, NA, 7, 6), male_pop),
    one("HIV prevalence", "Female", c(30, NA, NA, 5, 2, 3), female_pop),
    one("HIV diagnoses", "Male", rep(2, 6), c(male_pop[1:3], 1000, 3000, 800)),
    one("HIV diagnoses", "Female", rep(1, 6), female_pop)
  )
  factors <- c(
    "indicator",
    "geography",
    "data_status",
    "race_ethnicity",
    "sex",
    "age",
    "transmission"
  )
  out[factors] <- lapply(out[factors], factor)
  out
}

report_state_atlas <- function() {
  data.frame(
    indicator = rep(c("HIV prevalence", "HIV diagnoses"), each = 2),
    year = 2022,
    race_ethnicity = "All races/ethnicities",
    sex = rep(c("Male", "Female"), 2),
    age = "Ages 13 years and older",
    cases = c(1000, 10, 10, 9),
    population = c(1e6, 20000, 1e6, 20000),
    geography = "Alabama",
    data_status = "Not Suppressed",
    fips = "01"
  )
}

# Seven tracts: three in 01001 and one each in 01003, 01005, 01007 and 01011;
# none in 01009. Every sex-by-age band holds 10 people, so each tract has
# (0.4 + 20) * 10 = 204 men and 204 women aged 13 and older, except the tract
# in 01011, which has no women.
report_acs <- function() {
  tracts <- c(
    "01001000100",
    "01001000200",
    "01001000300",
    "01003000100",
    "01005000100",
    "01007000100",
    "01011000100"
  )
  male <- sprintf("B01001_%03d", 3:25)
  female <- sprintf("B01001_%03d", 27:49)
  grid <- expand.grid(
    GEOID = tracts,
    variable = c("B01001_001", male, female),
    stringsAsFactors = FALSE
  )
  grid$NAME <- paste("Census Tract", grid$GEOID)
  no_women <- grid$GEOID == "01011000100"
  grid$estimate <- 10
  grid$estimate[no_women & grid$variable %in% female] <- 0
  grid$estimate[grid$variable == "B01001_001"] <- 460
  grid$estimate[no_women & grid$variable == "B01001_001"] <- 230
  grid$moe <- 0
  grid
}

# The default pipeline on the made-up data, with a raised floor.
report_fixture <- function(env = parent.frame()) {
  local_fake_sources(
    state = report_state_atlas(),
    acs = report_acs(),
    env = env
  )
  county <- report_county_atlas()
  suppressMessages(suppressWarnings({
    imputed <- impute_suppressed(county, cap = 4, floor = 1)
    areas <- extrapolate_from_county(imputed)
  }))
  list(county = county, imputed = imputed, areas = areas)
}

# One value of the report, by measure and (optionally) category, for one
# group of HIV prevalence unless `indicator` says otherwise.
report_value <- function(
  report,
  measure,
  category = NA,
  group = "sum of groups",
  indicator = "HIV prevalence",
  state_fips = "all"
) {
  rows <- report$measure == measure &
    report$group == group &
    report$indicator == indicator &
    report$state_fips == state_fips &
    (if (is.na(category)) {
      is.na(report$category)
    } else {
      report$category %in% category
    })
  report$value[rows]
}

# A made-up crosswalk for 2010 tracts: tract 01001000300 now lies in county
# 01003, and tract 01001000200 is split evenly between the two counties.
report_crosswalk <- data.frame(
  tract_2010 = c("01001000200", "01001000200", "01001000300"),
  county_fips = c("01001", "01003", "01003"),
  share = c(0.5, 0.5, 1)
)

# Stands in for CDC in the test of data-raw/measured-effects.R: answers state
# requests with state rows and one state's county request with county rows,
# for 2017, 2022 and 2023 only, in any one stratification. Other years return
# no rows, as CDC does for a year it has not released.
report_fake_cdc <- function(payload) {
  ids <- as.integer(strsplit(payload$VariableIDs, ",")[[1]])
  stratify_by <- if (602 %in% ids) {
    "sex"
  } else if (656 %in% ids) {
    "age"
  } else if (553 %in% ids) {
    "race"
  }
  years <- c(2017, 2022, 2023)
  county_ids <- lookup[["counties"]]$geo_id
  if (any(ids %in% county_ids)) {
    fips <- lookup[["counties"]]$fips[match(
      ids[ids %in% county_ids],
      county_ids
    )]
    rows <- fake_county_atlas(stratify_by, suppressed = "01003", years = years)
    rows <- rows[substr(rows$fips, 1, 2) %in% substr(fips, 1, 2), ]
  } else {
    rows <- fake_state_atlas(
      no_county = 50,
      stratify_by = stratify_by,
      years = years
    )
  }
  fake_state_response(rows, payload)
}

# A tiny made-up detail table, as data-raw/measured-effects.R builds it, for
# .pipeline_summary(): one unstratified run with three states.
#
# | State        | State cases | Published | Imputed | Remainder | Share |
# |--------------|-------------|-----------|---------|-----------|-------|
# | 01 Alabama   | 1,000       | 900       | 40      | 60        | 0.06  |
# | 02 Alaska    | 500         | 520       | 10      | -30       | -0.06 |
# | 04 Arizona   | 200         | 150       | 10      | 40        | 0.20  |
#
# Nationally: 1,700 state cases, 1,570 published, 60 imputed; remainder 70
# signed, 100 positive, -30 negative. No county row reaches an area, so the
# shares of allocated rows have a denominator of 0.
#
# Under `cap = Inf` the states' imputed cases are 100, 10 and 30 (140 in
# all); under `method = "zero"` they are 0.
summary_detail <- function() {
  rows <- function(
    scenario,
    state_fips,
    step,
    measure,
    value,
    category = NA_character_,
    unit = "cases"
  ) {
    data.frame(
      year = 2022,
      indicator = "HIV prevalence",
      stratify_by = "none",
      scenario = scenario,
      group = "all",
      state_fips = state_fips,
      step = step,
      measure = measure,
      category = category,
      unit = unit,
      basis = "count",
      value = value,
      denominator = NA_character_,
      note = NA_character_
    )
  }
  states <- c("01", "02", "04")
  by_state <- function(scenario, measure, value) {
    rows(scenario, states, "no county", measure, value)
  }
  national <- function(step, measure, value, ...) {
    rows("default", "all", step, measure, value, ...)
  }
  status <- c("Not Suppressed", "Data suppressed")
  source <- c("published", "state_rate")
  rule <- "stratum population"
  dplyr::bind_rows(
    national("download", "county_rows", c(80, 20), status, "county rows"),
    national("imputation", "suppressed_rows", 20, unit = "county rows"),
    national("imputation", "suppressed_rows_raw_above_cap", 12),
    national("imputation", "suppressed_rows_raw_below_floor", 0),
    national("imputation", "suppressed_rows_raw_within_limits", 8),
    national("imputation", "cases_raw_estimate", 100),
    national("imputation", "cases_removed_by_cap", 40),
    national("imputation", "county_rows_by_source", c(80, 20), source),
    national("imputation", "cases_by_source", c(1570, 60), source),
    national("no county", "state_cases", 1700),
    national("no county", "published_cases", 1570),
    national("no county", "imputed_cases", 60),
    national("no county", "remainder_signed", 70),
    national("no county", "remainder_positive", 100),
    national("no county", "remainder_negative", -30),
    national("no county", "state_strata_remainder_negative", 1),
    national("no county", "state_strata_without_total", 0),
    national("allocation", "county_rows_allocated", 0, rule),
    national("allocation", "cases_allocated", 0, rule),
    national("allocation", "area_rows_cases_exceed_population", 0, rule),
    by_state("default", "state_cases", c(1000, 500, 200)),
    by_state("default", "published_cases", c(900, 520, 150)),
    by_state("default", "imputed_cases", c(40, 10, 10)),
    by_state("default", "remainder_signed", c(60, -30, 40)),
    rows("cap = Inf", "all", "no county", "published_cases", 1570),
    rows("cap = Inf", "all", "no county", "imputed_cases", 140),
    by_state("cap = Inf", "imputed_cases", c(100, 10, 30)),
    rows("method = zero", "all", "no county", "published_cases", 1570),
    rows("method = zero", "all", "no county", "imputed_cases", 0),
    by_state("method = zero", "imputed_cases", c(0, 0, 0))
  )
}

# The rows of a compact summary for one measure (and category, scenario or
# state).
summary_rows <- function(
  compact,
  measure,
  category = NA,
  scenario = "default",
  state_fips = "all"
) {
  rows <- compact$measure == measure &
    compact$scenario == scenario &
    compact$state_fips %in% state_fips &
    (if (is.na(category)) TRUE else compact$category %in% category)
  compact[rows, ]
}

# The most rows .pipeline_summary() can give for one year: `groups` strata in
# all (not counting "all" or "sum of groups"), `runs` stratifications and
# `states` states. Each stratum has at most 10 figures; each run has at most
# 76 national figures, 19 across-state figures and two values per state.
summary_row_bound <- function(groups, runs, states) {
  10 * groups + (76 + 19 + 2 * states) * runs
}
