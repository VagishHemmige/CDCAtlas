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

# two_state_atlas() run through the pipeline and the report, by state, after
# `change_state` has altered the state rows. Returns the report's rows for
# HIV prevalence.
report_two_states <- function(
  change_state,
  suppressed = both_counties,
  env = parent.frame()
) {
  data <- two_state_atlas(suppressed)
  local_fake_sources(
    state = change_state(data$state),
    acs = data$acs,
    env = env
  )
  suppressMessages(suppressWarnings({
    imputed <- impute_suppressed(data$county)
    areas <- extrapolate_from_county(imputed)
  }))
  detail <- .pipeline_report(data$county, imputed, areas, by_state = TRUE)
  detail[detail$indicator == "HIV prevalence", ]
}

# The same counties' unstratified totals, for fit_to = "county_total"
# (#179). HIV prevalence: 01003's total of 13 is shared by its seeds 10 and
# 0.5 (13 * 10 / 10.5 = 12.381 and 0.619); 01005's 14 by 4 and 3 (8 and 6);
# 01007 has one suppressed cell, so it isn't fitted. HIV diagnoses: 2 + 1.
report_unstratified <- function() {
  prevalence <- c(70, 13, 14, 9, 9, 9)
  data.frame(
    indicator = rep(c("HIV prevalence", "HIV diagnoses"), each = 6),
    year = 2022,
    geography = paste("County", report_counties),
    data_status = "Not Suppressed",
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = "Ages 13 years and older",
    cases = c(prevalence, rep(3, 6)),
    population = 50000,
    fips = report_counties
  )
}

# The pipeline on the made-up data, fitted to county totals.
report_fit_fixture <- function(env = parent.frame()) {
  local_fake_sources(
    state = report_state_atlas(),
    acs = report_acs(),
    county_unstratified = report_unstratified(),
    env = env
  )
  county <- report_county_atlas()
  suppressMessages(suppressWarnings({
    imputed <- impute_suppressed(
      county,
      cap = 4,
      floor = 1,
      fit_to = "county_total"
    )
    areas <- extrapolate_from_county(imputed)
  }))
  list(county = county, imputed = imputed, areas = areas)
}
