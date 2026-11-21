# Made-up data for test-pipeline-summary.R. The pipeline fixtures it builds on
# are in helper-pipeline.R and helper-pipeline-report.R.

# A tiny made-up detail table (a .pipeline_report() table with a `scenario`
# column) for .pipeline_summary(): one unstratified run with three states.
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
    (if (anyNA(category)) TRUE else compact$category %in% category)
  compact[rows, ]
}

# The most rows .pipeline_summary() can give for one year: `groups` strata in
# all (not counting "all" or "sum of groups"), `runs` stratifications and
# `states` states. Each stratum has at most 10 figures; each run has at most
# 76 national figures, 19 across-state figures and two values per state.
summary_row_bound <- function(groups, runs, states) {
  10 * groups + (76 + 19 + 2 * states) * runs
}

# report_two_states() as a detail table (with the `scenario` column the
# summary expects), summarized.
summary_two_states <- function(change_state, env = parent.frame()) {
  detail <- report_two_states(change_state, env = env)
  detail$scenario <- "default"
  .pipeline_summary(detail)
}
