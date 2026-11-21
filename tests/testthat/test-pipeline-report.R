# .pipeline_report(): what each pipeline step did, as one long table. The
# made-up data and its hand-worked answers are in helper-pipeline-report.R.

test_that("the report is one long table with labelled units", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_s3_class(report, "tbl_df")
  expect_named(
    report,
    c(
      "year",
      "indicator",
      "stratify_by",
      "group",
      "state_fips",
      "step",
      "measure",
      "category",
      "unit",
      "basis",
      "value",
      "denominator",
      "note"
    )
  )
  expect_setequal(report$stratify_by, "sex")
  expect_setequal(report$year, 2022)
  expect_setequal(report$group, c("Male", "Female", "sum of groups"))
  expect_setequal(report$state_fips, "all")
  expect_setequal(
    report$step,
    c(
      "download",
      "imputation",
      "no county",
      "allocation",
      "denominators",
      "geography"
    )
  )
  expect_in(
    report$unit,
    c(
      "county rows",
      "state strata",
      "area rows",
      "areas",
      "cases",
      "people",
      "proportion"
    )
  )
  expect_in(report$basis, c("count", "published", "modelled", "acs", "derived"))
  # Every proportion names its denominator, and nothing else has one.
  expect_equal(!is.na(report$denominator), report$unit == "proportion")
  keys <- c("indicator", "group", "state_fips", "step", "measure", "category")
  expect_equal(sum(duplicated(report[keys])), 0)
})

test_that("download rows are counted by data status, with and without a count", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(report_value(report, "county_rows", "Not Suppressed"), 7)
  expect_equal(report_value(report, "county_rows", "Data suppressed"), 5)
  expect_equal(
    report_value(report, "county_rows_without_count", "Data suppressed"),
    5
  )
  expect_equal(
    report_value(report, "county_rows", "Data suppressed", group = "Male"),
    3
  )
  expect_equal(report_value(report, "published_county_cases"), 93)
  expect_equal(
    report_value(report, "published_county_cases", group = "Male"),
    53
  )
})

test_that("the cap and the floor are counted only where they changed the estimate", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(report_value(report, "suppressed_rows"), 5)
  expect_equal(report_value(report, "suppressed_rows_estimated"), 4)
  # 01003 Male (raw 10) is capped; 01005 Male (raw exactly 4) is not.
  expect_equal(report_value(report, "suppressed_rows_raw_above_cap"), 1)
  expect_equal(
    report_value(report, "suppressed_rows_raw_above_cap", group = "Male"),
    1
  )
  expect_equal(
    report_value(report, "suppressed_rows_raw_above_cap", group = "Female"),
    0
  )
  # 01003 Female (raw 0.5) is raised to the floor of 1.
  expect_equal(report_value(report, "suppressed_rows_raw_below_floor"), 1)
  expect_equal(
    report_value(report, "suppressed_rows_raw_below_floor", group = "Female"),
    1
  )
  expect_equal(report_value(report, "suppressed_rows_raw_within_limits"), 2)
  expect_equal(report_value(report, "cases_raw_estimate"), 10 + 4 + 0.5 + 3)
  expect_equal(report_value(report, "cases_removed_by_cap"), 6)
  expect_equal(report_value(report, "cases_added_by_floor"), 0.5)
})

test_that("case mass is reported by source, and the imputed share of the state total", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(report_value(report, "county_rows_by_source", "published"), 7)
  expect_equal(report_value(report, "county_rows_by_source", "state_rate"), 4)
  expect_equal(
    report_value(report, "county_rows_by_source", "not estimated"),
    1
  )
  expect_equal(report_value(report, "cases_by_source", "published"), 93)
  expect_equal(report_value(report, "cases_by_source", "state_rate"), 12)
  expect_equal(
    report_value(report, "cases_by_source", "state_rate", group = "Male"),
    8
  )
  expect_equal(
    report_value(report, "imputed_share_of_state_cases"),
    12 / 1010
  )
  expect_equal(
    report_value(report, "imputed_share_of_state_cases", group = "Female"),
    4 / 10
  )
  share <- report[report$measure == "imputed_share_of_state_cases", ]
  expect_setequal(share$unit, "proportion")
  expect_match(share$denominator, "state_cases", fixed = TRUE)
})

test_that("rows that could not be estimated are counted by reason", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(
    report_value(
      report,
      "county_rows_not_estimated",
      "suppressed, no county population"
    ),
    1
  )
  not_estimated <- report[report$measure == "county_rows_not_estimated", ]
  expect_setequal(not_estimated$category, "suppressed, no county population")
  expect_setequal(not_estimated$group, c("Male", "sum of groups"))
})

test_that("remainders are signed, positive-only and negative-only", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(report_value(report, "state_strata"), 2)
  expect_equal(report_value(report, "state_strata_without_total"), 0)
  expect_equal(report_value(report, "state_cases"), 1010)
  expect_equal(report_value(report, "unpublished_cases"), 1010 - 93)
  expect_equal(report_value(report, "remainder_signed"), 939 - 34)
  expect_equal(report_value(report, "remainder_positive"), 939)
  expect_equal(report_value(report, "remainder_negative"), -34)
  expect_equal(report_value(report, "state_strata_remainder_negative"), 1)
  expect_equal(report_value(report, "state_strata_remainder_positive"), 1)
  expect_equal(report_value(report, "remainder_signed", group = "Male"), 939)
  expect_equal(report_value(report, "remainder_signed", group = "Female"), -34)
  expect_equal(
    report_value(report, "remainder_positive", group = "Female"),
    0
  )
  expect_equal(
    report_value(report, "remainder_positive_share_of_state_cases"),
    939 / 1010
  )
})

test_that("diagnoses are never counted in prevalence measures", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )
  prevalence_only <- run
  keep <- run$county$indicator == "HIV prevalence"
  prevalence_only$county <- run$county[keep, ]
  local_fake_sources(state = report_state_atlas(), acs = report_acs())
  suppressMessages(suppressWarnings({
    prevalence_only$imputed <- impute_suppressed(
      prevalence_only$county,
      cap = 4,
      floor = 1
    )
    prevalence_only$areas <- extrapolate_from_county(prevalence_only$imputed)
  }))
  alone <- .pipeline_report(
    prevalence_only$county,
    prevalence_only$imputed,
    prevalence_only$areas,
    cap = 4,
    floor = 1
  )

  expect_setequal(report$indicator, c("HIV prevalence", "HIV diagnoses"))
  expect_equal(
    report[report$indicator == "HIV prevalence", ],
    alone,
    ignore_attr = TRUE
  )
  # Diagnoses has its own remainders: -2 (Male) and 3 (Female).
  expect_equal(
    report_value(
      report,
      "state_strata_remainder_negative",
      indicator = "HIV diagnoses"
    ),
    1
  )
  expect_equal(
    report_value(report, "remainder_negative", indicator = "HIV diagnoses"),
    -2
  )
  expect_equal(
    report_value(report, "suppressed_rows", indicator = "HIV diagnoses"),
    0
  )
})

test_that("allocation is counted by rule, and rows left out by reason", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(
    report_value(report, "county_rows_allocated", "stratum population"),
    8
  )
  expect_equal(
    report_value(report, "county_rows_allocated", "total population"),
    1
  )
  expect_equal(report_value(report, "area_rows", "stratum population"), 12)
  expect_equal(report_value(report, "area_rows", "total population"), 1)
  expect_equal(
    report_value(report, "cases_allocated", "stratum population"),
    54 + 39
  )
  expect_equal(report_value(report, "cases_allocated", "total population"), 3)

  # 01011 Female: 3 cases shared into a tract with no women.
  expect_equal(
    report_value(
      report,
      "area_rows_cases_exceed_population",
      "total population"
    ),
    1
  )
  expect_equal(
    report_value(
      report,
      "area_rows_cases_exceed_population",
      "stratum population"
    ),
    0
  )
  expect_equal(
    report_value(
      report,
      "cases_in_area_rows_exceeding_population",
      "total population"
    ),
    3
  )
  expect_equal(
    report_value(
      report,
      "people_in_area_rows_exceeding_population",
      "total population"
    ),
    0
  )

  expect_equal(
    report_value(
      report,
      "county_rows_not_allocated",
      "no ACS tracts for this county"
    ),
    2
  )
  expect_equal(
    report_value(
      report,
      "cases_not_allocated",
      "no ACS tracts for this county"
    ),
    9
  )
  expect_equal(
    report_value(
      report,
      "county_rows_not_allocated",
      "suppressed, not estimated"
    ),
    1
  )
})

test_that("imputed counts left out of the areas are not summed as published", {
  county <- fake_county_atlas(suppressed = "01003")
  acs <- fake_acs()
  local_fake_sources(acs = acs[substr(acs$GEOID, 1, 5) == "01001", ])
  suppressMessages(suppressWarnings({
    imputed <- impute_suppressed(county)
    areas <- extrapolate_from_county(imputed)
  }))
  report <- .pipeline_report(county, imputed, areas)
  reason <- "no ACS tracts for this county"

  expect_equal(
    report_value(report, "county_rows_not_allocated", reason, group = "all"),
    1
  )
  expect_equal(
    report_value(report, "cases_not_allocated", reason, group = "all"),
    0
  )
  expect_equal(
    report_value(report, "imputed_cases_not_allocated", reason, group = "all"),
    4
  )
  left_out <- report[report$measure == "imputed_cases_not_allocated", ]
  expect_setequal(left_out$basis, "modelled")
})

test_that("notes say when an indicator's remainder or population means something else", {
  indicators <- c(
    "Estimated HIV prevalence (undiagnosed and diagnosed)",
    "Knowledge of Status"
  )
  county <- fake_county_atlas(indicators = indicators)
  local_fake_sources(state = fake_state_atlas(indicators = indicators))
  suppressMessages({
    imputed <- impute_suppressed(county)
    areas <- extrapolate_from_county(imputed)
  })
  report <- .pipeline_report(county, imputed, areas)

  remainder <- report[report$measure == "remainder_signed", ]
  expect_match(remainder$note, "EHE counties")
  compared <- report[
    report$step == "denominators" &
      report$indicator == "Knowledge of Status",
  ]
  expect_match(compared$note, "people living with HIV")
  others <- report[
    report$step == "denominators" &
      report$indicator != "Knowledge of Status",
  ]
  expect_equal(others$note, rep(NA_character_, nrow(others)))
})

test_that("what public data cannot show is NA with a note, never a number", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )
  unknown <- function(measure, category = NA) {
    rows <- report$measure == measure &
      report$group == "sum of groups" &
      report$indicator == "HIV prevalence" &
      (is.na(category) | report$category %in% category)
    report[rows, ]
  }

  suppressed <- unknown("observed_cases_in_suppressed_rows")
  expect_equal(suppressed$value, NA_real_)
  expect_match(suppressed$note, "withholds")

  not_estimated <- unknown("cases_by_source", "not estimated")
  expect_equal(not_estimated$value, NA_real_)
  expect_match(not_estimated$note, "unknown")

  left_out <- unknown("cases_not_allocated", "suppressed, not estimated")
  expect_equal(left_out$value, NA_real_)
  expect_match(left_out$note, "remainder")

  no_residence <- unknown("cases_with_unknown_county_of_residence")
  expect_equal(no_residence$value, NA_real_)
  expect_match(no_residence$note, "difference")

  absent <- unknown("county_rows_expected_but_absent")
  expect_equal(absent$value, NA_real_)

  # Every NA says why.
  expect_equal(sum(is.na(report$value) & is.na(report$note)), 0)
})

test_that("CDC and ACS county populations are compared, with the rows counted", {
  run <- report_fixture()
  report <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )

  expect_equal(report_value(report, "county_rows_populations_compared"), 9)
  # Male: 01001 (612 - 50000), 01003 (204 - 10000), 01005 (204 - 4000) and
  # 01011 (204 - 800).
  difference <- "population_difference_acs_minus_cdc"
  expect_equal(report_value(report, difference, "min", group = "Male"), -49388)
  expect_equal(report_value(report, difference, "max", group = "Male"), -596)
  expect_equal(
    report_value(report, difference, "median", group = "Male"),
    mean(c(-9796, -3796))
  )
  expect_equal(
    report_value(
      report,
      "population_relative_difference_acs_vs_cdc",
      "max",
      group = "Male"
    ),
    -596 / 800
  )
})

test_that("crosswalked and duplicated areas are counted", {
  local_mocked_bindings(.tract_crosswalk = function() report_crosswalk)
  county <- fake_county_atlas(years = 2017)
  local_fake_sources(state = fake_state_atlas(years = 2017))
  suppressMessages({
    imputed <- impute_suppressed(county)
    areas <- extrapolate_from_county(imputed)
  })
  report <- .pipeline_report(county, imputed, areas)
  value <- function(measure) {
    report_value(report, measure, group = "all")
  }

  expect_setequal(report$stratify_by, "none")
  expect_setequal(report$group, "all")
  expect_equal(value("area_rows_total"), 5)
  expect_equal(value("areas"), 4)
  expect_equal(value("area_rows_geoid_in_two_counties"), 2)
  expect_equal(value("areas_in_two_counties"), 1)
  expect_equal(value("area_rows_crosswalk"), 3)
  expect_equal(value("areas_crosswalk"), 2)
  expect_equal(value("county_rows_crosswalk"), 2)

  # 2022 uses 2020 tracts, so nothing is crosswalked or duplicated.
  run <- report_fixture()
  later <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1
  )
  expect_equal(report_value(later, "area_rows_crosswalk"), 0)
  expect_equal(report_value(later, "area_rows_geoid_in_two_counties"), 0)
})

test_that("by_state adds each state's totals and remainder", {
  data <- two_state_atlas()
  local_fake_sources(state = data$state, acs = data$acs)
  suppressMessages({
    imputed <- impute_suppressed(data$county)
    areas <- extrapolate_from_county(imputed)
  })
  national <- .pipeline_report(data$county, imputed, areas)
  report <- .pipeline_report(data$county, imputed, areas, by_state = TRUE)
  value <- function(measure, state_fips) {
    report_value(report, measure, group = "all", state_fips = state_fips)
  }

  expect_equal(report[report$state_fips == "all", ], national)
  by_state <- report[report$state_fips != "all", ]
  expect_setequal(by_state$state_fips, c("01", "02"))
  expect_setequal(
    by_state$measure,
    c("state_cases", "published_cases", "imputed_cases", "remainder_signed")
  )
  # Prevalence: counties hold 111 and 121; 01003 and 02003 are suppressed and
  # capped at 4. State 02 has 300 more cases with no county.
  expect_equal(value("state_cases", "01"), 232)
  expect_equal(value("state_cases", "02"), 532)
  expect_equal(value("imputed_cases", "02"), 4)
  expect_equal(value("remainder_signed", "01"), 232 - 111 - 4)
  expect_equal(value("remainder_signed", "02"), 532 - 111 - 4)
  expect_equal(
    value("remainder_signed", "all"),
    value("remainder_signed", "01") + value("remainder_signed", "02")
  )
})

test_that("the report works without the tract step, for sensitivity runs", {
  local_fake_sources(state = report_state_atlas(), acs = report_acs())
  county <- report_county_atlas()
  suppressWarnings(imputed <- impute_suppressed(county, method = "zero"))
  report <- .pipeline_report(county, imputed)

  expect_setequal(report$step, c("download", "imputation", "no county"))
  expect_equal(report_value(report, "cases_by_source", "zero"), 0)
  expect_equal(report_value(report, "county_rows_by_source", "zero"), 5)
  expect_equal(report_value(report, "remainder_signed"), 1010 - 93)
  expect_equal(report_value(report, "suppressed_rows_raw_above_cap"), 0)
})

test_that("the report refuses limits that the imputation didn't use", {
  run <- report_fixture()
  expect_snapshot(
    .pipeline_report(run$county, run$imputed, run$areas, cap = 4, floor = 0),
    error = TRUE
  )
  expect_snapshot(
    .pipeline_report(run$imputed, run$imputed, run$areas),
    error = TRUE
  )
  expect_snapshot(
    .pipeline_report(run$county, run$county, run$areas),
    error = TRUE
  )
})

# .pipeline_summary(): the compact summary, derived from the detail table. The
# made-up detail table is summary_detail() in helper-pipeline-report.R.

test_that("the compact summary gives the remainder nationally and across states", {
  compact <- .pipeline_summary(summary_detail())
  one <- function(...) summary_rows(compact, ...)

  expect_equal(one("remainder_signed")$value, 70)
  expect_equal(one("remainder_positive")$value, 100)
  expect_equal(one("remainder_negative")$value, -30)
  expect_equal(one("remainder_signed_share_of_state_cases")$value, 70 / 1700)
  expect_equal(one("remainder_positive_share_of_state_cases")$value, 100 / 1700)
  expect_equal(one("remainder_negative_share_of_state_cases")$value, -30 / 1700)

  # Shares by state: 0.06, -0.06 and 0.20.
  share <- one("state_remainder_share")
  expect_equal(share$category, c("min", "p25", "median", "p75", "max"))
  expect_equal(share$value, c(-0.06, 0, 0.06, 0.13, 0.20))
  expect_equal(
    share$extreme_state,
    c("Alaska (02)", NA, NA, NA, "Arizona (04)")
  )
  expect_match(share$note, "type 7", fixed = TRUE)
  expect_equal(one("states_remainder_negative")$value, 1)
  expect_equal(one("states_remainder_share_above_5pct")$value, 2)
  expect_equal(one("states_remainder_share_above_10pct")$value, 1)
  expect_equal(one("states_without_remainder_share")$value, 0)
})

test_that("the compact summary gives imputation shares with their denominators", {
  compact <- .pipeline_summary(summary_detail())
  one <- function(...) summary_rows(compact, ...)

  suppressed <- one("suppressed_rows_share_of_county_rows")
  expect_equal(suppressed$value, 20 / 100)
  expect_match(suppressed$numerator, "suppressed_rows", fixed = TRUE)
  expect_match(suppressed$denominator, "county_rows", fixed = TRUE)
  expect_equal(one("imputed_share_of_state_cases")$value, 60 / 1700)
  expect_equal(one("raw_above_cap_share_of_estimated_rows")$value, 12 / 20)
  expect_equal(one("raw_below_floor_share_of_estimated_rows")$value, 0)
  expect_equal(one("cases_removed_by_cap")$value, 40)
  expect_equal(one("cases_removed_by_cap_share_of_raw_estimate")$value, 0.4)

  # Imputed shares by state: 0.04, 0.02 and 0.05.
  share <- one("state_imputed_share")
  expect_equal(share$value, c(0.02, 0.03, 0.04, 0.045, 0.05))
  expect_equal(
    share$extreme_state,
    c("Alaska (02)", NA, NA, NA, "Arizona (04)")
  )
})

test_that("the compact summary compares each alternative with the default", {
  compact <- .pipeline_summary(summary_detail())
  one <- function(measure, scenario) {
    summary_rows(compact, measure, scenario = scenario)
  }

  # Default: 1570 published + 60 imputed = 1630.
  expect_equal(one("cases_difference_from_default", "cap = Inf")$value, 80)
  expect_equal(
    one("cases_percent_change_from_default", "cap = Inf")$value,
    100 * 80 / 1630
  )
  expect_equal(one("cases_difference_from_default", "method = zero")$value, -60)
  # By state the default totals are 940, 530 and 160.
  largest <- one("largest_state_percent_change_from_default", "cap = Inf")
  expect_equal(largest$value, 100 * 20 / 160)
  expect_equal(largest$extreme_state, "Arizona (04)")
  largest <- one("largest_state_percent_change_from_default", "method = zero")
  expect_equal(largest$value, -100 * 10 / 160)
  expect_equal(largest$extreme_state, "Arizona (04)")
  expect_setequal(
    compact$scenario[compact$step == "sensitivity"],
    c("cap = Inf", "method = zero")
  )
  expect_setequal(compact$scenario[compact$step != "sensitivity"], "default")
})

test_that("a zero or missing denominator gives NA with a note, never Inf or 0", {
  compact <- .pipeline_summary(summary_detail())

  # No county row was allocated, so there is nothing to take a share of.
  fallback <- summary_rows(compact, "county_rows_fallback_share")
  expect_equal(fallback$value, NA_real_)
  expect_match(fallback$note, "Denominator is 0", fixed = TRUE)
  # The made-up detail table has no denominators step.
  expect_equal(sum(compact$step == "denominators"), 0)

  no_cases <- summary_detail()
  alaska <- no_cases$state_fips == "02" & no_cases$measure == "state_cases"
  no_cases$value[alaska] <- 0
  compact <- .pipeline_summary(no_cases)
  series <- summary_rows(
    compact,
    "state_remainder_share_of_state_cases",
    state_fips = "02"
  )
  expect_equal(series$value, NA_real_)
  expect_match(series$note, "Denominator is 0", fixed = TRUE)
  expect_equal(
    summary_rows(compact, "states_without_remainder_share")$value,
    1
  )
  expect_equal(
    summary_rows(compact, "state_remainder_share", "min")$value,
    0.06
  )

  expect_equal(sum(is.infinite(compact$value)), 0)
  expect_equal(sum(is.na(compact$value) & is.na(compact$note)), 0)
})

test_that("the only rows by state are the two series for plots", {
  compact <- .pipeline_summary(summary_detail())
  by_state <- compact[compact$state_fips != "all", ]

  expect_setequal(
    by_state$measure,
    c(
      "state_remainder_share_of_state_cases",
      "state_imputed_share_of_state_cases"
    )
  )
  expect_equal(nrow(by_state), 2 * 3)
  expect_setequal(by_state$state, c("Alabama", "Alaska", "Arizona"))
  expect_equal(
    summary_rows(
      compact,
      "state_remainder_share_of_state_cases",
      state_fips = c("01", "02", "04")
    )$value,
    c(0.06, -0.06, 0.20)
  )
  expect_named(
    compact,
    c(
      "year",
      "indicator",
      "stratify_by",
      "scenario",
      "group",
      "state_fips",
      "state",
      "step",
      "measure",
      "category",
      "unit",
      "basis",
      "value",
      "numerator",
      "denominator",
      "extreme_state",
      "note"
    )
  )
  # Every share says what it divides by what.
  shares <- compact$unit %in% c("proportion", "percent")
  expect_equal(sum(is.na(compact$denominator[shares])), 0)
  expect_equal(sum(is.na(compact$numerator[shares])), 0)
  expect_lte(nrow(compact), summary_row_bound(0, 1, 3))
})

test_that("the compact summary of a pipeline run is traceable and small", {
  run <- report_fixture()
  detail <- .pipeline_report(
    run$county,
    run$imputed,
    run$areas,
    cap = 4,
    floor = 1,
    by_state = TRUE
  )
  detail$scenario <- "default"
  compact <- .pipeline_summary(detail)
  prevalence <- compact[compact$indicator == "HIV prevalence", ]
  one <- function(measure, group = "sum of groups", category = NA) {
    rows <- summary_rows(prevalence, measure, category)
    rows$value[rows$group == group]
  }

  # Two sexes, one run, one state.
  expect_lte(nrow(prevalence), summary_row_bound(2, 1, 1))
  expect_equal(one("remainder_signed"), 939 - 34)
  expect_equal(one("remainder_signed", "Male"), 939)
  expect_equal(one("suppressed_rows_share_of_county_rows"), 5 / 12)
  expect_equal(one("raw_above_cap_share_of_estimated_rows"), 1 / 4)
  expect_equal(one("county_rows_allocated_share"), 9 / 12)
  expect_equal(one("cases_allocated_share_of_county_cases"), 96 / 105)
  expect_equal(
    one("county_rows_fallback_share", category = "total population"),
    1 / 9
  )
  expect_equal(
    one("county_rows_fallback_share", "Female", "any fallback rule"),
    1 / 5
  )
  expect_equal(one("area_rows_cases_exceed_population"), 1)
  expect_equal(
    one("cases_not_allocated", category = "no ACS tracts for this county"),
    9
  )
  expect_equal(one("observed_cases_in_suppressed_rows"), NA_real_)
  # Figures given only for the groups together are not repeated by group.
  expect_equal(
    one("population_relative_difference_acs_vs_cdc", "Male", "max"),
    numeric()
  )
  expect_equal(
    one("population_relative_difference_acs_vs_cdc", category = "max"),
    -596 / 800
  )
})

test_that("data-raw/measured-effects.R runs end to end on made-up data", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  local_fake_sources()
  local_mocked_bindings(.request_atlas = report_fake_cdc)
  out_dir <- withr::local_tempdir()
  step_summary <- withr::local_tempfile()
  withr::local_envvar(
    MEASURED_EFFECTS_SOURCE_ONLY = "true",
    MEASURED_YEARS = "2022,latest",
    MEASURED_STRATIFICATIONS = "none,sex",
    MEASURED_ALTERNATIVES = "zero,cap_inf,floor_1,acs",
    MEASURED_OUT_DIR = out_dir,
    GITHUB_STEP_SUMMARY = step_summary,
    CENSUS_API_KEY = "made-up-key"
  )
  script_env <- new.env(parent = environment())
  source(script, local = script_env)
  log <- utils::capture.output(
    files <- suppressMessages(script_env$measured_effects())
  )

  summary <- utils::read.csv(
    files$detail,
    colClasses = c(state_fips = "character")
  )
  meta <- jsonlite::read_json(files$meta, simplifyVector = TRUE)

  # "latest" steps back from 2025 to the first year with prevalence rows.
  expect_equal(meta$years$latest_used, 2023)
  expect_equal(meta$years$latest_tried, c(2025, 2024, 2023))
  expect_setequal(summary$year, c(2022, 2023))
  expect_setequal(summary$indicator, "HIV prevalence")
  expect_setequal(summary$stratify_by, c("none", "sex"))
  expect_setequal(
    summary$scenario,
    c("default", "method = zero", "cap = Inf", "floor = 1", "population = acs")
  )
  expect_equal(nrow(meta$runs), 4)
  expect_equal(meta$runs$error, rep(NA, 4))
  expect_equal(meta$complete, TRUE)
  expect_match(meta$retrieval, "not yet recorded")
  expect_equal(meta$package$version, as.character(packageVersion("CDCAtlas")))

  # Unstratified 2022 prevalence: county 01003 is suppressed, and capped at 4.
  one <- function(scenario, measure, category = NA) {
    rows <- summary$scenario == scenario &
      summary$year == 2022 &
      summary$stratify_by == "none" &
      summary$state_fips == "all" &
      summary$measure == measure &
      (is.na(category) | summary$category %in% category)
    summary$value[rows]
  }
  expect_equal(one("default", "cases_by_source", "state_rate"), 4)
  expect_equal(one("method = zero", "cases_by_source", "zero"), 0)
  expect_equal(one("cap = Inf", "cases_by_source", "state_rate"), 14.1)
  expect_equal(one("default", "suppressed_rows_raw_above_cap"), 1)
  # Sensitivity runs stop at imputation: no tract steps.
  expect_setequal(
    summary$step[summary$scenario != "default"],
    c("imputation", "no county")
  )
  expect_in(
    c("allocation", "denominators", "geography"),
    summary$step[summary$scenario == "default"]
  )

  # The compact summary is the detail table summarized, and is printed to the
  # log between markers, with the metadata, so it can be read from a job log.
  compact <- utils::read.csv(
    files$csv,
    colClasses = c(state_fips = "character")
  )
  expected <- withr::local_tempfile()
  utils::write.csv(
    .pipeline_summary(summary),
    expected,
    row.names = FALSE,
    na = ""
  )
  expect_equal(readLines(files$csv), readLines(expected))
  begin <- which(log == "----- BEGIN measured-effects.csv -----")
  end <- which(log == "----- END measured-effects.csv -----")
  expect_equal(log[(begin + 1):(end - 1)], readLines(files$csv))
  expect_in(
    c(
      "----- BEGIN measured-effects-meta.json -----",
      "----- END measured-effects-meta.json -----"
    ),
    log
  )
  expect_no_match(log, "made-up-key", fixed = TRUE)
  expect_equal(basename(files$detail), "measured-effects-detail.csv")
  expect_equal(meta$files$committed, "measured-effects.csv")
  expect_equal(meta$quantile_type, 7)
  markdown <- readLines(step_summary)
  expect_match(markdown, "| 2022 | none |", fixed = TRUE, all = FALSE)
  expect_match(markdown, "| 2023 | sex |", fixed = TRUE, all = FALSE)

  # A compact summary that is too large is written but not printed.
  options <- script_env$measured_effects_options()
  options$max_compact_rows <- 10
  expect_error(
    suppressMessages(utils::capture.output(
      script_env$measured_effects(options)
    )),
    "more than 10 rows"
  )
})

test_that("the compact summary for the default scope is at most 3,000 rows", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  local_fake_sources()
  local_mocked_bindings(.request_atlas = report_fake_cdc)
  withr::local_envvar(
    MEASURED_EFFECTS_SOURCE_ONLY = "true",
    MEASURED_YEARS = NA,
    MEASURED_STRATIFICATIONS = NA,
    MEASURED_ALTERNATIVES = NA,
    MEASURED_BY_STATE = NA,
    MEASURED_OUT_DIR = withr::local_tempdir(),
    GITHUB_STEP_SUMMARY = NA
  )
  script_env <- new.env(parent = environment())
  source(script, local = script_env)
  utils::capture.output(
    files <- suppressMessages(script_env$measured_effects())
  )
  compact <- utils::read.csv(
    files$csv,
    colClasses = c(state_fips = "character")
  )

  # The default scope: three years, each unstratified and by sex (2 strata),
  # age (6) and race (7); the made-up data have one state.
  expect_setequal(compact$year, c(2017, 2022, 2023))
  expect_setequal(compact$stratify_by, c("none", "sex", "age", "race"))
  expect_lte(nrow(compact), 3 * summary_row_bound(15, 4, 1))
  # On real data CDC's state list has 57 entries, so at most 57 states.
  expect_equal(nrow(lookup[["state"]]), 57)
  expect_lte(3 * summary_row_bound(15, 4, 57), 3000)
  by_state <- compact[compact$state_fips != "all", ]
  expect_setequal(
    by_state$measure,
    c(
      "state_remainder_share_of_state_cases",
      "state_imputed_share_of_state_cases"
    )
  )
  expect_setequal(by_state$group, c("all", "sum of groups"))
})
