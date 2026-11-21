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
    # extrapolate_from_county() warns about the split tract itself.
    expect_warning(
      areas <- extrapolate_from_county(imputed),
      "split between counties"
    )
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
    c(
      "state_strata_without_total",
      "state_cases",
      "published_cases",
      "imputed_cases",
      "remainder_signed"
    )
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

test_that("a state with no published total is unknown, not zero", {
  # State 02's prevalence total is absent; state 01's is 232.
  run <- report_two_states(function(state) {
    state[!(state$fips == "02" & state$indicator == "HIV prevalence"), ]
  })
  detail <- function(measure, state_fips) {
    run$detail[
      run$detail$measure == measure & run$detail$state_fips == state_fips,
    ]
  }

  for (measure in c(
    "state_cases",
    "published_cases",
    "imputed_cases",
    "remainder_signed"
  )) {
    unknown <- detail(measure, "02")
    expect_equal(unknown$value, NA_real_, label = measure)
    expect_match(unknown$note, "no published state total", label = measure)
  }
  expect_equal(detail("state_strata_without_total", "02")$value, 1)
  expect_equal(detail("state_strata_without_total", "01")$value, 0)
  expect_equal(detail("state_cases", "01")$value, 232)
  # Nationally the one known state is summed, and the gap is noted.
  expect_equal(detail("state_cases", "all")$value, 232)
  expect_match(detail("state_cases", "all")$note, "1 state strat")
  expect_match(detail("remainder_signed", "all")$note, "1 state strat")
  expect_equal(detail("remainder_signed", "all")$value, 232 - 111 - 4)
  expect_equal(detail("state_strata_without_total", "all")$value, 1)

  compact <- function(...) summary_rows(run$compact, ...)
  series <- compact("state_remainder_share_of_state_cases", state_fips = "02")
  expect_equal(series$value, NA_real_)
  expect_match(series$note, "no published state total")
  imputed <- compact("state_imputed_share_of_state_cases", state_fips = "02")
  expect_equal(imputed$value, NA_real_)
  expect_equal(compact("states_without_remainder_share")$value, 1)
  expect_equal(compact("states_without_imputed_share")$value, 1)
  expect_equal(compact("states_remainder_negative")$value, 0)
  expect_equal(compact("states_remainder_share_above_5pct")$value, 1)
  share <- compact("state_remainder_share", c("min", "max"))
  expect_equal(share$value, rep(117 / 232, 2))
  expect_equal(share$extreme_state, rep("Alabama (01)", 2))
  expect_match(share$note, "over 1 states", fixed = TRUE)
})

test_that("no state with a published total leaves every case measure unknown", {
  run <- report_two_states(function(state) {
    state[state$indicator != "HIV prevalence", ]
  })
  national <- run$detail[run$detail$state_fips == "all", ]
  cases <- c(
    "state_cases",
    "published_cases",
    "unpublished_cases",
    "imputed_cases",
    "remainder_signed",
    "remainder_positive",
    "remainder_negative",
    "state_strata_remainder_positive",
    "state_strata_remainder_negative",
    "state_strata_remainder_zero",
    "remainder_signed_share_of_state_cases",
    "remainder_positive_share_of_state_cases",
    "imputed_share_of_state_cases"
  )
  unknown <- national[national$measure %in% cases, ]
  expect_setequal(unknown$measure, cases)
  expect_equal(unknown$value, rep(NA_real_, length(cases)))
  expect_match(unknown$note, "no published state total")
  expect_equal(
    national$value[national$measure == "state_strata_without_total"],
    2
  )

  compact <- run$compact[run$compact$step %in% "no county", ]
  figures <- compact[
    compact$unit %in% c("cases", "proportion") & compact$state_fips == "all",
  ]
  expect_equal(figures$value, rep(NA_real_, nrow(figures)))
  expect_equal(sum(is.na(run$compact$value) & is.na(run$compact$note)), 0)
  expect_equal(
    summary_rows(run$compact, "states_without_remainder_share")$value,
    2
  )
})

test_that("a published state total of zero stays zero", {
  # State 02 publishes 0 cases, though its counties publish 111.
  run <- report_two_states(function(state) {
    state$cases[state$fips == "02" & state$indicator == "HIV prevalence"] <- 0
    state
  })
  detail <- function(measure, state_fips) {
    run$detail[
      run$detail$measure == measure & run$detail$state_fips == state_fips,
    ]
  }

  zero <- detail("state_cases", "02")
  expect_equal(zero$value, 0)
  expect_equal(zero$note, NA_character_)
  expect_equal(detail("state_strata_without_total", "02")$value, 0)
  expect_equal(detail("published_cases", "02")$value, 111)
  expect_equal(detail("imputed_cases", "02")$value, 0)
  expect_equal(detail("remainder_signed", "02")$value, -111)
  expect_equal(detail("state_cases", "all")$value, 232)
  expect_equal(detail("remainder_signed", "all")$value, 117 - 111)

  compact <- function(...) summary_rows(run$compact, ...)
  series <- compact("state_remainder_share_of_state_cases", state_fips = "02")
  expect_equal(series$value, NA_real_)
  expect_match(series$note, "Denominator is 0", fixed = TRUE)
  # Left out of the shares and counted; its negative remainder still counts.
  expect_equal(compact("states_without_remainder_share")$value, 1)
  expect_equal(compact("states_remainder_negative")$value, 1)
  expect_equal(compact("state_remainder_share", "max")$value, 117 / 232)
  expect_equal(compact("state_cases")$value, 232)
})

test_that("the report refuses areas allocated from a different imputation", {
  local_fake_sources(state = report_state_atlas(), acs = report_acs())
  county <- report_county_atlas()
  suppressMessages(suppressWarnings({
    default <- impute_suppressed(county)
    uncapped <- impute_suppressed(county, cap = Inf)
    default_areas <- extrapolate_from_county(default)
    uncapped_areas <- extrapolate_from_county(uncapped)
  }))

  expect_no_error(.pipeline_report(county, default, default_areas))
  expect_no_error(
    .pipeline_report(county, uncapped, uncapped_areas, cap = Inf)
  )
  expect_snapshot(
    .pipeline_report(county, default, uncapped_areas),
    error = TRUE
  )
  expect_snapshot(
    .pipeline_report(county, uncapped, default_areas, cap = Inf),
    error = TRUE
  )
  # Areas for only part of the data are refused too.
  expect_snapshot(
    .pipeline_report(
      county,
      default,
      suppressMessages(suppressWarnings(extrapolate_from_county(
        default[default$indicator == "HIV prevalence", ]
      )))
    ),
    error = TRUE
  )
})

test_that("the report refuses areas kept from before a population changed", {
  local_fake_sources(state = report_state_atlas(), acs = report_acs())
  county <- report_county_atlas()
  suppressMessages(suppressWarnings({
    imputed <- impute_suppressed(county)
    areas <- extrapolate_from_county(imputed)
  }))
  # Only a published county's CDC population changes: every count, and the
  # state remainder, stay the same.
  changed <- county
  row <- changed$fips == "01001" &
    changed$sex == "Male" &
    changed$indicator == "HIV prevalence"
  changed$population[row] <- 60000
  suppressMessages(suppressWarnings(
    changed_imputed <- impute_suppressed(changed)
  ))
  expect_equal(changed_imputed$cases, imputed$cases)

  expect_snapshot(
    .pipeline_report(changed, changed_imputed, areas),
    error = TRUE
  )
})

test_that("the report refuses allocations it can't account for yet", {
  # Unknown race shared across the known groups: the Unknown rows are in
  # neither the areas nor the rows left out.
  county <- county_with_unknown_race()
  local_fake_sources(state = state_with_unknown_race())
  suppressMessages({
    imputed <- impute_suppressed(county)
    reported <- extrapolate_from_county(imputed)
    redistributed <- extrapolate_from_county(
      imputed,
      unknown_race = "redistribute"
    )
  })
  expect_gt(sum(redistributed$county_cases_unknown), 0)
  expect_no_error(.pipeline_report(county, imputed, reported))
  expect_snapshot(
    .pipeline_report(county, imputed, redistributed),
    error = TRUE
  )

  # The remainder spread across counties: it would be counted both as the
  # remainder and as allocated cases.
  data <- two_state_atlas()
  local_fake_sources(state = data$state, acs = data$acs)
  suppressMessages({
    imputed <- impute_suppressed(data$county)
    spread <- extrapolate_from_county(imputed, no_county = "spread")
  })
  expect_gt(sum(spread$county_cases_spread), 0)
  expect_snapshot(
    .pipeline_report(data$county, imputed, spread),
    error = TRUE
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
    MEASURED_PRINT_SUMMARY = NA,
    MEASURED_EFFECTS_SYNTHETIC = NA,
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
  # Uncapped: the state rate (292 per 1,000,000) times 50,000 people.
  expect_equal(one("cap = Inf", "cases_by_source", "state_rate"), 14.6)
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

  # With printing turned off, a run prints no figures and writes no job
  # summary. What it does say is progress, the year found, file names and row
  # counts: no figure, and none of the warnings it recorded.
  unlink(step_summary)
  withr::local_envvar(MEASURED_PRINT_SUMMARY = "false")
  said <- NULL
  quiet <- utils::capture.output(
    said <- capture_messages(script_env$measured_effects())
  )
  expect_equal(quiet, character())
  expect_equal(file.exists(step_summary), FALSE)
  said <- trimws(unlist(strsplit(cli::ansi_strip(said), "\n")))
  said <- said[nzchar(said)]
  allowed <- c(
    "^Latest year with HIV prevalence: [0-9]{4}\\.$",
    "^[0-9TZ:-]+ [0-9]{4} (none|sex): (started|done)\\.$",
    "^v Wrote [0-9]+ rows to$",
    "^v Wrote$",
    "measured-effects(-detail|-meta)?\\.(csv|json)'\\.$"
  )
  expect_equal(said[!grepl(paste(allowed, collapse = "|"), said)], character())
  expect_gt(length(said), 8)
})

test_that("a run that stops early leaves nothing in its output folder", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  local_fake_sources()
  local_mocked_bindings(.request_atlas = report_fake_cdc)
  # A summary left by an earlier run sits in the default folder; this run is
  # given a new folder as its argument, as the workflow does.
  earlier <- withr::local_tempdir()
  stale <- c("measured-effects.csv", "measured-effects-meta.json")
  file.create(file.path(earlier, stale))
  out_dir <- file.path(withr::local_tempdir(), "measured-effects")
  withr::local_envvar(
    MEASURED_EFFECTS_SOURCE_ONLY = "true",
    MEASURED_EFFECTS_SYNTHETIC = NA,
    MEASURED_YEARS = "2022",
    MEASURED_STRATIFICATIONS = "none,county",
    MEASURED_OUT_DIR = earlier,
    GITHUB_STEP_SUMMARY = NA
  )
  script_env <- new.env(parent = environment())
  source(script, local = script_env)
  options <- script_env$measured_effects_options(out_dir)

  expect_equal(options$out_dir, out_dir)
  expect_error(
    suppressMessages(script_env$measured_effects(options)),
    "Unknown stratification"
  )
  expect_equal(list.files(out_dir), character())
  expect_setequal(list.files(earlier), stale)
})

test_that("the script's synthetic mode labels everything and needs no key", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  skip_if_not_installed("pkgload")
  out_dir <- withr::local_tempdir()
  step_summary <- withr::local_tempfile()
  # A subprocess, as the workflow runs it, with no key and no way out to the
  # network: every request would go to a proxy that isn't there.
  withr::local_envvar(
    MEASURED_EFFECTS_SYNTHETIC = "true",
    MEASURED_EFFECTS_SOURCE_ONLY = NA,
    MEASURED_YEARS = "2022,latest",
    MEASURED_STRATIFICATIONS = "none,sex",
    MEASURED_ALTERNATIVES = NA,
    MEASURED_BY_STATE = NA,
    MEASURED_PRINT_SUMMARY = NA,
    MEASURED_OUT_DIR = NA,
    GITHUB_STEP_SUMMARY = step_summary,
    CENSUS_API_KEY = NA,
    http_proxy = "http://127.0.0.1:9",
    https_proxy = "http://127.0.0.1:9",
    NOT_CRAN = "true"
  )
  withr::local_dir(dirname(dirname(normalizePath(script))))
  log <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(file.path("data-raw", "measured-effects.R")), shQuote(out_dir)),
    stdout = TRUE,
    stderr = TRUE
  ))

  expect_null(attr(log, "status"))
  expect_setequal(
    list.files(out_dir),
    c(
      "SYNTHETIC-measured-effects.csv",
      "SYNTHETIC-measured-effects-detail.csv",
      "SYNTHETIC-measured-effects-meta.json"
    )
  )
  for (file in c(
    "SYNTHETIC-measured-effects.csv",
    "SYNTHETIC-measured-effects-detail.csv"
  )) {
    table <- utils::read.csv(file.path(out_dir, file))
    expect_match(unique(table$synthetic), "^SYNTHETIC", label = file)
    expect_equal(names(table)[1], "synthetic", label = file)
  }
  meta <- jsonlite::read_json(
    file.path(out_dir, "SYNTHETIC-measured-effects-meta.json"),
    simplifyVector = TRUE
  )
  expect_equal(meta$synthetic, TRUE)
  expect_match(meta$data, "^SYNTHETIC")
  expect_match(meta$retrieval, "^SYNTHETIC")
  expect_in(
    c(
      "----- BEGIN SYNTHETIC measured-effects.csv -----",
      "----- END SYNTHETIC measured-effects.csv -----",
      "----- BEGIN SYNTHETIC measured-effects-meta.json -----"
    ),
    log
  )
  expect_match(
    log,
    "No request was sent to CDC AtlasPlus or the Census",
    fixed = TRUE,
    all = FALSE
  )
  expect_match(readLines(step_summary)[1], "SYNTHETIC", fixed = TRUE)
})
