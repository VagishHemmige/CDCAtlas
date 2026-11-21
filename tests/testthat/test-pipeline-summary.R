# .pipeline_summary(): the compact summary, derived from the detail table. The
# made-up detail table is summary_detail() in helper-pipeline-summary.R.

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
  # No state is named when the alternative changes nothing.
  same <- summary_detail()
  same$value[same$scenario == "cap = Inf" & same$state_fips != "all"] <-
    c(40, 10, 10)
  unchanged <- summary_rows(
    .pipeline_summary(same),
    "largest_state_percent_change_from_default",
    scenario = "cap = Inf"
  )
  expect_equal(unchanged$value, 0)
  expect_equal(unchanged$extreme_state, NA_character_)
  expect_match(unchanged$note, "No state changes")
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

test_that("a state with no published total is left out across states, not counted as 0", {
  # State 02's prevalence total is absent; state 01's is 232.
  summary <- summary_two_states(function(state) {
    state[!(state$fips == "02" & state$indicator == "HIV prevalence"), ]
  })
  compact <- function(...) summary_rows(summary, ...)
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

test_that("no state with a published total leaves every remainder figure unknown", {
  summary <- summary_two_states(function(state) {
    state[state$indicator != "HIV prevalence", ]
  })
  compact <- summary[summary$step %in% "no county", ]
  figures <- compact[
    compact$unit %in% c("cases", "proportion") & compact$state_fips == "all",
  ]
  expect_equal(figures$value, rep(NA_real_, nrow(figures)))
  expect_equal(sum(is.na(summary$value) & is.na(summary$note)), 0)
  expect_equal(
    summary_rows(summary, "states_without_remainder_share")$value,
    2
  )
})

test_that("a state that publishes zero cases is left out of the shares but still counted", {
  # State 02 publishes 0 cases, though its counties publish 111.
  summary <- summary_two_states(function(state) {
    state$cases[state$fips == "02" & state$indicator == "HIV prevalence"] <- 0
    state
  })
  compact <- function(...) summary_rows(summary, ...)
  series <- compact("state_remainder_share_of_state_cases", state_fips = "02")
  expect_equal(series$value, NA_real_)
  expect_match(series$note, "Denominator is 0", fixed = TRUE)
  # Left out of the shares and counted; its negative remainder still counts.
  expect_equal(compact("states_without_remainder_share")$value, 1)
  expect_equal(compact("states_remainder_negative")$value, 1)
  expect_equal(compact("state_remainder_share", "max")$value, 117 / 232)
  expect_equal(compact("state_cases")$value, 232)
})
