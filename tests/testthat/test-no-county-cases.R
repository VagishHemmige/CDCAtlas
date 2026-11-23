test_that("the remainder is the state total minus published and imputed counts", {
  local_fake_sources(
    state = fake_state_atlas(no_county = 500, suppressed = character(0))
  )
  county <- impute_suppressed(
    fake_county_atlas(suppressed = both_counties),
    cap = Inf
  )
  report <- without_unplaced_warning(no_county_cases(county))

  expect_equal(report$state, c("Alabama", "Alabama"))
  expect_equal(report$state_cases, c(32, 232) + 500)
  expect_equal(report$published_cases, c(0, 0))
  # Imputed: the state rate includes the no-county cases; two counties of
  # 50,000 in a state of 1,000,000.
  expect_equal(report$imputed_cases, (c(32, 232) + 500) / 10)
  expect_equal(
    report$no_county_cases,
    report$state_cases - report$published_cases - report$imputed_cases
  )
  expect_equal(report$no_county_withheld, c(NA_character_, NA_character_))
})

test_that("a state stratum with exactly one county row without a published count has its remainder withheld (#173)", {
  # The state total minus the published counts would be 01003's count plus
  # the 500 cases with no county; with none it would be 01003's count exactly.
  local_fake_sources(
    state = fake_state_atlas(no_county = 500, suppressed = character(0))
  )
  county <- impute_suppressed(
    fake_county_atlas(suppressed = "01003"),
    cap = Inf
  )

  expect_snapshot(report <- no_county_cases(county))
  expect_equal(report$state_cases, c(32, 232) + 500)
  expect_equal(report$imputed_cases, (c(32, 232) + 500) / 20)
  for (column in c("published_cases", "unpublished_cases", "no_county_cases")) {
    expect_equal(report[[column]], c(NA_real_, NA_real_), label = column)
  }
  expect_equal(
    report$no_county_withheld,
    rep("one county row without a published count", 2)
  )
})

test_that("a remainder is withheld only where one county row lacks a published count (#173)", {
  # Diagnoses: 01003 suppressed (one row). Prevalence: none (no row), or both
  # (two rows). Only the first is withheld.
  local_fake_sources()
  one <- fake_county_atlas(suppressed = "01003")
  none <- fake_county_atlas()
  two <- fake_county_atlas(suppressed = both_counties)
  prevalence <- function(x) x[x$indicator == "HIV prevalence", ]
  county <- rbind(
    one[one$indicator == "HIV diagnoses", ],
    prevalence(none)
  )
  county_two <- rbind(one[one$indicator == "HIV diagnoses", ], prevalence(two))

  report <- suppressMessages(no_county_cases(
    impute_suppressed(county, method = "zero")
  ))
  # Zero-filled, both prevalence counties leave every case in no county row.
  report_two <- without_unplaced_warning(suppressMessages(no_county_cases(
    impute_suppressed(county_two, method = "zero")
  )))

  withheld <- "one county row without a published count"
  expect_equal(report$no_county_withheld, c(withheld, NA))
  expect_equal(report$no_county_cases, c(NA, 0))
  expect_equal(report_two$no_county_withheld, c(withheld, NA))
  expect_equal(report_two$no_county_cases, c(NA, 232))
})

test_that("before imputation, one row without a count also withholds the remainder (#173)", {
  local_fake_sources()
  expect_snapshot(
    report <- no_county_cases(fake_county_atlas(suppressed = "01003"))
  )
  expect_equal(report$unpublished_cases, c(NA_real_, NA_real_))
  expect_equal(report$state_cases, c(32, 232))
})

test_that("the report has one row per state, indicator, year and stratum", {
  local_fake_sources(
    state = fake_state_atlas(stratify_by = "race", years = 2021:2022)
  )
  county <- impute_suppressed(
    fake_county_atlas("race", years = 2021:2022),
    method = "zero"
  )
  report <- no_county_cases(county)

  expect_equal(nrow(report), 2 * 2 * 7)
  expect_equal(report$no_county_cases, rep(0, nrow(report)))
  expect_named(
    report,
    c(
      "state_fips",
      "state",
      "indicator",
      "year",
      "race_ethnicity",
      "sex",
      "age",
      "state_cases",
      "published_cases",
      "unpublished_cases",
      "imputed_cases",
      "no_county_cases",
      "no_county_withheld"
    )
  )
  expect_equal(report$no_county_withheld, rep(NA_character_, nrow(report)))
})

test_that("a suppressed state total gives NA", {
  state <- fake_state_atlas()
  state$cases[1] <- NA
  local_fake_sources(state = state)
  report <- no_county_cases(impute_suppressed(fake_county_atlas()))

  expect_equal(report$no_county_cases, c(NA, 0))
})

test_that("each state's remainder uses its own total", {
  two <- two_state_atlas(both_counties)
  local_fake_sources(state = two$state)
  report <- without_unplaced_warning(
    no_county_cases(impute_suppressed(two$county, method = "zero"))
  )

  expect_equal(report$state, rep(c("Alabama", "Alaska"), each = 2))
  expect_equal(report$no_county_cases, c(32, 232, 332, 532))
})

test_that("before imputation, only the published gap is reported", {
  local_fake_sources(
    state = fake_state_atlas(no_county = 500, suppressed = character(0))
  )
  report <- no_county_cases(fake_county_atlas(suppressed = both_counties))

  expect_equal(report$unpublished_cases, c(32, 232) + 500)
  expect_equal(report$imputed_cases, c(NA_real_, NA_real_))
  expect_equal(report$no_county_cases, c(NA_real_, NA_real_))
})

test_that("rows impute_suppressed() couldn't estimate count towards the remainder", {
  county <- fake_county_atlas(suppressed = both_counties)
  county$population <- NA
  local_fake_sources(state = fake_state_atlas(no_county = 500))
  not_estimated <- suppressWarnings(impute_suppressed(county))

  expect_snapshot(report <- no_county_cases(not_estimated))
  expect_equal(report$imputed_cases, c(0, 0))
  expect_equal(report$no_county_cases, c(32, 232) + 500)
})

test_that("no_county_cases() refuses published rows with no count and state data", {
  local_fake_sources()
  county <- impute_suppressed(fake_county_atlas())
  county$cases[1] <- NA
  expect_snapshot(error = TRUE, no_county_cases(county))
  expect_snapshot(error = TRUE, no_county_cases(fake_state_atlas()))
})

test_that("kept 'Data not available' rows count towards the remainder", {
  # 01001 suppressed and 01003 "Data not available": two rows without a
  # count. Dropping 01003 leaves one, so that remainder is withheld (#173).
  county <- fake_county_atlas(suppressed = "01001")
  county$cases[county$fips == "01003"] <- NA
  county$data_status[county$fips == "01003"] <- "Data not available"
  local_fake_sources()

  kept <- without_unplaced_warning(no_county_cases(
    impute_suppressed(county, method = "zero", not_available = "keep")
  ))
  dropped <- suppressMessages(no_county_cases(suppressMessages(
    impute_suppressed(county, method = "zero", not_available = "drop")
  )))

  expect_equal(kept$no_county_cases, c(32, 232))
  expect_equal(kept$no_county_withheld, c(NA_character_, NA_character_))
  expect_equal(dropped$no_county_cases, c(NA_real_, NA_real_))
  expect_equal(
    dropped$no_county_withheld,
    rep("one county row without a published count", 2)
  )
})

test_that("for HIV estimates the report says the remainder includes non-EHE counties (#109)", {
  indicator <- "Estimated HIV incidence"
  local_fake_sources(
    state = fake_state_atlas(no_county = 70, indicators = indicator)
  )
  county <- impute_suppressed(
    fake_county_atlas(indicators = indicator),
    method = "zero"
  )
  expect_message(no_county_cases(county), "EHE")
  expect_snapshot(report <- no_county_cases(county))
  expect_equal(report$no_county_cases, 70)
})

test_that("reported data get no EHE message", {
  local_fake_sources()
  county <- impute_suppressed(fake_county_atlas(), method = "zero")
  expect_no_message(no_county_cases(county))
})

# Two states: Alabama (01001) has a count; Alaska's only county (02003) is
# "Data not available" with no count, so its whole state total is unpublished.
two_states_one_unavailable <- function() {
  county <- fake_county_atlas()
  county$fips[county$fips == "01003"] <- "02003"
  county$cases[county$fips == "02003"] <- NA
  county$data_status[county$fips == "02003"] <- "Data not available"
  alabama <- fake_state_atlas()
  alaska <- alabama
  alaska$fips <- "02"
  alaska$geography <- "Alaska"
  alaska$cases <- c(100, 200)
  list(county = county, state = rbind(alabama, alaska))
}

test_that("a state whose only rows are unavailable keeps its remainder (#110)", {
  data <- two_states_one_unavailable()
  local_fake_sources(state = data$state)

  report <- without_unplaced_warning(
    no_county_cases(impute_suppressed(data$county))
  )
  # Alaska's one county row has no count, so its remainder is withheld
  # (#173), but its rows are still in the report.
  alaska <- report[report$state_fips == "02", ]
  expect_equal(alaska$state_cases, c(100, 200))
  expect_equal(alaska$no_county_cases, c(NA_real_, NA_real_))
  expect_equal(
    alaska$no_county_withheld,
    rep("one county row without a published count", 2)
  )
})

test_that("not_available = 'drop' warns when it removes a whole state's rows (#110)", {
  data <- two_states_one_unavailable()
  local_fake_sources(state = data$state)

  expect_snapshot(
    county <- impute_suppressed(data$county, not_available = "drop")
  )
  expect_equal(unique(county$fips), "01001")
})

test_that("the report is the same with or without cases_uncapped (#151)", {
  local_fake_sources()
  county <- impute_suppressed(fake_county_atlas(suppressed = "01003"))
  expect_contains(names(county), "cases_uncapped")
  # Data imputed by a version without the column.
  older <- county[setdiff(names(county), "cases_uncapped")]

  report <- without_unplaced_warning(no_county_cases(county))
  expect_identical(report, without_unplaced_warning(no_county_cases(older)))
  # The capped counts are the ones counted: 1.6 and 4, not 1.6 and 11.6.
  expect_equal(report$imputed_cases, c(1.6, 4))
})

# States with many cases in no county row (#164) --------------------------------
# placed_atlas() gives one state whose counties hold `placed` of `state_cases`
# cases, so the share with no county is known exactly.

test_that("a state with exactly 25% of its cases in no county row gets no warning (#164)", {
  data <- placed_atlas(75, 100, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state)

  expect_no_warning(report <- no_county_cases(data$county))
  expect_equal(report$no_county_cases / report$state_cases, 0.25)
})

test_that("a state with more than 25% of its cases in no county row gets a warning (#164)", {
  data <- placed_atlas(74, 100, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state)

  expect_warning(
    report <- no_county_cases(data$county),
    class = "cdcatlas_state_unplaced_cases"
  )
  expect_snapshot(report <- no_county_cases(data$county))
  # The report itself is what it would be without the warning.
  expect_equal(report$no_county_cases, 26)
  expect_equal(report$state_cases, 100)
})

test_that("several states with many cases in no county row share one warning (#164)", {
  data <- bind_atlas(
    placed_atlas(13, 100, "02", "Alaska", indicators = "HIV diagnoses"),
    placed_atlas(27, 100, "44", "Rhode Island", indicators = "HIV diagnoses"),
    placed_atlas(90, 100, "01", "Alabama", indicators = "HIV diagnoses")
  )
  local_fake_sources(state = data$state)

  caught <- catch_class(report <- no_county_cases(data$county))
  expect_length(caught, 1)
  expect_match(conditionMessage(caught[[1]]), "Alaska \\(87%\\)")
  expect_match(conditionMessage(caught[[1]]), "Rhode Island \\(73%\\)")
  expect_no_match(conditionMessage(caught[[1]]), "Alabama")
  expect_snapshot(report <- no_county_cases(data$county))
})

test_that("only the states with the largest shares are named, then how many more (#164)", {
  fips <- c("01", "02", "04", "05", "06", "08", "09")
  names <- lookup[["state"]]$name[match(fips, lookup[["state"]]$fips)]
  states <- Map(
    function(fips, name, placed) {
      placed_atlas(placed, 100, fips, name, indicators = "HIV diagnoses")
    },
    fips,
    names,
    c(10, 20, 30, 40, 50, 60, 70)
  )
  data <- do.call(bind_atlas, unname(states))
  local_fake_sources(state = data$state)

  expect_snapshot(report <- no_county_cases(data$county))
})

test_that("a state with no published total, or no cases, is not assessed (#164)", {
  suppressed <- placed_atlas(10, NA, indicators = "HIV diagnoses")
  local_fake_sources(state = suppressed$state)
  expect_no_warning(report <- no_county_cases(suppressed$county))
  expect_equal(report$no_county_cases, NA_real_)

  none <- placed_atlas(0, 0, indicators = "HIV diagnoses")
  local_fake_sources(state = none$state)
  expect_no_warning(report <- no_county_cases(none$county))
  expect_equal(report$no_county_cases, 0)
})

test_that("a negative remainder gives no warning (#164)", {
  data <- placed_atlas(120, 100, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state)

  expect_no_warning(report <- no_county_cases(data$county))
  expect_equal(report$no_county_cases, -20)
})

test_that("a state's strata are assessed together (#164)", {
  # Female: 90 of 100 cases in no county row. Male: none of 900. Together 90
  # of 1,000, or 9%: no warning, though one stratum is far above 25%.
  low <- placed_atlas(0, 0, stratify_by = "sex", indicators = "HIV diagnoses")
  low$county$cases <- ifelse(low$county$sex == "Male", 450, 5)
  low$state$cases <- ifelse(low$state$sex == "Male", 900, 100)
  local_fake_sources(state = low$state)
  expect_no_warning(report <- no_county_cases(low$county))
  expect_equal(report$no_county_cases[order(report$sex)], c(90, 0))

  # Female: 90 of 100. Male: 10 of 100. Together 100 of 200, or 50%: one
  # share for the state, though Male alone is under 25%.
  high <- low
  high$county$cases <- ifelse(high$county$sex == "Male", 45, 5)
  high$state$cases <- 100
  local_fake_sources(state = high$state)
  caught <- catch_class(no_county_cases(high$county))
  expect_length(caught, 1)
  expect_match(conditionMessage(caught[[1]]), "Alabama \\(50%\\)")
})

test_that("strata with no published state total are left out of a state's share (#164)", {
  # Male has no published state total. Female: 40 of 100 in no county row.
  data <- placed_atlas(
    60,
    100,
    stratify_by = "sex",
    indicators = "HIV diagnoses"
  )
  data$state$cases[data$state$sex == "Male"] <- NA
  local_fake_sources(state = data$state)

  caught <- catch_class(report <- no_county_cases(data$county))
  expect_length(caught, 1)
  message <- conditionMessage(caught[[1]])
  expect_match(message, "Alabama \\(40%\\)")
  # 40% is of the Female total only; the state's share over both sexes is
  # unknown, so the warning says what the percentage is of.
  expect_match(
    message,
    "1 state has more than 25% of its published state total in no county row of `x`.",
    fixed = TRUE
  )
  expect_match(
    message,
    "Percentages are of the state totals CDC published for the strata in `x`.",
    fixed = TRUE
  )
  expect_equal(report$no_county_cases[order(report$sex)], c(40, NA))
})

test_that("a state's share uses only the strata in `x` (#164)", {
  # CDC publishes both totals: Female 100, of which the counties hold 60, and
  # Male 100, all of it in the counties. Over both strata 40 of 200, or 20%,
  # is in no county row: no warning.
  data <- placed_atlas(
    60,
    100,
    stratify_by = "sex",
    indicators = "HIV diagnoses"
  )
  data$county$cases[data$county$sex == "Male"] <- 50
  local_fake_sources(state = data$state)
  expect_no_warning(no_county_cases(data$county))

  # With only the Female rows in `x`, the Male total is not in the report, so
  # the share is 40 of the Female 100. The warning says which totals the
  # percentage is of, and limits its consequence to those strata.
  female <- data$county[data$county$sex == "Female", ]
  caught <- catch_class(report <- no_county_cases(female))
  expect_equal(report$state_cases, 100)
  expect_length(caught, 1)
  message <- conditionMessage(caught[[1]])
  expect_match(message, "Alabama \\(40%\\)")
  expect_match(
    message,
    "Percentages are of the state totals CDC published for the strata in `x`.",
    fixed = TRUE
  )
  expect_match(
    message,
    "too low by about that share for those strata.",
    fixed = TRUE
  )
})

test_that("each year and indicator is assessed on its own (#164)", {
  # Diagnoses 2021: 10 of 100 in no county row. Diagnoses 2022: 50 of 100.
  # Prevalence, both years: 10 of 100. Over everything together 80 of 400,
  # 20%, is under the threshold; diagnoses 2022 alone is over it.
  data <- placed_atlas(90, 100, years = 2021:2022)
  odd <- data$county$indicator == "HIV diagnoses" & data$county$year == 2022
  data$county$cases[odd] <- 25
  local_fake_sources(state = data$state)

  caught <- catch_class(report <- no_county_cases(data$county))
  expect_length(caught, 1)
  message <- conditionMessage(caught[[1]])
  expect_match(message, "HIV diagnoses 2022: Alabama \\(50%\\)")
  expect_no_match(message, "2021")
  expect_no_match(message, "prevalence")
  expect_snapshot(report <- no_county_cases(data$county))
})

test_that("only five indicator and year lines are listed, then how many more (#164)", {
  data <- placed_atlas(50, 100, years = 2016:2022, indicators = "HIV diagnoses")
  local_fake_sources(state = data$state)

  caught <- catch_class(report <- no_county_cases(data$county))
  expect_length(caught, 1)
  message <- conditionMessage(caught[[1]])
  expect_match(
    message,
    "^1 state has more than 25% of its published state total"
  )
  expect_match(message, "HIV diagnoses 2020: Alabama \\(50%\\)")
  expect_no_match(message, "HIV diagnoses 2021")
  expect_match(message, "and 2 more combinations of indicator and year")
})

test_that("county data that hasn't been imputed gets no warning about unplaced cases (#164)", {
  # Without imputed counts there is no remainder to assess, only the gap
  # left by published counts.
  data <- placed_atlas(10, 100, indicators = "HIV diagnoses")
  county <- data$county[setdiff(
    names(data$county),
    c("cases_source", "cases_uncapped")
  )]
  local_fake_sources(state = data$state)

  expect_no_warning(report <- no_county_cases(county))
  expect_equal(report$unpublished_cases, 90)
})

test_that("HIV estimates, whose counties are EHE only, are not assessed (#164)", {
  # Their remainder includes counties CDC doesn't estimate, so a large share
  # says nothing about suppression; the EHE message covers them.
  data <- placed_atlas(10, 100, indicators = "Estimated HIV incidence")
  local_fake_sources(state = data$state)

  expect_no_warning(suppressMessages(report <- no_county_cases(data$county)))
  expect_equal(report$no_county_cases, 90)
})
