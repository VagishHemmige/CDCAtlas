# Tests for the helpers behind extrapolate_to_tract = TRUE. CDC requests are
# replaced with small made-up tables, so these run without the network.

# Census variables -------------------------------------------------------------

# Stands in for tidycensus::load_variables(): every Census variable the lookup
# knows about, with a made-up variable name.
fake_census_variables <- function() {
  vars <- unique(variable_lookup_atlas[c("label", "concept")])
  vars$name <- sprintf("V%03d", seq_len(nrow(vars)))
  vars
}

matched_variables <- function(stratify_by) {
  suppressMessages(nrow(.census_variables_finder(stratify_by, fake_census_variables())))
}

test_that("Census variables are found for each single stratification", {
  expect_equal(matched_variables(NULL), 2)
  expect_equal(matched_variables("sex"), 8)
  expect_equal(matched_variables("race"), 28)
  expect_equal(matched_variables("age"), 84)
})

test_that("Current behaviour: no Census variables match two stratifications", {
  expect_equal(matched_variables(c("sex", "age")), 0)
  expect_equal(matched_variables(c("sex", "race")), 0)
  expect_equal(matched_variables(c("race", "age")), 0)
})

test_that("Current behaviour: the Census lookup has only HIV age groups", {
  expect_setequal(
    unique(variable_lookup_atlas$age),
    c("Ages 13 years and older", "13-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )
})

# Filling in suppressed county counts -----------------------------------------

county_rows <- function(county_fips, county_cases, county_population_atlas) {
  data.frame(
    indicator = "HIV diagnoses",
    year = 2022,
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = "Ages 13 years and older",
    county_fips = county_fips,
    county_cases = county_cases,
    county_population_atlas = county_population_atlas
  )
}

state_rows <- function(fips, cases) {
  data.frame(
    indicator = "HIV diagnoses",
    year = 2022,
    geography = paste("State", fips),
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = "Ages 13 years and older",
    cases = cases,
    fips = fips
  )
}

fill_in <- function(counties, states) {
  local_mocked_bindings(.request_atlas = function(payload) states, .env = parent.frame())
  out <- .fill_in_censored_count(counties, payload_state = list())
  out[order(out$county_fips), ]
}

test_that("suppressed counties share the state remainder by population", {
  counties <- county_rows(
    county_fips = c("01001", "01003", "01005"),
    county_cases = c(40, NA, NA),
    county_population_atlas = c(5000, 1000, 3000)
  )
  out <- fill_in(counties, state_rows("01", 100))

  # 100 state cases - 40 reported = 60, split 1000:3000.
  expect_equal(out$county_cases, c(40, 15, 45))
  expect_equal(sum(out$county_cases), 100)
})

test_that("reported counties are left unchanged", {
  counties <- county_rows(c("01001", "01003"), c(40, 60), c(5000, 1000))
  out <- fill_in(counties, state_rows("01", 100))
  expect_equal(out$county_cases, c(40, 60))
})

test_that("suppressed counties with no population get zero", {
  counties <- county_rows(c("01001", "01003"), c(40, NA), c(5000, 0))
  out <- fill_in(counties, state_rows("01", 100))
  expect_equal(out$county_cases, c(40, 0))
})

test_that("each state is filled in separately", {
  counties <- county_rows(
    county_fips = c("01001", "01003", "02001", "02003"),
    county_cases = c(40, NA, 10, NA),
    county_population_atlas = c(5000, 1000, 2000, 2000)
  )
  out <- fill_in(counties, rbind(state_rows("01", 100), state_rows("02", 30)))
  expect_equal(out$county_cases, c(40, 60, 10, 20))
})

test_that("a suppressed state count leaves suppressed counties missing", {
  counties <- county_rows(c("01001", "01003"), c(40, NA), c(5000, 1000))
  out <- fill_in(counties, state_rows("01", NA))
  expect_equal(out$county_cases, c(40, NA))
})

test_that("Current behaviour: reported counties above the state total give a negative fill-in", {
  counties <- county_rows(c("01001", "01003"), c(120, NA), c(5000, 1000))
  out <- fill_in(counties, state_rows("01", 100))
  expect_equal(out$county_cases, c(120, -20))
})

test_that("fill-in adds state columns and keeps one row per county", {
  counties <- county_rows(c("01001", "01003"), c(40, NA), c(5000, 1000))
  out <- fill_in(counties, state_rows("01", 100))
  expect_equal(nrow(out), 2)
  expect_contains(names(out), c("state_fips", "state_name", "state_cases", "county_cases"))
  expect_equal(intersect(c("is_censored", "censored_pop_total", "county_cases_filled"), names(out)), character(0))
})
