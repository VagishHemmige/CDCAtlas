# Made-up HIV county rows for one county, indicator and year, stratified by
# sex or age: `cases` NA means suppressed unless `status` says otherwise.
fit_rows <- function(
  cases,
  stratify_by = "sex",
  labels = .fit_strata(stratify_by),
  status = NULL,
  fips = "01001",
  indicator = "HIV prevalence",
  year = 2022
) {
  status <- status %||%
    ifelse(is.na(cases), "Data suppressed", "Not Suppressed")
  data.frame(
    indicator = indicator,
    year = year,
    fips = fips,
    data_status = status,
    race_ethnicity = "All races/ethnicities",
    sex = if (stratify_by == "sex") labels else "Both sexes",
    age = if (stratify_by == "age") labels else .fit_universe(),
    cases = cases
  )
}

# The matching unstratified county row.
fit_total <- function(
  cases,
  status = if (is.na(cases)) "Data suppressed" else "Not Suppressed",
  fips = "01001",
  indicator = "HIV prevalence",
  year = 2022,
  age = .fit_universe()
) {
  data.frame(
    indicator = indicator,
    year = year,
    fips = fips,
    data_status = status,
    race_ethnicity = "All races/ethnicities",
    sex = "Both sexes",
    age = age,
    cases = cases
  )
}

test_that("the requested HIV strata are CDC's labels", {
  expect_equal(.fit_strata("sex"), c("Male", "Female"))
  expect_equal(
    .fit_strata("age"),
    c("13-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )
  expect_equal(.fit_universe(), "Ages 13 years and older")
})

test_that("suppressed cells share the residual in proportion to their seeds", {
  x <- fit_rows(c(NA, NA))
  out <- .fit_county_total(x, c(2, 6), fit_total(10), "sex")
  expect_equal(out$cases, c(2.5, 7.5))
  expect_equal(out$fit_reason, rep("fitted to county total", 2))

  x <- fit_rows(c(10, NA, 20, NA, 30, 40), "age")
  seed <- c(NA, 1, NA, 3, NA, NA)
  out <- .fit_county_total(x, seed, fit_total(108), "age")
  expect_equal(out$cases, c(NA, 2, NA, 6, NA, NA))
  expect_equal(sum(x$cases, out$cases, na.rm = TRUE), 108)
  expect_equal(
    out$fit_reason,
    c(NA, "fitted to county total", NA, "fitted to county total", NA, NA)
  )
})

test_that("a small positive residual is fitted, with fractional counts", {
  x <- fit_rows(c(NA, NA))
  out <- .fit_county_total(x, c(3, 3), fit_total(1), "sex")
  expect_equal(out$cases, c(0.5, 0.5))
})

test_that("published rows get no reason, and nothing to fit gives nothing", {
  x <- fit_rows(c(4, 6))
  out <- .fit_county_total(x, c(NA, NA), fit_total(10), "sex")
  expect_equal(out$cases, c(NA_real_, NA_real_))
  expect_equal(out$fit_reason, c(NA_character_, NA_character_))
})

test_that("one suppressed cell is never fitted", {
  x <- fit_rows(c(10, 20, NA, 40, 50, 60), "age")
  out <- .fit_county_total(x, c(NA, NA, 5, NA, NA, NA), fit_total(185), "age")
  expect_equal(out$cases, rep(NA_real_, 6))
  expect_equal(out$fit_reason[3], "one suppressed cell")
})

test_that("fewer than two positive seeds is not fitted", {
  x <- fit_rows(c(NA, NA))
  for (seed in list(c(0, 0), c(0, 5))) {
    out <- .fit_county_total(x, seed, fit_total(10), "sex")
    expect_equal(out$cases, c(NA_real_, NA_real_))
    expect_equal(out$fit_reason, rep("fewer than two positive seeds", 2))
  }
})

test_that("a missing seed stops the fit, even with two positive seeds", {
  x <- fit_rows(c(10, NA, NA, NA, 30, 40), "age")
  out <- .fit_county_total(x, c(NA, 2, NA, 3, NA, NA), fit_total(100), "age")
  expect_equal(out$cases, rep(NA_real_, 6))
  expect_equal(out$fit_reason[2:4], rep("a suppressed cell has no seed", 3))
})

test_that("a zero or negative residual is not fitted, with one reason", {
  x <- fit_rows(c(10, NA, NA, 30, 40, 50), "age")
  seed <- c(NA, 2, 3, NA, NA, NA)
  out <- .fit_county_total(x, seed, fit_total(130), "age")
  expect_equal(out$cases, rep(NA_real_, 6))
  expect_equal(
    out$fit_reason[2:3],
    rep("published total not usable for fitting", 2)
  )
  expect_no_warning(.fit_county_total(x, seed, fit_total(130), "age"))

  expect_warning(
    out <- .fit_county_total(x, seed, fit_total(120), "age"),
    class = "cdcatlas_negative_county_residual"
  )
  expect_equal(
    out$fit_reason[2:3],
    rep("published total not usable for fitting", 2)
  )
})

test_that("the negative-residual warning gives counts only", {
  x <- rbind(
    fit_rows(c(10, NA, NA, 30, 40, 50), "age"),
    fit_rows(c(10, NA, NA, 30, 40, 50), "age", fips = "01003")
  )
  totals <- rbind(fit_total(120), fit_total(100, fips = "01003"))
  seed <- rep(c(NA, 2, 3, NA, NA, NA), 2)
  expect_snapshot(out <- .fit_county_total(x, seed, totals, "age"))
})

test_that("a missing, unavailable or duplicated county total is not usable", {
  x <- fit_rows(c(NA, NA))
  seed <- c(2, 6)
  not_available <- "county total not available"
  totals <- list(
    fit_total(10, fips = "01003"),
    fit_total(NA, status = "Data not available"),
    fit_total(NA, status = "Not Suppressed"),
    fit_total(10, status = "Data suppressed"),
    rbind(fit_total(10), fit_total(10)),
    fit_total(10, age = "All age groups")
  )
  for (total in totals) {
    out <- .fit_county_total(x, seed, total, "sex")
    expect_equal(out$cases, c(NA_real_, NA_real_))
    expect_equal(out$fit_reason, rep(not_available, 2))
  }
})

test_that("a suppressed county total keeps the fallback", {
  x <- fit_rows(c(NA, NA))
  out <- .fit_county_total(x, c(2, 6), fit_total(NA), "sex")
  expect_equal(out$cases, c(NA_real_, NA_real_))
  expect_equal(out$fit_reason, rep("county total suppressed", 2))
})

test_that("incomplete strata are not fitted, even with two positive seeds", {
  # A filtered age download: two suppressed rows and one age band missing.
  x <- fit_rows(c(10, NA, NA, 40, 50), "age", labels = .fit_strata("age")[-6])
  out <- .fit_county_total(x, c(NA, 2, 3, NA, NA), fit_total(150), "age")
  expect_equal(out$cases, rep(NA_real_, 5))
  expect_equal(
    out$fit_reason[2:3],
    rep("strata incomplete or inconsistent", 2)
  )

  # A label that isn't one of the requested strata.
  x <- fit_rows(c(NA, NA), labels = c("Male", "Unknown"))
  out <- .fit_county_total(x, c(2, 6), fit_total(10), "sex")
  expect_equal(out$fit_reason, rep("strata incomplete or inconsistent", 2))

  # Sex strata outside HIV's age universe.
  x <- fit_rows(c(NA, NA))
  x$age <- "All age groups"
  out <- .fit_county_total(x, c(2, 6), fit_total(10), "sex")
  expect_equal(out$fit_reason, rep("strata incomplete or inconsistent", 2))
})

test_that("contradictory or unexplained cells make the strata inconsistent", {
  # A count marked suppressed.
  x <- fit_rows(
    c(10, NA, NA, 30, 40, 50),
    "age",
    status = c("Data suppressed", rep("Data suppressed", 2), rep("x", 3))
  )
  out <- .fit_county_total(x, c(NA, 2, 3, NA, NA, NA), fit_total(150), "age")
  expect_equal(out$fit_reason[2:3], rep("strata incomplete or inconsistent", 2))

  # A cell with no count that isn't marked suppressed.
  x <- fit_rows(
    c(10, NA, NA, NA, 40, 50),
    "age",
    status = c("x", "Data suppressed", "Data suppressed", "Data not available",
               "x", "x")
  )
  out <- .fit_county_total(x, c(NA, 2, 3, NA, NA, NA), fit_total(150), "age")
  expect_equal(out$cases, rep(NA_real_, 6))
  expect_equal(
    out$fit_reason[2:4],
    rep("strata incomplete or inconsistent", 3)
  )
})

test_that("structural reasons win over the residual", {
  # Total suppressed and one suppressed cell: the total is reported.
  x <- fit_rows(c(10, NA))
  out <- .fit_county_total(x, c(NA, 5), fit_total(NA), "sex")
  expect_equal(out$fit_reason[2], "county total suppressed")

  # Incomplete strata and a negative residual: no residual warning.
  x <- fit_rows(c(10, NA, NA, 40, 50), "age", labels = .fit_strata("age")[-6])
  expect_no_warning(
    out <- .fit_county_total(x, c(NA, 2, 3, NA, NA), fit_total(50), "age")
  )
  expect_equal(out$fit_reason[2], "strata incomplete or inconsistent")

  # One suppressed cell and a zero residual.
  x <- fit_rows(c(10, NA))
  out <- .fit_county_total(x, c(NA, 5), fit_total(10), "sex")
  expect_equal(out$fit_reason[2], "one suppressed cell")
})

test_that("race is never fitted and needs no totals", {
  x <- fit_rows(c(NA, NA))
  x$race_ethnicity <- c("White", "Asian")
  x$sex <- "Both sexes"
  out <- .fit_county_total(x, c(2, 6), fit_total(0)[0, ], "race")
  expect_equal(out$cases, c(NA_real_, NA_real_))
  expect_equal(out$fit_reason, rep("race not fitted", 2))
})

test_that("groups are fitted on their own, in the input's row order", {
  a <- fit_rows(c(NA, NA))
  b <- fit_rows(c(NA, NA), indicator = "HIV diagnoses")
  c <- fit_rows(c(NA, NA), year = 2017)
  d <- fit_rows(c(NA, NA), fips = "01003")
  x <- rbind(a, b, c, d)[c(1, 3, 5, 7, 2, 4, 6, 8), ]
  seed <- c(1, 1, 1, 1, 3, 3, 3, 3)
  totals <- rbind(
    fit_total(8),
    fit_total(4, indicator = "HIV diagnoses"),
    fit_total(NA, year = 2017),
    # A county only in the totals is ignored.
    fit_total(100, fips = "01005")
  )
  out <- .fit_county_total(x, seed, totals, "sex")
  expect_equal(out$cases, c(2, 1, NA, NA, 6, 3, NA, NA))
  expect_equal(
    out$fit_reason,
    rep(
      c(
        "fitted to county total",
        "fitted to county total",
        "county total suppressed",
        "county total not available"
      ),
      2
    )
  )
})

test_that("a county with every cell published and a suppressed total gets no reason", {
  x <- fit_rows(c(4, 6))
  out <- .fit_county_total(x, c(NA, NA), fit_total(NA), "sex")
  expect_equal(out$fit_reason, c(NA_character_, NA_character_))
})
