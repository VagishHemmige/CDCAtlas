# data-raw/measured-effects.R, run end to end on made-up data. The script is
# not part of the built package, so these tests are skipped there. The fake
# CDC is in helper-measured-effects-script.R.

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
  # The compact file is that summary, then the consistency rows: each
  # stratified run against the unstratified run of its year (here, by sex,
  # in 2022 and 2023).
  written <- readLines(files$csv)
  expect_equal(written[seq_along(readLines(expected))], readLines(expected))
  consistency <- compact[compact$step == "consistency", ]
  expect_equal(
    nrow(compact) - nrow(consistency),
    length(readLines(expected)) - 1
  )
  expect_setequal(consistency$stratify_by, "sex")
  expect_setequal(consistency$year, c(2022, 2023))
  expect_equal(sum(summary$step == "consistency"), nrow(consistency))
  # County 01003 is suppressed in both made-up tables, so only 01001 has a
  # published unstratified count, and its strata are published too.
  one <- function(measure, category = NA) {
    rows <- consistency$year == 2022 &
      consistency$measure == measure &
      (is.na(category) | consistency$category %in% category)
    consistency$value[rows]
  }
  expect_equal(one("counties_unstratified_not_published"), 1)
  expect_equal(one("counties_compared"), 1)
  expect_equal(one("counties_by_suppressed_strata", "none"), 1)
  expect_equal(one("ratio_stratified_to_published", "median"), 233 / 116)
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

test_that("'latest' steps back only on evidence that the year is released in part", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  local_fake_sources()
  withr::local_envvar(MEASURED_EFFECTS_SOURCE_ONLY = "true")
  script_env <- new.env(parent = environment())
  source(script, local = script_env)
  latest <- function() {
    suppressMessages(
      script_env$measured_effects_latest_year("hiv", "HIV prevalence")
    )
  }
  state_rows <- function(...) {
    rows <- fake_state_atlas(...)
    function(payload) rows
  }

  # A valid partial release: 2025 and 2024 have diagnoses only; 2023 has both.
  local_mocked_bindings(.request_atlas = report_fake_cdc)
  expect_equal(latest(), list(used = 2023, tried = c(2025, 2024, 2023)))

  # Prevalence rows with no count are not a release either.
  local_mocked_bindings(
    .request_atlas = function(payload) {
      rows <- report_fake_cdc(payload)
      if (!any(rows$year == 2025)) {
        return(rows)
      }
      rows <- fake_state_response(fake_state_atlas(years = 2025), payload)
      rows$cases[rows$indicator == "HIV prevalence"] <- NA
      rows
    }
  )
  expect_equal(latest()$tried, c(2025, 2024, 2023))

  # A response for another disease says nothing about HIV: stop, with the year.
  local_mocked_bindings(
    .request_atlas = state_rows(indicators = "Chlamydia", years = 2025)
  )
  expect_snapshot(latest(), error = TRUE)

  # Rows for another year say nothing about the year asked for.
  local_mocked_bindings(.request_atlas = state_rows(years = 2024))
  expect_snapshot(latest(), error = TRUE)

  # A failed request, "returned no data" included, is not evidence either.
  local_mocked_bindings(
    .request_atlas = function(payload) {
      cli::cli_abort("CDC AtlasPlus returned no data for this request.")
    }
  )
  expect_snapshot(latest(), error = TRUE)
})

test_that("stratified counts are compared with each county's published total", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  withr::local_envvar(MEASURED_EFFECTS_SOURCE_ONLY = "true")
  script_env <- new.env(parent = environment())
  source(script, local = script_env)
  data <- consistency_fixture()
  said <- capture_messages(
    rows <- script_env$measured_effects_consistency(
      data$unstratified,
      data$stratified
    )
  )
  value <- function(measure, category = NA) {
    keep <- rows$measure == measure &
      (if (is.na(category)) TRUE else rows$category %in% category)
    rows$value[keep]
  }

  expect_setequal(rows$step, "consistency")
  expect_setequal(rows$stratify_by, "sex")
  expect_setequal(rows$state_fips, "all")
  expect_setequal(rows$year, 2022)
  expect_in(rows$unit, c("counties", "cases", "proportion"))

  # Which counties are compared.
  expect_equal(value("counties_only_in_unstratified"), 1)
  expect_equal(value("counties_only_in_stratified"), 1)
  expect_equal(value("counties_unstratified_not_published"), 1)
  expect_equal(value("counties_unstratified_published"), 7)
  expect_equal(value("counties_compared"), 5)
  expect_equal(
    value("counties_not_comparable", "exactly one suppressed stratum"),
    1
  )
  expect_equal(
    value("counties_not_comparable", "a stratum has no count"),
    1
  )

  # The premise: how many suppressed stratum cells a county has. Counts only.
  cells <- "counties_by_suppressed_strata"
  expect_equal(value(cells, "none"), 3)
  expect_equal(value(cells, "exactly one"), 1)
  expect_equal(value(cells, "two or more"), 3)
  expect_match(
    rows$note[rows$measure == cells & rows$category == "exactly one"],
    "count only",
    ignore.case = TRUE
  )

  # Published totals 100 + 50 + 10 + 0 + 5; strata 100 + 8 + 9 + 0 + 8.
  expect_equal(value("published_unstratified_cases"), 165)
  expect_equal(value("stratified_cases"), 125)
  expect_equal(value("shortfall_signed"), 40)
  expect_equal(value("shortfall_positive"), 43)
  expect_equal(value("shortfall_negative"), -3)

  # Ratios, where the published total is above 0: 0.16, 0.9, 1 and 1.6.
  expect_equal(value("counties_with_ratio"), 4)
  expect_equal(value("counties_published_zero"), 1)
  ratio <- rows[rows$measure == "ratio_stratified_to_published", ]
  expect_equal(
    ratio$category,
    c("min", "p05", "p25", "median", "p75", "p95", "max")
  )
  expect_equal(ratio$value, c(0.16, 0.271, 0.715, 0.95, 1.15, 1.51, 1.6))
  expect_match(ratio$note, "type 7", fixed = TRUE)

  # Thresholds are strict: a ratio of exactly 0.9 is not below 0.9.
  below <- "counties_ratio_beyond"
  expect_equal(value(below, "below 0.9"), 1)
  expect_equal(value(below, "below 0.75"), 1)
  expect_equal(value(below, "below 0.5"), 1)
  expect_equal(value(below, "above 1.1"), 1)
  expect_equal(value("share_of_counties_ratio_beyond", "below 0.9"), 1 / 4)
  shares <- rows[rows$unit == "proportion", ]
  expect_equal(sum(is.na(shares$denominator)), 0)

  # Counties with at least one suppressed stratum: 01003 and 01019.
  expect_equal(value("counties_compared_with_suppressed_stratum"), 2)
  with <- "counties_ratio_beyond_with_suppressed_stratum"
  expect_equal(value(with, "below 0.9"), 1)
  expect_equal(value(with, "above 1.1"), 1)
  expect_equal(
    value("shortfall_share_in_counties_with_suppressed_stratum"),
    (42 - 3) / 40
  )

  # No county is named anywhere, the exactly-one county least of all.
  text <- c(unlist(lapply(rows, as.character)), said)
  expect_no_match(text, "0100[0-9]|0101[0-9]")
  expect_equal(sum(is.na(rows$value) & is.na(rows$note)), 0)
  expect_lte(nrow(rows), 39)
})

test_that("the consistency check guards its join and its denominators", {
  script <- test_path("..", "..", "data-raw", "measured-effects.R")
  skip_if_not(file.exists(script), "data-raw/ is not in the built package.")
  withr::local_envvar(MEASURED_EFFECTS_SOURCE_ONLY = "true")
  script_env <- new.env(parent = environment())
  source(script, local = script_env)
  data <- consistency_fixture()

  # A county listed twice would be counted twice.
  twice <- rbind(data$unstratified, data$unstratified[1, ])
  expect_error(
    script_env$measured_effects_consistency(twice, data$stratified),
    "more than once"
  )
  twice <- rbind(data$stratified, data$stratified[1, ])
  expect_error(
    script_env$measured_effects_consistency(data$unstratified, twice),
    "more than once"
  )

  # Only the published-zero county: nothing to take a ratio or a share of.
  only <- function(table) table[table$fips == "01017", ]
  rows <- script_env$measured_effects_consistency(
    only(data$unstratified),
    only(data$stratified)
  )
  share <- rows[rows$measure == "share_of_counties_ratio_beyond", ]
  expect_equal(share$value, rep(NA_real_, 4))
  expect_match(share$note, "Denominator is 0", fixed = TRUE)
  in_suppressed <- rows[
    rows$measure == "shortfall_share_in_counties_with_suppressed_stratum",
  ]
  expect_equal(in_suppressed$value, NA_real_)
  expect_equal(sum(rows$measure == "ratio_stratified_to_published"), 0)
  expect_equal(sum(is.infinite(rows$value)), 0)
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
  # Each of the three stratified runs of a year adds at most 39 rows that
  # compare it with the year's unstratified run.
  consistency <- compact$step == "consistency"
  expect_lte(sum(!consistency), 3 * summary_row_bound(15, 4, 1))
  expect_lte(sum(consistency), 3 * 3 * 39)
  expect_setequal(compact$stratify_by[consistency], c("sex", "age", "race"))
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
