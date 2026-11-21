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
