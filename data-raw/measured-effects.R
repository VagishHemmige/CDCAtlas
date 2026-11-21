# Builds the measured-effects summary: what each pipeline step does to the
# numbers, on real CDC AtlasPlus and Census data (#153). The website articles
# read the summary; they do not compute it.
#
# For each year and stratification it runs the default pipeline
#
#   get_atlas() -> impute_suppressed() -> extrapolate_from_county()
#
# keeps one indicator, and counts what each step did with .pipeline_report()
# (R/pipeline-report.R). It then imputes again under each alternative
# (`method = "zero"`, `cap = Inf`, `floor = 1`, `population = "acs"`) and
# reports the imputation and remainder steps for it. The alternatives stop at
# imputation: extrapolation is not rerun for them, to keep the run time down,
# so the summary has no tract-level results for the alternatives.
#
# It writes three files to the output folder:
#
#   measured-effects.csv         the compact summary: the figures the articles
#                                plot or quote (.pipeline_summary()), at most
#                                3,000 rows. The only data file meant to be
#                                committed.
#   measured-effects-detail.csv  every measure, by group, state and scenario
#                                (.pipeline_report() with a `scenario`
#                                column): tens of thousands of rows. Kept as a
#                                workflow artifact only; .gitignore lists it
#                                so it cannot be committed by accident.
#   measured-effects-meta.json   when and how the summary was made
#
# In both CSV files a missing value is an empty field: read them with
# `na.strings = ""`.
#
# Every figure in the compact summary is computed from rows of the detail
# table by .pipeline_summary(). If the compact summary has more than 3,000
# rows the files are still written, but nothing is printed and the run stops:
# the summary is then the wrong shape to commit.
#
# Real data are not printed unless asked. On a public repository a job log,
# a job summary and an artifact are public, so whether figures from a real
# run are shown anywhere is the maintainer's decision. By default a real run
# only writes its files, and says only its progress, the year it found for
# "latest", file names and numbers of rows; the warnings it records go into
# the metadata file, not the log. (The workflow uploads none of the files
# unless asked.) With MEASURED_PRINT_SUMMARY=true it also prints the
# compact summary and the metadata between marker lines and, on GitHub, adds a
# short table to the job summary.
#
# Synthetic mode (MEASURED_EFFECTS_SYNTHETIC=true) runs the same code on the
# made-up CDC and Census data of the offline tests
# (tests/testthat/helper-pipeline.R and helper-pipeline-report.R, sourced from
# the repository; needs testthat). It sends no request to CDC AtlasPlus or the
# Census, needs no Census API key and does not read one. Everything it
# produces says SYNTHETIC: the file names, a first column in both CSV files,
# the metadata, the marker lines and the job summary. It always prints,
# because made-up numbers disclose nothing. Pull requests run this mode, to
# exercise the whole path without real data.
#
# Run from the package root, with a Census API key in CENSUS_API_KEY:
#
#   Rscript data-raw/measured-effects.R [output folder]
#
# A full default run makes about 56 CDC requests and one national tract
# download per year and stratification (twice with the "acs" alternative), so
# allow one to two hours. `.github/workflows/measured-effects.yaml` can run it
# on GitHub by hand; it uploads a file only when asked to, and the compact
# summary and the metadata are committed by pull request.
#
# Settings, as environment variables (defaults in brackets):
#
#   MEASURED_DISEASE          get_atlas() disease ["hiv"]
#   MEASURED_INDICATOR        the one indicator to report ["HIV prevalence"]
#   MEASURED_YEARS            comma-separated years; "latest" is the newest
#                             year CDC has released for the indicator
#                             ["2017,2022,latest"]
#   MEASURED_STRATIFICATIONS  comma-separated: none, sex, age, race
#                             ["none,sex,age,race"]
#   MEASURED_ALTERNATIVES     comma-separated: zero, cap_inf, floor_1, acs;
#                             "none" for no alternatives
#                             ["zero,cap_inf,floor_1,acs"]
#   MEASURED_BY_STATE         also report each state in the detail table, and
#                             the figures across states in the summary?
#                             ["true"]
#   MEASURED_OUT_DIR          output folder, unless given as an argument
#                             ["vignettes/articles/data"]
#   MEASURED_PRINT_SUMMARY    print a real run's compact summary and metadata,
#                             and write the job summary? ["false"]
#   MEASURED_EFFECTS_SYNTHETIC  run on made-up data only? ["false"]
#
# "latest" starts at the latest year in the package's own code table and
# steps back, at most two years, until CDC returns state rows with counts for
# the indicator. The year used and the years tried are recorded.
#
# Saved CDC responses are not used: the run keeps its cache in a new temporary
# folder, so every response is downloaded during the run, once, and the county
# data and state totals of a run come from the same downloads. The package
# does not yet record a download time for each response (#150), so the run's
# start and end times are the retrieval interval.
#
# One indicator is reported at a time, so diagnoses and prevalence are never
# added together. Nothing here estimates a suppressed count.
#
# tests/testthat/test-pipeline-report.R runs this script end to end on
# made-up data (MEASURED_EFFECTS_SOURCE_ONLY=true defines the functions
# without running them). That shows it executes, not that its numbers are
# right on real data.

measured_effects_options <- function(args = character()) {
  setting <- function(name, default) {
    value <- Sys.getenv(name, "")
    if (nzchar(value)) value else default
  }
  list_of <- function(value) {
    trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  }
  out_dir <- if (length(args) > 0 && nzchar(args[[1]])) {
    args[[1]]
  } else {
    setting("MEASURED_OUT_DIR", file.path("vignettes", "articles", "data"))
  }
  alternatives <- list_of(
    setting("MEASURED_ALTERNATIVES", "zero,cap_inf,floor_1,acs")
  )
  list(
    disease = setting("MEASURED_DISEASE", "hiv"),
    indicator = setting("MEASURED_INDICATOR", "HIV prevalence"),
    years = list_of(setting("MEASURED_YEARS", "2017,2022,latest")),
    stratifications = list_of(
      setting("MEASURED_STRATIFICATIONS", "none,sex,age,race")
    ),
    alternatives = setdiff(alternatives, "none"),
    by_state = identical(tolower(setting("MEASURED_BY_STATE", "true")), "true"),
    out_dir = out_dir,
    print_summary = identical(
      tolower(setting("MEASURED_PRINT_SUMMARY", "false")),
      "true"
    ),
    synthetic = identical(
      tolower(setting("MEASURED_EFFECTS_SYNTHETIC", "false")),
      "true"
    ),
    max_compact_rows = 3000
  )
}

# The alternatives to the default imputation: the impute_suppressed()
# arguments that differ, and the limits .pipeline_report() needs to know.
measured_effects_alternatives <- list(
  zero = list(label = "method = zero", args = list(method = "zero")),
  cap_inf = list(label = "cap = Inf", args = list(cap = Inf)),
  floor_1 = list(label = "floor = 1", args = list(floor = 1)),
  acs = list(label = "population = acs", args = list(population = "acs"))
)

measured_effects <- function(options = measured_effects_options()) {
  pkg <- asNamespace("CDCAtlas")
  started <- Sys.time()
  utc <- function(time) format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  synthetic <- isTRUE(options$synthetic)
  synthetic_label <- "SYNTHETIC: made-up data, not CDC's or the Census's"
  # Text kept in the metadata never includes the Census API key. Synthetic
  # mode does not read the key at all.
  key <- if (synthetic) "" else Sys.getenv("CENSUS_API_KEY")
  clean <- function(text) {
    text <- cli::ansi_strip(text)
    if (nzchar(key)) {
      text <- gsub(key, "<CENSUS_API_KEY>", text, fixed = TRUE)
    }
    text
  }

  known <- pkg$.disease_indicators()
  indicators <- known$indicator[known$disease == options$disease]
  if (!options$indicator %in% indicators) {
    cli::cli_abort(c(
      "{.val {options$indicator}} is not an indicator of
       {.val {options$disease}}.",
      "i" = "Its indicators are {.val {indicators}}."
    ))
  }
  unknown <- setdiff(options$alternatives, names(measured_effects_alternatives))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown alternative{?s}: {.val {unknown}}.")
  }
  bad <- setdiff(options$stratifications, c("none", "sex", "age", "race"))
  if (length(bad) > 0) {
    cli::cli_abort("Unknown stratification{?s}: {.val {bad}}.")
  }
  if (!synthetic) {
    pkg$.check_census_ready(rlang::current_env())
  }

  # A new, empty cache for this run only (see the top of this file).
  cache <- tempfile("measured-effects-cache-")
  old_cache <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = cache)
  on.exit(
    {
      if (is.na(old_cache)) {
        Sys.unsetenv("R_USER_CACHE_DIR")
      } else {
        Sys.setenv(R_USER_CACHE_DIR = old_cache)
      }
      unlink(cache, recursive = TRUE)
    },
    add = TRUE
  )

  latest <- NULL
  if ("latest" %in% options$years) {
    latest <- measured_effects_latest_year(options$disease, options$indicator)
    cli::cli_inform("Latest year with {options$indicator}: {latest$used}.")
  }
  years <- sort(unique(as.numeric(c(
    setdiff(options$years, "latest"),
    latest$used
  ))))
  if (anyNA(years)) {
    cli::cli_abort("{.envvar MEASURED_YEARS} must be years or {.val latest}.")
  }

  results <- list()
  runs <- list()
  for (year in years) {
    for (stratification in options$stratifications) {
      label <- paste(year, stratification)
      cli::cli_inform("{utc(Sys.time())} {label}: started.")
      run_started <- Sys.time()
      warnings <- character()
      acs_years <- NULL
      result <- tryCatch(
        withCallingHandlers(
          suppressMessages(measured_effects_run(
            options,
            year,
            if (stratification == "none") NULL else stratification
          )),
          warning = function(w) {
            warnings <<- c(warnings, clean(conditionMessage(w)))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) e
      )
      failed <- inherits(result, "error")
      if (!failed) {
        acs_years <- result$acs_years
        results[[label]] <- result$report
      }
      runs[[label]] <- tibble::tibble(
        year = year,
        stratify_by = stratification,
        started_utc = utc(run_started),
        finished_utc = utc(Sys.time()),
        acs_years = paste(acs_years, collapse = ","),
        rows = if (failed) 0L else nrow(result$report),
        warnings = list(unique(warnings)),
        error = if (failed) clean(conditionMessage(result)) else NA_character_
      )
      cli::cli_inform(
        "{utc(Sys.time())} {label}: {if (failed) 'FAILED' else 'done'}."
      )
    }
  }

  detail <- dplyr::bind_rows(results)
  compact <- if (nrow(detail) > 0) pkg$.pipeline_summary(detail)
  runs <- dplyr::bind_rows(runs)
  complete <- all(is.na(runs$error))
  finished <- Sys.time()

  package_versions <- function(packages) {
    installed <- packages[vapply(
      packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )]
    lapply(rlang::set_names(installed), \(p) as.character(packageVersion(p)))
  }
  defaults <- function(fn) {
    lapply(formals(fn)[-1], function(default) {
      value <- eval(default)
      if (is.null(value)) "NULL" else as.character(value[[1]])
    })
  }
  meta <- list(
    files = list(
      committed = "measured-effects.csv",
      artifact_only = "measured-effects-detail.csv",
      note = paste(
        "measured-effects.csv is the compact summary: every figure in it is",
        "computed from rows of measured-effects-detail.csv, which is kept as",
        "a workflow artifact and is not committed."
      )
    ),
    rows = list(compact = NROW(compact), detail = nrow(detail)),
    quantile_type = pkg$.summary_quantile_type,
    complete = complete,
    run = list(started_utc = utc(started), finished_utc = utc(finished)),
    synthetic = synthetic,
    data = if (synthetic) {
      synthetic_label
    } else {
      "CDC AtlasPlus and Census ACS data, downloaded during the run"
    },
    retrieval = if (synthetic) {
      paste(
        "SYNTHETIC run: nothing was retrieved. No request was sent to CDC",
        "AtlasPlus or the Census; every response was made up."
      )
    } else {
      paste(
        "Per-response download times are not yet recorded by the package, so",
        "the run window (run.started_utc to run.finished_utc) is the interval",
        "in which every CDC AtlasPlus and Census response was retrieved. Saved",
        "responses were not used: the run kept its cache in a new temporary",
        "folder, so each response was downloaded once, during the run."
      )
    },
    package = list(
      name = "CDCAtlas",
      version = as.character(packageVersion("CDCAtlas")),
      git_commit = measured_effects_commit()
    ),
    request = list(
      disease = options$disease,
      indicator = options$indicator,
      years = I(options$years),
      stratifications = I(options$stratifications),
      alternatives = I(vapply(
        measured_effects_alternatives[options$alternatives],
        \(alternative) alternative$label,
        character(1),
        USE.NAMES = FALSE
      )),
      by_state = options$by_state
    ),
    years = list(
      used = I(years),
      latest_in_code_table = pkg$.latest_atlas_year(),
      latest_used = latest$used,
      latest_tried = if (!is.null(latest)) I(latest$tried)
    ),
    scope = paste(
      "Scenario 'default' is the default pipeline, every option at its",
      "default. The other scenarios change one impute_suppressed() argument",
      "and report the imputation and remainder steps only: extrapolation was",
      "not rerun for them."
    ),
    options = list(
      impute_suppressed = defaults(pkg$impute_suppressed),
      extrapolate_from_county = defaults(pkg$extrapolate_from_county)
    ),
    runs = runs,
    r = list(version = R.version.string, platform = R.version$platform),
    packages = package_versions(c(
      "tidycensus",
      "dplyr",
      "httr",
      "jsonlite",
      "memoise",
      "cachem",
      "cli",
      "rlang"
    ))
  )

  dir.create(options$out_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- if (synthetic) "SYNTHETIC-" else ""
  files <- list(
    csv = file.path(options$out_dir, paste0(prefix, "measured-effects.csv")),
    detail = file.path(
      options$out_dir,
      paste0(prefix, "measured-effects-detail.csv")
    ),
    meta = file.path(
      options$out_dir,
      paste0(prefix, "measured-effects-meta.json")
    ),
    complete = complete
  )
  # In synthetic mode every row of both tables says so, in a first column.
  labelled <- function(table) {
    table <- if (is.null(table)) data.frame() else as.data.frame(table)
    if (synthetic) {
      table <- cbind(synthetic = rep(synthetic_label, nrow(table)), table)
    }
    table
  }
  utils::write.csv(labelled(detail), files$detail, row.names = FALSE, na = "")
  utils::write.csv(labelled(compact), files$csv, row.names = FALSE, na = "")
  jsonlite::write_json(
    meta,
    files$meta,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  cli::cli_inform(c(
    "v" = "Wrote {NROW(compact)} row{?s} to {.file {files$csv}}.",
    "v" = "Wrote {nrow(detail)} row{?s} to {.file {files$detail}}.",
    "v" = "Wrote {.file {files$meta}}.",
    "!" = if (!complete) {
      "{sum(!is.na(runs$error))} run{?s} failed; see {.field runs} in the
       metadata."
    }
  ))
  if (NROW(compact) > options$max_compact_rows) {
    cli::cli_abort(c(
      "The compact summary has more than {options$max_compact_rows} rows
       ({nrow(compact)}), so it is not printed.",
      "i" = "It is meant to hold only figures to plot or quote. The files
             were written; look at {.file {files$csv}}."
    ))
  }
  # Figures from real data are shown only when asked (see the top of this
  # file); made-up figures always are.
  if (!synthetic && !isTRUE(options$print_summary)) {
    return(invisible(files))
  }
  measured_effects_print(files$csv, clean, synthetic)
  measured_effects_print(files$meta, clean, synthetic)
  step_summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(step_summary) && !is.null(compact)) {
    cat(
      measured_effects_markdown(compact, meta),
      file = step_summary,
      sep = "\n",
      append = TRUE
    )
  }
  invisible(files)
}

# Synthetic mode: the same run with CDC and the Census replaced by the made-up
# data of the offline tests. The test helpers are sourced from the repository
# (they are not part of the script, so the made-up data live in one place),
# and the package's own request functions are replaced, so nothing here can
# reach CDC AtlasPlus or the Census: a real request function would stop.
measured_effects_synthetic <- function(options) {
  rlang::check_installed("testthat", reason = "for synthetic mode.")
  helper_files <- file.path(
    "tests",
    "testthat",
    c("helper-pipeline.R", "helper-pipeline-report.R")
  )
  if (!all(file.exists(helper_files))) {
    cli::cli_abort(c(
      "Synthetic mode needs {.file {helper_files}}.",
      "i" = "Run the script from the package root of a repository checkout."
    ))
  }
  # The helpers call testthat's mocking functions and the package's internals.
  helpers <- new.env(parent = asNamespace("CDCAtlas"))
  for (name in c("local_mocked_bindings", "test_path")) {
    assign(name, getExportedValue("testthat", name), envir = helpers)
  }
  for (file in helper_files) {
    sys.source(file, envir = helpers)
  }

  answered <- 0
  refuse <- function(...) {
    stop("Synthetic mode must not send a request to CDC AtlasPlus.")
  }
  helpers$local_fake_sources(env = environment())
  testthat::local_mocked_bindings(
    .request_atlas = function(payload) {
      answered <<- answered + 1
      helpers$report_fake_cdc(payload)
    },
    .request_atlas_internal = refuse,
    .request_atlas_download = refuse,
    .atlas_get = refuse,
    .package = "CDCAtlas"
  )
  options$synthetic <- TRUE
  cli::cli_inform(c(
    "!" = "SYNTHETIC mode: made-up CDC and Census data. No figure below
           describes real data."
  ))
  files <- measured_effects(options)
  cli::cli_inform(c(
    "v" = "SYNTHETIC run finished. No request was sent to CDC AtlasPlus or
           the Census: {answered} CDC request{?s} and every Census request
           were answered with made-up data."
  ))
  files
}

# Print a file between marker lines, so it can be copied out of a job log.
# `clean` removes the Census API key, should it ever appear.
measured_effects_print <- function(path, clean, synthetic = FALSE) {
  name <- sub("^SYNTHETIC-", "", basename(path))
  if (synthetic) {
    name <- paste("SYNTHETIC", name)
  }
  writeLines(c(
    paste("----- BEGIN", name, "-----"),
    clean(readLines(path, warn = FALSE)),
    paste("----- END", name, "-----")
  ))
}

# A short Markdown table of the national remainder and imputation figures for
# each year and stratification, groups together, for the job summary.
measured_effects_markdown <- function(compact, meta) {
  national <- compact[
    compact$scenario == "default" &
      compact$state_fips == "all" &
      compact$group %in% c("all", "sum of groups"),
  ]
  figures <- c(
    "Remainder, signed" = "remainder_signed",
    "Positive only" = "remainder_positive",
    "Negative only" = "remainder_negative",
    "Signed, % of state cases" = "remainder_signed_share_of_state_cases",
    "County rows suppressed, %" = "suppressed_rows_share_of_county_rows",
    "Imputed, % of state cases" = "imputed_share_of_state_cases",
    "Estimates above the cap, %" = "raw_above_cap_share_of_estimated_rows"
  )
  runs <- unique(national[c("year", "stratify_by")])
  lines <- vapply(
    seq_len(nrow(runs)),
    function(i) {
      run <- national[
        national$year == runs$year[i] &
          national$stratify_by == runs$stratify_by[i],
      ]
      cells <- vapply(
        figures,
        function(measure) {
          row <- run[run$measure == measure, ]
          if (nrow(row) != 1 || is.na(row$value)) {
            "NA"
          } else if (row$unit == "proportion") {
            format(round(100 * row$value, 2), nsmall = 2)
          } else {
            format(round(row$value), big.mark = ",", trim = TRUE)
          }
        },
        character(1)
      )
      paste0(
        "| ",
        paste(c(runs$year[i], runs$stratify_by[i], cells), collapse = " | "),
        " |"
      )
    },
    character(1)
  )
  header <- c("Year", "Stratified by", names(figures))
  c(
    paste0(
      "### ",
      if (isTRUE(meta$synthetic)) "SYNTHETIC (made-up data) " else "",
      "Measured effects: ",
      meta$request$indicator
    ),
    "",
    if (isTRUE(meta$synthetic)) {
      c(
        "**SYNTHETIC: every number below comes from made-up data. None describes CDC's or the Census's data.**",
        ""
      )
    },
    paste0(
      "Run ",
      meta$run$started_utc,
      " to ",
      meta$run$finished_utc,
      ", commit `",
      meta$package$git_commit,
      "`. Default pipeline, all groups together. Imputed cases are modelled, ",
      "not observed. The remainder is a difference, not identified cases."
    ),
    "",
    paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("|", strrep(" --- |", length(header))),
    lines,
    ""
  )
}

# One year and stratification: the default pipeline and its report, then each
# alternative imputation and the report of its imputation and remainder steps.
measured_effects_run <- function(options, year, stratify_by) {
  pkg <- asNamespace("CDCAtlas")
  county <- pkg$get_atlas(
    options$disease,
    "county",
    year,
    stratify_by = stratify_by
  )
  # One indicator only. Every step works within an indicator, so dropping the
  # others changes nothing for the one kept.
  county <- county[as.character(county$indicator) == options$indicator, ]
  if (nrow(county) == 0) {
    cli::cli_abort(
      "CDC returned no {options$indicator} county rows for {year}."
    )
  }

  imputed <- pkg$impute_suppressed(county)
  areas <- pkg$extrapolate_from_county(imputed)
  default <- pkg$.pipeline_report(
    county,
    imputed,
    areas,
    by_state = options$by_state
  )
  default$scenario <- rep("default", nrow(default))
  acs_years <- sort(unique(areas$acs_year))
  rm(areas)

  others <- lapply(options$alternatives, function(name) {
    alternative <- measured_effects_alternatives[[name]]
    imputed <- do.call(pkg$impute_suppressed, c(list(county), alternative$args))
    limits <- alternative$args[intersect(
      names(alternative$args),
      c("cap", "floor", "population")
    )]
    report <- do.call(
      pkg$.pipeline_report,
      c(list(county, imputed, by_state = options$by_state), limits)
    )
    # Nationally: the imputation and remainder steps. By state: imputed cases.
    keep <- report$step %in%
      c("imputation", "no county") &
      (report$state_fips == "all" | report$measure == "imputed_cases")
    report <- report[keep, ]
    report$scenario <- rep(alternative$label, nrow(report))
    report
  })

  report <- dplyr::bind_rows(default, others)
  columns <- c("year", "indicator", "stratify_by", "scenario")
  list(
    report = report[c(columns, setdiff(names(report), columns))],
    acs_years = acs_years
  )
}

# The newest year CDC has released for the indicator: from the latest year in
# the package's code table, back at most two years, the first whose state
# rows have counts. Only CDC's "no data" answer moves on to the next year; any
# other failure stops the run.
measured_effects_latest_year <- function(disease, indicator) {
  pkg <- asNamespace("CDCAtlas")
  newest <- pkg$.latest_atlas_year()
  tried <- numeric()
  for (year in newest - 0:2) {
    tried <- c(tried, year)
    state <- tryCatch(
      suppressWarnings(suppressMessages(pkg$get_atlas(disease, "state", year))),
      error = function(e) {
        if (grepl("returned no data", conditionMessage(e), fixed = TRUE)) {
          return(NULL)
        }
        stop(e)
      }
    )
    released <- !is.null(state) &&
      any(as.character(state$indicator) == indicator & !is.na(state$cases))
    if (released) {
      return(list(used = year, tried = tried))
    }
  }
  cli::cli_abort(c(
    "CDC returned no {indicator} state counts for {.val {tried}}.",
    "i" = "Give the years in {.envvar MEASURED_YEARS} instead of
           {.val latest}."
  ))
}

# The commit the summary was built from, or NA outside a git checkout.
measured_effects_commit <- function() {
  commit <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  if (length(commit) == 1 && nzchar(commit)) commit else NA_character_
}

if (!identical(Sys.getenv("MEASURED_EFFECTS_SOURCE_ONLY"), "true")) {
  pkgload::load_all(quiet = TRUE)
  options <- measured_effects_options(commandArgs(trailingOnly = TRUE))
  files <- if (options$synthetic) {
    measured_effects_synthetic(options)
  } else {
    measured_effects(options)
  }
  if (!files$complete) {
    stop("Some runs failed; the summary is incomplete.", call. = FALSE)
  }
}
