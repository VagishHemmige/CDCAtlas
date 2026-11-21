# Shared downloads for the live tests (test-live-*.R), so each county
# request and pipeline run is made at most once per test run. Nothing here
# makes a request until a live test calls it.

skip_unless_census <- function() {
  skip_if_not_installed("tidycensus")
  skip_if(
    Sys.getenv("CENSUS_API_KEY") == "",
    "No Census API key (CENSUS_API_KEY)."
  )
}

live_counties <- new.env()
live_states <- new.env()
live_runs <- new.env()

# The state a county payload asks for (county requests are split by state),
# as "Name (FIPS)"; NA for any other payload.
live_payload_state <- function(payload) {
  ids <- as.numeric(strsplit(payload$VariableIDs, ",")[[1]])
  counties <- lookup[["counties"]]
  fips <- unique(substr(counties$fips[counties$geo_id %in% ids], 1, 2))
  if (length(fips) != 1) {
    return(NA_character_)
  }
  states <- lookup[["state"]]
  paste0(states$name[match(fips, states$fips)], " (", fips, ")")
}

# One uncached request to CDC. A failure keeps its own message and also
# carries the state the request was for (`live_state`), so a county download
# that fails part-way can say where.
live_request <- function(payload) {
  tryCatch(
    .request_atlas_internal(payload),
    error = function(e) {
      e$live_state <- live_payload_state(payload)
      stop(e)
    }
  )
}

# Runs `code` once per `key` in `store`, keeping its value, its warnings, or
# its error. Later calls get the same value and see the same warnings, or the
# same error, without running `code` again, so a failed download is not
# retried by every test that needs it.
live_once <- function(store, key, code) {
  if (is.null(store[[key]])) {
    warnings <- character()
    value <- tryCatch(
      withCallingHandlers(
        code,
        warning = function(w) {
          warnings <<- c(warnings, cli::ansi_strip(conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    store[[key]] <- list(value = value, warnings = warnings)
  }
  run <- store[[key]]
  for (w in run$warnings) {
    warning(w, call. = FALSE)
  }
  if (inherits(run$value, "error")) {
    stop(run$value)
  }
  run$value
}

# get_atlas(disease, "county", year, stratify_by), fresh from CDC (bypassing
# the on-disk cache) the first time, then from memory.
live_county <- function(year, stratify_by = NULL, disease = "hiv") {
  local_mocked_bindings(.request_atlas = live_request)
  live_once(
    live_counties,
    paste(disease, year, stratify_by %||% "none"),
    suppressMessages(get_atlas(
      disease,
      "county",
      year,
      stratify_by = stratify_by
    ))
  )
}

# The indicators of `disease` CDC has released for a year and stratification,
# from one small state-level request (made once per run): the indicators with
# state rows. None when CDC has no data at all for the year. When CDC answers
# for some indicators only (HIV 2025 came with diagnoses but not prevalence,
# #149), get_atlas() warns; that warning is expected here and dropped.
live_released_indicators <- function(
  year,
  stratify_by = NULL,
  disease = "hiv"
) {
  local_mocked_bindings(.request_atlas = live_request)
  state <- live_without_unreleased_warning(tryCatch(
    live_once(
      live_states,
      paste(disease, year, stratify_by %||% "none"),
      suppressMessages(get_atlas(
        disease,
        "state",
        year,
        stratify_by = stratify_by
      ))
    ),
    error = function(e) {
      # A state request is a single request, so "no data" is CDC's answer for
      # the whole year, not for part of it.
      if (grepl("returned no data", conditionMessage(e), fixed = TRUE)) {
        return(NULL)
      }
      stop(e)
    }
  ))
  indicators <- .disease_indicators()
  intersect(
    indicators$indicator[indicators$disease == disease],
    unique(as.character(state$indicator))
  )
}

live_without_unreleased_warning <- function(code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      if (
        grepl("CDC returned no rows for", conditionMessage(w), fixed = TRUE)
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# live_county() for a year CDC may not have released. NULL only on positive
# evidence that the year is unreleased: the state-level request has no rows
# for any of the disease's indicators (the counties are then not requested).
# Otherwise the county download must work and must have rows for every
# indicator CDC has released for states. A county download is one request per
# state and stops at the first that fails, so its failing, "returned no data"
# included, says nothing about the year as a whole: that stops with the year,
# stratification and state rather than letting a test fall back to an earlier
# year.
live_county_if_released <- function(year, stratify_by = NULL, disease = "hiv") {
  released <- live_released_indicators(year, stratify_by, disease)
  if (length(released) == 0) {
    return(NULL)
  }
  what <- paste0(
    disease,
    " ",
    year,
    ", ",
    if (is.null(stratify_by)) "unstratified" else paste("by", stratify_by)
  )
  county <- live_without_unreleased_warning(tryCatch(
    live_county(year, stratify_by, disease),
    error = function(e) {
      rlang::abort(
        c(
          paste0("The county download for ", what, " failed."),
          "x" = paste0(
            "Failing request: ",
            if (is.null(e$live_state) || is.na(e$live_state)) {
              "state not known"
            } else {
              e$live_state
            },
            "."
          ),
          "i" = paste0(
            "CDC has state rows for ",
            paste(released, collapse = " and "),
            " in ",
            year,
            ", so the year is released and an earlier year is not used ",
            "instead."
          )
        ),
        parent = e
      )
    }
  ))
  absent <- setdiff(released, unique(as.character(county$indicator)))
  if (length(absent) > 0) {
    rlang::abort(c(
      paste0(
        "The county download for ",
        what,
        " has no rows for ",
        paste(absent, collapse = " and "),
        "."
      ),
      "i" = paste0(
        "CDC has state rows for it in ",
        year,
        ", so an earlier year is not used instead."
      )
    ))
  }
  county
}

# Expects that CDC gave no rate where it gave no count (#157): every row of a
# county response from get_atlas() marked "Data suppressed" with `cases` NA
# also has NA rate100000, lowerci_rate and upperci_rate. impute_suppressed()
# fills in `cases` on those rows and leaves the rate columns alone, so a rate
# there would sit beside an estimate it doesn't describe. A failure names the
# rows (the first `max_shown`). A response with no such row fails too: the
# check would say nothing about it. One expectation per call. Returns the
# number of rows checked.
expect_no_rate_on_suppressed <- function(
  county,
  label = "county response",
  max_shown = 20
) {
  rate_columns <- c("rate100000", "lowerci_rate", "upperci_rate")
  needed <- c(
    rate_columns,
    c("fips", "indicator", "year", "data_status", "cases"),
    c("race_ethnicity", "sex", "age")
  )
  absent <- setdiff(needed, names(county))
  if (length(absent) > 0) {
    fail(paste0(label, ": no column ", paste(absent, collapse = ", "), "."))
    return(invisible(0L))
  }

  suppressed <- is.na(county$cases) &
    as.character(county$data_status) %in% "Data suppressed"
  if (!any(suppressed)) {
    fail(paste0(
      label,
      ": no row is marked \"Data suppressed\" with no count, so nothing was ",
      "checked."
    ))
    return(invisible(0L))
  }

  rates <- as.data.frame(county[rate_columns])
  has_rate <- suppressed & rowSums(!is.na(rates)) > 0
  rows <- county[has_rate, , drop = FALSE]
  given <- vapply(
    which(has_rate),
    function(i) {
      values <- unlist(rates[i, ])
      paste0(
        rate_columns[!is.na(values)],
        " = ",
        values[!is.na(values)],
        collapse = ", "
      )
    },
    character(1)
  )
  described <- paste0(
    "* ",
    as.character(rows$fips),
    " | ",
    as.character(rows$indicator),
    " | ",
    as.character(rows$year),
    " | ",
    as.character(rows$race_ethnicity),
    ", ",
    as.character(rows$sex),
    ", ",
    as.character(rows$age),
    ": ",
    given,
    recycle0 = TRUE
  )
  more <- length(described) - max_shown
  expect(
    !any(has_rate),
    paste0(
      label,
      ": ",
      sum(has_rate),
      " of ",
      sum(suppressed),
      " suppressed rows with no count have a rate or a rate bound ",
      "(fips | indicator | year | stratum):\n",
      paste(utils::head(described, max_shown), collapse = "\n"),
      if (more > 0) paste0("\n... and ", more, " more")
    )
  )
  invisible(sum(suppressed))
}

# For each of `indicators`, the most recent year with rows for it among
# `responses`, a list of get_atlas() results named by year (NULL for a year
# CDC has no data for). NA when no response has rows for the indicator.
latest_year_with_rows <- function(responses, indicators) {
  years <- sort(as.numeric(names(responses)), decreasing = TRUE)
  out <- rlang::set_names(rep(NA_real_, length(indicators)), indicators)
  for (indicator in indicators) {
    for (year in years) {
      response <- responses[[as.character(year)]]
      if (any(as.character(response$indicator) == indicator)) {
        out[[indicator]] <- year
        break
      }
    }
  }
  out
}

# Full default pipeline on real data: live_county() -> impute_suppressed() ->
# extrapolate_from_county(), every option at its default, as HOPE runs it. Each
# run is made once and shared by the tests that use it. `warnings` includes
# warnings from the county download, even if an earlier test made it.
live_pipeline <- function(
  year,
  stratify_by = NULL,
  to = "tract",
  disease = "hiv"
) {
  skip_unless_census()
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  run <- NULL
  warnings <- capture_warnings(
    run <- live_once(
      live_runs,
      paste(disease, year, stratify_by %||% "none", to),
      suppressMessages({
        county <- live_county(year, stratify_by, disease)
        imputed <- impute_suppressed(county)
        list(
          imputed = imputed,
          areas = extrapolate_from_county(imputed, to = to)
        )
      })
    )
  )
  c(run, list(warnings = cli::ansi_strip(warnings)))
}
