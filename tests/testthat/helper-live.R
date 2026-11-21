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
# from one small state-level request (made once per run): the requested
# indicators that have state rows. Never none: nothing is inferred from a
# response until it is shown to be an answer to the request.
#
# An indicator counts as unreleased only on positive evidence: the state
# response holds only indicators that were asked for, at least one other of
# them has rows, and this one has none, as for HIV 2025, which came with
# diagnoses but not prevalence (#149). get_atlas() warns about that; the
# warning is expected here and dropped.
#
# Everything else stops with the year and stratification:
# - the state request fails, "CDC AtlasPlus returned no data" included (the
#   package says that whenever a response has no `sourcedata`, whatever the
#   reason);
# - the response has an indicator that wasn't asked for (chlamydia rows for
#   an HIV request are a wrong response, not a sign HIV is unreleased);
# - the response has none of the indicators asked for.
#
# Two consequences, both deliberate: if CDC ever has no state rows at all for
# the disease in the package's latest year, a test using this fails rather
# than moving to an earlier year, and a person needs to look. And for a
# disease with a single indicator (chlamydia, say), its absence can never be
# told from a wrong or empty response, so there is no stepping back at all:
# an unreleased year always fails. The latest-years check uses HIV, which has
# two.
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
      rlang::abort(
        c(
          paste0(
            "Couldn't establish whether CDC has released ",
            disease,
            " ",
            year,
            ", ",
            if (is.null(stratify_by)) {
              "unstratified"
            } else {
              paste("by", stratify_by)
            },
            ": the state request failed."
          ),
          "i" = paste0(
            "A failed request is not evidence that the year is unreleased, ",
            "so an earlier year is not used instead."
          )
        ),
        parent = e
      )
    }
  ))
  indicators <- .disease_indicators()
  live_released_from_state(
    state,
    requested = indicators$indicator[indicators$disease == disease],
    year = year,
    what = paste0(
      disease,
      " ",
      year,
      ", ",
      if (is.null(stratify_by)) "unstratified" else paste("by", stratify_by)
    )
  )
}

# The rule itself, with no request: which of the `requested` indicators a
# state response (`state`, from get_atlas()) shows CDC has released for
# `year`. `what` names the request in messages. It stops unless the response
# is an answer to the request: only indicators that were asked for, only rows
# for the year asked for, and at least one of the indicators. Rows for another
# year are not evidence about this one, whichever indicators they hold.
#
# Kept apart from the request so that data-raw/measured-effects.R, which
# sources this file, decides its "latest" year by the same rule as the live
# tests. It uses nothing from testthat.
live_released_from_state <- function(
  state,
  requested,
  year,
  what,
  call = rlang::caller_env()
) {
  returned <- unique(as.character(state$indicator))
  not_relevant <- "so nothing is inferred from it and an earlier year is not used instead."
  unexpected <- setdiff(returned, requested)
  if (length(unexpected) > 0) {
    rlang::abort(
      c(
        paste0(
          "Couldn't establish whether CDC has released ",
          what,
          ": the state response has indicators that weren't asked for."
        ),
        "x" = paste0("Returned: ", paste(sort(returned), collapse = ", "), "."),
        "x" = paste0("Asked for: ", paste(requested, collapse = ", "), "."),
        "i" = paste("The response doesn't match the request,", not_relevant)
      ),
      call = call
    )
  }
  years <- unique(as.numeric(as.character(state$year)))
  other_years <- years[is.na(years) | years != year]
  if (length(other_years) > 0) {
    rlang::abort(
      c(
        paste0(
          "Couldn't establish whether CDC has released ",
          what,
          ": the state response has rows for another year."
        ),
        "x" = paste0("Returned: ", paste(sort(years), collapse = ", "), "."),
        "x" = paste0("Asked for: ", year, "."),
        "i" = paste("The response doesn't match the request,", not_relevant)
      ),
      call = call
    )
  }
  released <- intersect(requested, returned)
  if (length(released) == 0) {
    rlang::abort(
      c(
        paste0(
          "Couldn't establish whether CDC has released ",
          what,
          ": the state response has none of the indicators asked for."
        ),
        "x" = paste0("Asked for: ", paste(requested, collapse = ", "), "."),
        "i" = paste("The response has no rows for the request,", not_relevant)
      ),
      call = call
    )
  }
  released
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

# live_county() for a year CDC may have released only in part. It never gives
# up on a year: live_released_indicators() must first show which indicators
# CDC has released for states (and stops if it can't), then the county
# download must work and must have rows for every one of those. The result
# can lack an indicator only when the state response lacked it too, beside
# another that was there. A county download is one request per
# state and stops at the first that fails, so its failing, "returned no data"
# included, says nothing about the year as a whole: that stops with the year,
# stratification and state rather than letting a test fall back to an earlier
# year.
live_county_if_released <- function(year, stratify_by = NULL, disease = "hiv") {
  released <- live_released_indicators(year, stratify_by, disease)
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
# `responses`, a list of get_atlas() results named by year. NA when no
# response has rows for the indicator.
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
