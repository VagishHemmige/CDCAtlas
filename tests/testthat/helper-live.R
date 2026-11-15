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
live_runs <- new.env()

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
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
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
