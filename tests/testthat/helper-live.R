# Shared downloads for the live tests (test-live-*.R), so each county
# request and pipeline run is made at most once per test run. Nothing here
# makes a request until a live test calls it.

skip_unless_census <- function() {
  skip_if_not_installed("tidycensus")
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "No Census API key (CENSUS_API_KEY).")
}

live_counties <- new.env()

# get_atlas("hiv", "county", year, stratify_by), fresh from CDC (bypassing the
# on-disk cache) the first time, then from memory.
live_county <- function(year, stratify_by = NULL) {
  key <- paste(year, stratify_by %||% "none")
  if (is.null(live_counties[[key]])) {
    local_mocked_bindings(.request_atlas = .request_atlas_internal)
    live_counties[[key]] <- suppressMessages(
      get_atlas("hiv", "county", year, stratify_by = stratify_by)
    )
  }
  live_counties[[key]]
}

# Full default pipeline on real data: get_atlas() -> impute_suppressed() ->
# extrapolate_from_county(), every option at its default, as HOPE runs it. Each
# run is downloaded once and shared by the tests below.

live_runs <- new.env()

live_pipeline <- function(year, stratify_by = NULL, to = "tract") {
  key <- paste(year, stratify_by %||% "none", to)
  if (is.null(live_runs[[key]])) {
    skip_unless_census()
    local_mocked_bindings(.request_atlas = .request_atlas_internal)
    warnings <- character()
    keep_warning <- function(w) {
      warnings <<- c(warnings, cli::ansi_strip(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
    withCallingHandlers(
      suppressMessages({
        county <- live_county(year, stratify_by)
        imputed <- impute_suppressed(county)
        areas <- extrapolate_from_county(imputed, to = to)
      }),
      warning = keep_warning
    )
    live_runs[[key]] <- list(imputed = imputed, areas = areas, warnings = warnings)
  }
  live_runs[[key]]
}
