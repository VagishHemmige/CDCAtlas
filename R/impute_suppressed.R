#' Fill in suppressed county counts
#'
#' @description
#' CDC suppresses small county counts. `impute_suppressed()` gives each
#' suppressed county a count so county data can be added up or shared across
#' census tracts with [extrapolate_from_county()]. Run it on county data from
#' [get_atlas()] before [no_county_cases()] or [extrapolate_from_county()].
#'
#' Rows are handled by what CDC published:
#'
#' * A published count is kept, whatever its data status. (Connecticut's 2017
#'   planning regions are marked "Data not available" but have counts.)
#' * A row marked "Data not available" with no count has no data to estimate
#'   from; `not_available` says whether it is dropped or kept. Either way its
#'   cases are still in the state total, so [no_county_cases()] counts them in
#'   the remainder.
#' * Any other row with no count is suppressed and is filled in by `method`.
#'
#' @param x County data from `get_atlas(geography = "county")`.
#' @param method How suppressed counts are filled in:
#'   * `"state_rate"` (default): the state's rate in the same indicator, year
#'     and stratum, times the county's population (see `population`), kept
#'     between `floor` and `cap`.
#'   * `"zero"`: 0. Useful as a sensitivity analysis.
#' @param cap The largest count `"state_rate"` can give a suppressed county.
#'   Use `Inf` for no cap. The default, 4, reflects CDC suppressing counts of
#'   1 to 4, but complementary suppression can hide larger counts, so it is a
#'   default rather than a hard limit.
#' @param floor The smallest count `"state_rate"` can give. The default, 0,
#'   allows for counties suppressed because their population is under 100,
#'   which may have no cases. Use 1 to treat every suppressed county as having
#'   at least one case. Must not be larger than `cap`.
#' @param population The county population `"state_rate"` multiplies by:
#'   * `"cdc"` (default): CDC's own county population, the same source as the
#'     state rate's denominator.
#'   * `"acs"`: the county's ACS 5-year population in the same stratum and
#'     ages, summed from its tracts as [extrapolate_from_county()] does. Needs
#'     tidycensus and a Census API key.
#'
#'   Neither covers Connecticut's 2017 planning regions: CDC gives them no
#'   population, and 2017 ACS tracts belong to the old counties (#30).
#' @param unestimated What to do with a suppressed row `"state_rate"` can't
#'   estimate, because the state rate or county population is missing:
#'   * `"na"` (default): leave it `NA`, with a warning. [no_county_cases()]
#'     counts its cases in the remainder with no county, and
#'     [extrapolate_from_county()] leaves it out (or stops, with
#'     `unestimated = "error"`).
#'   * `"zero"`: set it to 0, with a warning.
#' @param not_available Rows marked "Data not available" with no count:
#'   * `"drop"` (default): remove them, with a message.
#'   * `"keep"`: keep them with `NA` cases. The later steps leave them out.
#'
#' @return `x` with suppressed counts filled in and a new column
#'   `cases_source`:
#'   * `"published"`: CDC's count.
#'   * `"state_rate"` or `"zero"`: filled in by `method`.
#'   * `"not estimated"`: `"state_rate"` couldn't estimate it; `NA`, or 0 with
#'     `unestimated = "zero"`.
#'   * `"not available"`: kept with `not_available = "keep"`; `NA`.
#'
#'   `data_status` is unchanged, so suppressed rows can still be identified.
#'   `cap`, `floor`, `population` and `unestimated` apply only to
#'   `"state_rate"`.
#'
#' @details
#' `"state_rate"` downloads the state data matching `x` (same diseases, years
#' and stratification) from CDC AtlasPlus, and with `population = "acs"` the
#' tract populations from the Census.
#'
#' @seealso [no_county_cases()] for cases in state totals that no county
#'   accounts for; [extrapolate_from_county()] for the next step.
#'
#' @examples
#' \dontrun{
#' county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
#' county <- impute_suppressed(county)
#'
#' # Sensitivity analysis: suppressed counties have no cases
#' county_zero <- impute_suppressed(
#'   get_atlas("hiv", "county", 2022, stratify_by = "sex"),
#'   method = "zero"
#' )
#' }
#'
#' @export
impute_suppressed <- function(
  x,
  method = c("state_rate", "zero"),
  cap = 4,
  floor = 0,
  population = c("cdc", "acs"),
  unestimated = c("na", "zero"),
  not_available = c("drop", "keep")
) {
  call <- rlang::current_env()
  .check_county_data(x, arg = "x", call = call)
  if ("cases_source" %in% names(x)) {
    cli::cli_abort(
      c(
        "{.arg x} has already been through {.fn impute_suppressed}.",
        "i" = "It has a {.field cases_source} column."
      ),
      call = call
    )
  }
  method <- rlang::arg_match(method)
  population <- rlang::arg_match(population)
  unestimated <- rlang::arg_match(unestimated)
  not_available <- rlang::arg_match(not_available)
  .check_count_limit(cap, "cap", call)
  .check_count_limit(floor, "floor", call)
  if (floor > cap) {
    cli::cli_abort(
      "{.arg floor} ({floor}) must not be larger than {.arg cap} ({cap}).",
      call = call
    )
  }

  unavailable <- is.na(x$cases) &
    as.character(x$data_status) %in% "Data not available"
  if (any(unavailable) && not_available == "drop") {
    x <- x[!unavailable, , drop = FALSE]
    cli::cli_inform(c(
      "i" = "Dropped {sum(unavailable)} row{?s} marked {.val Data not
             available} with no count.",
      " " = "Their cases are still in the state totals; see
             {.fn no_county_cases}."
    ))
    unavailable <- rep(FALSE, nrow(x))
  }

  suppressed <- is.na(x$cases) & !unavailable
  x$cases_source <- ifelse(suppressed, method, "published")
  x$cases_source[unavailable] <- "not available"
  if (!any(suppressed)) {
    return(x)
  }

  if (method == "zero") {
    x$cases[suppressed] <- 0
    return(x)
  }

  if (population == "acs") {
    .check_census_ready(call)
  }
  state <- .fetch_state_totals(x)
  state_rate <- ifelse(
    !is.na(state$population) & state$population > 0,
    state$cases / state$population,
    NA_real_
  )
  keys <- c("state_fips", .atlas_keys)
  x_keys <- .row_key(
    data.frame(
      state_fips = substr(as.character(x$fips), 1, 2),
      as.data.frame(x[.atlas_keys])
    ),
    keys
  )
  rate <- state_rate[match(x_keys, .row_key(state, keys))]
  county_population <- if (population == "cdc") {
    x$population
  } else {
    .acs_county_population(x)
  }
  estimate <- pmin(cap, pmax(floor, rate * county_population))
  x$cases[suppressed] <- estimate[suppressed]

  missing <- suppressed & is.na(x$cases)
  x$cases_source[missing] <- "not estimated"
  if (any(missing)) {
    counties <- unique(as.character(x$geography[missing]))
    if (unestimated == "zero") {
      x$cases[missing] <- 0
    }
    set_to <- if (unestimated == "zero") "0" else "NA"
    cli::cli_warn(c(
      "{sum(missing)} suppressed row{?s} could not be estimated and {?is/are}
       set to {.code {set_to}}.",
      "i" = "No state rate or no county population for {.val {counties}}.",
      "i" = "They are marked {.code cases_source = \"not estimated\"}."
    ))
  }

  x
}

.check_count_limit <- function(value, arg, call) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || value < 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number of 0 or more, or {.code Inf}.",
      call = call
    )
  }
}

# ACS county population for each row of `x`, in the row's stratum and ages, as
# the sum of the county's tracts (the denominator extrapolate_from_county() uses).
.acs_county_population <- function(x) {
  stratify_by <- .atlas_stratify_by(x)
  stratify_by <- if (length(stratify_by) == 0) NULL else stratify_by
  ages <- .default_ages(x, stratify_by)
  county <- .county_cases(x)
  county$stratum <- .row_stratum(county, stratify_by)
  states <- sort(unique(county$state_fips))

  out <- rep(NA_real_, nrow(county))
  for (year in sort(unique(county$year))) {
    pop <- .area_population(year, states, stratify_by, ages) |>
      dplyr::summarize(
        population = sum(area_population_acs),
        .by = c(county_fips, stratum)
      )
    rows <- county$year == year
    out[rows] <- pop$population[match(
      .row_key(county[rows, ], c("county_fips", "stratum")),
      .row_key(pop, c("county_fips", "stratum"))
    )]
  }
  out
}
