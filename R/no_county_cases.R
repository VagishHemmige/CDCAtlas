#' Cases in state totals that no county accounts for
#'
#' @description
#' A state's total in CDC AtlasPlus includes cases with no valid county of
#' residence, so it is usually larger than the sum of its counties.
#' `no_county_cases()` compares each state total with its counties. Run it
#' after [impute_suppressed()] for the remainder with no county; on county data
#' straight from [get_atlas()] it reports only the gap left by published
#' counts.
#'
#' Most of the gap is cases with no county, not suppressed counties (#27), so
#' it can't be used to fill in suppressed counties. [extrapolate_from_county()]
#' leaves these cases out by default; `no_county = "spread"` shares them across
#' the state's counties instead.
#'
#' @param x County data from `get_atlas(geography = "county")`, usually after
#'   [impute_suppressed()].
#'
#' @return A tibble with one row per state, indicator, year and stratum:
#'   \describe{
#'     \item{state_fips, state}{State FIPS code and name.}
#'     \item{indicator, year, race_ethnicity, sex, age}{As in `x`.}
#'     \item{state_cases}{The state total published by CDC.}
#'     \item{published_cases}{Sum of the published county counts in `x`.}
#'     \item{unpublished_cases}{`state_cases - published_cases`: cases in
#'       suppressed counties and cases with no county together (the figures in
#'       #27).}
#'     \item{imputed_cases}{Sum of the counts [impute_suppressed()] filled in;
#'       `NA` if `x` hasn't been imputed.}
#'     \item{no_county_cases}{`unpublished_cases - imputed_cases`. Can be
#'       negative if the imputed counts are too high, and is `NA` if `x` hasn't
#'       been imputed.}
#'   }
#'
#'   Every column computed from `state_cases` is `NA` if CDC suppressed the
#'   state total.
#'
#' @details
#' Downloads the state data matching `x` (same diseases, years and
#' stratification) from CDC AtlasPlus. The remainder also includes any county
#' without a count in `x`: "Data not available" rows (whether
#' [impute_suppressed()] dropped or kept them) and counties removed by
#' filtering `x`, and suppressed rows [impute_suppressed()] couldn't estimate
#' (marked `cases_source = "not estimated"`), which count as 0 imputed cases.
#'
#' @seealso [impute_suppressed()], [extrapolate_from_county()].
#'
#' @examples
#' \dontrun{
#' county <- get_atlas("hiv", "county", 2022)
#' county <- impute_suppressed(county)
#' no_county_cases(county)
#' }
#'
#' @export
no_county_cases <- function(x) {
  call <- rlang::current_env()
  .check_county_data(x, arg = "x", call = call)
  if ("cases_source" %in% names(x)) {
    .check_imputed(x, arg = "x", call = call, unestimated = "leave out")
    .announce_not_estimated(x, "counted in the remainder with no county")
  }
  .no_county_cases(x, .fetch_state_totals(x))
}

# The report itself, given the state rows. Separate so extrapolate_from_county()
# can reuse the state rows it has already fetched.
.no_county_cases <- function(x, state) {
  keys <- c("state_fips", .atlas_keys)
  source <- if ("cases_source" %in% names(x)) {
    x$cases_source
  } else {
    ifelse(is.na(x$cases), "not imputed", "published")
  }
  county <- data.frame(
    state_fips = substr(as.character(x$fips), 1, 2),
    lapply(as.data.frame(x[.atlas_keys]), as.character),
    published = ifelse(source == "published", x$cases, 0),
    imputed = dplyr::case_when(
      source %in% c("published", "not available", "not estimated") ~ 0,
      source == "not imputed" ~ NA_real_,
      .default = x$cases
    )
  )
  county$year <- as.numeric(county$year)

  report <- county |>
    dplyr::summarize(
      published_cases = sum(published),
      imputed_cases = sum(imputed),
      .by = dplyr::all_of(keys)
    )
  state_cases <- state$cases[match(.row_key(report, keys), .row_key(state, keys))]

  report$state <- lookup[["state"]]$name[
    match(report$state_fips, lookup[["state"]]$fips)
  ]
  report$state_cases <- state_cases
  report$unpublished_cases <- state_cases - report$published_cases
  if (!"cases_source" %in% names(x)) {
    report$imputed_cases <- NA_real_
  }
  report$no_county_cases <- report$unpublished_cases - report$imputed_cases

  tibble::as_tibble(report[c(
    "state_fips", "state", .atlas_keys,
    "state_cases", "published_cases", "unpublished_cases", "imputed_cases",
    "no_county_cases"
  )])
}

# Message naming the counties whose suppressed rows impute_suppressed() couldn't
# estimate, and what happens to their cases.
.announce_not_estimated <- function(x, what_happens) {
  rows <- is.na(x$cases) & x$cases_source %in% "not estimated"
  if (!any(rows)) {
    return(invisible())
  }
  counties <- unique(as.character(x$geography[rows]))
  cli::cli_inform(c(
    "i" = "{sum(rows)} suppressed row{?s} that {.fn impute_suppressed} couldn't
           estimate {?was/were} {what_happens}.",
    " " = "{cli::qty(length(counties))}Count{?y/ies}: {.val {counties}}."
  ))
}
