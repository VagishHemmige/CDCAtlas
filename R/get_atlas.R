#' Retrieve surveillance data from CDC AtlasPlus
#'
#' `get_atlas()` provides a programmatic interface to the CDC AtlasPlus platform,
#' returning case counts, population denominators, and rates for HIV, sexually
#' transmitted infections (STIs), viral hepatitis and tuberculosis surveillance.
#' The function submits a
#' payload to the AtlasPlus data endpoint and parses the response into a tidy
#' tibble.
#'
#' The function supports multiple diseases, geographic levels, year ranges,
#' and optional stratification. Stratified queries expand results into all
#' requested combinations (e.g., age by race). When possible, numeric fields
#' are returned as `double` and identifiers as character.
#'
#' `get_atlas()` only downloads. For census tract or block group estimates,
#' pass county data to [impute_suppressed()] and then
#' [extrapolate_from_county()]:
#'
#' ```r
#' county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
#' county <- impute_suppressed(county)
#' no_county_cases(county)
#' tracts <- extrapolate_from_county(county)
#' ```
#'
#' **Supported diseases**
#'
#' Some values return more than one indicator; the `indicator` column says
#' which row is which.
#' - `"chlamydia"`
#' - `"gonorrhea"`
#' - `"adult syphilis"`: three indicators, one per stage: primary and
#'   secondary syphilis; early non-primary, non-secondary syphilis; and
#'   syphilis of unknown duration or late syphilis.
#' - `"congenital syphilis"`
#' - `"tuberculosis"`
#' - `"hiv"`: two indicators, HIV diagnoses (new diagnoses in the year) and
#'   HIV prevalence (people living with diagnosed HIV).
#' - `"estimate"`: three modeled HIV indicators: estimated HIV incidence,
#'   estimated HIV prevalence (diagnosed and undiagnosed), and Knowledge of
#'   Status. Knowledge of Status is a percentage, not a count. AtlasPlus says
#'   CDC has postponed release of estimates for HIV incidence and HIV
#'   prevalence, so recent years may be missing; check AtlasPlus for what is
#'   available.
#' - `"hepatitis"`: three indicators: hepatitis A, acute hepatitis B and acute
#'   hepatitis C.
#'
#' HIV indicators cover people aged 13 and older; the other diseases cover all
#' ages.
#'
#' **Geography**
#' - `"national"`
#' - `"region"`: the four Census regions (Northeast, Midwest, South, West) and
#'   U.S. territories and freely associated states. Only for
#'   `disease = "estimate"`: CDC returns regions only for its HIV estimates.
#'   The `fips` column holds CDC's own codes for these, not Census FIPS codes.
#' - `"state"`
#' - `"county"`
#' - `"msa"` (metropolitan statistical area)
#'
#' **Stratification**
#' The CDC AtlasPlus API supports stratification by one or two dimensions:
#' - `"age"`
#' - `"race"`
#' - `"sex"`
#'
#' Stratification increases the number of returned rows. For example,
#' `stratify_by = c("race", "sex")` returns the race × sex combinations CDC
#' offers for each geography-year pair. CDC doesn't offer every disease,
#' geography and stratification together (hepatitis by sex at state level,
#' for example); such a request stops with "CDC AtlasPlus returned no data".
#' If CDC returns some of a disease's indicators but not others, a warning
#' names the missing ones.
#'
#' @param disease A single disease name (e.g. `"gonorrhea"`).
#' Multiple diseases are not currently supported in a single request.
#' @param geography Character scalar. One of `"national"`, `"region"` (HIV
#' estimates only), `"state"`, `"county"` (not hepatitis or congenital
#' syphilis), or `"msa"` (`"hiv"` only), as AtlasPlus offers them.
#' @param extrapolate_to_tract `r lifecycle::badge("deprecated")` No longer
#'   supported. Use [impute_suppressed()] and then [extrapolate_from_county()].
#' @param year Integer vector of years, from 2000 to the latest year in the
#' package's copy of CDC's code table (2025 in this version). CDC lists a year
#' before every disease has data for it; such a request returns no data.
#' Can be a single year (e.g., `2022`) or a sequence (e.g., `2018:2022`).
#' @param stratify_by Optional character vector. Zero, one, or two of
#' `"age"`, `"race"`, `"sex"`. If `NULL`, totals (unstratified) are returned.
#' Each disease has its own age groups, as AtlasPlus offers them: 13-24 to 65+
#' for `"hiv"` and `"estimate"`; 0-14, 15-19 and so on to 65+ for the other
#' diseases, plus "Unknown" for chlamydia, gonorrhea and adult syphilis.
#' Race groups also differ: hepatitis has Asian/Pacific Islander and Other in
#' place of Asian, Native Hawaiian/Other Pacific Islander and Multiracial;
#' HIV and the HIV estimates have no Unknown group.
#' Congenital syphilis can't be stratified.
#' @param refresh If `TRUE`, download the data again from CDC even if a saved
#'   copy is less than 30 days old, and save the new copy. For county data
#'   this also drops the saved state totals that [impute_suppressed()],
#'   [no_county_cases()] and [extrapolate_from_county()] use with them, so
#'   those are downloaded again too. Default `FALSE`.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{indicator}{Disease/condition name (factor)}
#'   \item{year}{Calendar year (numeric)}
#'   \item{geography}{Geographic unit name (factor)}
#'   \item{data_status}{Data quality flags (factor)}
#'   \item{race_ethnicity}{Race/ethnicity category (factor, if stratified)}
#'   \item{sex}{Sex category (factor, if stratified)}
#'   \item{age}{Age group (factor, if stratified)}
#'   \item{transmission}{Transmission category (factor)}
#'   \item{rate100000}{Rate per 100,000 population (numeric): per 100,000
#'     people aged 13 and older for HIV, and per 100,000 live births for
#'     congenital syphilis. For Knowledge of Status, the percentage of people
#'     with HIV who know their status.}
#'   \item{cases}{Case count (numeric). For Knowledge of Status, the number
#'     who know their status.}
#'   \item{population}{Population denominator (numeric). For Knowledge of
#'     Status, estimated HIV prevalence.}
#'   \item{lowerci_rate, upperci_rate}{95% CI bounds for `rate100000`
#'     (numeric); for Knowledge of Status, of the percentage. Usually empty
#'     (`NA`) for reported cases; given for the modeled estimates.}
#'   \item{rse}{Relative standard error (numeric)}
#'   \item{lowerci_cases, upperci_cases}{95% CI bounds for `cases` (numeric).
#'     Given for estimated HIV incidence and prevalence; usually empty (`NA`)
#'     for reported cases and for Knowledge of Status.}
#'   \item{fips}{FIPS code (character)}
#' }
#'
#' Columns and naming conventions may differ depending on CDC schema changes.
#' The function attempts to standardize but does not guarantee historical
#' compatibility for all years.
#'
#' @details
#' This function is a reverse-engineered wrapper around the AtlasPlus
#' public query system. The CDC does not publish an official API specification,
#' and internal identifiers may change without notice. If the CDC modifies the
#' backend schema, requests may fail. Please report issues via GitHub.
#'
#' ## Saved downloads
#'
#' Data are requested from CDC one disease and one year at a time (and for
#' counties, one state at a time). Each response is saved in the folder given
#' by `tools::R_user_dir("CDCAtlas", "cache")` and reused for 30 days, so data
#' already downloaded, alone or as part of a larger request, aren't downloaded
#' again. After 30 days the data are
#' downloaded again, which picks up any revisions CDC has made. Use
#' `refresh = TRUE` to download them sooner, or [clear_atlas_cache()] to delete
#' every saved response.
#'
#' @seealso [impute_suppressed()] and [extrapolate_from_county()] for tract
#'   and block group estimates from county data.
#'
#' @examples
#' \dontrun{
#' # National gonorrhea rates for 2018–2022
#' get_atlas(disease = "gonorrhea", geography = "national", year = 2018:2022)
#'
#' # State-level chlamydia, stratified by race
#' get_atlas(
#'   disease = "chlamydia",
#'   geography = "state",
#'   year = 2022,
#'   stratify_by = "race"
#' )
#'
#' # County gonorrhea, shared across census tracts
#' county <- get_atlas(disease = "gonorrhea", geography = "county", year = 2022)
#' tracts <- extrapolate_from_county(impute_suppressed(county))
#' }
#'
#' @export

get_atlas <- function(
  disease,
  geography,
  year,
  stratify_by = NULL,
  extrapolate_to_tract = deprecated(),
  refresh = FALSE
) {
  if (lifecycle::is_present(extrapolate_to_tract)) {
    lifecycle::deprecate_stop(
      "0.0.0.9001",
      "get_atlas(extrapolate_to_tract)",
      "extrapolate_from_county()",
      details = "Run `impute_suppressed()` on county data first."
    )
  }

  #Validate input
  .validate_get_atlas_inputs(
    disease = disease,
    geography = geography,
    year = year,
    stratify_by = stratify_by,
    call = rlang::current_call()
  )
  if (!isTRUE(refresh) && !isFALSE(refresh)) {
    cli::cli_abort("{.arg refresh} must be {.code TRUE} or {.code FALSE}.")
  }

  #Construct the payloads for the call to the CDC site based on what users request.
  #County requests are split into one payload per state.
  payloads <- .build_payloads(
    disease = disease,
    geography = geography,
    year = year,
    stratify_by = stratify_by
  )

  if (refresh) {
    .forget_atlas_requests(payloads)
    # The later pipeline steps compare county data with state totals
    # (.fetch_state_totals()); refresh those too, so both are equally new.
    if (geography == "county") {
      .forget_atlas_requests(.build_payloads(
        disease,
        "state",
        year,
        stratify_by
      ))
    }
    # impute_suppressed(fit_to = "county_total") fits HIV county data by sex
    # or age to the unstratified county totals
    # (.fetch_county_unstratified()); refresh those too.
    one_way <- length(stratify_by) == 1 && stratify_by %in% c("sex", "age")
    is_hiv <- identical(unname(disease), "hiv")
    if (geography == "county" && is_hiv && one_way) {
      .forget_atlas_requests(.build_payloads("hiv", "county", year, NULL))
    }
  }
  out <- .request_atlas_all(payloads)
  .warn_missing_indicators(out, disease, year)
  out
}

# CDC can answer a request without one of the indicators it asked for (HIV in
# 2025 returned diagnoses but not prevalence). Say so rather than returning
# an incomplete table silently (#116).
.warn_missing_indicators <- function(out, disease, year) {
  lookup <- .disease_indicators()
  wanted <- expand.grid(
    indicator = lookup$indicator[lookup$disease == disease],
    year = sort(unique(year)),
    stringsAsFactors = FALSE
  )
  returned <- paste(as.character(out$indicator), out$year)
  missing <- wanted[!paste(wanted$indicator, wanted$year) %in% returned, ]
  if (nrow(missing) == 0) {
    return(invisible())
  }
  cli::cli_warn(c(
    "CDC returned no rows for {nrow(missing)} requested indicator-year{?s}:",
    rlang::set_names(
      paste(missing$indicator, missing$year),
      rep("*", nrow(missing))
    ),
    "i" = "CDC may not have released them yet, or may not offer them for this
           geography and stratification."
  ))
}
