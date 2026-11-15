#' Retrieve STI surveillance data from CDC AtlasPlus
#'
#' `get_atlas()` provides a programmatic interface to the CDC AtlasPlus platform,
#' returning case counts, population denominators, and rate estimates for
#' sexually transmitted infection (STI) surveillance. The function submits a
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
#' - `"chlamydia"`
#' - `"gonorrhea"`
#' - `"adult syphilis"`
#' - `"congenital syphilis"`
#' - `"tuberculosis"`
#' - `"hiv"` (will provide confirmed diagnoses)
#' - `"estimate"` (estimated HIV counts)
#' - `"hepatitis"`
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
#' `stratify_by = c("race", "sex")` returns all race × sex combinations
#' for each geography-year pair.
#'
#' @param disease A single disease name (e.g. `"gonorrhea"`).
#' Multiple diseases are not currently supported in a single request.
#' @param geography Character scalar. One of `"national"`, `"region"` (HIV
#' estimates only), `"state"`, `"county"`, or `"msa"`.
#' @param extrapolate_to_tract `r lifecycle::badge("deprecated")` No longer
#'   supported. Use [impute_suppressed()] and then [extrapolate_from_county()].
#' @param year Integer vector of years, from 2000 to the latest year in the
#' package's copy of CDC's code table (2025 in this version). CDC lists a year
#' before every disease has data for it; such a request returns no data.
#' Can be a single year (e.g., `2022`) or a sequence (e.g., `2018:2022`).
#' @param stratify_by Optional character vector. Zero, one, or two of
#' `"age"`, `"race"`, `"sex"`. If `NULL`, totals (unstratified) are returned.
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
#'   \item{rate100000}{Incidence rate per 100,000 (numeric)}
#'   \item{cases}{Case count (numeric)}
#'   \item{population}{Population denominator (numeric)}
#'   \item{lowerci_rate, upperci_rate}{95% CI bounds for rate (numeric)}
#'   \item{rse}{Relative standard error (numeric)}
#'   \item{lowerci_cases, upperci_cases}{95% CI bounds for cases (numeric)}
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
    stratify_by=NULL,
    extrapolate_to_tract=deprecated(),
    refresh=FALSE)
{

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
    disease=disease,
    geography=geography,
    year=year,
    stratify_by=stratify_by
  )

if (refresh) {
  .forget_atlas_requests(payloads)
  # The later pipeline steps compare county data with state totals
  # (.fetch_state_totals()); refresh those too, so both are equally new.
  if (geography == "county") {
    .forget_atlas_requests(.build_payloads(disease, "state", year, stratify_by))
  }
}
.request_atlas_all(payloads)
}
