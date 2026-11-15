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
#' @param geography Character scalar. One of `"national"`, `"state"`,
#' `"county"`, or `"msa"`. Region-level queries are not supported yet.
#' @param extrapolate_to_tract `r lifecycle::badge("deprecated")` No longer
#'   supported. Use [impute_suppressed()] and then [extrapolate_from_county()].
#' @param year Integer vector of years. Valid values are from 2000 to 2023.
#' Can be a single year (e.g., `2022`) or a sequence (e.g., `2018:2022`).
#' @param stratify_by Optional character vector. Zero, one, or two of
#' `"age"`, `"race"`, `"sex"`. If `NULL`, totals (unstratified) are returned.
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
#'   \item{rate100000}{Rate per 100,000 population (numeric)}
#'   \item{cases}{Case count (numeric)}
#'   \item{population}{Population denominator (numeric)}
#'   \item{lowerci_rate, upperci_rate}{95% CI bounds for rate (numeric). Usually
#'     empty (`NA`) for reported cases; given for the modeled estimates.}
#'   \item{rse}{Relative standard error (numeric)}
#'   \item{lowerci_cases, upperci_cases}{95% CI bounds for cases (numeric).
#'     Usually empty (`NA`) for reported cases; given for the modeled
#'     estimates.}
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
    extrapolate_to_tract=deprecated())
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



#Construct the payloads for the call to the CDC site based on what users request.
#County requests are split into one payload per state.
payloads <- .build_payloads(
    disease=disease,
    geography=geography,
    year=year,
    stratify_by=stratify_by
  )

.request_atlas_all(payloads)
}
