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
#' By default, the function returns data directly from the requested AtlasPlus
#' geography. If `extrapolate_to_tract = TRUE`, county-level results are
#' post-processed into census tract-level estimates using helper functions in
#' this package. These tract estimates are derived estimates and are not
#' directly returned by CDC AtlasPlus.
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
#' @param extrapolate_to_tract Logical scalar. If `FALSE`, return the AtlasPlus
#'   results for the requested geography. If `TRUE`, pass the returned data to
#'   the package's tract extrapolation workflow. Extrapolation requires
#'   `geography = "county"`, at most one `stratify_by` variable, and the
#'   tidycensus package. Age-stratified extrapolation supports only
#'   `disease = "hiv"`.
#' @param fill_suppressed How suppressed county counts are filled in before
#'   tract extrapolation. `"state_rate"` (default) uses the state's rate in the
#'   same stratum times the county's population, capped at 4 because CDC
#'   suppresses counts of 1 to 4. `"zero"` sets them to 0. Ignored unless
#'   `extrapolate_to_tract = TRUE`.
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
#'   \item{rate100000}{Incidence rate per 100,000 (numeric)}
#'   \item{cases}{Case count (numeric)}
#'   \item{population}{Population denominator (numeric)}
#'   \item{lowerci_rate, upperci_rate}{95\\% CI bounds for rate (numeric)}
#'   \item{rse}{Relative standard error (numeric)}
#'   \item{lowerci_cases, upperci_cases}{95\\% CI bounds for cases (numeric)}
#'   \item{FIPS code (character)}
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
#' ## Tract extrapolation
#'
#' With `extrapolate_to_tract = TRUE`, each county's count is shared across its
#' census tracts in proportion to the tract's population in the same stratum,
#' from the ACS 5-year estimates ending in `year` (ages 13 and older for HIV
#' indicators, all ages otherwise). The result has one row per tract, stratum
#' and indicator. Tract estimates add up to the county count in every stratum.
#'
#' - Suppressed counties are filled in as set by `fill_suppressed`; the
#'   `county_cases_source` column says which counts were published.
#' - Counties CDC marks "Data not available" with no count are dropped.
#' - Cases in a state's total that no county accounts for (records without a
#'   valid county) are not allocated to tracts. They are reported per state in
#'   `attr(x, "no_county_cases")`.
#' - If no tract in a county has population in a stratum, that stratum is
#'   shared by the tracts' total population; the `allocation` column says
#'   which rule was used.
#' - Race populations come from ACS table B03002, which has no age detail; for
#'   HIV they are scaled by each tract's share of residents aged 13 and older.
#' - Tract estimates are modelled, not observed: they assume the same
#'   prevalence across tracts within a county and stratum.
#'
#' @seealso [cdc_atlas_meta()] for metadata lookup and variable dictionaries.
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
#' get_atlas(
#'   disease = "gonorrhea",
#'   geography = "county",
#'   year = 2022,
#'   extrapolate_to_tract = TRUE
#' )
#' }
#'
#' @export

get_atlas <- function(
    disease,
    geography,
    year,
    stratify_by=NULL,
    extrapolate_to_tract=FALSE,
    fill_suppressed=c("state_rate", "zero"))
{

#Validate input
  .validate_get_atlas_inputs(
    disease = disease,
    geography = geography,
    year = year,
    stratify_by = stratify_by,
    extrapolate_to_tract = extrapolate_to_tract,
    call = rlang::current_call()
  )
  fill_suppressed <- rlang::arg_match(fill_suppressed)



#Construct the payloads for the call to the CDC site based on what users request.
#County requests are split into one payload per state.
payloads <- .build_payloads(
    disease=disease,
    geography=geography,
    year=year,
    stratify_by=stratify_by
  )

df <- .request_atlas_all(payloads)

if (extrapolate_to_tract==TRUE){

  payload_state <- .build_payload(
    disease=disease,
    geography="state",
    year=year,
    stratify_by=stratify_by
  )
  df<-.extrapolate_to_tract(df, payload_state, stratify_by, suppressed = fill_suppressed)

}

df
}
