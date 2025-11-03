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
#'
#'
#' **Supported diseases**
#' - `"chlamydia"`
#' - `"gonorrhea"`
#' - `"syphilis"`
#'
#' **Geography**
#' - `"national"`
#' - `"region"`
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
#' @param disease Character scalar. A single disease name (e.g. `"gonorrhea"`).
#' Multiple diseases are not currently supported in a single request.
#' @param geography Character scalar. One of `"national"`, `"region"`, `"state"`,
#' `"county"`, or `"msa"`.
#' @param restrict_to Optional character vector restricting results to specific
#' states or counties (case-insensitive). If omitted, all units at that
#' geographic level are returned.
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
#'   \item{lowerci_rate, upperci_rate}{95% CI bounds for rate (numeric)}
#'   \item{rse}{Relative standard error (numeric)}
#'   \item{lowerci_cases, upperci_cases}{95% CI bounds for cases (numeric)}
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
#' # Limit to specific states
#' get_atlas(
#'   disease = "gonorrhea",
#'   geography = "state",
#'   year = 2022,
#'   restrict_to = c("NY", "CA", "TX")
#' )
#' }
#'
#' @export

get_atlas <- function(
    disease, #valid options are any of: "chlaymydia", "gonorrhea"
    geography, #valid options are one of: "national", "region", "state", "county", "msa"
    year, #Valid years are 2000-2023
    stratify_by=NULL)
{

#In future, should validate input
#.validate_options()



#Construct the payload for the call to the CDC site based on what users request
payload <- .build_payload(
    disease=disease,
    geography=geography,
    year=year,
    stratify_by=stratify_by
  )

#Use in debugging; currently commented oout
#print(glue::glue("Payload: {payload}"))

df<-.request_atlas(payload)
df
}
