


#' Retrieve data from CDC AtlasPlus
#'
#' @param disease Character scalar, e.g. "Gonorrhea"
#' @param geography "state", "county", "national"
#' @param restrict_to Optional vector of states to limit geography
#' @param year Numeric vector of years, e.g. 2018:2022.  Valid values are 2000-2023.
#' @param stratify_by Optional vector of variables to stratify by.  Options are "age", "race", "sex"
#'
#' @return A tibble
#' @export
#'

get_atlas <- function(
    disease, #valid options are any of: "chlaymydia", "gonorrhea"
    geography, #valid options are one of: "national", "region", "state", "county"
    year, #Valid years are 2000-2023
    stratify_by=NULL)
{

  #.validate_options()

  url_data <- "https://gis.cdc.gov/grasp/AtlasPlus/qtOutputData"
  payload <- .build_payload(
    disease=disease,
    geography=geography,
    year=year,
    stratify_by=stratify_by
  )

  print(glue::glue("Payload: {payload}"))

  res <- httr::POST(
    url_data,
    body = payload,
    encode = "json",
    httr::add_headers(
      "Accept" = "application/json, text/javascript, */*; q=0.01",
      "Content-Type" = "application/json; charset=utf-8",
      "X-Requested-With" = "XMLHttpRequest"
    )
  )

  if (httr::status_code(res) != 200)
  stop(paste("❌ Request failed:", httr::status_code(res)))

  dat <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))

  if (is.null(dat$sourcedata))
    stop("❌ No data returned. Check payload IDs.")

  # --- Step 5: Convert to a data frame and label columns ---
  df <- as.data.frame(dat$sourcedata)

df
}
