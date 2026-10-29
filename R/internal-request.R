#' Internal AtlasPlus request helper (uncached)
#'
#' @description
#' Low-level helper that sends a POST JSON payload to the
#' AtlasPlus `qtOutputData` endpoint.
#'
#' @details
#' Requires the internal `varvals` lookup table for factor labeling.
#' Implements rate limiting and user-agent headers for API politeness.
#'
#' @keywords internal

.request_atlas_internal<- function(payload) {
#Url for CDC Atlas
url_data <- "https://gis.cdc.gov/api/AtlasPlus/qtOutputData"

#Rate limiter
client_rate()  # <-- Invoke the rate limiter

#Call CDC site
res <- httr::POST(
  url_data,
  body = payload,
  encode = "json",
  httr::add_headers(
    "Accept" = "application/json, text/javascript, */*; q=0.01",
    "Content-Type" = "application/json; charset=utf-8",
    "X-Requested-With" = "XMLHttpRequest",
    "User-Agent" = "R package CDCAtlas (https://VagishHemmige.github.com/CDCAtlas)"
  )
)

#Error handling: pass on CDC's own explanation when it gives one
status <- httr::status_code(res)
if (status != 200) {
  cdc_message <- .cdc_error_message(res)
  cli::cli_abort(
    c(
      "CDC AtlasPlus request failed (HTTP status {status}).",
      "i" = if (!is.null(cdc_message)) "CDC says: {cdc_message}",
      "i" = "If this keeps happening, CDC may have changed AtlasPlus. Please report it at {.url https://github.com/VagishHemmige/CDCAtlas/issues}."
    ),
    call = NULL
  )
}

#Encode JSON
dat <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))

if (is.null(dat$sourcedata))
  stop("No data returned. Check payload IDs.")

# --- Convert to a data frame and label columns --
#Uses internal package data varvals
df <- as.data.frame(dat$sourcedata)%>%
  #Add fips
  dplyr::left_join(dplyr::select(varvals, id, fips), by=(c("V3"="id")))%>%

  #Decode data
  dplyr::mutate(
    across(
      c(V1, V3:V8),    # <- put your coded columns here
      ~factor(.x, levels = varvals$id, labels = varvals$name)
    )
  )%>%
  dplyr::mutate(V2=V2+1478)%>%
  dplyr::rename(
    indicator=V1,
    year=V2,
    geography=V3,
    data_status=V4,
    race_ethnicity=V5,
    sex=V6,
    age=V7,
    transmission=V8,
    rate100000=V9,
    cases=V10,
    population=V11,
    lowerci_rate=V12,
    upperci_rate=V13,
    rse=V14,
    lowerci_cases=V15,
    upperci_cases=V16
  )


df <- .label_atlas_columns(df)

return(df)
}


# Short form of CDC's error message, without the server's stack trace.
# Returns NULL when the response has no readable message.
.cdc_error_message <- function(res) {
  message <- tryCatch(
    jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))$message,
    error = function(e) NULL
  )
  if (!is.character(message) || length(message) != 1) {
    return(NULL)
  }
  message <- sub("\\s{2,}at\\s.*$", "", message)
  trimws(gsub("\\s+", " ", message))
}


# Variable labels and unused factor levels. Applied to each response, and again
# after combining several responses, because binding rows drops the labels.
.label_atlas_columns <- function(df) {

  labelled::var_label(df$indicator)       <- "Indicator (disease or condition being measured)"
  labelled::var_label(df$year)            <- "Calendar year"
  labelled::var_label(df$geography)       <- "Geographic level (state, county, national, MSA, etc.)"
  labelled::var_label(df$data_status)     <- "Data completeness / suppression notes"
  labelled::var_label(df$race_ethnicity)  <- "Race and ethnicity category"
  labelled::var_label(df$sex)             <- "Sex category"
  labelled::var_label(df$age)             <- "Age group"
  labelled::var_label(df$transmission)    <- "Transmission category (HIV/STI)"
  labelled::var_label(df$rate100000)      <- "Incidence rate per 100,000 population"
  labelled::var_label(df$cases)           <- "Number of reported cases"
  labelled::var_label(df$population)      <- "Population denominator"
  labelled::var_label(df$lowerci_rate)    <- "Lower 95% confidence interval of incidence rate"
  labelled::var_label(df$upperci_rate)    <- "Upper 95% confidence interval of incidence rate"
  labelled::var_label(df$rse)             <- "Relative standard error of rate"
  labelled::var_label(df$lowerci_cases)   <- "Lower 95% confidence interval of case count"
  labelled::var_label(df$upperci_cases)   <- "Upper 95% confidence interval of case count"
  labelled::var_label(df$fips)   <- "Census FIPS code for geographical merging with Census data"

  df <- df |>
    dplyr::mutate(
      dplyr::across(where(is.factor), ~ droplevels(.x))
    )

  df
}


# Send one or more payloads and combine the results into one data frame.
.request_atlas_all <- function(payloads) {
  if (length(payloads) == 1) {
    return(.request_atlas(payloads[[1]]))
  }

  results <- vector("list", length(payloads))
  for (i in cli::cli_progress_along(payloads, "Downloading county data by state")) {
    results[[i]] <- .request_atlas(payloads[[i]])
  }

  # Binding puts factor levels in order of first appearance (state by state).
  # Restore the order a single request gives: CDC's lookup order.
  combined <- dplyr::bind_rows(results) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.factor),
      ~ factor(as.character(.x), levels = intersect(unique(varvals$name), unique(as.character(.x))))
    ))

  .label_atlas_columns(combined)
}


#' Internal rate limiter
#'
#' Uses ratelimitr to ensure we don't overwhelm AtlasPlus.
#' @keywords internal
client_rate <- ratelimitr::limit_rate(
  function() Sys.sleep(0),
  ratelimitr::rate(n = 5, period = 1)  # max 5 requests per second
)



#' Internal AtlasPlus request helper with caching
#'
#' @description
#' Cached wrapper around `.request_atlas_uncached()`.
#'
#' @keywords internal
.request_atlas <- memoise::memoise(
  .request_atlas_internal,
  cache = memoise::cache_filesystem(tools::R_user_dir("CDCAtlas", "cache"))
)

