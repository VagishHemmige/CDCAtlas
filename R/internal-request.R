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

.request_atlas_internal <- function(payload) {
  #Url for CDC Atlas
  url_data <- "https://gis.cdc.gov/api/AtlasPlus/qtOutputData"

  #Call CDC site, rate limited and with a time limit. A network failure gets a
  #plain message, a failed request CDC's own explanation, and a timeout is
  #retried (#13).
  res <- .with_atlas_retries(function() {
    client_rate()
    res <- .atlas_call(httr::POST(
      url_data,
      body = payload,
      encode = "json",
      .atlas_headers(),
      httr::timeout(.atlas_timeout_seconds)
    ))
    status <- httr::status_code(res)
    if (status != 200) {
      .abort_cdc_status(res, status)
    }
    res
  })

  #Encode JSON
  dat <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))

  if (is.null(dat$sourcedata)) {
    cli::cli_abort(
      c(
        "CDC AtlasPlus returned no data for this request.",
        "i" = "CDC may not have released this year for this disease yet, or
             may not report this combination of geography and strata.",
        "i" = "Check what AtlasPlus offers at
             {.url https://gis.cdc.gov/grasp/nchhstpatlas/main.html}."
      ),
      call = NULL
    )
  }

  # --- Convert to a data frame and label columns --
  #Uses internal package data varvals
  df <- as.data.frame(dat$sourcedata) |>
    #Add fips
    dplyr::left_join(
      dplyr::select(varvals, "id", "fips"),
      by = c("V3" = "id")
    ) |>

    #Decode data
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("V1", "V3", "V4", "V5", "V6", "V7", "V8")),
        ~ factor(.x, levels = varvals$id, labels = varvals$name)
      )
    ) |>
    dplyr::mutate(V2 = .data$V2 + 1478) |>
    dplyr::rename(
      indicator = "V1",
      year = "V2",
      geography = "V3",
      data_status = "V4",
      race_ethnicity = "V5",
      sex = "V6",
      age = "V7",
      transmission = "V8",
      rate100000 = "V9",
      cases = "V10",
      population = "V11",
      lowerci_rate = "V12",
      upperci_rate = "V13",
      rse = "V14",
      lowerci_cases = "V15",
      upperci_cases = "V16"
    )

  df <- tibble::as_tibble(.label_atlas_columns(df))

  return(df)
}


# Seconds to wait for CDC. One state's counties usually come back in a few
# seconds, but CDC's database can be slow; its own timeout is reported as an
# HTTP 400 (see .abort_cdc_status()).
.atlas_timeout_seconds <- 120

# Seconds to wait before each retry of a request that timed out (#13).
.atlas_retry_waits <- c(10, 30)

# Calls `send()`, and calls it again after each wait in .atlas_retry_waits if
# CDC or the network times out (errors of class "cdcatlas_timeout"). Other
# errors, and a timeout on the last try, stop at once.
.with_atlas_retries <- function(send) {
  waits <- .atlas_retry_waits
  for (retry in seq_along(waits)) {
    result <- tryCatch(send(), cdcatlas_timeout = function(cnd) cnd)
    if (!inherits(result, "cdcatlas_timeout")) {
      return(result)
    }
    cli::cli_inform(c(
      "!" = "CDC AtlasPlus timed out. Trying again in {waits[retry]} seconds
             (retry {retry} of {length(waits)})."
    ))
    .atlas_sleep(waits[retry])
  }
  send()
}

# Kept separate so tests can skip the waiting.
.atlas_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

# Runs an httr request, turning a failure to reach CDC (no connection, DNS,
# time limit) into a message that says so (#13). Running out of time is
# classed "cdcatlas_timeout", so it can be retried.
.atlas_call <- function(request) {
  tryCatch(
    request,
    error = function(e) {
      timed_out <- grepl(
        "Timeout was reached",
        conditionMessage(e),
        fixed = TRUE
      )
      cli::cli_abort(
        c(
          "Couldn't reach CDC AtlasPlus.",
          "x" = "{conditionMessage(e)}",
          "i" = "Check your internet connection, or try again later; CDC's
                 site is sometimes slow or down."
        ),
        class = if (timed_out) "cdcatlas_timeout",
        call = NULL
      )
    }
  )
}

# Stops with CDC's own explanation when it gives one. CDC reports its database
# timing out as HTTP 400 "Execution Timeout Expired", which is usually
# temporary; that error is classed "cdcatlas_timeout", so it can be retried.
.abort_cdc_status <- function(res, status) {
  cdc_message <- .cdc_error_message(res)
  timed_out <- !is.null(cdc_message) &&
    grepl("Execution Timeout Expired", cdc_message, fixed = TRUE)
  cli::cli_abort(
    c(
      "CDC AtlasPlus request failed (HTTP status {status}).",
      "i" = if (!is.null(cdc_message)) "CDC says: {cdc_message}",
      "i" = if (timed_out) {
        "CDC's database timed out. This is usually temporary: try again in a
         few minutes."
      } else {
        "If this keeps happening, CDC may have changed AtlasPlus. Please
         report it at {.url https://github.com/VagishHemmige/CDCAtlas/issues}."
      }
    ),
    class = if (timed_out) "cdcatlas_timeout",
    call = NULL
  )
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


# Headers for every AtlasPlus request.
.atlas_headers <- function() {
  httr::add_headers(
    "Accept" = "application/json, text/javascript, */*; q=0.01",
    "Content-Type" = "application/json; charset=utf-8",
    "X-Requested-With" = "XMLHttpRequest",
    "User-Agent" = "R package CDCAtlas (https://github.com/VagishHemmige/CDCAtlas)"
  )
}


# CDC's AtlasPlus set-up data: the code table (`varvals`) and the menus the web
# app is built from. data-raw/DATASET.R builds R/sysdata.rda from it, and the
# live tests compare it with the stored copy. Not cached: about 2.6 MB, needed
# only when rebuilding or checking the lookups.
.atlas_init_data <- function() {
  client_rate()
  res <- .atlas_get("https://gis.cdc.gov/api/AtlasPlus/getInitData/00")

  status <- httr::status_code(res)
  if (status != 200) {
    cdc_message <- .cdc_error_message(res)
    cli::cli_abort(
      c(
        "CDC AtlasPlus set-up request failed (HTTP status {status}).",
        "i" = if (!is.null(cdc_message)) "CDC says: {cdc_message}"
      ),
      call = NULL
    )
  }

  init <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  if (is.null(init$varvals)) {
    cli::cli_abort(
      c(
        "CDC AtlasPlus set-up data has no {.field varvals} code table.",
        "i" = "CDC may have changed AtlasPlus."
      ),
      call = NULL
    )
  }
  init
}

# The one GET request, kept separate so tests can replace it.
.atlas_get <- function(url) {
  .atlas_call(httr::GET(
    url,
    .atlas_headers(),
    httr::timeout(.atlas_timeout_seconds)
  ))
}


# Variable labels and unused factor levels. Applied to each response, and again
# after combining several responses, because binding rows drops the labels.
.label_atlas_columns <- function(df) {
  labelled::var_label(
    df$indicator
  ) <- "Indicator (disease or condition being measured)"
  labelled::var_label(df$year) <- "Calendar year"
  labelled::var_label(
    df$geography
  ) <- "Geographic level (state, county, national, MSA, etc.)"
  labelled::var_label(df$data_status) <- "Data completeness / suppression notes"
  labelled::var_label(df$race_ethnicity) <- "Race and ethnicity category"
  labelled::var_label(df$sex) <- "Sex category"
  labelled::var_label(df$age) <- "Age group"
  labelled::var_label(df$transmission) <- "Transmission category (HIV/STI)"
  labelled::var_label(
    df$rate100000
  ) <- "Rate per 100,000 population (percentage for Knowledge of Status; see ?get_atlas for denominators)"
  labelled::var_label(df$cases) <- "Number of reported cases"
  labelled::var_label(df$population) <- "Population denominator"
  labelled::var_label(
    df$lowerci_rate
  ) <- "Lower 95% confidence interval of rate"
  labelled::var_label(
    df$upperci_rate
  ) <- "Upper 95% confidence interval of rate"
  labelled::var_label(df$rse) <- "Relative standard error of rate"
  labelled::var_label(
    df$lowerci_cases
  ) <- "Lower 95% confidence interval of case count"
  labelled::var_label(
    df$upperci_cases
  ) <- "Upper 95% confidence interval of case count"
  labelled::var_label(
    df$fips
  ) <- "Census FIPS code for geographical merging with Census data"

  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.factor), ~ droplevels(.x))
    )

  df
}


# Send one or more payloads and combine the results into one data frame.
.request_atlas_all <- function(payloads) {
  if (length(payloads) == 1) {
    return(.request_atlas(payloads[[1]]))
  }

  results <- vector("list", length(payloads))
  for (i in cli::cli_progress_along(
    payloads,
    "Downloading from CDC AtlasPlus"
  )) {
    results[[i]] <- .request_atlas(payloads[[i]])
  }

  # Binding puts factor levels in order of first appearance (state by state).
  # Restore the order a single request gives: CDC's lookup order.
  combined <- dplyr::bind_rows(results) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.factor),
      ~ factor(
        as.character(.x),
        levels = intersect(unique(varvals$name), unique(as.character(.x)))
      )
    ))

  .label_atlas_columns(combined)
}


#' Internal rate limiter
#'
#' Uses ratelimitr to ensure we don't overwhelm AtlasPlus.
#' @keywords internal
client_rate <- ratelimitr::limit_rate(
  function() Sys.sleep(0),
  ratelimitr::rate(n = 5, period = 1) # max 5 requests per second
)


# Cached AtlasPlus requests.
#
# Responses are kept on disk for 30 days, so CDC revisions reach users within a
# month, and get_atlas(refresh = TRUE) replaces them sooner. The age counts
# from the download: reading a response does not make it newer, so "fifo".
# The cache is set up at the first request, not when the package is built or
# loaded, so it is always in the current user's cache folder.
.atlas_cache_max_age <- 30 * 24 * 60 * 60

.atlas_cache <- new.env(parent = emptyenv())

.atlas_cache_dir <- function() {
  file.path(tools::R_user_dir("CDCAtlas", "cache"), "atlasplus")
}

# The memoised request function, made again if the cache folder has changed
# (for example, R_USER_CACHE_DIR set after the first request) or has been
# deleted: cachem refuses to use a cache whose folder is gone.
.atlas_cached_request <- function() {
  dir <- .atlas_cache_dir()
  if (!identical(.atlas_cache$dir, dir) || !dir.exists(dir)) {
    .atlas_cache$request <- memoise::memoise(
      .request_atlas_download,
      cache = cachem::cache_disk(
        dir,
        max_age = .atlas_cache_max_age,
        evict = "fifo"
      )
    )
    .atlas_cache$dir <- dir
  }
  .atlas_cache$request
}

# Looks up .request_atlas_internal() at each call, so tests can replace it.
.request_atlas_download <- function(payload) {
  .request_atlas_internal(payload)
}

.request_atlas <- function(payload) {
  .atlas_cached_request()(payload)
}

# Drop the cached responses to these payloads, so the next request downloads
# them again.
.forget_atlas_requests <- function(payloads) {
  drop <- memoise::drop_cache(.atlas_cached_request())
  for (payload in payloads) {
    drop(payload)
  }
  invisible()
}
