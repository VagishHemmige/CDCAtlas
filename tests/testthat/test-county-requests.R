# Requests to CDC: county responses combined across states, CDC's errors, the
# time limit and retries. How a request is split into payloads is tested in
# test-build_payload.R. These tests run offline: CDC's responses are replaced
# with made-up ones in the same format, and the on-disk cache is bypassed.

# Combining responses ---------------------------------------------------------

# A made-up CDC response: one row per geography code, indicator and year in
# the payload, in CDC's format (16 columns, coded values). The year comes from
# the payload, so tests of the year check what was requested.
all_geo_ids <- c(
  5000,
  lookup[["state"]]$geo_id,
  county_ids,
  lookup[["msa"]]$geo_id
)
year_ids <- varvals$id[varvals$vtid == 2]

fake_cdc_response <- function(payload) {
  ids <- payload_ids(payload)
  geos <- intersect(ids, all_geo_ids)
  indicators <- intersect(ids, c(203, 205))
  years <- intersect(ids, year_ids)
  rows <- expand.grid(geo = geos, indicator = indicators, year = years)
  sourcedata <- cbind(
    rows$indicator,
    rows$year,
    rows$geo,
    400,
    551,
    601,
    650,
    801,
    12.5,
    10,
    80000,
    NA,
    NA,
    NA,
    NA,
    NA
  )
  jsonlite::toJSON(list(sourcedata = sourcedata), na = "null")
}

# Replace CDC (and the cache) with the made-up responses, counting requests.
local_fake_cdc <- function(env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0
  local_mocked_bindings(.request_atlas = .request_atlas_internal, .env = env)
  local_mocked_bindings(client_rate = function() invisible(NULL), .env = env)
  local_mocked_bindings(
    POST = function(url, body, ...) {
      calls$n <- calls$n + 1
      structure(list(body = fake_cdc_response(body)), class = "fake_response")
    },
    status_code = function(res) 200L,
    content = function(res, ...) as.character(res$body),
    .package = "httr",
    .env = env
  )
  calls
}

test_that("a county request combines every state into one table", {
  calls <- local_fake_cdc()
  out <- suppressMessages(get_atlas("hiv", "county", 2017))

  expect_equal(calls$n, length(unique(county_states)))
  expect_equal(nrow(out), 2 * length(county_ids))
  expect_setequal(out$fips, lookup[["counties"]]$fips)
  expect_equal(sum(duplicated(out[c("fips", "indicator")])), 0)
})

test_that("the combined table keeps column types and labels", {
  local_fake_cdc()
  out <- suppressMessages(get_atlas("hiv", "county", 2017))

  expect_s3_class(out$geography, "factor")
  expect_s3_class(out$indicator, "factor")
  expect_setequal(levels(out$indicator), c("HIV diagnoses", "HIV prevalence"))
  expect_equal(unique(out$year), 2017)
  expect_identical(labelled::var_label(out$cases), "Number of reported cases")
  expect_identical(
    labelled::var_label(out$fips),
    "Census FIPS code for geographical merging with Census data"
  )
})

test_that("get_atlas() returns a tibble, for one request or many (#17)", {
  local_fake_cdc()
  state <- get_atlas("hiv", "state", 2017)
  county <- suppressMessages(get_atlas("hiv", "county", 2017))

  expect_s3_class(state, "tbl_df")
  expect_s3_class(county, "tbl_df")
})

test_that("rate labels fit every indicator, not only incidence", {
  local_fake_cdc()
  out <- suppressMessages(get_atlas("hiv", "state", 2017))

  expect_identical(
    labelled::var_label(out$rate100000),
    "Rate per 100,000 population (percentage for Knowledge of Status; see ?get_atlas for denominators)"
  )
  expect_identical(
    labelled::var_label(out$lowerci_rate),
    "Lower 95% confidence interval of rate"
  )
  expect_identical(
    labelled::var_label(out$upperci_rate),
    "Upper 95% confidence interval of rate"
  )
})

test_that("combined factor levels follow CDC's lookup order, as a single request would", {
  local_fake_cdc()
  out <- suppressMessages(get_atlas("hiv", "county", 2017))
  lookup_order <- intersect(unique(varvals$name), levels(out$geography))
  expect_identical(levels(out$geography), lookup_order)
})

test_that("the year in the result is the year requested", {
  local_fake_cdc()
  out <- suppressMessages(get_atlas("hiv", "state", c(2017, 2022)))
  expect_setequal(out$year, c(2017, 2022))
})

test_that("a state request is still one CDC request", {
  calls <- local_fake_cdc()
  suppressMessages(get_atlas("hiv", "state", 2017))
  expect_equal(calls$n, 1)
})

# Errors ----------------------------------------------------------------------

test_that("a failed request passes on CDC's message without the server trace", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_sleep = function(seconds) invisible(NULL))
  local_mocked_bindings(
    POST = function(...) structure(list(), class = "fake_response"),
    status_code = function(res) 400L,
    content = function(res, ...) {
      paste0(
        '{"success":false,"message":"Execution Timeout Expired.  The timeout period elapsed ',
        'prior to completion of the operation or the server is not responding.    at ',
        'Microsoft.Data.SqlClient.SqlConnection.OnError(SqlException exception)"}'
      )
    },
    .package = "httr"
  )
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 2017))
})

test_that("a failed request without a readable message still explains itself", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(
    POST = function(...) structure(list(), class = "fake_response"),
    status_code = function(res) 404L,
    content = function(res, ...) "<html>Not Found</html>",
    .package = "httr"
  )
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 2017))
})

# Requests --------------------------------------------------------------------

test_that("requests identify the package with a working link", {
  seen <- NULL
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(
    POST = function(url, ...) {
      seen <<- list(...)
      structure(list(), class = "fake_response")
    },
    status_code = function(res) 404L,
    content = function(res, ...) "",
    .package = "httr"
  )
  try(get_atlas("hiv", "state", 2017), silent = TRUE)
  config <- Filter(function(x) inherits(x, "request"), seen)[[1]]
  expect_equal(
    unname(config$headers[["User-Agent"]]),
    "R package CDCAtlas (https://github.com/VagishHemmige/CDCAtlas)"
  )
})

# Request problems (#13) -------------------------------------------------------

test_that("requests have a time limit", {
  seen <- NULL
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(
    POST = function(url, ...) {
      seen <<- list(...)
      structure(list(), class = "fake_response")
    },
    status_code = function(res) 404L,
    content = function(res, ...) "",
    .package = "httr"
  )
  try(get_atlas("hiv", "state", 2017), silent = TRUE)
  configs <- Filter(function(x) inherits(x, "request"), seen)
  timeouts <- unlist(lapply(configs, function(x) x$options$timeout_ms))
  expect_equal(unname(timeouts), .atlas_timeout_seconds * 1000)
})

test_that("a request that can't reach CDC says so", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(
    POST = function(...) stop("Could not resolve host: gis.cdc.gov"),
    .package = "httr"
  )
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 2017))
})

test_that("an empty response says what may be wrong", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(
    POST = function(...) structure(list(), class = "fake_response"),
    status_code = function(res) 200L,
    content = function(res, ...) '{"sourcedata": null}',
    .package = "httr"
  )
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 2017))
})

# Retries (#13) ----------------------------------------------------------------

# Made-up outcomes for one request: a response, or a failure to reach CDC.
cdc_ok <- function(body) list(status = 200L, text = fake_cdc_response(body))
cdc_timeout <- function(body) {
  list(
    status = 400L,
    text = paste0(
      '{"success":false,"message":"Execution Timeout Expired.  The timeout period ',
      'elapsed prior to completion of the operation or the server is not responding."}'
    )
  )
}
cdc_not_found <- function(body) {
  list(status = 404L, text = "<html>Not Found</html>")
}
network_timeout <- function(body) {
  stop("Timeout was reached: Operation timed out")
}
no_host <- function(body) stop("Could not resolve host: gis.cdc.gov")
# curl 6.0.0 and later class their errors by libcurl error code; the message
# below deliberately lacks "Timeout was reached".
curl_timeout <- function(body) {
  rlang::abort(
    "Operation timed out after 120000 milliseconds",
    class = c("curl_error_operation_timedout", "curl_error")
  )
}
curl_no_host <- function(body) {
  rlang::abort(
    "Could not resolve host: gis.cdc.gov",
    class = c("curl_error_couldnt_resolve_host", "curl_error")
  )
}

# Replace CDC with `outcomes`, one per request (the last one repeats), and
# record the requests made and the waits between them.
local_cdc_outcomes <- function(outcomes, env = parent.frame()) {
  calls <- new.env()
  calls$n <- 0
  calls$waits <- numeric()
  local_mocked_bindings(.request_atlas = .request_atlas_internal, .env = env)
  local_mocked_bindings(client_rate = function() invisible(NULL), .env = env)
  local_mocked_bindings(
    .atlas_sleep = function(seconds) calls$waits <- c(calls$waits, seconds),
    .env = env
  )
  local_mocked_bindings(
    POST = function(url, body, ...) {
      calls$n <- calls$n + 1
      outcome <- outcomes[[min(calls$n, length(outcomes))]]
      structure(outcome(body), class = "fake_response")
    },
    status_code = function(res) res$status,
    content = function(res, ...) as.character(res$text),
    .package = "httr",
    .env = env
  )
  calls
}

test_that("a CDC timeout is retried twice, then fails", {
  calls <- local_cdc_outcomes(list(cdc_timeout))
  expect_snapshot(error = TRUE, get_atlas("hiv", "state", 2017))
  expect_equal(calls$n, 3)
  expect_equal(calls$waits, c(10, 30))
})

test_that("a request that works on a retry returns the data", {
  expected <- local({
    local_cdc_outcomes(list(cdc_ok))
    suppressMessages(get_atlas("hiv", "state", 2017))
  })
  calls <- local_cdc_outcomes(list(cdc_timeout, cdc_ok))
  out <- suppressMessages(get_atlas("hiv", "state", 2017))
  expect_equal(calls$n, 2)
  expect_equal(calls$waits, 10)
  expect_identical(out, expected)
})

test_that("a network timeout is retried", {
  calls <- local_cdc_outcomes(list(network_timeout, network_timeout, cdc_ok))
  suppressMessages(get_atlas("hiv", "state", 2017))
  expect_equal(calls$n, 3)
  expect_equal(calls$waits, c(10, 30))
})

test_that("a timeout classed by curl is retried, whatever its message", {
  calls <- local_cdc_outcomes(list(curl_timeout, cdc_ok))
  suppressMessages(get_atlas("hiv", "state", 2017))
  expect_equal(calls$n, 2)
  expect_equal(calls$waits, 10)
})

test_that("other failures are not retried", {
  for (outcome in list(cdc_not_found, no_host, curl_no_host)) {
    calls <- local_cdc_outcomes(list(outcome, cdc_ok))
    expect_error(get_atlas("hiv", "state", 2017))
    expect_equal(calls$n, 1)
    expect_equal(calls$waits, numeric())
  }
})
