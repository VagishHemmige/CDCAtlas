# County requests are split into one CDC request per state, then combined.
# These tests run offline: CDC's responses are replaced with made-up ones in
# the same format, and the on-disk cache is bypassed.

payload_ids <- function(payload) {
  as.integer(strsplit(payload$VariableIDs, ",")[[1]])
}

county_ids <- lookup[["counties"]]$geo_id
county_states <- substr(lookup[["counties"]]$fips, 1, 2)

# Splitting -------------------------------------------------------------------

test_that("county requests are split into one payload per state", {
  payloads <- .build_payloads("hiv", "county", 2017)

  expect_length(payloads, length(unique(county_states)))
  expect_named(payloads, sort(unique(county_states)))
})

test_that("together the state payloads request every county exactly once", {
  payloads <- .build_payloads("hiv", "county", 2017)
  requested <- unlist(lapply(payloads, function(p) intersect(payload_ids(p), county_ids)))

  expect_setequal(requested, county_ids)
  expect_equal(anyDuplicated(requested), 0)
})

test_that("each state payload asks for that state's counties only", {
  payloads <- .build_payloads("hiv", "county", 2017)
  al <- intersect(payload_ids(payloads[["01"]]), county_ids)

  expect_setequal(al, county_ids[county_states == "01"])
})

test_that("state payloads keep the same indicator, year and category codes", {
  full <- payload_ids(.build_payload("hiv", "county", 2017, stratify_by = "sex"))
  non_geo <- setdiff(full, county_ids)

  for (payload in .build_payloads("hiv", "county", 2017, stratify_by = "sex")) {
    expect_equal(setdiff(payload_ids(payload), county_ids), non_geo)
  }
})

test_that("national requests use CDC's United States code, 5000", {
  ids <- payload_ids(.build_payload("hiv", "national", 2022))
  expect_contains(ids, 5000)
  expect_equal(intersect(ids, 500), integer(0))
  expect_identical(varvals$name[varvals$id == 5000], "United States")
})

test_that("other geographies are still sent as one payload", {
  for (geography in c("national", "state", "msa")) {
    payloads <- .build_payloads("hiv", geography, 2022)
    expect_length(payloads, 1)
    expect_identical(payloads[[1]], .build_payload("hiv", geography, 2022))
  }
})

# Combining responses ---------------------------------------------------------

# A made-up CDC response: one row per geography code and indicator, in CDC's
# format (16 columns, coded values).
all_geo_ids <- c(5000, lookup[["state"]]$geo_id, county_ids, lookup[["msa"]]$geo_id)

fake_cdc_response <- function(payload) {
  ids <- payload_ids(payload)
  geos <- intersect(ids, all_geo_ids)
  indicators <- intersect(ids, c(203, 205))
  rows <- expand.grid(geo = geos, indicator = indicators)
  sourcedata <- cbind(
    rows$indicator, 539, rows$geo, 400, 551, 601, 650, 801,
    12.5, 10, 80000, NA, NA, NA, NA, NA
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
  expect_identical(labelled::var_label(out$fips), "Census FIPS code for geographical merging with Census data")
})

test_that("combined factor levels follow CDC's lookup order, as a single request would", {
  local_fake_cdc()
  out <- suppressMessages(get_atlas("hiv", "county", 2017))
  lookup_order <- intersect(unique(varvals$name), levels(out$geography))
  expect_identical(levels(out$geography), lookup_order)
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
  local_mocked_bindings(
    POST = function(...) structure(list(), class = "fake_response"),
    status_code = function(res) 400L,
    content = function(res, ...) paste0(
      '{"success":false,"message":"Execution Timeout Expired.  The timeout period elapsed ',
      'prior to completion of the operation or the server is not responding.    at ',
      'Microsoft.Data.SqlClient.SqlConnection.OnError(SqlException exception)"}'
    ),
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
