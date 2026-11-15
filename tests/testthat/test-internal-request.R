fake_init_response <- function(json, status = 200L) {
  structure(
    list(
      url = "https://gis.cdc.gov/api/AtlasPlus/getInitData/00",
      status_code = status,
      headers = list(`content-type` = "application/json; charset=utf-8"),
      content = charToRaw(json)
    ),
    class = "response"
  )
}

test_that(".atlas_init_data() returns CDC's code table", {
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_get = function(url) {
    fake_init_response(
      '{"varvals": [{"id": 5001, "name": "Alabama", "vtid": 3, "fips": "01"}],
        "combo": {}}'
    )
  })

  init <- .atlas_init_data()

  expect_named(init, c("varvals", "combo"), ignore.order = TRUE)
  expect_equal(init$varvals$id, 5001)
  expect_equal(init$varvals$fips, "01")
})

test_that(".atlas_init_data() errors when CDC refuses the request", {
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_get = function(url) {
    fake_init_response('{"message": "Server busy"}', status = 503L)
  })

  expect_snapshot(.atlas_init_data(), error = TRUE)
})

test_that(".atlas_init_data() errors when the code table is missing", {
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_get = function(url) {
    fake_init_response('{"combo": {}}')
  })

  expect_snapshot(.atlas_init_data(), error = TRUE)
})

# A throwaway user cache folder that doesn't exist yet, and a stand-in for the
# download that counts how often CDC would be asked.
local_request_cache <- function(env = parent.frame()) {
  cache_home <- file.path(withr::local_tempdir(.local_envir = env), "new", "folder")
  withr::local_envvar(R_USER_CACHE_DIR = cache_home, .local_envir = env)
  calls <- new.env()
  calls$n <- 0
  local_mocked_bindings(
    .request_atlas_internal = function(payload) {
      calls$n <- calls$n + 1
      data.frame(cases = calls$n, fips = "01")
    },
    .env = env
  )
  calls
}

cached_files <- function() {
  list.files(.atlas_cache_dir(), full.names = TRUE)
}

test_that("the cache folder is made at the first request, inside the user's cache", {
  local_request_cache()
  expect_equal(dir.exists(.atlas_cache_dir()), FALSE)

  .request_atlas(list(a = 1))

  expect_equal(dir.exists(.atlas_cache_dir()), TRUE)
  expect_equal(
    startsWith(.atlas_cache_dir(), Sys.getenv("R_USER_CACHE_DIR")),
    TRUE
  )
  expect_length(cached_files(), 1)
})

test_that("a repeated request is answered from the cache", {
  calls <- local_request_cache()

  first <- .request_atlas(list(a = 1))
  second <- .request_atlas(list(a = 1))

  expect_equal(calls$n, 1)
  expect_equal(second, first)
})

test_that("a cached response older than 30 days is downloaded again", {
  calls <- local_request_cache()
  .request_atlas(list(a = 1))
  Sys.setFileTime(cached_files(), Sys.time() - 31 * 24 * 60 * 60)

  out <- .request_atlas(list(a = 1))

  expect_equal(calls$n, 2)
  expect_equal(out$cases, 2)
})

test_that("reading a cached response doesn't make it count as newer", {
  local_request_cache()
  .request_atlas(list(a = 1))
  old <- Sys.time() - 20 * 24 * 60 * 60
  Sys.setFileTime(cached_files(), old)

  .request_atlas(list(a = 1))

  expect_equal(
    abs(as.numeric(file.mtime(cached_files())) - as.numeric(old)) < 2,
    TRUE
  )
})

test_that("deleting the cache folder during a session starts a new cache", {
  calls <- local_request_cache()
  .request_atlas(list(a = 1))
  unlink(.atlas_cache_dir(), recursive = TRUE)

  .request_atlas(list(a = 1))
  .request_atlas(list(a = 1))

  expect_equal(calls$n, 2)
  expect_length(cached_files(), 1)
})

test_that("get_atlas(refresh = TRUE) downloads again and updates the cache", {
  calls <- local_request_cache()
  local_mocked_bindings(.label_atlas_columns = function(df) df)

  get_atlas("hiv", "national", 2020)
  get_atlas("hiv", "national", 2020)
  expect_equal(calls$n, 1)

  fresh <- get_atlas("hiv", "national", 2020, refresh = TRUE)
  expect_equal(calls$n, 2)
  expect_equal(fresh$cases, 2)

  expect_equal(get_atlas("hiv", "national", 2020)$cases, 2)
  expect_equal(calls$n, 2)
})

test_that("get_atlas(refresh = TRUE) for counties also refreshes the state totals", {
  calls <- local_request_cache()
  local_mocked_bindings(.label_atlas_columns = function(df) df)
  county <- data.frame(
    indicator = "HIV prevalence",
    year = c(2017, 2022),
    race_ethnicity = "All races/ethnicities",
    sex = c("Male", "Female"),
    age = "Ages 13 years and older"
  )
  .fetch_state_totals(county)
  .fetch_state_totals(county)
  expect_equal(calls$n, 1)

  suppressMessages(
    get_atlas("hiv", "county", c(2022, 2017), stratify_by = "sex", refresh = TRUE)
  )
  downloads <- calls$n
  .fetch_state_totals(county)
  expect_equal(calls$n, downloads + 1)
})

test_that("get_atlas(refresh = TRUE) for states leaves county downloads alone", {
  calls <- local_request_cache()
  local_mocked_bindings(.label_atlas_columns = function(df) df)
  suppressMessages(get_atlas("hiv", "county", 2022))
  downloads <- calls$n

  get_atlas("hiv", "state", 2022, refresh = TRUE)
  suppressMessages(get_atlas("hiv", "county", 2022))
  expect_equal(calls$n, downloads + 1)
})

test_that("clear_atlas_cache() deletes every saved download", {
  calls <- local_request_cache()
  .request_atlas(list(a = 1))
  .request_atlas(list(a = 2))

  expect_snapshot(deleted <- clear_atlas_cache())
  expect_equal(deleted, 2)
  expect_equal(dir.exists(.atlas_cache_dir()), FALSE)

  .request_atlas(list(a = 1))
  expect_equal(calls$n, 3)
})

test_that("clear_atlas_cache() says when there is nothing to delete", {
  local_request_cache()
  expect_snapshot(deleted <- clear_atlas_cache())
  expect_equal(deleted, 0)
})

test_that("clear_atlas_cache() returns invisibly", {
  local_request_cache()
  expect_invisible(suppressMessages(clear_atlas_cache()))
})

test_that("get_atlas() checks `refresh`", {
  expect_snapshot(get_atlas("hiv", "national", 2020, refresh = NA), error = TRUE)
})
