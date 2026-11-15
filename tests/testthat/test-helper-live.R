# The live tests share downloads through helper-live.R. These offline checks
# make sure sharing keeps each download's warnings and failures visible to
# every test that uses it, whichever test ran first.

local_fresh_live_cache <- function(env = parent.frame()) {
  withr::defer(
    {
      rm(list = ls(live_counties), envir = live_counties)
      rm(list = ls(live_runs), envir = live_runs)
    },
    envir = env
  )
}

test_that("live_county() downloads once and repeats its warnings to each caller", {
  local_fresh_live_cache()
  calls <- 0
  local_mocked_bindings(get_atlas = function(...) {
    calls <<- calls + 1
    warning("download warning")
    data.frame(fips = "01001")
  })

  first <- capture_warnings(out_first <- live_county(1999))
  second <- capture_warnings(out_second <- live_county(1999))

  expect_equal(calls, 1)
  expect_equal(first, "download warning")
  expect_equal(second, "download warning")
  expect_equal(out_second, out_first)
})

test_that("live_county() remembers a failed download instead of retrying", {
  local_fresh_live_cache()
  calls <- 0
  local_mocked_bindings(get_atlas = function(...) {
    calls <<- calls + 1
    stop("CDC is down")
  })

  expect_error(live_county(1999), "CDC is down")
  expect_error(live_county(1999), "CDC is down")
  expect_equal(calls, 1)
})

test_that("live_pipeline() reports warnings from a download made earlier", {
  skip_if_not_installed("tidycensus")
  withr::local_envvar(CENSUS_API_KEY = "not-a-real-key")
  local_fresh_live_cache()
  local_mocked_bindings(
    get_atlas = function(...) {
      warning("download warning")
      data.frame(fips = "01001")
    },
    impute_suppressed = function(x, ...) x,
    extrapolate_from_county = function(x, ...) x
  )

  suppressWarnings(live_county(1999))
  run <- live_pipeline(1999)

  expect_contains(run$warnings, "download warning")
})
