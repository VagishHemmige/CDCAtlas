# Loads CDCAtlas in a new R session (the installed package when the tests run
# on an installed copy, the source otherwise), with its user cache folder in
# `cache_home`, then runs `code` there. An installed package also has a
# DESCRIPTION file (covr runs the tests inside one), so look for R source files.
in_fresh_session <- function(cache_home, code = function() NULL) {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  source_dir <- test_path("../..")
  path <- if (file.exists(file.path(source_dir, "R", "get_atlas.R"))) {
    normalizePath(source_dir)
  }
  callr::r(
    function(path, code) {
      if (is.null(path)) {
        loadNamespace("CDCAtlas")
      } else {
        pkgload::load_all(path, quiet = TRUE)
      }
      code()
    },
    args = list(path = path, code = code),
    env = c(callr::rcmd_safe_env(), R_USER_CACHE_DIR = cache_home)
  )
}

# Replaces the download with a stub, makes one request twice, and reports where
# the response was saved and how often CDC would have been asked.
cached_request_in_session <- function() {
  ns <- asNamespace("CDCAtlas")
  calls <- 0
  stub <- function(payload) {
    calls <<- calls + 1
    data.frame(cases = 1)
  }
  unlockBinding(".request_atlas_internal", ns)
  assign(".request_atlas_internal", stub, envir = ns)
  ns$.request_atlas(list(a = 1))
  ns$.request_atlas(list(a = 1))
  list(
    dir = ns$.atlas_cache_dir(),
    files = length(list.files(ns$.atlas_cache_dir())),
    calls = calls
  )
}

test_that("loading the package writes nothing to the user's cache folder", {
  cache_home <- withr::local_tempdir()
  in_fresh_session(cache_home)
  expect_equal(
    list.files(cache_home, recursive = TRUE, include.dirs = TRUE),
    character()
  )
})

test_that("the first request makes the cache folder, even with no parent folder", {
  cache_home <- file.path(withr::local_tempdir(), "not", "made", "yet")
  out <- in_fresh_session(cache_home, cached_request_in_session)
  expect_equal(startsWith(out$dir, cache_home), TRUE)
  expect_equal(out$files, 1)
  expect_equal(out$calls, 1)
})

test_that("requests are cached in a cache folder that already exists", {
  cache_home <- withr::local_tempdir()
  out <- in_fresh_session(cache_home, cached_request_in_session)
  expect_equal(startsWith(out$dir, cache_home), TRUE)
  expect_equal(out$files, 1)
  expect_equal(out$calls, 1)
})

test_that("a new R session reuses responses saved by an earlier one", {
  cache_home <- withr::local_tempdir()
  in_fresh_session(cache_home, cached_request_in_session)
  out <- in_fresh_session(cache_home, cached_request_in_session)
  expect_equal(out$files, 1)
  expect_equal(out$calls, 0)
})
