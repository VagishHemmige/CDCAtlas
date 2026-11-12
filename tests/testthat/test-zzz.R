test_that("loading the package creates no folder on the user's disk", {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  cache_home <- withr::local_tempdir()
  source_dir <- test_path("../..")
  path <- if (file.exists(file.path(source_dir, "DESCRIPTION"))) {
    normalizePath(source_dir)
  }
  callr::r(
    function(path) {
      if (is.null(path)) {
        loadNamespace("CDCAtlas")
      } else {
        pkgload::load_all(path, quiet = TRUE)
      }
    },
    args = list(path = path),
    env = c(callr::rcmd_safe_env(), XDG_CACHE_HOME = cache_home)
  )
  expect_equal(dir.exists(file.path(cache_home, "CDCAtlas")), FALSE)
})
