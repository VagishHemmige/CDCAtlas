# Called by run.sh with: repository folder, tests folder, optional file pattern.
args <- commandArgs(trailingOnly = TRUE)
repo_dir <- args[[1]]
tests_dir <- args[[2]]
pattern <- if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else NULL

stopifnot(
  identical(Sys.getenv("CDCATLAS_LIVE_TESTS"), "false"),
  identical(Sys.getenv("CENSUS_API_KEY"), ""),
  identical(Sys.getenv("https_proxy"), "http://127.0.0.1:9"),
  identical(Sys.getenv("no_proxy"), "")
)

files <- list.files(tests_dir, pattern = "^test-.*\\.R$")
if (!is.null(pattern)) {
  files <- grep(pattern, files, value = TRUE)
}
if (length(files) == 0) {
  message("No audit tests to run in ", tests_dir, ".")
  quit(status = 0)
}

pkgload::load_all(repo_dir, quiet = TRUE, export_all = TRUE)
results <- testthat::test_dir(
  tests_dir,
  filter = if (is.null(pattern)) NULL else pattern,
  package = NULL,
  load_package = "none",
  stop_on_failure = FALSE,
  reporter = "summary"
)
failed <- sum(as.data.frame(results)$failed) +
  sum(as.data.frame(results)$error)
quit(status = if (failed > 0) 1 else 0)
