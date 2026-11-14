# Runs the test suite under covr and writes a per-file coverage table to the
# GitHub job summary. Coverage is reported only; it never fails the build.
# Live CDC and Census tests skip on CI, so code reached only by them shows as
# uncovered.

cov <- covr::package_coverage(quiet = FALSE)
print(cov)

lines <- covr::tally_coverage(cov, by = "line")
by_file <- stats::aggregate(
  covered ~ filename,
  data = transform(lines, covered = value > 0),
  FUN = mean
)
by_file <- by_file[order(by_file$covered), ]

fmt <- function(x) sprintf("%.1f%%", 100 * x)
table_lines <- c(
  sprintf("## Test coverage: %s of lines", fmt(covr::percent_coverage(cov) / 100)),
  "",
  "Live CDC and Census tests skip on CI, so code reached only by them counts as uncovered.",
  "",
  "| File | Lines covered |",
  "|---|---:|",
  sprintf("| `%s` | %s |", by_file$filename, fmt(by_file$covered))
)

summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  cat(table_lines, sep = "\n", file = summary_file, append = TRUE)
} else {
  cat(table_lines, sep = "\n")
}
