# Synthetic audit checks

Reusable checks written during a whole-package review
([PACKAGE_REVIEW.md](../../PACKAGE_REVIEW.md)). They are not package tests:
nothing here runs in R CMD check or CI, and nothing changes package behavior.
The folder is excluded from package builds.

No review has been run yet, so `tests/` is empty.

## Run

Use a separate, inspected checkout with R, the package's Imports, `pkgload`,
`testthat` (3.1.9 or newer) and `withr` installed. From the repository root, in
Bash (Git Bash on Windows):

```sh
bash tools/audit/run.sh            # every check
bash tools/audit/run.sh no-county  # files whose names contain "no-county"
```

The launcher disables R startup files for itself and child processes, sets
`CDCATLAS_LIVE_TESTS=false`, unsets `CENSUS_API_KEY`, points the request cache
at a temporary folder, runs Rscript with `--vanilla`, and loads the package with
`pkgload::load_all()`. It returns nonzero when a check fails. Original package
tests are not run.

## Adding checks

- One file per area, `tests/test-<area>.R`, using testthat 3.
- Use made-up inputs whose answers can be worked out by hand. Mock requests at
  the package's own request functions (for example `.request_atlas()` and
  `.get_acs_areas()`), never with live data.
- Describe each check's expectation and its source in the test description or a
  short comment, so the check can be understood without the review report.
- Leave a failing check failing. Do not weaken it, skip it or convert it to an
  expected failure.

## Recorded execution

For each review, record the revision checked, OS, R and package versions, the
command and the result here, and say which checks pass, fail, depend on a
decision, or could not run.
