# Synthetic audit checks

Optional local runner for temporary synthetic checks during a whole-package
review ([PACKAGE_REVIEW.md](../../PACKAGE_REVIEW.md)). It does not run in R CMD
check or CI. The folder is excluded from package builds.

Standing checks belong in the existing `tests/testthat/` files and workflows.
Keep temporary review checks in the review deliverable or owning issue until
integrated with the fix; remove duplicates afterwards. An empty `tests/` directory
means there are no local checks for this runner, not that no review has occurred.

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

It also points R's HTTP and HTTPS proxy settings at an address that refuses
connections, so a request that a check forgot to mock fails rather than
reaching CDC or the Census. This covers httr and curl, which honor the proxy
variables; it is not a firewall, and code that ignores them could still reach
the network. Mock every request anyway, and inspect checks before running
them.

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

Record the revision, OS, R and dependency versions, commands and results in the
review deliverable or PR, including passing, failing, decision-dependent and
blocked checks. Do not accumulate review logs or a second backlog in this folder.
