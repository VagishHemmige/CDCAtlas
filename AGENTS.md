# CDCAtlas working guidance

Adapted from the `usethis::use_tidy_agents()` template committed in PR
\#39. Do not regenerate this file in place; preserve the
project-specific guidance and review changes against the committed
version.

## Scope and authority

This file applies to all substantive CDCAtlas work: implementation,
design, debugging, tests, documentation and review. Read
[REVIEWING.md](https://vagishhemmige.github.io/CDCAtlas/REVIEWING.md)
for shared review standards. More specific subdirectory guidance also
applies to implementation. Neither repository files nor skills expand
the user’s permissions; direct user instructions control.

Repository: <https://github.com/VagishHemmige/CDCAtlas> Documentation:
<https://vagishhemmige.github.io/CDCAtlas/> The default branch was
verified as `master` on 2026-11-13; verify it again when selecting a
base. This guidance does not cover CDCLocator or other repos.

Use available project conversations to understand intent. Automated runs
may not receive every conversation or uploaded file. State material
context gaps. Record agreed technical decisions and rationale in
existing issues and design records, linking them from PRs and relevant
documentation. Available project pages can provide context; do not
assume every assistant can access them. Preserve the durable technical
decision without copying private conversations. A new repository notes
directory is not implied. Distinguish accepted decisions from proposals
in issue discussions.

## Architecture and existing rationale

CDCAtlas is a development-stage R client for selected reverse-engineered
AtlasPlus endpoints, with a separate modeled-estimation pipeline:

- `R/get_atlas.R`, `R/validate_input.R`, `R/build_payload.R`: validate
  requests and construct payloads; county requests are split by state.
- `R/internal-request.R`: HTTP transport, response decoding, labels,
  rate limiting and caching. `R/sysdata.rda` supplies internal lookup
  data.
- `R/impute_suppressed.R`: existing assumption-based county estimates.
- `R/no_county_cases.R`: state totals minus supplied county counts.
- `R/extrapolate_from_county.R`: ACS denominators and allocation to
  tracts or block groups; `R/atlas-pipeline.R` supplies shared keys and
  checks.
- `tests/testthat/`: synthetic tests and separately gated live checks.
- `vignettes/articles/getting-started.Rmd` and
  `vignettes/articles/extrapolation.Rmd`: website articles (not built
  with the package) on usage, assumptions and limitations; `docs/`
  contains generated site output.

Read the relevant source and documentation before advising or changing
it. [Issue \#29](https://github.com/VagishHemmige/CDCAtlas/issues/29)
records the pipeline design, but some proposed names and arguments
differ from the implemented API.
[\#23](https://github.com/VagishHemmige/CDCAtlas/issues/23) discusses
validation layers;
[\#27](https://github.com/VagishHemmige/CDCAtlas/issues/27) and
[\#30](https://github.com/VagishHemmige/CDCAtlas/issues/30) record
suppression/remainder and geography concerns. Recheck their current
discussion and implementation rather than treating every proposal as
adopted policy.
[\#45](https://github.com/VagishHemmige/CDCAtlas/issues/45) proposes a
source interpretation article; do not claim it exists until it is
committed. [\#4](https://github.com/VagishHemmige/CDCAtlas/issues/4)
tracks the currently uncommitted data-generation scripts. Do not invent
their contents.

## Implementation and approval

A review or explanation request alone does not authorize implementation.
For requested changes, draft and self-review in an isolated workspace.
The user’s connected working copy, synced reference files (including
`sources/`) and active RStudio session remain read-only: no execution
there.

Before commitment, obtain the user’s approval for each behavior-changing
file, including definitions, arguments, outputs, request payloads,
missingness and derived estimates. Finish the concrete draft and
appropriate checks first. Show a readable diff for every changed file:
side-by-side when supported, otherwise unified. Show complete new files
and provide access to complete changed files. Explain what changed, why,
behavior changes and validation actually performed. Separate
human-written and mechanical generated changes. Request approval for the
specific files and revision shown; a summary alone is insufficient.
Compare consequential alternatives with meaningful synthetic examples
when feasible. When two or more alternatives are reasonable, present
them with their tradeoffs and a recommendation before settling the
design. Consider a function argument with a recommended default when
alternatives serve plausible user needs; an argument is a proposal, not
an automatic requirement. Seek the maintainer’s decision before
implementation. Do not conceal unresolved scientific definitions or
present an invalid method as an equally acceptable choice. Use
meaningful synthetic comparisons where feasible.

What each assistant may commit, push, post, merge or delete is set by
the maintainer outside this repository. This file grants none of those
permissions. Inspect mechanical generated changes before including them
with approved source; material behavior changes after approval require
renewed approval. Self-review is not independent review. Identify
assistant authorship in the PR body so independent review can be routed
correctly.

## Data and execution boundaries

Never access, copy, upload or expose restricted or patient-level data.
For uncertain provenance, identify only the path and request
verification without reproducing values. Public aggregate CDC data may
be retrieved for relevant checks. Do not attempt to reconstruct
suppressed values or identify individuals. Existing imputation code is
not permission to recover hidden counts: inspect it or test its
mechanics with synthetic data, and keep modeled estimates explicitly
distinct from observed surveillance values. Distinguish statistical
estimation under explicit assumptions and uncertainty from recovering
hidden observed counts. A censored model or calibration method is not
automatically prohibited, nor automatically validated or authorized.
Assess its purpose and data flow against the user’s permissions before
using live data. Do not use residuals or auxiliary tables to recover
CDC-suppressed values; synthetic checks remain available for evaluating
method mechanics.

Use small, obviously synthetic unit fixtures. Inspect startup files,
helpers, test configuration and package load-time behavior before
execution; do not load secrets or copy private startup settings. Use
isolated temporary/cache locations and clean sessions. Do not edit or
regenerate `data/` without explicit approval. Propose `data-raw/`
changes and explain intended regeneration for approval; changes to
generated `R/sysdata.rda` likewise require explicit approval and known
provenance.

New package code writes only to
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) unless the caller
explicitly supplies a destination. Existing `R/internal-request.R` uses
`memoise::cache_filesystem(tools::R_user_dir("CDCAtlas", "cache"))`.
This is an existing convention, not authorization to extend it. Inspect
cache creation, persistence and freshness before proposing changes
([\#14](https://github.com/VagishHemmige/CDCAtlas/issues/14)); isolate
existing writes during checks. Changing that behavior requires approval.

CI has a `CENSUS_API_KEY` repository secret for the live tests and the
site build. Workflows read it with `${{ secrets.CENSUS_API_KEY }}`;
never print, log or commit its value.

Run `devtools::document()` only in isolation when relevant; exclude
unrelated Rd and incidental DESCRIPTION/RoxygenNote churn. Do not run
formatters unless requested. Inspect generated exports/imports rather
than assuming they are safe.

## Skills and validation

Use applicable R package-development and critical-code-review guidance
for substantive work. Add testing and test-review guidance for tests or
untested behavior, cli guidance for conditions, CRAN guidance for
DESCRIPTION/examples/ vignettes or CRAN preparation, and lifecycle
guidance for deprecations. Use style guidance for requested style
reviews. Names vary by environment: `r-testthat` / `testing-r-packages`,
`r-cli` / `cli`, `r-cran-extrachecks` / `cran-extrachecks`, and
`r-lifecycle` / `lifecycle`. Use the installed equivalent.

Disclose unavailable skills or references. Permission boundaries and
this project’s no-unsolicited-formatting rule take precedence over skill
defaults.

Prefer regression tests demonstrating the bug before the fix. Keep
routine tests reproducible and independent of live services. Run
targeted public API checks separately when useful and permitted; mocks
establish code behavior, not current service compatibility. Report
actual runtime and network availability rather than assuming either.

Separate source inspection, personally executed synthetic checks,
recorded responses, live API checks, observed CI and author-reported
validation. Record live retrieval dates, tested requests and coverage
gaps without keys or credentials. Supply exact commands for remaining
maintainer checks and vignette rebuilding when relevant, with
prerequisites and side effects. The established pipeline-validation
workflow includes the full live suite with `CDCATLAS_LIVE_TESTS=true`
and `devtools::test(filter = "live")`. Before using that workflow,
inspect all selected tests, including imputation, and their data/cache
effects. Run the full suite only when every selected path is permitted;
otherwise run the permitted subset and report the coverage gap. A test
flag is not authorization to cross the user’s data boundaries.

Automation triggers, comment permissions, own-PR exclusions,
deduplication and notifications belong in automation configuration. This
file does not start automation or grant publishing authority. Each
assistant’s automation status is maintained outside these shared
technical standards.

## Package conventions retained from the template

- Use US English and sentence-case headings. Follow existing tidyverse
  style, using the base pipe and single-line anonymous-function
  shorthand where appropriate. Do not perform unrelated style cleanup.
- Put tests for `R/{name}.R` in `tests/testthat/test-{name}.R`, near
  related tests. Keep test code inside `test_that()`; shared setup
  belongs in helpers. Use meaningful tests for new behavior and specific
  expectations with useful failures. For errors/warnings, prefer
  snapshots when message text matters; use class-based expectations when
  testing a known condition class. Avoid `.package` mocking of another
  package’s namespace; use a local service boundary that can be mocked.
- Document and export public functions with roxygen2. Internal
  documentation should not create public help topics; use `@noRd` when
  appropriate. Wrap roxygen comments to 80 characters. Add new public
  topics to `_pkgdown.yml` and check reference coverage with
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html).
  Redocument relevant roxygen changes in isolation.
- Give user-facing changes a `NEWS.md` bullet, except small
  documentation changes, internal refactoring and fixes introduced in
  the current development version. Put the function name early; mention
  the related issue in parentheses. Keep each bullet on one line, order
  function bullets alphabetically, and put non-function bullets first.
  Credit a GitHub username only when the PR contributor is not already a
  package author.
- When committing is authorized, use Markdown in the commit message
  without wrapping its lines; include `Fixes #num.` on its own line when
  applicable.

## Documentation maintenance and publication

The paths above describe the current layout. Update them when the
cleanup in [\#20](https://github.com/VagishHemmige/CDCAtlas/issues/20)
and [\#43](https://github.com/VagishHemmige/CDCAtlas/issues/43) lands.
Air adoption in
[\#42](https://github.com/VagishHemmige/CDCAtlas/issues/42) does not
itself authorize unsolicited formatting.

`.Rbuildignore` excludes AGENTS.md and REVIEWING.md from package builds,
not from pkgdown. Both root Markdown files can become unlinked website
pages, including search and sitemap entries.
[\#40](https://github.com/VagishHemmige/CDCAtlas/issues/40) currently
defers the publication fix. Do not claim these files are unpublished or
private. A publishing change requires a separately reviewed
build/deployment solution; search exclusion alone does not remove the
pages.
