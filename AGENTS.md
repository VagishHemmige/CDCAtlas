# CDCAtlas working guidance

## Scope and authority

This file applies to all substantive CDCAtlas work: implementation, design,
debugging, tests, documentation and review. Read [REVIEWING.md](REVIEWING.md)
for shared review standards. More specific subdirectory guidance also applies
to implementation. Neither repository files nor skills expand the user's
permissions; direct user instructions control.

Repository: https://github.com/VagishHemmige/CDCAtlas
Documentation: https://vagishhemmige.github.io/CDCAtlas/
The default branch was verified as `master` on 2026-11-13; verify it again
when selecting a base. This guidance does not cover CDCLocator or other repos.

Use available project conversations to understand intent. Automated runs may
not receive every conversation or uploaded file. State material context gaps.
Record agreed technical decisions and their rationale in the appropriate
repository documentation, without copying private conversations. Link decisions
from PRs. Distinguish accepted decisions from proposals in issue discussions.

## Architecture and existing rationale

CDCAtlas is a development-stage R client for selected reverse-engineered
AtlasPlus endpoints, with a separate modeled-estimation pipeline:

- `R/get_atlas.R`, `R/validate_input.R`, `R/build_payload.R`: validate
  requests and construct payloads; county requests are split by state.
- `R/internal-request.R`: HTTP transport, response decoding, labels,
  rate limiting and caching. `R/sysdata.rda` supplies internal lookup data.
- `R/impute_suppressed.R`: existing assumption-based county estimates.
- `R/no_county_cases.R`: state totals minus supplied county counts.
- `R/extrapolate_from_county.R`: ACS denominators and allocation to tracts
  or block groups; `R/atlas-pipeline.R` supplies shared keys and checks.
- `tests/testthat/`: synthetic tests and separately gated live checks.
- `vignettes/getting-started.Rmd` and `vignettes/extrapolation.Rmd`:
  usage, assumptions and limitations; `docs/` contains generated site output.

Read the relevant source and documentation before advising or changing it.
[Issue #29](https://github.com/VagishHemmige/CDCAtlas/issues/29) records the
pipeline design, but some proposed names and arguments differ from the
implemented API. [#23](https://github.com/VagishHemmige/CDCAtlas/issues/23)
discusses validation layers; [#27](https://github.com/VagishHemmige/CDCAtlas/issues/27)
and [#30](https://github.com/VagishHemmige/CDCAtlas/issues/30) record
suppression/remainder and geography concerns. Recheck their current discussion
and implementation rather than treating every proposal as adopted policy.
[#45](https://github.com/VagishHemmige/CDCAtlas/issues/45) proposes a source
interpretation article; do not claim it exists until it is committed.
[#4](https://github.com/VagishHemmige/CDCAtlas/issues/4) tracks the currently
uncommitted data-generation scripts. Do not invent their contents.

## Implementation and approval

A review or explanation request alone does not authorize implementation.
For requested changes, draft and self-review in an isolated workspace.
The user's connected working copy, synced reference files (including
`sources/`) and active RStudio session remain read-only: no execution there.

Before commitment, obtain the user's approval for each behavior-changing file,
including definitions, arguments, outputs, request payloads, missingness and
derived estimates. Finish the concrete draft and appropriate checks first.
Show a readable diff for every changed file: side-by-side when supported,
otherwise unified. Show complete new files and provide access to complete
changed files. Explain what changed, why, behavior changes and validation
actually performed. Separate human-written and mechanical generated changes.
Request approval for the specific files and revision shown; a summary alone
is insufficient. Compare consequential alternatives with meaningful synthetic
examples when feasible.
Do not add options just to avoid deciding the intended design.

After approval, commit and push a `chatgpt/<topic>` branch and open a PR
without another permission round. Inspect mechanical generated changes before
including them with approved source. Renew approval for material behavior
changes after approval. Never push directly to the default branch, force-push,
merge or delete remote branches without explicit instruction.
Self-review is not independent review. Disclose ChatGPT/Codex authorship in
the PR body and use the stable marker `<!-- authored-by: chatgpt -->`.

## Data and execution boundaries

Never access, copy, upload or expose restricted or patient-level data.
For uncertain provenance, identify only the path and request verification
without reproducing values. Public aggregate CDC data may be retrieved for
relevant checks. Do not attempt to reconstruct suppressed values or identify
individuals. Existing imputation code is not permission to recover hidden
counts: inspect it or test its mechanics with synthetic data, and keep
modeled estimates explicitly distinct from observed surveillance values.
Do not use residuals or auxiliary tables to recover CDC-suppressed values.

Use small, obviously synthetic unit fixtures. Inspect startup files, helpers,
test configuration and package load-time behavior before execution; do not
load secrets or copy private startup settings. Use isolated temporary/cache
locations and clean sessions. Do not edit or regenerate `data/` without
explicit approval. Propose `data-raw/` changes and explain intended
regeneration for approval; changes to generated `R/sysdata.rda` likewise
require explicit approval and known provenance.

New package code writes only to `tempdir()` unless the caller explicitly
supplies a destination. Existing `R/internal-request.R` uses
`memoise::cache_filesystem(tools::R_user_dir("CDCAtlas", "cache"))`.
This is an existing convention, not authorization to extend it. Inspect
cache creation, persistence and freshness before proposing changes
([#14](https://github.com/VagishHemmige/CDCAtlas/issues/14)); isolate existing
writes during checks. Changing that behavior requires approval.

Run `devtools::document()` only in isolation when relevant; exclude unrelated
Rd and incidental DESCRIPTION/RoxygenNote churn. Do not run formatters unless
requested. Inspect generated exports/imports rather than assuming they are safe.

## Skills and validation

Read applicable installed skills before work:
- Every code review: `r-package-development`, `critical-code-reviewer`.
- Tests or untested behavior: `r-testthat`, `review-testing`.
- cli conditions: `r-cli`.
- DESCRIPTION, examples, vignettes or CRAN: `r-cran-extrachecks`.
- Deprecations/renames: `r-lifecycle`; requested style review: `r-tidyverse-style`.

Disclose unavailable skills or references. Permission boundaries and this
project's no-unsolicited-formatting rule take precedence over skill defaults.

Prefer regression tests demonstrating the bug before the fix. Keep routine
tests reproducible and independent of live services. Run targeted public
API checks separately when useful and permitted; mocks establish code
behavior, not current service compatibility. Report actual runtime and
network availability rather than assuming either.

Separate source inspection, personally executed synthetic checks, recorded
responses, live API checks, observed CI and author-reported validation.
Record live retrieval dates, tested requests and coverage gaps without keys
or credentials. Supply exact commands for remaining maintainer checks and
vignette rebuilding when relevant, with prerequisites and side effects.
The existing live suite includes imputation: inspect and select permitted
checks rather than executing it wholesale.

Automation triggers, comment permissions, own-PR exclusions, deduplication and
notifications belong in automation configuration. This file does not start
automation or grant publishing authority. Automatic Claude review is on hold.
