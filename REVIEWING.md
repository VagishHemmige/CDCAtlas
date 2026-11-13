# Shared CDCAtlas review standards

These standards apply to ChatGPT and Claude, within the user's permissions.
Read [AGENTS.md](AGENTS.md). A review is an assessment of a defined change,
not automatic permission to edit, publish, approve or merge.

## Policy revision and coverage

For PRs, fetch AGENTS.md and REVIEWING.md from the actual target branch at
its recorded current base SHA. Record both policy paths and that SHA.
Proposed policy changes do not govern their own review. If retrieval fails,
disclose the gap; do not claim the policy was read.
For local reviews, use committed policy at HEAD and assess uncommitted policy
edits separately. Implementation uses applicable working-branch and
subdirectory guidance.

Identify the copy, branch, exact commit, target, comparison scope and policy
revision. GitHub reviews exclude unpushed changes.
Use the merge base for the initial PR diff. Read changed human-written code
and relevant surrounding paths; trace effects through callers, joins,
returned objects, tests, documentation and affected workflows. Reserve
whole-package coverage claims for whole-package reviews.

On follow-up reviews, inspect changes since the last reviewed revision,
retaining relevant full-PR context. Account for changed bases and rewritten
history. Revisit unresolved findings only with new material evidence; do not
repeat resolved findings. For generated-only changes, check agreement with
source, exports and imports and keep the report proportionate.

## Development-aware judgment

Identify the stated milestone and intended behavior before judging it.
Distinguish:
- Verified defects introduced or exposed by the change.
- Validation or evidence gaps, without calling uncertainty a proven defect.
- Methodological decisions requiring an explicit rationale or user decision.
- Intentional unfinished work within a documented development milestone.
- Unrelated existing limitations and future release-readiness work.

Prioritize silent corruption, incorrect calculations, broken installation or
exports, misleading claims, and privacy violations. Intentional documented
interface changes can be acceptable during development; accidental breakage
remains a defect. An incomplete milestone is not automatically release-ready.
Do not demand unrelated cleanup or new options to postpone a design decision.
Evaluate tests and CI independently: passing assertions can encode an incorrect
scientific assumption. Explain whether external findings are agreed with,
partly agreed with or disagreed with, and why.

## Scientific and service checks

Verify affected behavior against current authoritative CDC documentation and
available metadata. Start with [CDC's AtlasPlus overview and documentation
links](https://www.cdc.gov/nchhstp/about/atlasplus.html), the application's
current Technical Notes, Glossary and FAQs, and metadata for the actual
request. Historical extracts require their matching historical documentation,
not silently substituted current definitions.
For ACS use the requested year's table metadata/universe and
[Census geography-vintage documentation](https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.html).
These are starting points, not evidence that every package mapping is correct.

Check, where affected:
- Indicator definition, unit, population, denominator, age universe,
  geography and reporting period. Distinguish diagnoses, prevalence, deaths,
  counts, rates and percentages; names such as `cases` or `rate100000`
  do not prove the underlying unit.
- Suppressed, unavailable, missing and true zero values. Never silently turn
  suppression or missingness into zero. Check primary/complementary
  suppression and indicator-specific rules without reconstructing hidden
  values. Existing explicit zero sensitivity options are assumptions, not
  observed zeros.
- Character geographic identifiers and leading zeros, geographic level,
  boundary changes, ACS vintage and crosswalk completeness.
- Overlapping strata, total rows versus subgroups, whether quantities are
  additive, uniqueness of keys, join cardinality and duplicated observations.
- Request filters and payload codes; splitting, batching or pagination where
  applicable; completeness across responses; schema changes, timeouts,
  malformed/empty/partial responses and error handling. Do not assume this
  endpoint has pagination merely because other APIs do.
- Returned indicator, geography, year and subgroup against the request,
  including unexpected or silently omitted cells.
- Source revisions, retrieval dates, cached-response age, and limitations of
  reverse-engineered endpoints. HTTP success alone proves none of the above.

For modeled county-to-tract/block-group or other allocation, distinguish
estimates from observed CDC values. Document assumptions and their rationale,
verify denominator compatibility and trace unknown categories, filtered
counties, no-county remainders, empty strata and unallocated rows. Check totals
within the appropriate indicator/year/geography/stratum, while separately
checking distribution and coverage. Matching a national total or preserving
county sums does not validate subgroup or geographic estimates.
Do not invent definitions or silently choose unresolved methodological defaults.
Preserve authoritative sources, versions/dates when available, and durable
rationale near the code or in existing scientific documentation; link from
PRs. The issue/vignette map in AGENTS.md supplies project rationale, not
independent verification of CDC definitions.

## Validation

Inspect test fixtures, helpers, startup and load-time effects before execution.
Use only the isolated, permitted environment described in AGENTS.md.
Inspect expectations independently of implementation; favor hand-checkable
synthetic cases and regression failures before fixes. Mock at service
boundaries, retaining meaningful parsing, arithmetic and join checks.
Consider missing rows, NA versus zero, unknown codes, duplicate keys,
incompatible denominators, mixed indicators and partial geographic coverage.
Use numerical tolerances appropriate to the calculation, not arbitrary
rounding to force agreement.

Keep these evidence categories separate:
1. Source inspection and mathematical reasoning.
2. Personally executed synthetic checks.
3. Recorded public-response checks (with provenance/retrieval date).
4. Live public-API checks (request, retrieval date, cache status, scope).
5. Observed CI results (revision and job).
6. Author-reported checks, not independently reproduced.

Never present synthetic checks as proof of live compatibility. Live checks
must respect the suppression and data boundaries in AGENTS.md. Do not
execute live checks blindly: inspect the existing imputation pipeline against
those boundaries before selecting checks.
Report OS, R/package versions if execution occurred, failures, skipped checks
and material coverage limits. Give exact remaining maintainer commands,
including vignette rebuilds when affected, only after stating prerequisites,
network/cache effects and what the commands establish.
Check dependency minimums and material Windows portability risks. Generated
documentation can be a staged follow-up if explicitly documented, but broken
exports/imports or installation are substantive defects.

## Reporting

Lead with the change's purpose and scoped conclusion. Group actionable
findings as Must fix, Should fix or Consider, omitting empty groups.
For each, give source path/line, evidence, failure mode or uncertainty,
practical consequence and suggested resolution. Keep methodological questions
distinct from verified defects. Avoid stylistic nitpicks and unrelated backlog
items. Explain any pre-existing issue that the change materially worsens.

State actual checks, policy revision and material limitations. A clean scoped
review means no actionable findings in the inspected scope, not comprehensive
correctness, formal approval or release readiness. Silence or a skipped/failed
automated review is not approval. Self-review is not independent review.
