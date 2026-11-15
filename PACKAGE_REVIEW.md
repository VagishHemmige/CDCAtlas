# Whole-package review protocol

This document is the protocol for an explicitly commissioned deep review
of CDCAtlas: current code, every open issue, and gaps for future
development.
[REVIEWING.md](https://vagishhemmige.github.io/CDCAtlas/REVIEWING.md)
remains the protocol for individual PR reviews. This plan applies only
when the maintainer explicitly commissions a review using
`PACKAGE_REVIEW.md`. Its presence does not trigger an audit or govern
routine PR reviews. Saving this protocol does not mean an audit has been
performed.

Read current
[AGENTS.md](https://vagishhemmige.github.io/CDCAtlas/AGENTS.md) before
starting. The coordinator inventories the repository reference records
named below and any maintainer-supplied decision or handoff context,
recording which revision was read. Outside context is for the
coordinator only; do not reproduce private conversations or project
notes in reviewer packets or public findings. Use REVIEWING.md’s
evidence, severity and reporting standards, but use whole-package scope
here. This is a commissioned review plan, not an expansion of
permissions or a replacement for repository policy. Explicit maintainer
instructions govern. Record material conflicts and resolve them before
dependent work.

Adapted from sRtr’s `PACKAGE_REVIEW.md` at
[`95992e4`](https://github.com/VagishHemmige/sRtr/blob/95992e4/PACKAGE_REVIEW.md).
The structure is the same; the reviewer roles, data boundaries and
failure modes are CDCAtlas’s.

## Purpose and boundaries

Determine whether the package requests, labels and transforms CDC
AtlasPlus data correctly; whether its estimates (suppressed-count
imputation, cases with no county, tract and block-group allocation) rest
on defensible, documented assumptions; whether it supports credible
research workflows; and whether it communicates its limitations. Assess
whether users can obtain invalid analyses even when code works as
written, including misleading names, unsafe defaults, silent row loss,
and estimates presented as observed counts.

Review is read-only with respect to package implementation. Do not
silently fix code or decide scientific defaults. Proposed repairs and
methodological choices belong in the report for maintainer
consideration. Review work includes writing and running independent
synthetic audit tests in the isolated workspace and preparing concrete
test and CI patches for review. Preserve these as deliverables outside
the repository until integration is authorized; do not silently install
them into the package or activate changed workflows. The lasting outcome
is appropriate checks in existing tests and workflows, not another audit
suite or repository archive.

### Data and execution boundaries

- **CDC AtlasPlus and Census data are public.** Public responses and
  counts may appear in findings. Never print, log or commit the
  `CENSUS_API_KEY` value.
- **Files governed by other terms** (for example AIDSVu ZIP-level
  downloads, health-department reports) stay out of the repository,
  reviewer packets and public findings. Cite their public methods pages
  instead. The maintainer’s local `data-raw/` may hold such files; they
  are gitignored.
- **The maintainer’s connected checkout and RStudio session are
  read-only.** Do not edit, regenerate or run anything there.
- **Live requests** to CDC and the Census, including the live-tests
  workflow, need authorization under the maintainer’s working
  instructions; this protocol grants none. Do not run
  `data-raw/DATASET.R` or regenerate `R/sysdata.rda`; inspect them and
  report.
- **Isolation for offline runs.** Execute only in an isolated workspace,
  after inspecting startup files, helpers, setup files and test guards.
  Use `tools/audit/run.sh`, which disables startup files, sets
  `CDCATLAS_LIVE_TESTS=false`, unsets `CENSUS_API_KEY` and points the
  memoise cache at a temporary folder so no real cache is read or
  written. Validate that isolation with a harmless synthetic startup
  file and a child process before relying on it; record the commands and
  the result.

## Review team

Use independent AI reviewer roles with fresh contexts where supported.
These are specialist perspectives, not claims of human credentials or
substitutes for authoritative evidence and human subject-matter
validation. Schedule roles in waves if concurrency is limited; do not
omit a role to fit available slots.

| Role | Required focus |
|----|----|
| HIV and STI surveillance epidemiologist | Indicator definitions (diagnoses, prevalence, estimates, Knowledge of Status), age universes, reporting delay and preliminary data, rates versus counts, use of these data in research |
| CDC AtlasPlus data specialist | Code tables and payloads, suppression and data-status values, county-map vintages, territories, unknown-county cases, response structure and changes to the endpoints |
| Census and ACS small-area methods reviewer | ACS tables and universes, geography vintages and relationship files, tract and block-group allocation, partial age bands, race tables, margins of error |
| Biostatistician and epidemiologist | Imputation assumptions and caps, the no-county remainder, allocation estimands, uncertainty, empty strata, combining strata and diseases |
| Senior R package engineer | Every exported function and internal helper, contracts, types, attributes, dependencies, namespace, errors, caching, rate limiting, side effects and architecture |
| Independent testing and reproducibility engineer | Owns the existing-test and CI audit, creation and execution of reusable synthetic audit tests, and concrete permanent-test and CI proposals; challenges claimed guarantees |
| Scientific librarian and provenance reviewer | Sources for every definition, code table and methodological claim, including version and section; reference-catalogue quality and placement |
| Researcher using the package | Owns the site, articles and documentation review (section 4): whether a new user can follow the articles to a correct result and understands what each result is; discoverability, practical misuse risks and missing capabilities |

Review existing capabilities and relevant gaps; do not invent
unsupported methods merely to fill a role’s assignment.

The coordinator maintains coverage, verifies findings, reconciles
duplicates and assembles the report. The coordinator is not blind. No
reviewer is expected to inspect every file, but every in-scope file must
have an assigned reader. Functions that produce estimates receive review
from the epidemiologist, the ACS methods reviewer, the biostatistician
and the package engineer.

Use available package-development, critical-code-review, test-review,
testthat, design and accessibility guidance where relevant. Apply
pkgdown and R package requirements rather than analysis-project rules
that do not fit this repository. Record unavailable capabilities.

The scientific librarian owns the reference review and curation
proposal: source locators, retrieval status, version, duplicates,
contradictions and placement against
[reference/README.md](https://vagishhemmige.github.io/CDCAtlas/reference/README.md).
Specialists verify the substantive interpretations in their domains; the
librarian does not settle methodological disagreements by authority.
Issues and PRs link to reference sections; check that references contain
no reverse issue links or CDCAtlas implementation status.

## Reference resources and reviewer packets

Use the existing [reference
catalogue](https://vagishhemmige.github.io/CDCAtlas/reference/README.md);
do not create a competing catalogue. The coordinator and librarian
prepare role-specific source packets from it before the first passes:
source links, pinned code locations, relevant primary documentation and
factual scope. Withhold the catalogue’s existing judgments about
CDCAtlas and issue conclusions until reconciliation. If a reviewer has
already seen them, record that exposure.

For each source identify the indicator or method, geography, years,
version or access date, exact section, and the package capability it
informs. Reviewers may discover additional sources; record them in the
shared source manifest and reconcile them into the catalogue as proposed
documentation changes. Verify historical claims rather than treating a
prior review or external code as authoritative.

AtlasPlus’s Technical Notes, Glossary and FAQs are inside the
application, not on a stable public page. Record how each was reached
and when; if one cannot be retrieved, state the gap.

## 1. Freeze and inventory the review target

Record repository, default branch, full commit SHA, date, package
version, policy revision, website URL and observed deployment revision
when available. For GitHub reviews, explicitly exclude unpushed changes.

Inventory all tracked files and functions, tests, documentation topics,
articles, workflows, metadata, `R/sysdata.rda` contents, `data-raw/`
scripts, reference sources, and every open issue and PR. Paginate
collection reads and reconcile counts. Check relevant closed issues and
merged PRs for duplicates and previously resolved findings. Keep
unmerged work separate from current default-branch behavior; a proposed
fix does not resolve a current-code finding.

Maintain a coverage ledger:

| Item or function | Revision | Assigned reviewers | Read | Executed checks | Source verification | Disposition or remaining gap |
|----|----|----|----|----|----|----|

Read all human-written in-scope code and documentation, not a sample.
Check generated documentation against its source. Name every exclusion
and its reason. Persist progress and reviewer assignments so an
interrupted audit can resume without silently losing coverage.

### Resuming versus commissioning a fresh review

When resuming the same audit, reuse its coverage and execution records,
identify intervening revision changes, and recheck affected conclusions.
A new review starts with the current target, primary sources and
independently derived expectations. Prior audit reports are historical
evidence, not authority. Withhold previous diagnoses, issue conclusions
and audit-specific regression cases from first-pass packets; introduce
them during reconciliation. Existing package tests remain part of the
implementation available for review. Record which checks are new, reused
or adapted.

## 2. Independent first passes

Give reviewers the frozen source, applicable rules, role questions and
relevant public references. Do not give them other reviewers’ findings,
private conversation history or issue diagnoses during the initial pass.
Record any unavoidable exposure and do not claim full blindness when it
was not achieved.

Require each reviewer to inspect actual implementation, not infer
behavior from function names, tests, documentation or author claims. For
each function, compare purpose, documented contract, actual behavior and
downstream use. Trace the pipeline end to end: request and payload -\>
response decoding and labels -\>
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
-\>
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
-\>
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md).
Check interactions, not merely isolated helpers, including data saved,
filtered or combined between steps.

Prioritize plausible silent errors: wrong or mixed denominators and age
universes, missing values treated as zero, suppressed versus
not-available rows confused, cases counted twice or lost between county,
state and no-county totals, rows dropped without a record,
geography-vintage mismatches, FIPS codes losing leading zeros,
percentages or rates treated as counts, and labels attached to the wrong
codes. Check empty inputs, territories, single-county states, filtered
subsets, repeated requests and cached responses.

## 3. Scientific definition audit

Build a definition table for each quantity the package returns or
derives: reported indicators, imputed suppressed counts, the no-county
remainder, tract and block-group estimates, and the populations behind
them. For each give the intended meaning, supported diseases, strata and
years, source fields and codes, denominator and age universe, geography
vintage, missingness rules, source version and section, package
assumptions, implementation location and test coverage.

Require three checks: 1. Epidemiological validity of the definition for
its intended use. 2. Source support from CDC or Census documentation for
interpreting the data that way. 3. Correct implementation, including
vintages and missingness.

Do not treat agreement among AI reviewers as evidence. Preserve
disagreements and state what would settle them. Distinguish
source-backed facts from package choices and unresolved questions. A
methodological choice needs options, consequences and a named maintainer
decision; do not add options merely to avoid deciding.

## 4. Site, articles and documentation

The researcher using the package owns this workstream. The
epidemiologist and the ACS methods reviewer check that explanations are
correct, the package engineer checks that the site matches the code, and
the librarian checks citations. Give the researcher the published site
and the help pages first, before any code, so the first reading is a
user’s.

### Reading for understanding

Read every article, the README, and the help page of every exported
function as a researcher who knows R but not AtlasPlus or the package
would.

- **Can a reader get to a correct result?** Follow each article’s steps
  in order in the isolated workspace, using mocked or recorded data
  where live requests are not authorized. Record any step that fails,
  needs knowledge the text does not give, or depends on an earlier
  unstated choice.
- **Is each result explained for what it is?** A reader should know that
  filled suppressed counts are estimates, what “cases with no county”
  means, that tract and block-group figures are allocations rather than
  observed counts, which assumptions each step makes, and its
  limitations.
- **Are choices explained?** For each option that changes a result, the
  reader should learn what it does, when to pick each value, and what
  the default assumes.
- **Are terms defined?** Suppression, data status, indicator names, age
  universe, preliminary and provisional data, geography vintage: defined
  at first use, and the same way in the articles and the help pages.
- **Does it read well?** Purpose before procedure, simple cases before
  advanced ones, headings that say what a section is for, plain
  language, short sentences, acronyms spelled out, examples that look
  like real use.
- **Where could a careful reader still go wrong?** For example, summing
  state rows into a national total that includes territories, comparing
  preliminary years, treating rates or percentages as counts, or
  combining strata the package does not support.

Record each finding as the passage, the problem, its likely consequence
for a reader, and a proposed rewrite. Keep these separate from code
defects; an explanation that contradicts the code is both, and is
reported as both.

### Site and code agree

- Every exported function is in the reference index. Help text matches
  the behavior: arguments, defaults, returned columns, messages and
  errors. Examples run.
- Articles use the current API, and the output they show matches what
  the code returns. Figures from CDC or the Census say when they were
  retrieved or are produced at build. Descriptions of methods match the
  code and the reference catalogue.
- The deployed site was built from the reviewed revision (compare the
  site’s `pkgdown.yml` and the `gh-pages` commit with the default
  branch), the site builds, and its links work. `llms.txt` and the
  Markdown copies match the pages. Record root Markdown files published
  as unlinked pages.
- Accessibility: alt text that conveys what each chart shows, headings
  in order, descriptive link text, nothing conveyed by color alone,
  readable tables.
- Release: nothing private, nothing governed by other terms, and no keys
  on the public site.

## 5. Independent validation, test creation and CI

The testing and reproducibility engineer owns this workstream. The
epidemiologist, ACS reviewer and biostatistician establish expected
results; the package engineer helps construct fixtures. The testing
reviewer independently challenges those expectations and
implementations. Every reviewer contributes relevant failure cases.

### Existing tests and CI

Audit every test file, helper and workflow. Map tests to functions,
definitions and complete user workflows. Record what is untested; line
coverage alone does not establish correctness.

For CI, inspect triggers, tested revisions, R and OS matrices,
dependency installation and caching, permissions, secrets handling,
external-service access, timeouts, skips and failure propagation. Check
whether meaningful failures can be hidden by `continue-on-error`,
unconditional skips, or jobs that never run, including the live-tests
workflow’s label trigger. Distinguish workflow configuration from
observed execution. Inspect required-check settings where access
permits; unavailable settings are an explicit verification gap.

### Create and preserve independent audit tests

Derive expectations independently of the implementation, from
documentation and hand-worked synthetic examples. Assess whether mocks
bypass the behavior claimed and whether tests would detect meaningfully
wrong results. Reproduce serious suspected defects when feasible.

Write meaningful new synthetic tests for uncovered high-risk behavior
and suspected defects during the audit. Keep their fixtures, commands
and results in the permitted durable review deliverable, separate from
the frozen baseline. Use the existing isolated runner where useful; its
presence does not require committing a second test suite. A later test
author must be able to reproduce a finding from its issue example or
retained deliverable without the conversation.

For lasting coverage, strengthen existing tests or place a regression
beside the owning function’s tests with its approved fix. Do not commit
raw reports, role transcripts, coverage ledgers, runtime downloads or
exploratory scripts. Keep source facts in the existing reference
catalogue and decisions in owning issues; do not create a tracking issue
or a second live backlog.

For each new test record its purpose, independently justified
expectation, source or accepted contract, affected paths, related
issues, dependencies, command and result. Classify deliverables as:

- **Passing:** evidence for specified behavior and candidate permanent
  coverage.
- **Failing:** a reproducible discrepancy linked to a finding, with
  defect versus incorrect test expectation distinguished by evidence.
- **Decision-dependent:** alternative synthetic comparisons awaiting a
  maintainer decision. Keep them out of permanent pass/fail proposals
  until the contract is selected; label observed behavior without
  endorsing it.
- **Blocked or not run:** the exact blocker and remaining verification
  work.

Retain a failing reproducer even if the fix is outside review scope. Do
not make it pass by weakening assertions, adding unjustified skips or
silently changing package behavior.

### Concrete test and CI proposals

A prioritized proposal for ongoing testing is a required outcome.
Identify what belongs in regular tests, live tests, audit runners or CI,
including existing tests that should be strengthened rather than
duplicated. For each useful audit check, recommend integration into an
existing owning test file, a temporary reproducer retained with its
issue/review deliverable, or disposal. Explain coverage gained,
priority, runtime and maintenance cost, and prerequisites. If no changes
are warranted, justify that from the assessment.

Deliver reviewable test additions and inert workflow patches for
material gaps, with destination files, rationale, commands,
dependencies, expected signals and validation already performed. Keep
these proposals in the durable review deliverable, not a new repository
proposal directory. Prefer existing workflows; add a job only for a
demonstrated gap that they cannot adequately cover. Store unapproved
workflow proposals as patch files, not active workflows. Preparing
patches does not authorize activating workflows, changing repository
settings or using secrets. Installing permanent tests and workflows is a
separate step through task branches and PRs after maintainer review. Fix
PRs must integrate, replace or retire the relevant reproducer with a
brief rationale. Demonstrate the regression before and after the fix; do
not merge known failing tests, encode incorrect output as expected, or
add skips to make a gate pass. Remove standalone duplicates when
ordinary tests cover them.

### Execution record and remaining checks

Record OS, R version, dependency versions, commands, results and skips.
Separate personally executed synthetic checks, source inspection,
observed CI at the reviewed revision, author-reported checks and
unverified claims. If R or dependencies are unavailable, investigate
safe isolated alternatives and report the exact blocker. Do not claim
execution from inspection alone. Synthetic checks establish behavior,
not agreement with CDC’s published figures; give exact live checks the
maintainer can authorize.

## 6. Reconcile every open issue

After independent discovery, give the relevant reviewers the full issue
and discussion, linked PRs and supporting material. Each open issue
receives a substantive assessment, even if it is mistaken, duplicated,
already fixed, intentional incremental work or a valid future
enhancement.

For every issue record: number, title, URL, reviewed timestamp and
revision; whether its premise is supported and by what evidence; current
implementation and downstream consequences; whether the suggested fix
solves the problem or creates others; related findings, issues and PRs,
merged or proposed; missing tests, source verification and remaining
decisions; recommended disposition and a complete draft comment.

Do not accept an issue’s proposed solution as authoritative. Do not use
generic “still relevant” comments in place of analysis. Refresh the
issue inventory at report completion; account for newly opened or closed
issues explicitly.

## 7. Cross-specialty challenge and future gaps

After the first passes, have the epidemiologist, ACS reviewer and
biostatistician challenge estimate definitions together; the AtlasPlus
specialist and package engineer examine request, response and lookup
handling; the testing reviewer challenges claimed guarantees. Recheck
findings against source, downgrade unsupported claims and record
corrections. Merge duplicates without discarding distinct consequences.

Assess future needs against concrete research workflows. For each gap
state the use case, source support, why it belongs in CDCAtlas rather
than an individual analysis, dependencies, priority and an acceptance
criterion. Separate correctness blockers, validation gaps, design
decisions, documentation needs and optional enhancements. Consider reuse
of existing capabilities before proposing new APIs.

## 8. Consolidated report and completion criteria

Deliver: - Executive assessment and prioritized findings. - An
issue-by-issue assessment and draft comment for every open issue. -
Proposed new issues, deduplicated against open and closed issues. - A
decision register with alternatives and consequences. - Definition and
source tables, and the coverage ledger. - Validation log, limitations,
reviewer disagreements and corrections. - Reusable audit tests and
synthetic fixtures, exact execution instructions, and a manifest
distinguishing passing, failing, decision-dependent and blocked
checks. - Concrete permanent-test and CI patches for material gaps. -
Site, article and help findings, with proposed rewrites. - A staged
roadmap appropriate to the package’s development stage.

Give each finding a stable ID, severity (Must fix / Should fix /
Consider), classification (verified defect / concern / validation gap /
decision / documentation / enhancement), location at a fixed revision,
evidence, consequence, confidence, proposed remedy and acceptance check.
Keep classification separate from severity. Do not report suggestions as
bugs.

An audit is complete only when every in-scope file, function and open
issue has a disposition; serious findings have a reproducer or a
specific verification blocker; scientific claims have sources or
explicit uncertainty; draft comments and new-issue proposals are ready
for maintainer review; and reusable audit tests and justified test and
CI proposals are preserved and linked. “Not assessed” remains a coverage
gap. Report analytical coverage, remaining validation and repository
integration separately. If gaps remain, label the report partial and
state what remains; posting findings does not establish that checks or
documentation were integrated.

### Integrate knowledge before closing the review

For each material discovery, identify its maintained destination or
explain why it belongs only in the archive. Read
[reference/README.md](https://vagishhemmige.github.io/CDCAtlas/reference/README.md)
before adding reference information and follow its placement rules.
Proposed regression tests belong beside the affected tests, user-facing
corrections in the relevant help or article, and process lessons in the
applicable review protocol. Retain audit reproducers until their
implementation is approved.

Prepare concrete, self-reviewed diffs for existing documentation as
review deliverables on a task branch or PR where permitted. Keep full
reports and bookkeeping outside the repository; a short PR description
and the owning issues are sufficient for integration status. Record each
material result as integrated (with link), proposed (with patch or
link), deferred (with owning issue and reason) or archive-only (with
reason). Track unresolved package choices in their owning issues;
reference entries may explain the alternatives without becoming a
decision log.

End the consolidated report with a short closing summary: what was
preserved, where it lives, and what remains unresolved. Future
first-pass packets must still exclude these accumulated diagnoses until
reconciliation.

## 9. Report first, posting afterwards

Present the consolidated report to the maintainer before posting any
audit comments or new issues. Wait for the maintainer’s response
authorizing the posting step; elapsed time is not approval.

The planned follow-up is a concise comment on every still-open issue and
new issues for accepted findings, prepared in the report first. Before
posting, refresh revisions and discussions, check for duplicate comments
and issues, and revise drafts when intervening work changes the
conclusion. Each comment is a dated reassessment tied to the reviewed
commit: current disposition (still applicable, partly resolved,
resolved, or needs a decision), specific evidence, and next step. An
unchanged conclusion still gets a brief record that it was reevaluated.
Omit repeated scope paragraphs, generic disclaimers, reviewer-role
footers and private finding IDs. Provide commit-pinned links and public
reproducers or small inline synthetic examples. Confirm each write
succeeded and map report IDs to posted URLs in the audit ledger.

Do not close issues, approve or merge PRs, or modify package code as
part of this audit. Those actions require their own authorization.
Behavior changes require per-file approval of concrete, self-reviewed
drafts under the maintainer’s working instructions. This document grants
no standing posting, implementation or automated-review permissions.

## 10. Posting templates

The templates are drafting prompts, not mandatory boilerplate. Replace
placeholders, omit inapplicable sections, and never fill a field with an
invented result. One comment per existing issue consolidates the
relevant reviewers’ findings. A finding already covered by an issue
belongs in that issue unless a distinct scope justifies a new linked
issue. Do not create or apply labels merely because a template names a
classification or severity.

### Comment on an existing open issue

``` markdown
Re-evaluated against [<commit>](<commit URL>) on <date>: <current disposition>.
<Plain-language conclusion and practical consequence.>

Evidence: <pinned code or source links and what they establish; check result>.
<When needed: inline synthetic example or commit-pinned reproducer; state
source-inspection-only or unexecuted status and material limitations.>
Next step: <specific action or maintainer decision, or no further action>.
```

For an already-fixed issue, cite the fixing commit and evidence at the
reviewed revision. For an unsupported premise, explain why and
acknowledge residual uncertainty. For a duplicate, name the overlap and
any unique part to keep. Recommending closure does not close an issue.

### New defect or validation-gap issue

Suggested title:
`<function or component>: <specific incorrect behavior or gap>`.

``` markdown
## Problem

<Concrete trigger, current behavior and expected behavior. For a validation
gap, identify the unverified guarantee without asserting an unproven defect.>

**Review date:** <date>.
**Reviewed revision:** <full commit SHA with link>.
**Classification:** <verified defect / concern / validation gap / documentation>.
**Priority:** <Must fix / Should fix / Consider>.
**Confidence:** <level and supporting evidence>.

## Evidence and impact

<Commit-pinned locations; affected functions, diseases, strata and workflows;
why the result matters; established in current code or only suspected.>

## Reproduction and validation

<Minimal synthetic example and exact commands, with actual versus expected
output. If not executed, say "Proposed reproducer, not executed" and why.
Separate personally executed checks, observed CI and author-reported results.>

## Source basis

<CDC or Census source URL, version or access date, section, and what it
supports. Identify package assumptions and unresolved choices separately.>

## Proposed resolution

<Focused remedy and affected paths. Name decisions needed before
implementation. Do not present an unapproved default as settled.>

## Acceptance criteria

- <Correct observable behavior, including edge cases.>
- <Regression test and live check where needed.>
- <Help, article or reference updates.>

## Related work

<Issues and PRs checked; why this is distinct.>
```

### New methodological decision issue

Suggested title:
`Decision: <specific definition or methodological choice>`.

``` markdown
## Decision needed

<The exact question, affected functions and outputs, and why the choice cannot
be inferred safely from CDC or Census documentation.>

**Review date:** <date>.
**Reviewed revision:** <full commit SHA with link>.
**Decision owner:** <maintainer or designated person>.
**Status:** Proposed; not decided.

## Current implementation and evidence

<Commit-pinned behavior, public sources with versions and sections, and
uncertainties. Separate documented facts from package assumptions.>

## Alternatives

| Option | Meaning | Consequences and limitations | Evidence |
| --- | --- | --- | --- |
| <A> | <Definition or estimand> | <Affected outputs and users> | <Source or assumption> |
| <B> | <Definition or estimand> | <Affected outputs and users> | <Source or assumption> |

## Synthetic comparison

<How the options change counts, denominators or estimates on the same synthetic
inputs; executed or hand-worked.>

## Reviewer positions

<Reasoned recommendation, disagreement, and evidence that would resolve it.>

## Completion criteria

- Maintainer records the selected definition and rationale.
- Required behavior changes and affected files are identified.
- Tests and documentation implement the selection consistently.

**Related work:** <issues and PRs>.
```

### New future-capability issue

Suggested title: `Proposal: <capability and concrete use case>`.

``` markdown
## Research use case

<The workflow currently blocked or error-prone, and who needs it.>

**Review date:** <date>.
**Reviewed revision:** <full commit SHA with link>.
**Classification:** Enhancement proposal.
**Priority and rationale:** <relative to correctness and validation work>.

## Existing support and gap

<What CDCAtlas already provides and why composition does not solve this.>

## Source feasibility and scope

<Verified AtlasPlus indicators, Census tables or methods with sources and
versions; limitations and unresolved feasibility.>

## Proposed capability

<Minimal behavior and outputs; dependencies and alternatives.>

## Acceptance criteria

- <End-to-end synthetic workflow.>
- <Source verification and needed decisions.>
- <Tests, documentation and remaining live validation.>

**Related work:** <issues and PRs; why this is distinct>.
```

Before publication, confirm that the text matches the
maintainer-reviewed report and any authorized revisions, refresh the
evidence against intervening changes, and record the comment or issue
URL in the posting ledger.
