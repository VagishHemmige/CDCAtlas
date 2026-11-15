# Reference material

Background on the data CDCAtlas works with: CDC AtlasPlus indicators and code
tables, suppression and data status, Census geography and ACS tables, and
methods for small-area estimation. This README is the guide to where new
reference information belongs.

This folder is excluded from the package build (`.Rbuildignore`) and is not
published on the pkgdown site.

| File | What it holds |
|---|---|
| [`web-sources.md`](web-sources.md) | Official pages, documents and papers consulted, with access dates and key passages |
| [`atlasplus-notes.md`](atlasplus-notes.md) | Facts about the AtlasPlus endpoints, code tables and responses, each with how and when it was checked |
| [`methodology-review.md`](methodology-review.md) | Topic-by-topic background: suppression, cases with no county, geography vintages, population-weighted allocation, age universes |
| [`repositories.md`](repositories.md) | Other code repositories reviewed, with the commit read and what was learned |

## Scope and placement

Keep CDCAtlas function behavior, defects, proposed fixes, test results and
implementation decisions out of this folder. Those belong in issues and PRs,
tests, function documentation or dated review records. For example, "CDC
reports earlier years on its current county map" is reference background;
whether a CDCAtlas function handles that correctly is an issue.

Use the table above to choose one primary home. Link related entries rather
than copying them. Issues and PRs cite reference sections; reference entries do
not link back to issues or track implementation status. Record uncertainty
without turning alternatives into approved package defaults. Amend existing
topical entries; do not append whole review reports or reviewer transcripts.
External code is an example to assess, not an authority.

## Conventions

- **Record what was actually read:** the commit for a repository, the access
  date for a web page, the release or vintage for a table. Mark anything cited
  from memory or second-hand as *not yet verified*.
- **Prefer short source summaries** with section, page or table locators. Use a
  short exact quotation only when the wording matters.
- **Code-table claims** ("code 650 is ages 13 and older") say what they were
  checked against: `R/sysdata.rda` at a named commit, or a dated request to the
  AtlasPlus endpoint.
- **Observations from live requests** are tagged `[observed <date>]` with the
  request (indicator, geography, year, strata). They describe CDC's data, not
  CDCAtlas.
- **Files with their own terms** (AIDSVu downloads, health-department reports)
  are described and cited here, never copied here.
- **Citing from code:** a `#` comment at the line that makes a choice may cite
  an entry by its heading (`# Census county changes, 2010s (reference/)`),
  never by file path, because files here may move. Reference entries never name
  CDCAtlas functions.
