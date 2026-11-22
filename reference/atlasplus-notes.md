# AtlasPlus endpoints, code tables and responses

AtlasPlus has no published API. These notes record what the web application's
own endpoints return, each with how and when it was checked. CDC can change any
of it without notice.

## Endpoints

- **Set-up data:** `GET https://gis.cdc.gov/api/AtlasPlus/getInitData/00`
  returns JSON (about 2.6 MB) whose fields include the code table `varvals`
  and the application's menus. [observed 2026-11-14, web fetch] The same path
  under `/grasp/AtlasPlus/` was used earlier; the `/api/` path is current.

## The code table (`varvals`)

Checked against `R/sysdata.rda` at
[`118d0df`](https://github.com/VagishHemmige/CDCAtlas/tree/118d0df),
committed on 26 April 2026. Another review's fetch of the set-up data on
14 November 2026 counted 3,656 rows, so CDC appears to have added codes since
(reported, not re-checked).

- **Columns:** `id`, `name`, `sortid`, `vtid` (code type), `parentid`, `fips`,
  `geoLevel`, `ehe`.
- **3,653 rows** in the stored copy. Code types (`vtid`), with example names:

  | `vtid` | Rows | Examples |
  |---|---|---|
  | 1 | 35 | Social determinants: Uninsured, Vacant housing |
  | 2 | 48 | Years, 1980 to 2025 (codes 502 to 547; 2024 is 546, 2025 is 547) |
  | 3 | 3,465 | Geographies (below) |
  | 4 | 14 | Data status ("Not Suppressed", "Data suppressed", "Data not available", several codes each) and urbanicity (Rural, Suburban, Urban) |
  | 5 | 11 | Race and ethnicity, from "All races/ethnicities" |
  | 6 | 10 | Sex and gender, from "Both sexes" |
  | 7 | 34 | Age groups, including 650 "Ages 13 years and older" and 651 "All age groups" |
  | 8 | 8 | Transmission category and country of birth |
  | 30 | 5 | Topic areas: HIV, Viral Hepatitis, STD, TB, SDOH |
  | 50 | 5 | Geography menu: National, State, County, Regions, MSA |
  | 80, 100–110 | 20 | Display settings (rate or cases, map breaks, chart types) |

- **Geographies (`vtid` 3) by `geoLevel`:** 1001 national (1: 5000 "United
  States"); 1002 states and territories (57: the 50 states and DC as
  5001–5051, five territories as 5052–5056, and 8451 Palau, FIPS 70); 1003
  counties (3,241); 1004 metropolitan statistical areas (161); 1005 regions
  (5).
- **Regions:** 8453 Northeast, 8454 Midwest, 8455 South, 8456 West, 8457 "U.S.
  territories and freely associated states". Their `fips` values are seven-digit
  codes (8300100 to 8300500), not Census region codes.
- **Counties include old codes:** the county list has the pre-change Alaska
  areas (02201, 02232, 02270, 02280), Shannon County SD (46113) and Bedford city
  VA (51515), and four island territories (60000, 66000, 69000, 78000), as well
  as current codes. See "Census county changes, 2010s" in
  [web-sources.md](web-sources.md).
- **MSAs:** 158 areas plus three summary rows (8436–8438).

## Responses

Recorded from live runs on the dates given; not re-run for this file.

- **Earlier years use CDC's current county map.** For 2017, Connecticut is
  reported by its nine planning regions (county-equivalents adopted later), and
  Alaska includes Chugach (02063) and Copper River (02066), created in 2019.
  Valdez-Cordova (02261) is still listed, with no population, in 2017 and
  2022. [observed 2026-10-30, HIV, county, 2017 and 2022]
- **Connecticut planning regions in 2017** have case counts but status "Data not
  available" and no population, so no rate. [observed 2026-10-29, HIV
  prevalence, county, 2017]
- **Puerto Rico by race:** state counts are given with population "Data not
  available", so no rate. Unstratified, sex and age rows have populations.
  [observed 2026-10-30, HIV, state, by race]
- **County suppression, HIV prevalence 2022:** 2,776 counties with counts,
  447 "Data suppressed", 8 "Data not available" (Connecticut's old counties).
  [observed 2026-10-29, HIV prevalence, county, 2022]
- **State totals exceed the sum of their published counties.** For HIV
  prevalence in 2022 the gap was 1,804 in North Carolina, 1,888 in Virginia and
  1,555 in Georgia, with 455 suppressed counties nationwide in that run.
  [observed 2026-10-29, HIV prevalence, state and county, 2022]
  - *Hypothesis:* much of each gap is cases with no county of residence.
  - *Not ruled out:* a suppressed county can hold more than 4 cases, because
    cells are also suppressed to prevent back-calculation, under re-release
    agreements, and in counties of 500,000 or more ("CDC, AtlasPlus Technical
    Notes" in [web-sources.md](web-sources.md)); rows marked "Data not
    available"; incomplete retrieval; and state and county figures that cover
    different geographies or releases.
  - *What would settle it:* CDC documentation of how cases with an unknown
    county are counted, or a comparison against a CDC source that reports them.
  - Hidden county counts cannot be recovered from the gap.

## County suppression in HIV prevalence, 2017, 2022 and 2024

Read from the logs of GitHub Actions runs in this repository; none has had an
independent check. Prevalence is people living with diagnosed HIV; HIV figures
are for ages 13 and older. Each entry names its source:

- *Measured-effects runs:* 44916253376 (21 November 2026) and 44974811287
  (22 November 2026 UTC) of the measured-effects script. [observed 2026-11-21
  and 2026-11-22, HIV prevalence, state and county, 2017, 2022 and 2024,
  unstratified and by sex, by age group and by race/ethnicity]
- *Live test suite runs:* 44861517172 (21 November 2026 UTC) and 44966885019
  (22 November 2026 UTC). [observed 2026-11-21 and 2026-11-22, county: HIV
  2017 and 2022, unstratified and by sex, age and race; chlamydia 2022 by age;
  HIV 2024 and 2025; both HIV indicators where returned]
- *Live test run 44985849638* (22 November 2026 UTC), counts only. [observed
  2026-11-22, HIV diagnoses and HIV prevalence, county, 2017, 2022, 2024 and
  2025, unstratified and by sex, age and race]

Entries:

- **Latest year with county rows:** 2025 for HIV diagnoses, 2024 for HIV
  prevalence. The 2025 county response has diagnoses rows and no prevalence
  rows. (Live test suite runs.)
- **Suppressed county rows carry no rate and no rate bounds,** in every county
  response of the live test suite runs.
- **State totals and counties with a count, unstratified.** "Counties
  without" is every county row without a count, suppressed or "Data not
  available"; for 2022 see also "County suppression, HIV prevalence 2022" under
  [Responses](#responses). (Measured-effects runs.)

  | Year | Sum of state totals | Counties with a published count | Counties without |
  |---|---|---|---|
  | 2017 | 1,002,796 | 2,724 | 507 |
  | 2022 | 1,102,798 | 2,776 | 455 |
  | 2024 | 1,157,995 | 2,813 | 418 |

  By race, the state rows for 2022 sum to 613 fewer. CDC shows no unknown-race
  row for HIV ("CDC, AtlasPlus Technical Notes", 1.9, in
  [web-sources.md](web-sources.md)).
- **Share of county rows suppressed** rises with stratification and falls
  slightly over the years (measured-effects runs):

  | Stratification | 2017 | 2022 | 2024 |
  |---|---|---|---|
  | None | 15% | 14% | 13% |
  | Sex | 44% | 43% | 42% |
  | Age group | 50% | 49% | 48% |
  | Race/ethnicity | 60% | 58% | 57% |

- **Suppressed stratum cells within a county whose unstratified count is
  published.** Counties by the number of their stratum cells that are
  suppressed (measured-effects runs, HIV prevalence):

  | Year | Stratification | None | Exactly one | Two or more |
  |---|---|---|---|---|
  | 2017 | Sex | 1,790 | 0 | 934 |
  | 2017 | Age group | 751 | 0 | 1,973 |
  | 2017 | Race/ethnicity | 234 | 5 | 2,485 |
  | 2022 | Sex | 1,826 | 0 | 950 |
  | 2022 | Age group | 793 | 0 | 1,983 |
  | 2022 | Race/ethnicity | 244 | 0 | 2,532 |
  | 2024 | Sex | 1,868 | 0 | 945 |
  | 2024 | Age group | 806 | 0 | 2,007 |
  | 2024 | Race/ethnicity | 244 | 0 | 2,569 |

  Live test run 44985849638 counted the "exactly one" column again for both
  HIV indicators. It was 0 for diagnoses and for prevalence, by sex, age and
  race, in 2022, 2024 and 2025. In 2017 it was 1 for diagnoses by age, 2 for
  diagnoses by race and 5 for prevalence by race, and 0 otherwise; the 2017
  responses also have a few cells that are neither published nor suppressed.
  That is consistent with complementary suppression ("CDC, AtlasPlus Technical
  Notes", 1.8.4). Only counts were recorded, not the counties, and whether
  any of the 2017 cells could be recovered was not assessed.
- **Published strata against the county's unstratified count** (live test run
  44985849638). Among counties where every stratum cell is published:
  - By sex and by age, the strata sum exactly to the unstratified count, for
    both indicators in every year.
  - By race, diagnoses also sum exactly. Prevalence sums to less in 47 of 233
    counties (2017), 51 of 244 (2022) and 54 of 244 (2024), and never to
    more. That is consistent with CDC showing no unknown-race row (Technical
    Notes, 1.9).

  In every county, the published strata alone never exceed the unstratified
  count.
