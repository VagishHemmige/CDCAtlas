# About CDC AtlasPlus data

This article explains what the numbers from CDC AtlasPlus mean and where
they can trip you up. It complements [Getting
started](https://vagishhemmige.github.io/CDCAtlas/articles/getting-started.md),
which shows how to call the functions, and [Extrapolation to
tracts](https://vagishhemmige.github.io/CDCAtlas/articles/extrapolation.md),
which shows the tract estimates. It has no code that downloads data.

AtlasPlus has no published API. CDCAtlas uses the endpoints that the
AtlasPlus web application itself uses, and CDC can change them without
notice. What follows was checked against the application and its
documentation in November 2026. Sources are listed at the end.

## What a row’s `data_status` means

Every row from
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
has a `data_status`:

| `data_status` | What it means | How CDCAtlas handles it |
|----|----|----|
| “Not Suppressed” | CDC published the count. | Kept as it is. |
| “Data suppressed” | CDC withheld a small count. | [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md) fills it in. |
| “Data not available” | CDC has no data for this row. | Kept by [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md), with or without a count; a row with no count is left out of the tracts, and dropped with `not_available = "drop"`. |
| “No reported cases” | CDC received no reports. | Kept as it is. A row with no count is not filled in: it stays `NA`, with a warning. |
| “CDC code” and a number | A code this version of CDCAtlas doesn’t know yet. | Kept, with a warning when downloaded. A row with no count is not filled in. |

Only rows marked “Data suppressed” are filled in. A row with no count
and a status other than “Data suppressed” or “Data not available” (or no
status) is left as `NA`, marked `cases_source = "not estimated"`, and
named in a warning. It is left out of the tracts and counted in the
state’s remainder (see below). “Data not available” rows are handled as
the table says: kept as `cases_source = "not available"` by default, or
dropped.

CDC’s technical notes give the suppression rules but don’t define these
labels. Their meaning here is inferred from those rules.

A few rows marked “Data not available” do have counts. Connecticut’s
planning regions in 2017 are an example: they have counts but no
population. Any published count is kept, whatever its status.

### Suppressed is not zero

A suppressed county has cases that CDC didn’t publish. Treating it as
zero undercounts.
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
estimates it instead, from the state’s rate in the same indicator, year
and stratum, times the county’s population, kept between `floor` and
`cap`. Filling suppressed counts with zero is available as
`method = "zero"`, as a sensitivity analysis.

The column `cases_uncapped` keeps the estimate before `floor` and `cap`
were applied, so `cases_uncapped > cap` picks out the rows whose value
the cap set. It is a modeled number (state rate times county
population), not CDC’s hidden count.

The suppression rules differ by disease, according to CDC’s technical
notes:

- **HIV, county level:** in counties under 500,000 people, when the
  population is under 100 or the count is 1 to 4. In counties of 500,000
  or more, when the population is under 100. Also when a jurisdiction’s
  re-release agreement requires it, and when a suppressed cell could be
  worked out from the others (complementary suppression).
- **STIs:** from 2019 data, under the 2017 CSTE re-release rules: when
  “20% of individuals in a group have the specified disease” (CDC’s
  wording) or the population is under 100, plus complementary
  suppression. Data for 2000 to 2018 follow the 1996 rules.
- **Tuberculosis:** no suppression at the state level. County cells are
  suppressed where populations have fewer than 5 total cases.
- **Viral hepatitis:** “Data not available” when a condition is not
  reportable or the data are unavailable.

So a suppressed HIV county in a county under 500,000 usually has 1 to 4
cases. But complementary suppression and re-release agreements can hide
larger counts. That is why `cap = 4` is a default, not a hard limit.

## Provisional data and cautions

For HIV, CDC’s technical notes say data are based on cases reported
through December 2025, and “Data through 2024 are considered
provisional.” Data are preliminary until a 12-month reporting delay has
passed, and should not be used for formal trend analyses.

The AtlasPlus application also says:

> “Due to the impact of the COVID-19 pandemic, data for 2020 and 2021
> may need to be interpreted with caution.”

> “CDC has postponed release of estimates for HIV incidence, HIV
> prevalence, and PrEP coverage.”

## Rates and denominators

The `rate100000` column is not the same kind of rate for every
indicator:

| Indicator | `rate100000` |
|----|----|
| HIV | per 100,000 people aged 13 and older |
| Congenital syphilis | per 100,000 live births |
| Knowledge of Status (`disease = "estimate"`) | a percentage |
| Other indicators | per 100,000 people |

AtlasPlus includes HIV surveillance data only for people aged 13 and
older, so HIV counts and populations are for ages 13 and older.

For Knowledge of Status, the columns hold:

- `rate100000`: the percentage who know their status;
- `lowerci_rate` and `upperci_rate`: the bounds of that percentage;
- `cases`: the number who know their status;
- `population`: the estimated number of people with HIV.

There are no confidence intervals for the counts.

Because its population is people living with HIV, not residents,
`impute_suppressed(population = "acs")` refuses Knowledge of Status; the
default `population = "cdc"` works.
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
shares a county’s count of people who know their status across tracts by
residents, as if people with HIV lived in each tract in proportion to
its population. So `area_cases / area_population_acs` is not the
percentage who know their status.

For HIV, cases with unknown race or ethnicity are in the totals but not
shown by race, so rows by race may not add up to the state or national
total.

## What AtlasPlus offers for each disease

AtlasPlus’s set-up data list, for each indicator, the geographies and
groups it offers.
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
follows them.

### Geographies

| `geography`             | Diseases                                     |
|-------------------------|----------------------------------------------|
| `"national"`, `"state"` | all                                          |
| `"county"`              | all except hepatitis and congenital syphilis |
| `"msa"`                 | `"hiv"` only                                 |
| `"region"`              | `"estimate"` only                            |

County data for the HIV estimates (`"estimate"`) cover only the Ending
the HIV Epidemic (EHE) counties, for selected years (AtlasPlus Technical
Notes, 1.1 and 1.4.1).

For reported data such as HIV diagnoses, CDC’s server doesn’t return
regions. The AtlasPlus web page asks for each region’s states and adds
them up in the browser. CDCAtlas doesn’t do that: suppressed state
counts would need care, and rates must be recomputed from cases and
populations.

Not every combination of disease, geography and stratification is
offered. A request CDC doesn’t answer stops with an error saying CDC
returned no data. If CDC returns some of a disease’s indicators but not
others (a year not yet released for one of them, for example),
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
warns and names the missing ones.

### Age groups

| `disease` | Age groups with `stratify_by = "age"` |
|----|----|
| `"hiv"`, `"estimate"` | 13-24, 25-34, 35-44, 45-54, 55-64, 65+ |
| `"chlamydia"`, `"gonorrhea"`, `"adult syphilis"` | 0-14, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-54, 55-64, 65+, Unknown |
| `"tuberculosis"`, `"hepatitis"` | the same, without Unknown |
| `"congenital syphilis"` | none |

Each set covers that disease’s ages once. Congenital syphilis can’t be
stratified by race, sex or age.

### Race and ethnicity groups

AtlasPlus combines race and Hispanic ethnicity into one variable,
`race_ethnicity`. Hispanic/Latino people of any race are one group; the
other groups are non-Hispanic.

| `disease` | Groups with `stratify_by = "race"` |
|----|----|
| `"hiv"`, `"estimate"` | American Indian/Alaska Native, Asian, Black/African American, Hispanic/Latino, Multiracial, Native Hawaiian/Other Pacific Islander, White |
| `"chlamydia"`, `"gonorrhea"`, `"adult syphilis"`, `"tuberculosis"` | the same, plus Unknown |
| `"hepatitis"` | American Indian/Alaska Native, Asian/Pacific Islander, Black/African American, Hispanic/Latino, White, Other, Unknown |

Hepatitis combines Asian and Pacific Islander and has no Multiracial
group, so its groups don’t line up with the others.

Cases of unknown age (and unknown race) have no Census population. By
default
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
leaves them out and lists them in `attr(result, "not_allocated")`. With
`unknown_age = "redistribute"` (or `unknown_race = "redistribute"`) they
are shared across the county’s known groups in proportion to their
counts.

## State totals and county counts

A state’s total is usually larger than the sum of its published county
counts. For HIV prevalence in 2022, the gap was 1,804 in North Carolina,
1,888 in Virginia and 1,555 in Georgia.

Suppressed counties explain part of each gap, but not all of it, if each
holds 1 to 4 cases. One explanation is cases whose county of residence
is unknown or not released. CDC’s documentation doesn’t say so, though,
and other explanations are not ruled out:

- complementary suppression and re-release agreements can hide more than
  4 cases in a county;
- rows marked “Data not available”;
- state and county figures may cover different releases.

[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
reports the difference between each state total and the county counts
supplied, after
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
has filled in suppressed counties. The difference also includes any
county left out of the data you pass in.
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
leaves the remainder out by default (`no_county = "report"`).
`no_county = "spread"` shares it across the state’s counties; that
treats it as cases with no county, which is an assumption.

Where a state stratum has exactly one county without a published count,
the difference is that county’s count plus any cases with no county.
Since CDC’s documentation doesn’t say there are any, it could be that
county’s count exactly, so
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
doesn’t report it: the column `no_county_withheld` says so, and
`no_county = "spread"` doesn’t spread it.

For the HIV estimates, the counties are EHE counties only, so the
remainder can include people in the state’s other counties and isn’t
necessarily cases with no county.
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
says so, and `no_county = "spread"` is refused for them.

## CDC’s county map

CDC reports every year on its current county map, not on the map of that
year:

- **Connecticut, 2017:** reported by its nine planning regions, which
  became county-equivalents in the 2020s. In 2017 they have counts but
  no population.
- **Alaska, 2017:** Chugach (02063) and Copper River (02066), created in
  2019 from Valdez-Cordova, have counts. Valdez-Cordova (02261) is
  listed with no population.
- **Alaska, 2022:** Valdez-Cordova (02261) is still listed beside its
  successors, with no population.

The Census tracts don’t match these older years. An ACS 5-year release
uses the tract map of its last year, so 2015–2019 is the last release on
2010 tracts:

| ACS 5-year release  | Tract map   |
|---------------------|-------------|
| 2015–2019           | 2010 tracts |
| 2016–2020 and later | 2020 tracts |

[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
uses the 5-year ACS ending in the data year, so 2017 uses 2010 tracts
and 2022 uses 2020 tracts. Tract estimates from 2017 and 2022 can’t be
compared tract by tract.

For ACS releases on 2010 tracts (2006–2010 to 2015–2019), a crosswalk
assigns Connecticut’s and Valdez-Cordova’s 2010 tracts to the current
counties. ACS 2016–2020 and 2017–2021 use 2020 tracts but still code
Connecticut by its old counties; planning regions first appear in
2018–2022. So Connecticut data for 2020 and 2021 is left out of tract
estimates with a warning that says why. Kusilvak, Alaska and Oglala
Lakota, South Dakota were renamed in 2015 with new codes, so ACS
releases before 2015 don’t have them either. These are the cases the
package knows about, not a full check of every ACS release. Rows left
out are listed in `attr(result, "not_allocated")`.

## Census denominators

[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
shares each county’s count across its tracts in proportion to each
tract’s population in the same stratum, from the ACS. This assumes every
tract in a county has the same rate within a stratum. The estimates add
up to the county counts by construction, so that alone doesn’t show the
pattern across tracts is right.

The populations come from these ACS tables:

- **No stratification, sex and age:** B01001 (sex by age).
- **Race:** B03002 (Hispanic or Latino origin by race) by default. Its
  groups match AtlasPlus’s, but it has no ages. For HIV, each tract’s
  race populations are scaled by the tract’s share of residents aged 13
  and older. `race_table = "B01001"` uses the race-specific sex-by-age
  tables instead, which have ages, but only their White and Hispanic
  tables match AtlasPlus’s groups.

The ACS age bands don’t start at 13. Each CDC age group counts the share
of each ACS band its ages cover, assuming ages are spread evenly within
a band. So ages 13 and older, and HIV’s 13-24 group, count 2/5 of the 10
to 14 band. That is an assumption, not a Census figure.

Denominators are for ages 13 and older for HIV, and all ages for other
diseases. Congenital syphilis isn’t supported, because its rates are per
live birth.

**Puerto Rico by race:** CDC gives Puerto Rico’s counts by race without
a population, so a suppressed municipio row by race can’t be estimated.
Those rows are left out.

## FIPS codes

The `fips` column is character, zero-padded: two digits for states and
five for counties. Keep it as character when joining to Census data, so
leading zeros aren’t lost.

## Saved downloads

Each response from CDC is saved on your computer and reused for 30 days,
so the same data aren’t downloaded again. Requests are made one disease
and one year at a time (and for counties, one state at a time), so data
already downloaded, alone or as part of a larger request, are reused.

- `get_atlas(refresh = TRUE)` downloads the request again. For county
  data it also refreshes the matching state totals that the later steps
  use.
- [`clear_atlas_cache()`](https://vagishhemmige.github.io/CDCAtlas/reference/clear_atlas_cache.md)
  deletes every saved response.

After 30 days the data are downloaded again, which picks up revisions
CDC has made.

## Sources

- CDC, [AtlasPlus](https://gis.cdc.gov/grasp/nchhstpatlas/main.html).
  The technical notes and glossary are inside the application (menu,
  “Technical notes”).
- CDC, [AtlasPlus
  glossary](https://gis.cdc.gov/grasp/nchhstpatlas/Content/docs/AtlasPlusGlossary.pdf).
- CDC, [NCHHSTP AtlasPlus
  overview](https://www.cdc.gov/nchhstp/about/atlasplus.html), with
  CDC’s suggested citation.
- CDC, [National HIV Surveillance
  System](https://www.cdc.gov/hiv-data/nhss/).
- Census Bureau, [ACS geography boundaries by
  year](https://www.census.gov/programs-surveys/acs/geography-acs/geography-boundaries-by-year.html).
- Census Bureau, [substantial changes to
  counties](https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html).
- Census Bureau, ACS tables
  [B01001](https://api.census.gov/data/2022/acs/acs5/groups/B01001.html)
  and
  [B03002](https://api.census.gov/data/2022/acs/acs5/groups/B03002.html)
  (2022 5-year).
