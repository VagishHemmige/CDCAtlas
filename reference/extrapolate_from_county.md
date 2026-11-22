# Share county counts across smaller Census areas

`extrapolate_from_county()` shares each county's count across the
county's census tracts or block groups, in proportion to each area's
population in the same stratum, from the American Community Survey (ACS)
5-year estimates. Run it on county data that has been through
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md).

The estimates are modelled, not observed: they assume the same rate in
every area of a county within a stratum.

For Knowledge of Status, `county_cases` is the number of people with HIV
who know their status, and it is shared across areas by their residents,
as if people with HIV lived in each area in proportion to its
population. The ACS columns are residents, not people with HIV, so
`area_cases / area_population_acs` is not the percentage who know their
status.

## Usage

``` r
extrapolate_from_county(
  x,
  to = c("tract", "block group"),
  no_county = c("report", "spread"),
  spread_by = c("population", "cases"),
  empty_strata = c("total", "equal", "none"),
  ages = c("auto", "13+", "all"),
  race_table = c("B03002", "B01001"),
  unknown_race = c("report", "redistribute"),
  unknown_age = c("report", "redistribute"),
  unestimated = c("leave out", "error"),
  missing_population = c("leave out", "error"),
  acs_year = NULL,
  weights = NULL
)
```

## Arguments

- x:

  County data from
  [`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
  that has been through
  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md),
  with at most one stratification variable.

- to:

  The areas to share county counts across. Both nest inside counties, so
  the estimates add up to the county counts exactly:

  - `"tract"` (default): census tracts. ACS 5-year estimates from 2009.

  - `"block group"`: census block groups. ACS 5-year estimates from
    2013; smaller areas have larger ACS margins of error.

  Areas that cross county lines, such as ZCTAs, are not supported yet.

- no_county:

  Cases in state totals with no county (see
  [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)):

  - `"report"` (default): left out of the estimates, with a message
    giving the total. The report is kept in
    `attr(result, "no_county_cases")`.

    For CDC's HIV estimates, whose county data cover only the Ending the
    HIV Epidemic (EHE) counties, the remainder can include people in
    counties outside EHE coverage, and a message says so.

  - `"spread"`: shared across the state's counties in `x` by their ACS
    population in the same stratum, then across areas as usual. `x` must
    hold every county CDC reports in each of its states, for each
    indicator, year and stratum it has (rows CDC lists as "Data not
    available" with no count aside), or the counties left out would have
    their cases spread too; to study part of a state, spread first and
    filter the result. Checking needs CDC's county table, which comes
    from the cache when `x` was downloaded in this session. Negative
    remainders are not spread, nor are remainders in a state with no
    allocated county in that stratum; the message says how many cases
    could not be spread. Not available for CDC's HIV estimates (EHE
    counties only).

  With either choice, a warning (class
  `"cdcatlas_state_unplaced_cases"`) names any state with more than 25%
  of its published state total in no county row of `x`; see
  [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md).
  With `"report"`, county and area estimates for such a state are too
  low by about that share, for the strata in `x`. With `"spread"`, any
  of those cases that reach areas are placed by assumption, not by data
  on where they are.

- spread_by:

  How `no_county = "spread"` shares a state's remainder among its
  counties:

  - `"population"` (default): by each county's ACS population in the
    stratum, as if cases with no county lived anywhere in the state.

  - `"cases"`: in proportion to each county's own count in the stratum,
    as if cases with no county resembled cases with one. A state stratum
    with no county cases falls back to population.

  Ignored for `no_county = "report"`.

- empty_strata:

  What to do when CDC reports cases in a county stratum but no area in
  the county has ACS population in it (for example a small county with
  no residents of one age group):

  - `"total"` (default): share by the areas' total population.

  - `"equal"`: share equally across the county's areas.

  - `"none"`: leave the estimates `NA`, with a warning.

- ages:

  Ages the Census populations cover:

  - `"auto"` (default): 13 and older for HIV indicators (as CDC reports
    them), all ages otherwise, decided indicator by indicator, so `x`
    can combine HIV with other diseases.

  - `"13+"` or `"all"`: set it yourself.

  When `x` is stratified by age, the age groups set the ages and `ages`
  is ignored: HIV groups start at 13, so the total population (the
  `"total"` fallback for empty strata) is ages 13 and older; other
  diseases' groups cover all ages.

- race_table:

  The ACS table for race populations, used when `x` is stratified by
  race:

  - `"B03002"` (default): Hispanic or Latino origin by race. Its groups
    match AtlasPlus exactly (Hispanic, and non-Hispanic for each race),
    but it has no ages, so for ages 13 and older each area's populations
    are scaled by the area's share of residents aged 13 and older.

  - `"B01001"`: the sex-by-age tables for each race (B01001B to
    B01001I), which give ages 13 and older directly. Only White
    (B01001H) and Hispanic (B01001I) match AtlasPlus; the other groups
    include Hispanic people of that race.

- unknown_race:

  Cases CDC reports with race "Unknown", when `x` is stratified by race:

  - `"report"` (default): left out, with a message, and listed in
    `attr(result, "not_allocated")`.

  - `"redistribute"`: shared across the county's known race groups in
    proportion to their counts, within each indicator and year, then
    across areas as usual. A county with no cases of known race keeps
    its unknown cases unallocated.

- unknown_age:

  Cases CDC reports with age "Unknown" (chlamydia, gonorrhea and
  syphilis), when `x` is stratified by age:

  - `"report"` (default): left out, with a message, and listed in
    `attr(result, "not_allocated")`.

  - `"redistribute"`: shared across the county's known age groups in
    proportion to their counts, within each indicator and year, then
    across areas as usual. A county with no cases of known age keeps its
    unknown cases unallocated.

- unestimated:

  Suppressed rows
  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
  couldn't estimate (`cases_source = "not estimated"`, with no count),
  such as a county CDC still lists after it was abolished:

  - `"leave out"` (default): left out, with a message naming the
    counties, and listed in `attr(result, "not_allocated")`. Their cases
    count towards the state's remainder with no county.

  - `"error"`: stop with an error.

- missing_population:

  What to do when the ACS population is missing (an `NA` estimate, or a
  variable the Census didn't return) for some area of a county:

  - `"leave out"` (default): leave that county stratum out, with a
    warning, and list it in `attr(result, "not_allocated")` with reason
    `"missing ACS population"`. Sharing over the other areas would move
    cases.

  - `"error"`: stop instead.

  A population of 0 is not missing; see `empty_strata`.

- acs_year:

  The last year of the ACS 5-year estimates. `NULL` (default) uses each
  data year. A single year can be given only if `x` has one year.

- weights:

  Not yet supported; reserved for sub-county weights.

## Value

A tibble with one row per county, area, indicator, year and stratum (see
below for the one tract that is in two counties):

- year, acs_year:

  Data year and ACS year used.

- area_type, geoid, area_name:

  `to`, and the area's Census GEOID and name.

- county_fips, county_name, state_fips:

  The county and state the area is in.

- indicator, race_ethnicity, sex, age, data_status:

  From `x`.

- county_cases, cases_source, county_population_atlas:

  The county's count, where it came from (see
  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md))
  and CDC's county population.

- county_cases_spread:

  Cases with no county added to this county (`no_county = "spread"`); 0
  otherwise.

- county_cases_unknown:

  Cases of unknown race or age added to this group (`unknown_race` or
  `unknown_age = "redistribute"`); 0 otherwise.

- county_cases_allocated:

  `county_cases + county_cases_spread + county_cases_unknown`: what the
  county's areas add up to.

- area_population_acs, county_population_acs:

  ACS population in the stratum, for the area and the sum of the
  county's areas.

- area_total_population, county_total_population:

  The same for the whole population at the same ages.

- allocation:

  How the county's count was shared: `"stratum population"`,
  `"total population"`, `"equal shares"` or `"none"` (see
  `empty_strata`).

- area_cases:

  Estimated cases in the area.

`area_population_acs - area_cases` means different things by indicator:

- HIV prevalence (people living with *diagnosed* HIV): residents aged 13
  and older without diagnosed HIV, which includes people with
  undiagnosed HIV.

- Estimated HIV prevalence (diagnosed and undiagnosed): closer to
  residents without HIV, under the estimate's own assumptions.

- Other indicators: no such meaning. New diagnoses and incidence need
  the population at risk, which excludes people already living with HIV;
  chlamydia, gonorrhea and syphilis count infections rather than people;
  for Knowledge of Status the population isn't residents.

It can be negative where `empty_strata` shares cases into a stratum with
no ACS population.

`geoid` is unique within `county_fips` (for one indicator, year and
stratum), not overall. With ACS 2010 to 2019, one 2010 tract
(09013890201) is split between two of Connecticut's planning regions, so
it has two rows with the same `geoid`, one for each planning region in
`x`, and so does each of its block groups. Each row holds a share of the
area: both `area_population_acs` and `area_total_population` are
multiplied by the share of the tract's land area in that planning
region, and `area_cases` is that county's count shared over them. Block
groups inherit their parent tract's land-area shares, which are not
measured shares of each block group's population. A warning of class
`"cdcatlas_duplicated_geoid"` says how many areas are affected.

To get one row per area, for example before joining to tract boundaries,
sum `area_cases`, `area_population_acs` and `area_total_population` by
`geoid` within year, indicator and stratum; keeping one of the two rows
would understate the tract. The county columns repeat on every row of a
county and must not be summed. The article ["Extrapolation to
tracts"](https://vagishhemmige.github.io/CDCAtlas/articles/extrapolation.html)
shows the step.

Two attributes record what was not allocated:
`attr(result, "no_county_cases")`, the
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
report, and `attr(result, "not_allocated")`, county rows left out, with
a `reason`.

## Details

The denominator is the sum of the county's areas, so the estimates add
up to `county_cases_allocated` in every county stratum.

Census populations come from ACS tables requested by variable code:
B01001 (sex by age) for all ages, sex and age, and for race the table
set by `race_table`. Each CDC age group counts the share of each B01001
age band that its ages cover, assuming ages are spread evenly within a
band: for ages 13 and older (and HIV's 13-24 group), 2/5 of the 10 to 14
band.
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
requests each disease's own age groups, which cover its ages once.

County rows with no Census denominator are left out and listed in
`attr(result, "not_allocated")`: race "Other", race or age "Unknown"
(unless redistributed), suppressed rows with no estimate (see
`unestimated`), rows kept by
`impute_suppressed(not_available = "keep")`, and counties with no ACS
areas (such as Valdez-Cordova, Alaska, which CDC still lists with no
population).

CDC reports every year on its current county map. ACS releases from
2006-2010 to 2015-2019 use 2010 tracts, so in Connecticut (now planning
regions) and the old Valdez-Cordova Census Area, Alaska (now Chugach and
Copper River), each 2010 tract and its block groups are assigned to the
current county they fall in. The one 2010 tract split between two
planning regions is shared by land area. The crosswalk ships in
`inst/extdata/tract-2010-crosswalk.csv`. It is derived by
`data-raw/tract-crosswalk.R` (in the package's GitHub repository) from
two Census Bureau relationship files: the 2020-to-2010 census tract
files for Connecticut and Alaska, and the 2022 county subdivision to
census tract file for Connecticut, which gives each tract its planning
region. These are US government works, in the public domain in the
United States.

CDC's current county codes the package knows an ACS release lacks:

- Connecticut's planning regions: handled by the crosswalk for ACS 2010
  to 2019; missing from ACS 2009, 2020 and 2021, which code Connecticut
  by its old counties; in ACS 2022 on.

- Chugach and Copper River, Alaska: handled by the crosswalk for ACS
  2010 to 2019; missing from ACS 2009; in ACS 2020 on.

- Kusilvak, Alaska (02158, formerly Wade Hampton 02270) and Oglala
  Lakota, South Dakota (46102, formerly Shannon 46113), renamed in 2015:
  missing from ACS releases before 2015.

This list is not a full check of every ACS vintage against CDC's map.
Counties a release doesn't match are left out with a warning that says
why, for the cases above. In 2010-2019 a block group follows its tract:
the block groups of the one split tract (09013890201) are shared between
Capitol and Northeastern Connecticut planning regions by the tract's
land area, not by where each block group lies.

Tract and block group boundaries change every ten years, so estimates
from ACS years on different sides of a decennial census (2017 and 2022,
say) can't be compared area by area.

Needs the tidycensus package and a Census API key in the
`CENSUS_API_KEY` environment variable (see
[`tidycensus::census_api_key()`](https://walker-data.com/tidycensus/reference/census_api_key.html)).
Also downloads the matching state data from CDC AtlasPlus for the
no-county report.

Congenital syphilis isn't supported: its rates are per 100,000 live
births, so Census populations are the wrong denominator.

## See also

[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md),
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md);
the article ["About CDC AtlasPlus
data"](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html)
on CDC's county map and the Census denominators.

## Examples

``` r
if (FALSE) { # \dontrun{
county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
county <- impute_suppressed(county)
tracts <- extrapolate_from_county(county)
block_groups <- extrapolate_from_county(county, to = "block group")

# Cases left out because they have no county
attr(tracts, "no_county_cases")
} # }
```
