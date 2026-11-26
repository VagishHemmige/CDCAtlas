# Fill in suppressed county counts

CDC suppresses small county counts. `impute_suppressed()` gives each
suppressed county a count so county data can be added up or shared
across census tracts with
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md).
Run it on county data from
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
before
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
or
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md).

Rows are handled by what CDC published:

- A published count is kept, whatever its data status. (Connecticut's
  2017 planning regions are marked "Data not available" but have
  counts.)

- A row marked "Data not available" with no count has no data to
  estimate from; `not_available` says whether it is kept or dropped. Its
  cases are still in the state total, so
  [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
  counts them in the remainder, as long as the state still has a row in
  that indicator, year and stratum.

- A row marked "Data suppressed" with no count is filled in by `method`.

- A row with no count and any other status (or none), such as "No
  reported cases" or a code this version doesn't know, is not filled in:
  it stays `NA` and is marked `"not estimated"`, with a warning.

## Usage

``` r
impute_suppressed(
  x,
  method = c("state_rate", "zero"),
  cap = 4,
  floor = 0,
  population = c("cdc", "acs"),
  unestimated = c("na", "zero"),
  not_available = c("keep", "drop"),
  fit_to = c("none", "county_total")
)
```

## Arguments

- x:

  County data from `get_atlas(geography = "county")`.

- method:

  How suppressed counts are filled in:

  - `"state_rate"` (default): the state's rate in the same indicator,
    year and stratum, times the county's population (see `population`),
    kept between `floor` and `cap`.

  - `"zero"`: 0. Useful as a sensitivity analysis.

- cap:

  The largest count `"state_rate"` can give a suppressed county. Use
  `Inf` for no cap. The default, 4, reflects CDC suppressing counts of 1
  to 4, but complementary suppression can hide larger counts, so it is a
  default rather than a hard limit. `cases_uncapped` in the output lets
  you count how often the cap sets the value. `floor = cap = 2.5` gives
  every row that can be estimated the same fixed value, 2.5.

- floor:

  The smallest count `"state_rate"` can give. The default, 0, allows for
  counties suppressed because their population is under 100, which may
  have no cases. Use 1 to treat every suppressed county as having at
  least one case. Must not be larger than `cap`.

- population:

  The county population `"state_rate"` multiplies by:

  - `"cdc"` (default): CDC's own county population, the same source as
    the state rate's denominator.

  - `"acs"`: the county's ACS 5-year population in the same stratum and
    ages, summed from its tracts as
    [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
    does. Needs tidycensus and a Census API key.

  `"acs"` accepts the same input as
  [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md):
  at most one stratification variable, and no congenital syphilis (whose
  rates are per 100,000 live births). It also refuses Knowledge of
  Status, whose denominator is people living with HIV, not residents.
  Other input stops before anything is downloaded; use `"cdc"` for it.

  CDC gives Connecticut's 2017 planning regions no population, so
  `"cdc"` can't estimate them; `"acs"` can, from 2010 tracts assigned to
  the planning regions (see
  [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)).

- unestimated:

  What to do with a suppressed row `"state_rate"` can't estimate,
  because the state rate or county population is missing:

  - `"na"` (default): leave it `NA`, with a warning.
    [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
    counts its cases in the remainder with no county, and
    [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
    leaves it out (or stops, with `unestimated = "error"`).

  - `"zero"`: set it to 0, with a warning.

- not_available:

  Rows marked "Data not available" with no count:

  - `"keep"` (default): keep them with `NA` cases. The later steps leave
    them out of the tracts, and their cases stay in the state's
    remainder.

  - `"drop"`: remove them, with a message. If that removes every row a
    state has in an indicator, year and stratum, that state's remainder
    is no longer reported by
    [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
    or spread by
    [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md),
    so this warns and names them.

- fit_to:

  Whether suppressed strata are fitted to each county's published total:

  - `"none"` (default): no fitting; each suppressed row keeps its
    `method` estimate.

  - `"county_total"`: for HIV diagnoses and HIV prevalence stratified by
    sex or age, the suppressed cells of each county, indicator and year
    share the county's published total, less its published cells, in
    proportion to their state-rate estimates before the cap. Needs
    `method = "state_rate"`. Race strata are never fitted, since HIV has
    no row for unknown race, so a county's race cells needn't add up to
    its total.

  A county is fitted only when two or more of its cells are suppressed,
  at least two have a positive estimate, its total is published, every
  requested stratum is present and the total is larger than the
  published cells; otherwise its rows keep the `method` estimate and
  `fit_reason` says why. Fitted counts add up to the county's total but
  are modeled: each one can be far from CDC's hidden count, and CDC's
  rule for which counts it suppresses is not used to place them. They
  are fractions, and the cap and floor don't apply to them. The fit
  matches each county's own total, never the state's; a warning reports
  how many state strata then add up to more than the state total.

## Value

`x` with suppressed counts filled in and three new columns,
`cases_source`, `cases_uncapped` and `fit_reason`. `cases_source` is:

- `"published"`: CDC's count.

- `"state_rate"` or `"zero"`: filled in by `method`.

- `"county_fit"`: fitted to the county's published total
  (`fit_to = "county_total"`).

- `"not estimated"`: `"state_rate"` couldn't estimate it (`NA`, or 0
  with `unestimated = "zero"`), or it had no count and wasn't marked
  "Data suppressed" (always `NA`).

- `"not available"`: kept with `not_available = "keep"`; `NA`.

`cases_uncapped` is, for rows filled in by `"state_rate"`, the estimate
before `cap` and `floor` were applied: the state rate times the county
population used (CDC's or ACS). On `"county_fit"` rows it is the same
estimate, the share each fitted cell was given. It is a modeled number,
not CDC's hidden count and not a way to recover it. It is `NA` for every
other row: published counts, rows filled in by `"zero"`, and rows marked
`"not estimated"` or `"not available"`. On `"state_rate"` rows `cases`
is the value to use; `cases_uncapped` shows what decided it:

- `cases_uncapped > cap`: the cap set `cases`.

- `cases_uncapped < floor`: the floor set `cases`.

- Otherwise `cases` equals `cases_uncapped`. An estimate exactly equal
  to `cap` or `floor` was not changed by it.

With `"state_rate"`, a message says how many suppressed rows were filled
in and, of those, how many had their value set by the cap
(`cases_uncapped > cap`) and by the floor (`cases_uncapped < floor`), as
counts and percentages. Rows that could not be estimated are not
counted. A limit that can't set anything (`cap = Inf`, `floor = 0`) is
not mentioned, and there is no message when no row was estimated or with
`method = "zero"`. A high percentage for the cap means most estimates
are the cap itself rather than the state rate.

`fit_reason` is `NA` on rows with a published count, and everywhere with
`fit_to = "none"`. With `fit_to = "county_total"`, every other row has
`"fitted to county total"` or the first reason that applies to its
county, indicator and year: `"race not fitted"`,
`"county total not available"`, `"county total suppressed"`,
`"strata incomplete or inconsistent"`, `"one suppressed cell"`,
`"a suppressed cell has no seed"`, `"fewer than two positive seeds"` or
`"published total not usable for fitting"` (the total is no larger than
the published cells). A message counts the county groups fitted and not
fitted, by reason, and the cap and floor message counts only rows filled
in by the state rate.

`data_status` is unchanged, so suppressed rows can still be identified.
`cap`, `floor`, `population` and `unestimated` apply only to
`"state_rate"`.
[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
and
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
use `cases` and ignore `cases_uncapped`.

## Details

`"state_rate"` downloads the state data matching `x` (same diseases,
years and stratification) from CDC AtlasPlus, and with
`population = "acs"` the tract populations from the Census.
`fit_to = "county_total"` by sex or age also downloads CDC's
unstratified county data for the states and years in `x` (one request
per state and year, saved and reused), and CDC's county data matching
`x` for the state check, which is usually already saved from
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md).

## See also

[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
for cases in state totals that no county accounts for;
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
for the next step; the article ["About CDC AtlasPlus
data"](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html)
for CDC's suppression rules.

## Examples

``` r
if (FALSE) { # \dontrun{
county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
county <- impute_suppressed(county)

# How many filled-in rows did the cap (4 by default) decide?
filled <- county[county$cases_source == "state_rate", ]
table(filled$indicator, capped = filled$cases_uncapped > 4)

# Fit suppressed sex strata to each county's published total
county_fit <- impute_suppressed(
  get_atlas("hiv", "county", 2022, stratify_by = "sex"),
  fit_to = "county_total"
)
table(county_fit$fit_reason, useNA = "ifany")

# Sensitivity analysis: suppressed counties have no cases
county_zero <- impute_suppressed(
  get_atlas("hiv", "county", 2022, stratify_by = "sex"),
  method = "zero"
)
} # }
```
