# Cases in state totals that no county accounts for

A state's total in CDC AtlasPlus is usually larger than the sum of its
counties. `no_county_cases()` compares each state total with the county
counts supplied. Run it after
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
for the remainder that no county accounts for; on county data straight
from
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
it reports only the gap left by published counts.

For CDC's HIV estimates (`get_atlas("estimate")`), county data cover
only the Ending the HIV Epidemic (EHE) counties, so the remainder can
include people in counties outside EHE coverage and isn't necessarily
cases with no county; a message says so.

The remainder is a difference, not a count of identified cases. One
likely explanation is cases whose county of residence is unknown or not
released, but CDC's documentation doesn't say so; it also includes any
counties left out of `x`. In 2022 the gap was far larger than suppressed
counties could hold at 1-4 cases each, so it can't be used to fill them
in.
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
leaves the remainder out by default; `no_county = "spread"` shares it
across the state's counties instead. See the "About CDC AtlasPlus data"
article.

## Usage

``` r
no_county_cases(x)
```

## Arguments

- x:

  County data from `get_atlas(geography = "county")`, usually after
  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md).

## Value

A tibble with one row per state, indicator, year and stratum:

- state_fips, state:

  State FIPS code and name.

- indicator, year, race_ethnicity, sex, age:

  As in `x`.

- state_cases:

  The state total published by CDC.

- published_cases:

  Sum of the published county counts in `x`.

- unpublished_cases:

  `state_cases - published_cases`: cases in suppressed counties together
  with the remainder no county accounts for.

- imputed_cases:

  Sum of the counts
  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
  filled in; `NA` if `x` hasn't been imputed.

- no_county_cases:

  `unpublished_cases - imputed_cases`. Can be negative if the imputed
  counts are too high, and is `NA` if `x` hasn't been imputed.

Every column computed from `state_cases` is `NA` if CDC suppressed the
state total.

## Details

Downloads the state data matching `x` (same diseases, years and
stratification) from CDC AtlasPlus. The remainder also includes any
county without a count in `x`: "Data not available" rows, counties
removed by filtering `x`, and suppressed rows
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
couldn't estimate (marked `cases_source = "not estimated"`), which count
as 0 imputed cases. The report has a row only for state groups (state,
indicator, year and stratum) with at least one row in `x`: if
`impute_suppressed(not_available = "drop")` or filtering removed every
row of a group, its state total is not reported.

## See also

[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md),
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md);
the article ["About CDC AtlasPlus
data"](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html)
on why state totals and county counts differ.

## Examples

``` r
if (FALSE) { # \dontrun{
county <- get_atlas("hiv", "county", 2022)
county <- impute_suppressed(county)
no_county_cases(county)
} # }
```
