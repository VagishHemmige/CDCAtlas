# Cases in state totals that no county accounts for

A state's total in CDC AtlasPlus includes cases with no valid county of
residence, so it is usually larger than the sum of its counties.
`no_county_cases()` compares each state total with its counties. Run it
after
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
for the remainder with no county; on county data straight from
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
it reports only the gap left by published counts.

Most of the gap is cases with no county, not suppressed counties (#27),
so it can't be used to fill in suppressed counties.
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
leaves these cases out by default; `no_county = "spread"` shares them
across the state's counties instead.

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

  `state_cases - published_cases`: cases in suppressed counties and
  cases with no county together (the figures in \#27).

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
county without a count in `x`: "Data not available" rows (whether
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
dropped or kept them) and counties removed by filtering `x`, and
suppressed rows
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
couldn't estimate (marked `cases_source = "not estimated"`), which count
as 0 imputed cases.

## See also

[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md),
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md).

## Examples

``` r
if (FALSE) { # \dontrun{
county <- get_atlas("hiv", "county", 2022)
county <- impute_suppressed(county)
no_county_cases(county)
} # }
```
