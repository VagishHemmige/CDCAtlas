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
  estimate from; `not_available` says whether it is dropped or kept.
  Either way its cases are still in the state total, so
  [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
  counts them in the remainder.

- Any other row with no count is suppressed and is filled in by
  `method`.

## Usage

``` r
impute_suppressed(
  x,
  method = c("state_rate", "zero"),
  cap = 4,
  floor = 0,
  population = c("cdc", "acs"),
  unestimated = c("na", "zero"),
  not_available = c("drop", "keep")
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
  default rather than a hard limit.

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

  Neither covers Connecticut's 2017 planning regions: CDC gives them no
  population, and 2017 ACS tracts belong to the old counties (#30).

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

  - `"drop"` (default): remove them, with a message.

  - `"keep"`: keep them with `NA` cases. The later steps leave them out.

## Value

`x` with suppressed counts filled in and a new column `cases_source`:

- `"published"`: CDC's count.

- `"state_rate"` or `"zero"`: filled in by `method`.

- `"not estimated"`: `"state_rate"` couldn't estimate it; `NA`, or 0
  with `unestimated = "zero"`.

- `"not available"`: kept with `not_available = "keep"`; `NA`.

`data_status` is unchanged, so suppressed rows can still be identified.
`cap`, `floor`, `population` and `unestimated` apply only to
`"state_rate"`.

## Details

`"state_rate"` downloads the state data matching `x` (same diseases,
years and stratification) from CDC AtlasPlus, and with
`population = "acs"` the tract populations from the Census.

## See also

[`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
for cases in state totals that no county accounts for;
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
for the next step.

## Examples

``` r
if (FALSE) { # \dontrun{
county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
county <- impute_suppressed(county)

# Sensitivity analysis: suppressed counties have no cases
county_zero <- impute_suppressed(
  get_atlas("hiv", "county", 2022, stratify_by = "sex"),
  method = "zero"
)
} # }
```
