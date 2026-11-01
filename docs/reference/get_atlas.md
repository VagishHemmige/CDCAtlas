# Retrieve STI surveillance data from CDC AtlasPlus

`get_atlas()` provides a programmatic interface to the CDC AtlasPlus
platform, returning case counts, population denominators, and rate
estimates for sexually transmitted infection (STI) surveillance. The
function submits a payload to the AtlasPlus data endpoint and parses the
response into a tidy tibble.

## Usage

``` r
get_atlas(
  disease,
  geography,
  year,
  stratify_by = NULL,
  extrapolate_to_tract = deprecated()
)
```

## Arguments

- disease:

  A single disease name (e.g. `"gonorrhea"`). Multiple diseases are not
  currently supported in a single request.

- geography:

  Character scalar. One of `"national"`, `"state"`, `"county"`, or
  `"msa"`. Region-level queries are not supported yet.

- year:

  Integer vector of years. Valid values are from 2000 to 2023. Can be a
  single year (e.g., `2022`) or a sequence (e.g., `2018:2022`).

- stratify_by:

  Optional character vector. Zero, one, or two of `"age"`, `"race"`,
  `"sex"`. If `NULL`, totals (unstratified) are returned.

- extrapolate_to_tract:

  **\[deprecated\]** No longer supported. Use
  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
  and then
  [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md).

## Value

A tibble with the following columns:

- indicator:

  Disease/condition name (factor)

- year:

  Calendar year (numeric)

- geography:

  Geographic unit name (factor)

- data_status:

  Data quality flags (factor)

- race_ethnicity:

  Race/ethnicity category (factor, if stratified)

- sex:

  Sex category (factor, if stratified)

- age:

  Age group (factor, if stratified)

- transmission:

  Transmission category (factor)

- rate100000:

  Incidence rate per 100,000 (numeric)

- cases:

  Case count (numeric)

- population:

  Population denominator (numeric)

- lowerci_rate, upperci_rate:

  95% CI bounds for rate (numeric)

- rse:

  Relative standard error (numeric)

- lowerci_cases, upperci_cases:

  95% CI bounds for cases (numeric)

- fips:

  FIPS code (character)

Columns and naming conventions may differ depending on CDC schema
changes. The function attempts to standardize but does not guarantee
historical compatibility for all years.

## Details

The function supports multiple diseases, geographic levels, year ranges,
and optional stratification. Stratified queries expand results into all
requested combinations (e.g., age by race). When possible, numeric
fields are returned as `double` and identifiers as character.

`get_atlas()` only downloads. For census tract or block group estimates,
pass county data to
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
and then
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md):

    county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
    county <- impute_suppressed(county)
    no_county_cases(county)
    tracts <- extrapolate_from_county(county)

**Supported diseases**

- `"chlamydia"`

- `"gonorrhea"`

- `"adult syphilis"`

- `"congenital syphilis"`

- `"tuberculosis"`

- `"hiv"` (will provide confirmed diagnoses)

- `"estimate"` (estimated HIV counts)

- `"hepatitis"`

**Geography**

- `"national"`

- `"state"`

- `"county"`

- `"msa"` (metropolitan statistical area)

**Stratification** The CDC AtlasPlus API supports stratification by one
or two dimensions:

- `"age"`

- `"race"`

- `"sex"`

Stratification increases the number of returned rows. For example,
`stratify_by = c("race", "sex")` returns all race × sex combinations for
each geography-year pair.

This function is a reverse-engineered wrapper around the AtlasPlus
public query system. The CDC does not publish an official API
specification, and internal identifiers may change without notice. If
the CDC modifies the backend schema, requests may fail. Please report
issues via GitHub.

## See also

[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
and
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
for tract and block group estimates from county data.

## Examples

``` r
if (FALSE) { # \dontrun{
# National gonorrhea rates for 2018–2022
get_atlas(disease = "gonorrhea", geography = "national", year = 2018:2022)

# State-level chlamydia, stratified by race
get_atlas(
  disease = "chlamydia",
  geography = "state",
  year = 2022,
  stratify_by = "race"
)

# County gonorrhea, shared across census tracts
county <- get_atlas(disease = "gonorrhea", geography = "county", year = 2022)
tracts <- extrapolate_from_county(impute_suppressed(county))
} # }
```
