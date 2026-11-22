# Extrapolation to tracts

``` r

library(CDCAtlas)
library(dplyr)
```

## Overview

CDC AtlasPlus reports many outcomes at the county level. For some
analyses, especially geographic access analyses, we may want estimates
at a smaller geographic unit: the Census tract or block group.

`CDCAtlas` estimates these in three steps:

1.  [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
    fills in county counts CDC suppressed.
2.  [`no_county_cases()`](https://vagishhemmige.github.io/CDCAtlas/reference/no_county_cases.md)
    reports cases in each state’s total that no county accounts for.
3.  [`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
    shares each county’s count across its tracts (or, with
    `to = "block group"`, its block groups) using Census populations.

This article uses tracts throughout; block groups work the same way.

The basic idea is:

1.  Retrieve county-level CDC AtlasPlus counts.
2.  Retrieve tract-level Census population denominators.
3.  Match each tract to its parent county.
4.  Allocate each county’s reported cases across tracts in proportion to
    the tract’s share of the relevant county population.

For example, if a tract contains 2% of a county’s population in the
relevant denominator, it receives 2% of the county’s reported cases.

This is a **population-weighted extrapolation**, not a direct CDC
tract-level surveillance estimate.

## Census data requirements

Tract-level extrapolation requires tract-level population denominators
from the U.S. Census Bureau. `CDCAtlas` uses the `tidycensus` package to
retrieve these denominators.

If you only use `CDCAtlas` to retrieve AtlasPlus county, state, or
national data, you do not need to set up `tidycensus`. However, if you
use
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md),
you will need:

1.  the `tidycensus` package installed
2.  a Census API key
3.  the API key saved in your R environment

You can install `tidycensus` (version 1.8.0 or later) from CRAN:

``` r

install.packages("tidycensus")
```

Then request a free Census API key from the U.S. Census Bureau and save
it with
[`tidycensus::census_api_key()`](https://walker-data.com/tidycensus/reference/census_api_key.html).
With `install = TRUE` it is written to your `.Renviron` file as
`CENSUS_API_KEY`, so it is there in future sessions:

``` r

tidycensus::census_api_key("YOUR KEY HERE", install = TRUE)
readRenviron("~/.Renviron") # or restart R
```

Without `install = TRUE`, the key lasts only for the current session.

See <https://walker-data.com/tidycensus/> for further details.

## Important assumptions

This method assumes that, within each county and stratum, cases are
distributed across tracts in proportion to the relevant Census
denominator.

For example:

- unstratified HIV prevalence may be allocated using total tract
  population
- sex-stratified counts may be allocated using tract population by sex
- age-stratified counts may be allocated using tract population by age
- race/ethnicity-stratified counts may be allocated using tract
  population by race/ethnicity

This can be useful for geographic access models, but it should not be
interpreted as observed tract-level surveillance data.

## Basic unstratified extrapolation

The simplest use case is extrapolating county-level data to tracts using
total tract population.

``` r

hiv_tract <- get_atlas(
  disease = "hiv",
  year = 2022,
  geography = "county"
) |>
  impute_suppressed() |>
  extrapolate_from_county()
#> Warning: 1 suppressed row could not be estimated and is set to `NA`.
#> ℹ No state rate or no county population for "Valdez-Cordova Census Area, AK".
#> ℹ They are marked `cases_source = "not estimated"`.
#> ℹ 1616 suppressed rows were filled in from state rates.
#> • The cap of 4 set the value of 744 rows (46%): the estimate was higher.
#>   cases_uncapped holds the estimates before the cap and floor.
#> Getting data from the 2018-2022 5-year ACS
#> 
#> Fetching tract data by state and combining the result.
#> 
#> ℹ Cases in state totals with no county are not allocated to tracts:
#> • HIV diagnoses 2022: 63
#> • HIV prevalence 2022: 20,122
#> ! 42 state strata with a negative remainder (counted as 0)
#>   See `attr(x, "no_county_cases")` for each state.
#> Warning: 7 states have more than 25% of their cases in no county row.
#> • HIV diagnoses 2022: Vermont (86%), Montana (44%), Alaska (35%), and Wyoming
#>   (34%)
#> • HIV prevalence 2022: Alaska (87%), Rhode Island (73%), Nebraska (60%), and
#>   Idaho (47%)
#> ! County and tract estimates for these states leave those cases out, so they
#>   are too low by about that share overall.
#> ℹ This happens when a state's county counts are largely suppressed, or when `x`
#>   lacks some of its counties. CDCAtlas doesn't check the cause.
#> ℹ See `attr(x, "no_county_cases")` for each state.
#> ℹ 1 row with no count that `impute_suppressed()` couldn't estimate was left out
#>   of the tract estimates and counted in the remainder with no county.
#>   County: "Valdez-Cordova Census Area, AK".
```

[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
and
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
report what they could not estimate or allocate. Those messages are
shown here on purpose: they say which cases are missing from the tract
estimates.

The returned data contain one row per tract and indicator (here, HIV
diagnoses and HIV prevalence), with the county-level AtlasPlus count
allocated to tracts.

``` r

head(hiv_tract)
#> # A tibble: 6 × 25
#>    year acs_year area_type geoid    area_name county_fips county_name state_fips
#>   <dbl>    <dbl> <chr>     <chr>    <chr>     <chr>       <chr>       <chr>     
#> 1  2022     2022 tract     0100102… Census T… 01001       Autauga Co… 01        
#> 2  2022     2022 tract     0100102… Census T… 01001       Autauga Co… 01        
#> 3  2022     2022 tract     0100102… Census T… 01001       Autauga Co… 01        
#> 4  2022     2022 tract     0100102… Census T… 01001       Autauga Co… 01        
#> 5  2022     2022 tract     0100102… Census T… 01001       Autauga Co… 01        
#> 6  2022     2022 tract     0100102… Census T… 01001       Autauga Co… 01        
#> # ℹ 17 more variables: indicator <chr>, race_ethnicity <chr>, sex <chr>,
#> #   age <chr>, data_status <chr>, county_cases <dbl>, cases_source <chr>,
#> #   county_population_atlas <dbl>, county_cases_spread <dbl>,
#> #   county_cases_unknown <dbl>, county_cases_allocated <dbl>,
#> #   area_population_acs <dbl>, county_population_acs <dbl>,
#> #   area_total_population <dbl>, county_total_population <dbl>,
#> #   allocation <chr>, area_cases <dbl>
```

A typical output includes columns such as:

``` r

names(hiv_tract)
#>  [1] "year"                    "acs_year"               
#>  [3] "area_type"               "geoid"                  
#>  [5] "area_name"               "county_fips"            
#>  [7] "county_name"             "state_fips"             
#>  [9] "indicator"               "race_ethnicity"         
#> [11] "sex"                     "age"                    
#> [13] "data_status"             "county_cases"           
#> [15] "cases_source"            "county_population_atlas"
#> [17] "county_cases_spread"     "county_cases_unknown"   
#> [19] "county_cases_allocated"  "area_population_acs"    
#> [21] "county_population_acs"   "area_total_population"  
#> [23] "county_total_population" "allocation"             
#> [25] "area_cases"
```

The main columns are:

- `geoid` and `area_name`: the tract
- `county_fips` and `county_name`: the county it is in
- `indicator` and `year`: what was counted, and when; `acs_year` is the
  ACS year of the population figures
- `county_cases`: the county’s count, and `county_cases_allocated`: the
  cases shared across the county’s tracts
- `area_population_acs` and `county_population_acs`: the ACS population
  of the tract and of the county’s tracts
- `area_cases`: the estimated cases in the tract

See
[`?extrapolate_from_county`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
for every column.

The key calculation is:

``` r

area_cases = county_cases_allocated * area_population_acs / county_population_acs
```

With the default options, `county_cases_allocated` is the county’s own
count. It also includes any cases with no county that
`no_county = "spread"` adds, and any cases of unknown race that
`unknown_race = "redistribute"` adds.

## Checking county totals

After extrapolation, tract-level estimates should sum back to the county
counts they share out (`county_cases_allocated`), allowing for small
floating-point differences.

``` r

hiv_tract |>
  group_by(county_fips, indicator) |>
  summarize(
    county_cases = first(county_cases_allocated),
    tract_cases_sum = sum(area_cases, na.rm = TRUE),
    difference = tract_cases_sum - county_cases,
    .groups = "drop"
  ) |>
  arrange(desc(abs(difference))) |>
  head()
#> # A tibble: 6 × 5
#>   county_fips indicator      county_cases tract_cases_sum difference
#>   <chr>       <chr>                 <dbl>           <dbl>      <dbl>
#> 1 06059       HIV prevalence         7317            7317  -9.09e-13
#> 2 01073       HIV prevalence         3771            3771   4.55e-13
#> 3 13067       HIV prevalence         3794            3794   4.55e-13
#> 4 47037       HIV prevalence         3934            3934  -4.55e-13
#> 5 72127       HIV prevalence         3829            3829   4.55e-13
#> 6 05119       HIV prevalence         1916            1916  -2.27e-13
```

In a clean allocation, the `difference` column should be very close to
zero.

## County rows that were not allocated

Some county rows can’t be shared across tracts. They are listed, with a
reason, in an attribute of the result:

``` r

attr(hiv_tract, "not_allocated") |>
  count(reason, indicator)
#> # A tibble: 3 × 3
#>   reason                      indicator          n
#>   <chr>                       <chr>          <int>
#> 1 no CDC data for this county HIV diagnoses      9
#> 2 no CDC data for this county HIV prevalence     8
#> 3 suppressed, not estimated   HIV prevalence     1
```

Reasons include counties CDC has no data for (“Data not available” with
no count), counties with no Census tracts in the ACS year, strata with
no Census population (race “Other”, for example), and suppressed
counties that
[`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md)
could not estimate. In CDC’s 2022 data, for example, Valdez-Cordova
Census Area, Alaska, is still listed although it was split into two
census areas in 2019; it is suppressed, has no CDC population and has no
2022 tracts, so it is left out. Use
`extrapolate_from_county(unestimated = "error")` to stop instead.

Cases in state totals with no county are reported separately:

``` r

attr(hiv_tract, "no_county_cases") |>
  head()
#> # A tibble: 6 × 12
#>   state_fips state   indicator       year race_ethnicity sex   age   state_cases
#>   <chr>      <chr>   <chr>          <dbl> <chr>          <chr> <chr>       <dbl>
#> 1 01         Alabama HIV diagnoses   2022 All races/eth… Both… Ages…         678
#> 2 01         Alabama HIV prevalence  2022 All races/eth… Both… Ages…       14556
#> 3 02         Alaska  HIV diagnoses   2022 All races/eth… Both… Ages…          37
#> 4 02         Alaska  HIV prevalence  2022 All races/eth… Both… Ages…         767
#> 5 04         Arizona HIV diagnoses   2022 All races/eth… Both… Ages…         849
#> 6 04         Arizona HIV prevalence  2022 All races/eth… Both… Ages…       18586
#> # ℹ 4 more variables: published_cases <dbl>, unpublished_cases <dbl>,
#> #   imputed_cases <dbl>, no_county_cases <dbl>
```

## Sex-stratified extrapolation

For sex-stratified AtlasPlus data, the allocation denominator should
also be sex-specific.

``` r

hiv_tract_sex <- get_atlas(
  disease = "hiv",
  year = 2022,
  geography = "county",
  stratify_by = "sex"
) |>
  impute_suppressed() |>
  extrapolate_from_county()
#> Warning: 1 suppressed row could not be estimated and is set to `NA`.
#> ℹ No state rate or no county population for "Valdez-Cordova Census Area, AK".
#> ℹ They are marked `cases_source = "not estimated"`.
#> ℹ 6522 suppressed rows were filled in from state rates.
#> • The cap of 4 set the value of 2762 rows (42%): the estimate was higher.
#>   cases_uncapped holds the estimates before the cap and floor.
#> Getting data from the 2018-2022 5-year ACS
#> 
#> Fetching tract data by state and combining the result.
#> 
#> ℹ Cases in state totals with no county are not allocated to tracts:
#> • HIV diagnoses 2022: 1,554
#> • HIV prevalence 2022: 42,234
#> ! 57 state strata with a negative remainder (counted as 0)
#>   See `attr(x, "no_county_cases")` for each state.
#> Warning: 13 states have more than 25% of their cases in no county row.
#> • HIV diagnoses 2022: Kansas (46%), Vermont (45%), Montana (44%), Wyoming
#>   (34%), and Alaska (27%)
#> • HIV prevalence 2022: New Hampshire (94%), Maine (93%), Idaho (81%), Alaska
#>   (81%), Rhode Island (74%), and 5 more
#> ! County and tract estimates for these states leave those cases out, so they
#>   are too low by about that share overall.
#> ℹ This happens when a state's county counts are largely suppressed, or when `x`
#>   lacks some of its counties. CDCAtlas doesn't check the cause.
#> ℹ See `attr(x, "no_county_cases")` for each state.
#> ℹ 1 row with no count that `impute_suppressed()` couldn't estimate was left out
#>   of the tract estimates and counted in the remainder with no county.
#>   County: "Valdez-Cordova Census Area, AK".
```

This means that male county cases are allocated across tracts using the
male population in each tract, and female county cases are allocated
using the female population in each tract.

Conceptually:

``` math
\widehat{\text{tract cases}}_{\text{male}} =
\text{county cases}_{\text{male}} \times
\frac{\text{tract population}_{\text{male}}}
{\text{county population}_{\text{male}}}
```

``` math
\widehat{\text{tract cases}}_{\text{female}} =
\text{county cases}_{\text{female}} \times
\frac{\text{tract population}_{\text{female}}}
{\text{county population}_{\text{female}}}
```
The resulting data should include one row per tract, indicator and sex.

``` r

hiv_tract_sex |>
  count(sex, indicator)
#> # A tibble: 4 × 3
#>   sex    indicator          n
#>   <chr>  <chr>          <int>
#> 1 Female HIV diagnoses  85396
#> 2 Female HIV prevalence 85396
#> 3 Male   HIV diagnoses  85396
#> 4 Male   HIV prevalence 85396
```

You can verify the allocation within each county and sex stratum.

``` r

hiv_tract_sex |>
  group_by(county_fips, sex, indicator) |>
  summarize(
    county_cases = first(county_cases_allocated),
    tract_cases_sum = sum(area_cases, na.rm = TRUE),
    difference = tract_cases_sum - county_cases,
    .groups = "drop"
  ) |>
  arrange(desc(abs(difference))) |>
  head()
#> # A tibble: 6 × 6
#>   county_fips sex    indicator      county_cases tract_cases_sum difference
#>   <chr>       <chr>  <chr>                 <dbl>           <dbl>      <dbl>
#> 1 12011       Male   HIV prevalence        15656           15656   1.82e-12
#> 2 48113       Male   HIV prevalence        16360           16360  -1.82e-12
#> 3 06037       Female HIV prevalence         5793            5793   9.09e-13
#> 4 12057       Male   HIV prevalence         5792            5792  -9.09e-13
#> 5 12095       Male   HIV prevalence         7405            7405   9.09e-13
#> 6 13067       Male   HIV prevalence         3046            3046   4.55e-13
```

## Age-stratified extrapolation

Age-stratified extrapolation works the same way, except that the Census
denominator is based on tract population within the corresponding
AtlasPlus age group.

``` r

hiv_tract_age <- get_atlas(
  disease = "hiv",
  year = 2022,
  geography = "county",
  stratify_by = "age"
) |>
  impute_suppressed() |>
  extrapolate_from_county()
#> Warning: 1 suppressed row could not be estimated and is set to `NA`.
#> ℹ No state rate or no county population for "Valdez-Cordova Census Area, AK".
#> ℹ They are marked `cases_source = "not estimated"`.
#> ℹ 20452 suppressed rows were filled in from state rates.
#> • The cap of 4 set the value of 5214 rows (25%): the estimate was higher.
#>   cases_uncapped holds the estimates before the cap and floor.
#> Getting data from the 2018-2022 5-year ACS
#> 
#> Fetching tract data by state and combining the result.
#> 
#> ℹ Cases in state totals with no county are not allocated to tracts:
#> • HIV diagnoses 2022: 347
#> • HIV prevalence 2022: 35,823
#> ! 260 state strata with a negative remainder (counted as 0)
#>   See `attr(x, "no_county_cases")` for each state.
#> Warning: 12 states have more than 25% of their cases in no county row.
#> • HIV diagnoses 2022: Vermont (44%), Montana (41%), and Wyoming (31%)
#> • HIV prevalence 2022: New Hampshire (84%), Maine (81%), Rhode Island (72%),
#>   Alaska (65%), Idaho (61%), and 5 more
#> ! County and tract estimates for these states leave those cases out, so they
#>   are too low by about that share overall.
#> ℹ This happens when a state's county counts are largely suppressed, or when `x`
#>   lacks some of its counties. CDCAtlas doesn't check the cause.
#> ℹ See `attr(x, "no_county_cases")` for each state.
#> ℹ 1 row with no count that `impute_suppressed()` couldn't estimate was left out
#>   of the tract estimates and counted in the remainder with no county.
#>   County: "Valdez-Cordova Census Area, AK".
```

The output contains one row per tract, indicator and age group.

``` r

hiv_tract_age |>
  count(age, indicator)
#> # A tibble: 12 × 3
#>    age   indicator          n
#>    <chr> <chr>          <int>
#>  1 13-24 HIV diagnoses  85396
#>  2 13-24 HIV prevalence 85396
#>  3 25-34 HIV diagnoses  85396
#>  4 25-34 HIV prevalence 85396
#>  5 35-44 HIV diagnoses  85396
#>  6 35-44 HIV prevalence 85396
#>  7 45-54 HIV diagnoses  85396
#>  8 45-54 HIV prevalence 85396
#>  9 55-64 HIV diagnoses  85396
#> 10 55-64 HIV prevalence 85396
#> 11 65+   HIV diagnoses  85396
#> 12 65+   HIV prevalence 85396
```

Again, the diagnostic check is whether tract estimates sum back to the
original county count within each county-age stratum.

``` r

hiv_tract_age |>
  group_by(county_fips, age, indicator) |>
  summarize(
    county_cases = first(county_cases_allocated),
    tract_cases_sum = sum(area_cases, na.rm = TRUE),
    difference = tract_cases_sum - county_cases,
    .groups = "drop"
  ) |>
  arrange(desc(abs(difference))) |>
  head()
#> # A tibble: 6 × 6
#>   county_fips age   indicator      county_cases tract_cases_sum difference
#>   <chr>       <chr> <chr>                 <dbl>           <dbl>      <dbl>
#> 1 12011       13-24 HIV prevalence          329             329  -5.68e-14
#> 2 06073       13-24 HIV prevalence          211             211   2.84e-14
#> 3 32003       13-24 HIV prevalence          227             227  -2.84e-14
#> 4 37119       13-24 HIV prevalence          256             256  -2.84e-14
#> 5 06029       13-24 HIV prevalence          125             125   1.42e-14
#> 6 12011       13-24 HIV diagnoses            87              87  -1.42e-14
```

## Race/ethnicity-stratified extrapolation

CDC AtlasPlus commonly uses a combined race/ethnicity variable. For
example, Hispanic/Latino is represented as a single ethnicity category,
while the remaining groups are usually non-Hispanic race categories.

``` r

hiv_tract_race <- get_atlas(
  disease = "hiv",
  year = 2022,
  geography = "county",
  stratify_by = "race"
) |>
  impute_suppressed() |>
  extrapolate_from_county()
#> Warning: 314 suppressed rows could not be estimated and are set to `NA`.
#> ℹ No state rate or no county population for "Valdez-Cordova Census Area, AK",
#>   "Aguada Municipio, PR", "Aguadilla Municipio, PR", "Aguas Buenas Municipio,
#>   PR", "Arroyo Municipio, PR", "Barceloneta Municipio, PR", "Barranquitas
#>   Municipio, PR", "Bayamón Municipio, PR", "Caguas Municipio, PR", "Camuy
#>   Municipio, PR", "Canóvanas Municipio, PR", "Cataño Municipio, PR", "Cayey
#>   Municipio, PR", "Ceiba Municipio, PR", "Ciales Municipio, PR", "Cidra
#>   Municipio, PR", "Coamo Municipio, PR", "Corozal Municipio, PR", …, "Cabo Rojo
#>   Municipio, PR", and "Arecibo Municipio, PR".
#> ℹ They are marked `cases_source = "not estimated"`.
#> ℹ 28196 suppressed rows were filled in from state rates.
#> • The cap of 4 set the value of 2339 rows (8%): the estimate was higher.
#>   cases_uncapped holds the estimates before the cap and floor.
#> Getting data from the 2018-2022 5-year ACS
#> 
#> Fetching tract data by state and combining the result.
#> 
#> ℹ Cases in state totals with no county are not allocated to tracts:
#> • HIV diagnoses 2022: 495
#> • HIV prevalence 2022: 34,627
#> ! 212 state strata with a negative remainder (counted as 0)
#>   See `attr(x, "no_county_cases")` for each state.
#> Warning: 13 states have more than 25% of their cases in no county row.
#> • HIV diagnoses 2022: Vermont (44%), Montana (39%), Wyoming (34%), and Puerto
#>   Rico (26%)
#> • HIV prevalence 2022: New Hampshire (88%), Maine (87%), Idaho (74%), Rhode
#>   Island (73%), Alaska (69%), and 5 more
#> ! County and tract estimates for these states leave those cases out, so they
#>   are too low by about that share overall.
#> ℹ This happens when a state's county counts are largely suppressed, or when `x`
#>   lacks some of its counties. CDCAtlas doesn't check the cause.
#> ℹ See `attr(x, "no_county_cases")` for each state.
#> ℹ 314 rows with no count that `impute_suppressed()` couldn't estimate were left
#>   out of the tract estimates and counted in the remainder with no county.
#>   Counties: "Valdez-Cordova Census Area, AK", "Aguada Municipio, PR",
#>   "Aguadilla Municipio, PR", "Aguas Buenas Municipio, PR", "Arroyo Municipio,
#>   PR", "Barceloneta Municipio, PR", "Barranquitas Municipio, PR", "Bayamón
#>   Municipio, PR", "Caguas Municipio, PR", "Camuy Municipio, PR", "Canóvanas
#>   Municipio, PR", "Cataño Municipio, PR", "Cayey Municipio, PR", "Ceiba
#>   Municipio, PR", "Ciales Municipio, PR", "Cidra Municipio, PR", "Coamo
#>   Municipio, PR", "Corozal Municipio, PR", …, "Cabo Rojo Municipio, PR", and
#>   "Arecibo Municipio, PR".
```

The tract populations come from ACS table B03002 (Hispanic or Latino
origin by race), whose groups match AtlasPlus’s: Hispanic/Latino of any
race, and non-Hispanic groups for the others. B03002 has no ages, so for
HIV each tract’s race populations are scaled by its share of residents
aged 13 and older. `race_table = "B01001"` uses the race-specific
sex-by-age tables instead, which have ages but match AtlasPlus’s groups
only for White and Hispanic. “Other” and “Unknown” have no Census
population, so they are left out (see the next section for “Unknown”).

``` r

hiv_tract_race |>
  count(race_ethnicity, indicator)
#> # A tibble: 14 × 3
#>    race_ethnicity                         indicator          n
#>    <chr>                                  <chr>          <int>
#>  1 American Indian/Alaska Native          HIV diagnoses  85096
#>  2 American Indian/Alaska Native          HIV prevalence 85343
#>  3 Asian                                  HIV diagnoses  85096
#>  4 Asian                                  HIV prevalence 85362
#>  5 Black/African American                 HIV diagnoses  84958
#>  6 Black/African American                 HIV prevalence 85375
#>  7 Hispanic/Latino                        HIV diagnoses  85096
#>  8 Hispanic/Latino                        HIV prevalence 85396
#>  9 Multiracial                            HIV diagnoses  85033
#> 10 Multiracial                            HIV prevalence 85322
#> 11 Native Hawaiian/Other Pacific Islander HIV diagnoses  84999
#> 12 Native Hawaiian/Other Pacific Islander HIV prevalence 85167
#> 13 White                                  HIV diagnoses  84924
#> 14 White                                  HIV prevalence 85122
```

## Choices you can change

Each default can be changed in
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md);
see
[`?extrapolate_from_county`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
for the details.

| Situation | Default | Alternative |
|----|----|----|
| Cases in a state’s total that no county accounts for | Left out and reported (`no_county = "report"`) | `no_county = "spread"` shares them across the state’s counties, by population or by cases (`spread_by`). Not available for CDC’s HIV estimates, whose county data cover only Ending the HIV Epidemic counties. |
| A county has cases in a stratum but no Census population in any of its tracts | Shared by the tracts’ total population (`empty_strata = "total"`) | `"equal"` shares equally across tracts; `"none"` keeps the tract rows with `area_cases` as `NA`, with a warning. |
| Cases of unknown race or age | Left out and listed (`unknown_race`, `unknown_age = "report"`) | `"redistribute"` shares them across the county’s known groups in proportion to their counts. |
| Suppressed counties [`impute_suppressed()`](https://vagishhemmige.github.io/CDCAtlas/reference/impute_suppressed.md) couldn’t estimate | Left out and listed (`unestimated = "leave out"`) | `"error"` stops. |
| A tract with no ACS population figure | The whole county stratum (that county, indicator, year and group) is left out and listed, so cases aren’t shared over an incomplete denominator (`missing_population = "leave out"`) | `"error"` stops. |
| Which ACS release | The 5-year release ending in the data year | `acs_year` sets it. |

County rows left out are listed, with the reason, in
`attr(result, "not_allocated")`. With `empty_strata = "none"`, the tract
rows stay in the result with `area_cases` as `NA` instead.

## Older years and Census geography

CDC reports every year on its current county map, while each ACS release
uses the tract map of its own years. The package assigns 2010 tracts in
Connecticut and Alaska to CDC’s current counties, so ACS releases from
2010 to 2019 cover them. A few combinations aren’t covered, such as
Connecticut with the 2020 and 2021 releases; those counties are left out
with a warning that says why. [About CDC AtlasPlus
data](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html#cdcs-county-map)
lists them.

### The one tract in two counties

One 2010 tract in Connecticut (GEOID 09013890201) lies in two of CDC’s
current counties, the Capitol and Northeastern Connecticut planning
regions. With ACS releases from 2010 to 2019, the result has two rows
for it, one in each planning region, and two rows for each of its block
groups with `to = "block group"`. So `geoid` is unique within
`county_fips`, not overall, and
[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
warns when this happens.

Each row holds a share of the tract: its ACS populations are multiplied
by the share of the tract’s land area in that planning region (about 39%
and 61%), and its cases are that planning region’s count shared over
them. A block group gets its parent tract’s shares, which are not
measured shares of the block group’s own population.

Before joining to boundaries, which have each tract once, sum the area
columns by `geoid` within year, indicator and stratum:

``` r

hiv_tract_2017 <- get_atlas(
  disease = "hiv",
  year = 2017,
  geography = "county"
) |>
  impute_suppressed() |>
  extrapolate_from_county()

hiv_tract_2017_one_row <- hiv_tract_2017 |>
  summarize(
    area_cases = sum(area_cases),
    area_population_acs = sum(area_population_acs),
    area_total_population = sum(area_total_population),
    .by = c(year, indicator, race_ethnicity, sex, age, geoid, area_name)
  )
```

Sum all three columns: keeping only one of the two rows, or the first
population, would understate the tract. The county columns
(`county_fips`, `county_cases`, `county_population_acs` and the rest)
are left out of the summary because the combined row belongs to two
counties; they repeat on every tract of a county, so they should not be
summed across tracts either.

## Multiple stratification variables

[`extrapolate_from_county()`](https://vagishhemmige.github.io/CDCAtlas/reference/extrapolate_from_county.md)
takes data stratified by at most one variable.
[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
can download two-way strata (sex by age, for example), but these can’t
be extrapolated yet.

## Working with geometry

The results have Census GEOIDs (`geoid`) but no boundaries. To map them,
join them to tract boundaries, for example from the tigris package. With
ACS releases from 2010 to 2019, first sum the one tract that is in two
counties (see [The one tract in two
counties](#the-one-tract-in-two-counties)).
