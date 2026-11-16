# Getting started with CDCAtlas

**CDCAtlas** provides R functions for retrieving public health
surveillance data from CDC AtlasPlus.

This article introduces common use patterns:

- retrieving national, state- and county-level data
- querying different diseases
- using sex, age, and race/ethnicity stratification
- comparing rates across groups
- preparing AtlasPlus data for plotting and analysis

This article does **not** cover tract-level extrapolation. That is an
advanced use case covered in [Extrapolation to
tracts](https://vagishhemmige.github.io/CDCAtlas/articles/extrapolation.md).

## Installation

``` r

# install.packages("remotes")
remotes::install_github("VagishHemmige/CDCAtlas")
```

## Setup

``` r

library(CDCAtlas)
library(dplyr)
library(ggplot2)
```

## Basic query: state-level chlamydia data

A simple query retrieves one disease, one geography level, and one year.

``` r

chlamydia_state <- get_atlas(
  disease   = "chlamydia",
  geography = "state",
  year      = 2022
)

head(chlamydia_state)
#> # A tibble: 6 × 17
#>   indicator  year geography  data_status race_ethnicity sex   age   transmission
#>   <fct>     <dbl> <fct>      <fct>       <fct>          <fct> <fct> <fct>       
#> 1 Chlamydia  2022 Alabama    Not Suppre… All races/eth… Both… All … All transmi…
#> 2 Chlamydia  2022 Alaska     Not Suppre… All races/eth… Both… All … All transmi…
#> 3 Chlamydia  2022 Arizona    Not Suppre… All races/eth… Both… All … All transmi…
#> 4 Chlamydia  2022 Arkansas   Not Suppre… All races/eth… Both… All … All transmi…
#> 5 Chlamydia  2022 California Not Suppre… All races/eth… Both… All … All transmi…
#> 6 Chlamydia  2022 Colorado   Not Suppre… All races/eth… Both… All … All transmi…
#> # ℹ 9 more variables: rate100000 <dbl>, cases <dbl>, population <dbl>,
#> #   lowerci_rate <dbl>, upperci_rate <dbl>, rse <dbl>, lowerci_cases <dbl>,
#> #   upperci_cases <dbl>, fips <chr>
```

The returned object is a data frame that can be used directly with
tidyverse tools. Each row is one indicator, place, year and stratum; the
`indicator` column says what was counted, and `data_status` says whether
a value is suppressed or not available.

``` r

glimpse(chlamydia_state)
#> Rows: 57
#> Columns: 17
#> $ indicator      <fct> Chlamydia, Chlamydia, Chlamydia, Chlamydia, Chlamydia, …
#> $ year           <dbl> 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2…
#> $ geography      <fct> Alabama, Alaska, Arizona, Arkansas, California, Colorad…
#> $ data_status    <fct> Not Suppressed, Not Suppressed, Not Suppressed, Not Sup…
#> $ race_ethnicity <fct> All races/ethnicities, All races/ethnicities, All races…
#> $ sex            <fct> Both sexes, Both sexes, Both sexes, Both sexes, Both se…
#> $ age            <fct> All age groups, All age groups, All age groups, All age…
#> $ transmission   <fct> All transmission categories, All transmission categorie…
#> $ rate100000     <dbl> 612.1, 727.7, 554.4, 588.3, 493.6, 456.3, 351.3, 508.3,…
#> $ cases          <dbl> 31060, 5338, 40796, 17918, 192647, 26646, 12738, 5177, …
#> $ population     <dbl> 5074296, 733583, 7359197, 3045637, 39029342, 5839926, 3…
#> $ lowerci_rate   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ upperci_rate   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ rse            <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ lowerci_cases  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ upperci_cases  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ fips           <chr> "01", "02", "04", "05", "06", "08", "09", "10", "11", "…
```

## Plot state-level rates

AtlasPlus’s state level also includes U.S. territories. Their FIPS codes
are above 56, so this keeps the 50 states and the District of Columbia:

``` r

is_state_or_dc <- function(fips) as.integer(fips) <= 56
```

``` r

chlamydia_state |>
  filter(is_state_or_dc(fips), !is.na(rate100000)) |>
  ggplot(aes(x = reorder(geography, rate100000), y = rate100000)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Chlamydia rate by state and DC, 2022",
    x = NULL,
    y = "Rate per 100,000"
  )
```

![Bar chart of 2022 chlamydia rates per 100,000 population for the 50
states and DC, sorted from highest to
lowest.](getting-started_files/figure-html/unnamed-chunk-7-1.png)

## Query county-level data

AtlasPlus can also return county-level data for supported diseases.

``` r

chlamydia_county <- get_atlas(
  disease   = "chlamydia",
  geography = "county",
  year      = 2022
)

head(chlamydia_county)
#> # A tibble: 6 × 17
#>   indicator  year geography  data_status race_ethnicity sex   age   transmission
#>   <fct>     <dbl> <fct>      <fct>       <fct>          <fct> <fct> <fct>       
#> 1 Chlamydia  2022 Autauga C… Not Suppre… All races/eth… Both… All … All transmi…
#> 2 Chlamydia  2022 Baldwin C… Not Suppre… All races/eth… Both… All … All transmi…
#> 3 Chlamydia  2022 Barbour C… Not Suppre… All races/eth… Both… All … All transmi…
#> 4 Chlamydia  2022 Bibb Coun… Not Suppre… All races/eth… Both… All … All transmi…
#> 5 Chlamydia  2022 Blount Co… Not Suppre… All races/eth… Both… All … All transmi…
#> 6 Chlamydia  2022 Bullock C… Not Suppre… All races/eth… Both… All … All transmi…
#> # ℹ 9 more variables: rate100000 <dbl>, cases <dbl>, population <dbl>,
#> #   lowerci_rate <dbl>, upperci_rate <dbl>, rse <dbl>, lowerci_cases <dbl>,
#> #   upperci_cases <dbl>, fips <chr>
```

A common workflow is to identify counties with the highest reported
rates.

``` r

chlamydia_county |>
  filter(!is.na(rate100000)) |>
  arrange(desc(rate100000)) |>
  select(geography, cases, rate100000) |>
  head(10)
#> # A tibble: 10 × 3
#>    geography                    cases rate100000
#>    <fct>                        <dbl>      <dbl>
#>  1 Reeves County, TX              552      4277.
#>  2 Todd County, SD                340      3688.
#>  3 Kusilvak Census Area, AK       287      3467 
#>  4 Dewey County, SD               174      3385.
#>  5 Bethel Census Area, AK         528      2892 
#>  6 Nome Census Area, AK           281      2857.
#>  7 Oglala Lakota County, SD       359      2656.
#>  8 Mellette County, SD             40      2114.
#>  9 Northwest Arctic Borough, AK   150      2021.
#> 10 Tunica County, MS              186      1967.
```

## Stratification by sex

Many AtlasPlus endpoints support stratification. For example, we can
retrieve state-level gonorrhea data by sex.

``` r

gonorrhea_sex <- get_atlas(
  disease   = "gonorrhea",
  geography = "state",
  year      = 2022,
  stratify_by = "sex"
)

head(gonorrhea_sex)
#> # A tibble: 6 × 17
#>   indicator  year geography data_status  race_ethnicity sex   age   transmission
#>   <fct>     <dbl> <fct>     <fct>        <fct>          <fct> <fct> <fct>       
#> 1 Gonorrhea  2022 Alabama   Not Suppres… All races/eth… Male  All … All transmi…
#> 2 Gonorrhea  2022 Alabama   Not Suppres… All races/eth… Fema… All … All transmi…
#> 3 Gonorrhea  2022 Alaska    Not Suppres… All races/eth… Male  All … All transmi…
#> 4 Gonorrhea  2022 Alaska    Not Suppres… All races/eth… Fema… All … All transmi…
#> 5 Gonorrhea  2022 Arizona   Not Suppres… All races/eth… Male  All … All transmi…
#> 6 Gonorrhea  2022 Arizona   Not Suppres… All races/eth… Fema… All … All transmi…
#> # ℹ 9 more variables: rate100000 <dbl>, cases <dbl>, population <dbl>,
#> #   lowerci_rate <dbl>, upperci_rate <dbl>, rse <dbl>, lowerci_cases <dbl>,
#> #   upperci_cases <dbl>, fips <chr>
```

Now we can compare rates by sex within each state.

``` r

gonorrhea_sex |>
  filter(is_state_or_dc(fips), !is.na(rate100000)) |>
  group_by(sex) |>
  slice_max(rate100000, n = 10, with_ties = FALSE) |>
  ungroup() |>
  ggplot(aes(x = reorder(geography, rate100000), y = rate100000)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sex, scales = "free_y") +
  labs(
    title = "States (and DC) with the highest gonorrhea rates by sex, 2022",
    x = NULL,
    y = "Rate per 100,000"
  )
```

![Two bar charts, one per sex, of the ten states or DC with the highest
2022 gonorrhea rates per 100,000
population.](getting-started_files/figure-html/unnamed-chunk-11-1.png)

## Stratification by age

AtlasPlus also supports age-stratified queries for many
disease/geography combinations. For national figures, ask for
`geography = "national"` rather than adding up states: CDC’s national
totals don’t depend on which state cells are suppressed, and state rows
include territories.

``` r

hiv_age <- get_atlas(
  disease   = "hiv",
  geography = "national",
  year      = 2022,
  stratify_by = "age"
)

hiv_age |>
  select(indicator, age, cases, rate100000)
#> # A tibble: 12 × 4
#>    indicator      age    cases rate100000
#>    <fct>          <fct>  <dbl>      <dbl>
#>  1 HIV diagnoses  13-24   7104       13.6
#>  2 HIV diagnoses  25-34  14077       30.8
#>  3 HIV diagnoses  35-44   8265       18.8
#>  4 HIV diagnoses  45-54   4539       11.2
#>  5 HIV diagnoses  55-64   2829        6.7
#>  6 HIV diagnoses  65+      871        1.5
#>  7 HIV prevalence 13-24  28153       53.7
#>  8 HIV prevalence 25-34 165315      362. 
#>  9 HIV prevalence 35-44 209819      477. 
#> 10 HIV prevalence 45-54 234447      578. 
#> 11 HIV prevalence 55-64 290975      687. 
#> 12 HIV prevalence 65+   158815      276.
```

``` r

hiv_age |>
  filter(indicator == "HIV diagnoses") |>
  ggplot(mapping=aes(x = age, y = cases)) +
  geom_col() +
  labs(
    title = "HIV diagnoses by age group, United States, 2022",
    x = "Age group",
    y = "Cases"
  )
```

![Bar chart of 2022 U.S. HIV diagnoses by age
group.](getting-started_files/figure-html/unnamed-chunk-13-1.png)

## Stratification by race/ethnicity

CDC AtlasPlus uses a combined race/ethnicity variable rather than
separate race and Hispanic ethnicity fields. The groups differ by
disease: hepatitis, for example, has Asian/Pacific Islander instead of
separate Asian and Native Hawaiian/Other Pacific Islander groups. [About
CDC AtlasPlus
data](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.md)
lists them.

``` r

hiv_race <- get_atlas(
  disease     = "hiv",
  geography   = "national",
  year        = 2022,
  stratify_by = "race"
)

hiv_race |>
  select(indicator, race_ethnicity, cases, rate100000) |>
  arrange(indicator, desc(cases))
#> # A tibble: 14 × 4
#>    indicator      race_ethnicity                          cases rate100000
#>    <fct>          <fct>                                   <dbl>      <dbl>
#>  1 HIV diagnoses  Black/African American                  14276       40.8
#>  2 HIV diagnoses  Hispanic/Latino                         12084       23.8
#>  3 HIV diagnoses  White                                    8873        5.2
#>  4 HIV diagnoses  Multiracial                              1401       25.8
#>  5 HIV diagnoses  Asian                                     773        4.4
#>  6 HIV diagnoses  American Indian/Alaska Native             206       10.1
#>  7 HIV diagnoses  Native Hawaiian/Other Pacific Islander     72       13.7
#>  8 HIV prevalence Black/African American                 427091     1220. 
#>  9 HIV prevalence White                                  301218      176. 
#> 10 HIV prevalence Hispanic/Latino                        272472      536. 
#> 11 HIV prevalence Multiracial                             65402     1204. 
#> 12 HIV prevalence Asian                                   16819       96.2
#> 13 HIV prevalence American Indian/Alaska Native            3033      149. 
#> 14 HIV prevalence Native Hawaiian/Other Pacific Islander    876      167.
```

Cases with unknown race/ethnicity are counted in the totals but not
shown by race, so the groups may not add up to the total.

``` r

hiv_race |>
  filter(indicator == "HIV diagnoses") |>
  ggplot(mapping =aes(x = reorder(race_ethnicity, cases), y = cases)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "HIV diagnoses by race/ethnicity, United States, 2022",
    x = NULL,
    y = "Cases"
  )
```

![Bar chart of 2022 U.S. HIV diagnoses by race and ethnicity, sorted
from most to
fewest.](getting-started_files/figure-html/unnamed-chunk-15-1.png)

## Multiple stratification variables

You can request combinations of stratification variables when supported
by the underlying AtlasPlus endpoint. For example, HIV data can be
queried by both sex and age.

``` r

hiv_age_sex <- get_atlas(
  disease     = "hiv",
  geography   = "national",
  year        = 2022,
  stratify_by = c("sex", "age")
)

hiv_age_sex |>
  select(indicator, sex, age, cases)
#> # A tibble: 24 × 4
#>    indicator     sex    age   cases
#>    <fct>         <fct>  <fct> <dbl>
#>  1 HIV diagnoses Male   13-24  6244
#>  2 HIV diagnoses Male   25-34 12043
#>  3 HIV diagnoses Male   35-44  6472
#>  4 HIV diagnoses Male   45-54  3308
#>  5 HIV diagnoses Male   55-64  2052
#>  6 HIV diagnoses Male   65+     607
#>  7 HIV diagnoses Female 13-24   860
#>  8 HIV diagnoses Female 25-34  2034
#>  9 HIV diagnoses Female 35-44  1793
#> 10 HIV diagnoses Female 45-54  1231
#> # ℹ 14 more rows
```

This allows direct comparison of age distributions by sex.

``` r

hiv_age_sex |>
  filter(indicator == "HIV diagnoses") |>
  ggplot(aes(x = age, y = cases)) +
  geom_col() +
  facet_wrap(~ sex) +
  labs(
    title = "HIV diagnoses by age and sex, United States, 2022",
    x = "Age group",
    y = "Cases"
  )
```

![Two bar charts, one per sex, of 2022 U.S. HIV diagnoses by age
group.](getting-started_files/figure-html/unnamed-chunk-17-1.png)

## Comparing years

You can also query multiple years and compare trends over time.
AtlasPlus warns that “due to the impact of the COVID-19 pandemic, data
for 2020 and 2021 may need to be interpreted with caution”
([AtlasPlus](https://gis.cdc.gov/grasp/nchhstpatlas/main.html)), so read
changes across those years carefully.

``` r

syphilis_trend <- get_atlas(
  disease   = "adult syphilis",
  geography = "national",
  year      = 2018:2022
)

syphilis_trend |>
  select(year, indicator, cases, rate100000)
#> # A tibble: 15 × 4
#>     year indicator                                 cases rate100000
#>    <dbl> <fct>                                     <dbl>      <dbl>
#>  1  2018 Primary and Secondary Syphilis            35063       10.7
#>  2  2018 Early Non-Primary, Non-Secondary Syphilis 38539       11.8
#>  3  2018 Unknown Duration or Late Syphilis         40137       12.3
#>  4  2019 Primary and Secondary Syphilis            38992       11.9
#>  5  2019 Early Non-Primary, Non-Secondary Syphilis 41655       12.7
#>  6  2019 Unknown Duration or Late Syphilis         47296       14.4
#>  7  2020 Primary and Secondary Syphilis            41655       12.6
#>  8  2020 Early Non-Primary, Non-Secondary Syphilis 43145       13  
#>  9  2020 Unknown Duration or Late Syphilis         46997       14.2
#> 10  2021 Primary and Secondary Syphilis            53767       16.2
#> 11  2021 Early Non-Primary, Non-Secondary Syphilis 51830       15.6
#> 12  2021 Unknown Duration or Late Syphilis         68261       20.6
#> 13  2022 Primary and Secondary Syphilis            59016       17.7
#> 14  2022 Early Non-Primary, Non-Secondary Syphilis 56913       17.1
#> 15  2022 Unknown Duration or Late Syphilis         87571       26.3
```

`"adult syphilis"` returns one indicator per stage.

``` r

syphilis_trend |>
ggplot(mapping= aes(x = year, y = cases, color=indicator, group=indicator)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Adult syphilis cases by stage, United States, 2018-2022",
    x = "Year",
    y = "Cases"
  )
```

![Line chart of U.S. adult syphilis cases from 2018 to 2022, one line
per stage.](getting-started_files/figure-html/unnamed-chunk-19-1.png)

## Working with censored or missing values

CDC AtlasPlus may suppress or censor some values, especially for small
counts. Depending on the disease and geography, `cases` or `rate100000`
may be missing. `data_status` says why.

A common first step is to inspect missingness.

``` r

chlamydia_county |>
  summarize(
    n_rows = n(),
    missing_cases = sum(is.na(cases)),
    missing_rates = sum(is.na(rate100000))
  )
#> # A tibble: 1 × 3
#>   n_rows missing_cases missing_rates
#>    <int>         <int>         <int>
#> 1   3231            19            19

chlamydia_county |>
  count(data_status)
#> # A tibble: 3 × 2
#>   data_status            n
#>   <fct>              <int>
#> 1 Data suppressed        9
#> 2 Data not available    10
#> 3 Not Suppressed      3212
```

Recent years can also be provisional. For HIV, AtlasPlus’s technical
notes say data are preliminary until a 12-month reporting delay has
passed and shouldn’t be used for formal trend analyses until then. Check
the technical notes for each indicator (in the AtlasPlus menu) before
comparing years.

## Choosing diseases, geographies and strata

[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
takes one of eight `disease` values. Some return several indicators (see
[`?get_atlas`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)),
and AtlasPlus offers each at different geographies:

| `disease` | Geographies | Notes |
|----|----|----|
| `"hiv"` | national, state, county, msa | HIV diagnoses and prevalence |
| `"estimate"` | national, region, state, county | Estimated HIV incidence and prevalence, and Knowledge of Status |
| `"chlamydia"`, `"gonorrhea"`, `"adult syphilis"` | national, state, county | Age groups include “Unknown” |
| `"tuberculosis"` | national, state, county |  |
| `"hepatitis"` | national, state | No county data |
| `"congenital syphilis"` | national, state | Can’t be stratified |

[`get_atlas()`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
stops with an error, before downloading, for many requests CDC doesn’t
offer, such as the geographies in the table. Some other combinations of
disease, geography and stratification aren’t offered either; those stop
with an error after CDC returns no data. If CDC returns some of a
disease’s indicators but not others, a warning names the missing ones.

``` r

get_atlas(disease = "estimate", geography = "region", year = 2021)
get_atlas(disease = "hepatitis", geography = "state", year = 2022)
get_atlas(disease = "tuberculosis", geography = "county", year = 2022)
```

Each disease has its own age groups: 13-24 to 65+ for `"hiv"` and
`"estimate"`, and 0-14 to 65+ for the others except congenital syphilis.
[About CDC AtlasPlus
data](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.md)
lists them, with what CDC’s numbers mean and where they trip people up.

## Saved downloads

Each response from CDC is saved on your computer and reused for 30 days,
so running the same code again doesn’t download the data again. To
download fresh data sooner, for example after CDC revises a recent year:

``` r

get_atlas(disease = "chlamydia", geography = "county", year = 2022, refresh = TRUE)

# Or delete every saved download
clear_atlas_cache()
```

## Next steps

After getting comfortable with basic queries, consider:

- [About CDC AtlasPlus
  data](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.md):
  suppression, denominators, what each disease offers, and CDC’s county
  map
- tract-level estimates ([Extrapolation to
  tracts](https://vagishhemmige.github.io/CDCAtlas/articles/extrapolation.md))
- comparing state and county results
- joining AtlasPlus data to Census or spatial data

See
[`?get_atlas`](https://vagishhemmige.github.io/CDCAtlas/reference/get_atlas.md)
for full argument documentation.
