# a missing count that isn't suppression gets a warning naming its status

    Code
      out <- impute_suppressed(county, method = "zero")
    Condition
      Warning:
      2 rows with no count are not marked "Data suppressed", so they are not filled in and left `NA`.
      i Data status: "No reported cases".
      i They are marked `cases_source = "not estimated"`.

# not_available = 'drop' drops rows marked 'Data not available' with no count

    Code
      out <- impute_suppressed(county, method = "zero", not_available = "drop")
    Message
      i Dropped 2 rows marked "Data not available" with no count.
        Their cases are still in the state totals, and `no_county_cases()` counts them in the remainder for any state group that still has rows.

# suppressed rows with no county population stay NA, with a warning

    Code
      out <- impute_suppressed(county)
    Condition
      Warning:
      2 suppressed rows could not be estimated and are set to `NA`.
      i No state rate or no county population for "County 01003".
      i They are marked `cases_source = "not estimated"`.

# suppressed rows with no state rate stay NA, with a warning

    Code
      out <- impute_suppressed(fake_county_atlas(suppressed = "01003"))
    Condition
      Warning:
      2 suppressed rows could not be estimated and are set to `NA`.
      i No state rate or no county population for "County 01003".
      i They are marked `cases_source = "not estimated"`.

# impute_suppressed() rejects data it can't use

    Code
      impute_suppressed(state)
    Condition
      Error in `impute_suppressed()`:
      ! `x` must be county data.
      i Use `get_atlas(geography = "county")`.

---

    Code
      impute_suppressed(county[c("fips", "cases")])
    Condition
      Error in `impute_suppressed()`:
      ! `x` is missing column indicator, year, race_ethnicity, sex, age, geography, data_status, and population.
      i Use data returned by `get_atlas()`.

---

    Code
      impute_suppressed("county")
    Condition
      Error in `impute_suppressed()`:
      ! `x` must be a data frame from `get_atlas()`.
      x You supplied a <character>.

---

    Code
      impute_suppressed(impute_suppressed(county, method = "zero"))
    Condition
      Error in `impute_suppressed()`:
      ! `x` has already been through `impute_suppressed()`.
      i It has a cases_source column.

---

    Code
      impute_suppressed(county, method = "mean")
    Condition
      Error in `impute_suppressed()`:
      ! `method` must be one of "state_rate" or "zero", not "mean".

---

    Code
      impute_suppressed(county, cap = -1)
    Condition
      Error in `impute_suppressed()`:
      ! `cap` must be a single number of 0 or more, or `Inf`.

---

    Code
      impute_suppressed(county, cap = c(1, 2))
    Condition
      Error in `impute_suppressed()`:
      ! `cap` must be a single number of 0 or more, or `Inf`.

---

    Code
      impute_suppressed(county[0, ])
    Condition
      Error in `impute_suppressed()`:
      ! `x` has no rows.

---

    Code
      impute_suppressed(numeric_fips)
    Condition
      Error in `impute_suppressed()`:
      ! fips in `x` must be character, such as "01001".
      i If you read the data from a file, read fips as character so leading zeros are kept.

---

    Code
      impute_suppressed(rbind(county, county[1, ]))
    Condition
      Error in `impute_suppressed()`:
      ! `x` has 1 duplicated county row.
      i Each county, indicator, year and stratum must appear once. Check how the data were combined.

---

    Code
      impute_suppressed(mixed)
    Condition
      Error in `impute_suppressed()`:
      ! `x` mixes sex totals with sex strata.
      i Keep either the "Both sexes" rows or the stratified rows.

# negative, infinite or non-numeric counts stop before any request (#115)

    Code
      impute_suppressed(negative, method = "zero")
    Condition
      Error in `impute_suppressed()`:
      ! cases in `x` must be 0 or more and finite.
      x 1 row has a negative or infinite value, such as -10.

---

    Code
      impute_suppressed(infinite)
    Condition
      Error in `impute_suppressed()`:
      ! population in `x` must be 0 or more and finite.
      x 1 row has a negative or infinite value, such as Inf.

---

    Code
      impute_suppressed(text)
    Condition
      Error in `impute_suppressed()`:
      ! cases in `x` must be numeric.
      x It is a character vector.

---

    Code
      no_county_cases(imputed)
    Condition
      Error in `no_county_cases()`:
      ! cases in `x` must be 0 or more and finite.
      x 4 rows have a negative or infinite value, such as -11.

---

    Code
      extrapolate_from_county(imputed)
    Condition
      Error in `extrapolate_from_county()`:
      ! cases in `x` must be 0 or more and finite.
      x 4 rows have a negative or infinite value, such as -11.

# state_rate is floored at `floor`

    Code
      impute_suppressed(county, floor = 5)
    Condition
      Error in `impute_suppressed()`:
      ! `floor` (5) must not be larger than `cap` (4).

---

    Code
      impute_suppressed(county, floor = NA)
    Condition
      Error in `impute_suppressed()`:
      ! `floor` must be a single number of 0 or more, or `Inf`.

# impute_suppressed() stops if `x` already has cases_uncapped

    Code
      impute_suppressed(county, method = "zero")
    Condition
      Error in `impute_suppressed()`:
      ! `x` already has a cases_uncapped column.
      i `impute_suppressed()` adds a column of that name. Rename or remove yours first.

# population = 'acs' stops on two-way strata before any request

    Code
      impute_suppressed(sex_by_age(), population = "acs")
    Condition
      Error in `impute_suppressed()`:
      ! `x` can have at most one stratification variable.
      x It is stratified by "sex" and "age".
      i `population = "cdc"` works for any strata.

# population = 'acs' stops for congenital syphilis

    Code
      impute_suppressed(congenital, population = "acs")
    Condition
      Error in `impute_suppressed()`:
      ! Census populations don't fit congenital syphilis.
      x Its rates are per 100,000 live births, not per 100,000 people.
      i `population = "cdc"` works for any strata.

# population = 'acs' stops for Knowledge of Status before any request (#108)

    Code
      impute_suppressed(status, population = "acs")
    Condition
      Error in `impute_suppressed()`:
      ! `population = "acs"` doesn't fit Knowledge of Status.
      x Its rate is a share of people living with HIV, not of residents.
      i Use `population = "cdc"`, CDC's estimate of people living with HIV in each county.

# population = 'acs' needs a Census key

    Code
      impute_suppressed(county, population = "acs")
    Condition
      Error in `impute_suppressed()`:
      ! Downloading Census populations needs a Census API key.
      x `CENSUS_API_KEY` is empty.
      i Get a key at <https://api.census.gov/data/key_signup.html> and set it with `tidycensus::census_api_key()`.

# unestimated = 'zero' sets rows with no rate to 0

    Code
      out <- impute_suppressed(county, unestimated = "zero")
    Condition
      Warning:
      2 suppressed rows could not be estimated and are set to `0`.
      i No state rate or no county population for "County 01003".
      i They are marked `cases_source = "not estimated"`.

# the message says how many estimates the cap decided (#164)

    Code
      out <- impute_suppressed(county)
    Message
      i 2 suppressed rows were filled in from state rates.
      * The cap of 4 set the value of 1 row (50%): the estimate was higher.
        cases_uncapped holds the estimates before the cap and floor.

# the message says how many estimates a raised floor decided (#164)

    Code
      out <- impute_suppressed(county, floor = 2)
    Message
      i 2 suppressed rows were filled in from state rates.
      * The cap of 4 set the value of 1 row (50%): the estimate was higher.
      * The floor of 2 set the value of 1 row (50%): the estimate was lower.
        cases_uncapped holds the estimates before the cap and floor.

# limits that can't decide anything aren't mentioned (#164)

    Code
      out <- impute_suppressed(county, cap = Inf)
    Message
      i 2 suppressed rows were filled in from state rates.

# fit_to = 'county_total' fits suppressed strata to the county total

    Code
      out <- impute_suppressed(county, fit_to = "county_total")
    Message
      i Fitted 2 county groups (county, indicator and year) to the county's published total.
        fit_reason says why for each row.

# unfitted groups keep the state-rate estimate, and only they count for the cap

    Code
      out <- impute_suppressed(county, fit_to = "county_total")
    Message
      i Fitted 1 county group (county, indicator and year) to the county's published total.
      i 1 group with unpublished strata was not fitted and keeps the state-rate estimate:
      * county total suppressed: 1
        fit_reason says why for each row.
      i 2 suppressed rows were filled in from state rates.
      * The cap of 4 set the value of 2 rows (100%): the estimate was higher.
        cases_uncapped holds the estimates before the cap and floor.

# eligibility survives `not_available = "drop"`, in row order

    Code
      out <- impute_suppressed(county, fit_to = "county_total", not_available = "drop")
    Message
      i Dropped 2 rows marked "Data not available" with no count.
        Their cases are still in the state totals, and `no_county_cases()` counts them in the remainder for any state group that still has rows.
    Condition
      Warning:
      In 2 groups of county, indicator and year, the strata are incomplete or inconsistent, so they were not fitted.
      i Check that each requested stratum appears once, the other columns match the requested population (HIV, ages 13 and over), and each count agrees with its data status.
    Message
      i Fitted 2 county groups (county, indicator and year) to the county's published total.
      i 2 groups with unpublished strata were not fitted and keep the state-rate estimate:
      * strata incomplete or inconsistent: 2
        fit_reason says why for each row.
      i 2 suppressed rows were filled in from state rates.
      * The cap of 4 set the value of 1 row (50%): the estimate was higher.
        cases_uncapped holds the estimates before the cap and floor.

# county counts above the state total are reported, not adjusted

    Code
      out <- suppressMessages(impute_suppressed(county, fit_to = "county_total"))
    Condition
      Warning:
      In 4 state strata, county counts after fitting add up to more than the state's published total.
      i They are reported, not adjusted: the fit matches each county's own total only.

# fit_to is checked before anything is downloaded

    Code
      impute_suppressed(by_sex, fit_to = "county")
    Condition
      Error in `impute_suppressed()`:
      ! `fit_to` must be one of "none" or "county_total", not "county".
      i Did you mean "county_total"?
    Code
      impute_suppressed(by_sex, method = "zero", fit_to = "county_total")
    Condition
      Error in `impute_suppressed()`:
      ! `fit_to = "county_total"` needs `method = "state_rate"`.
      i The fit shares each county's residual in proportion to the state-rate estimates.
    Code
      impute_suppressed(fake_county_atlas(suppressed = "01003"), fit_to = "county_total")
    Condition
      Error in `impute_suppressed()`:
      ! `fit_to = "county_total"` needs data stratified by sex, age or race.
      i Unstratified county counts are themselves the county totals.
    Code
      impute_suppressed(fake_county_atlas(stratify_by = "sex", indicators = "Chlamydia",
        suppressed = "01003"), fit_to = "county_total")
    Condition
      Error in `impute_suppressed()`:
      ! `fit_to = "county_total"` works only for HIV diagnoses and HIV prevalence.
      x `x` also has "Chlamydia".

---

    Code
      impute_suppressed(two_way, fit_to = "county_total")
    Condition
      Error in `impute_suppressed()`:
      ! `fit_to = "county_total"` needs data stratified by one variable.
      x `x` is stratified by "sex" and "age".

---

    Code
      impute_suppressed(with_column)
    Condition
      Error in `impute_suppressed()`:
      ! `x` already has a fit_reason column.
      i `impute_suppressed()` adds a column of that name. Rename or remove yours first.

