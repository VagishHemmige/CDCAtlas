# rows marked 'Data not available' with no count are dropped

    Code
      out <- impute_suppressed(county, method = "zero")
    Message
      i Dropped 2 rows marked "Data not available" with no count.
        Their cases are still in the state totals; see `no_county_cases()`.

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

