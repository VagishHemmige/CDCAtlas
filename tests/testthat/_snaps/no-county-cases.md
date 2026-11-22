# rows impute_suppressed() couldn't estimate count towards the remainder

    Code
      report <- no_county_cases(not_estimated)
    Message
      i 2 rows with no count that `impute_suppressed()` couldn't estimate were counted in the remainder with no county.
        County: "County 01003".
    Condition
      Warning:
      1 state has more than 25% of its published state total in no county row of `x`.
      * HIV diagnoses 2022: Alabama (98%)
      * HIV prevalence 2022: Alabama (85%)
      i Percentages are of the state totals CDC published, over the strata that have one.
      ! Adding up this state's county counts, or estimating smaller areas from them, leaves those cases out, so the results are too low by about that share.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.

# no_county_cases() refuses published rows with no count and state data

    Code
      no_county_cases(county)
    Condition
      Error in `no_county_cases()`:
      ! `x` has 1 row with no count.
      i Rows with a cases_source of "published" should have counts.
      i Check how the data were changed after `impute_suppressed()`.

---

    Code
      no_county_cases(fake_state_atlas())
    Condition
      Error in `no_county_cases()`:
      ! `x` must be county data.
      i Use `get_atlas(geography = "county")`.

# for HIV estimates the report says the remainder includes non-EHE counties (#109)

    Code
      report <- no_county_cases(county)
    Message
      i CDC's county data for "Estimated HIV incidence" cover only Ending the HIV Epidemic (EHE) counties.
        Their remainder can include people in counties outside EHE coverage, so it isn't necessarily cases with no county.

# not_available = 'drop' warns when it removes a whole state's rows (#110)

    Code
      county <- impute_suppressed(data$county, not_available = "drop")
    Condition
      Warning:
      Dropping rows marked "Data not available" removed every row of 2 state groups (indicator, year and stratum).
      i State: "Alaska".
      ! Their state totals are no longer counted by `no_county_cases()` or spread by `extrapolate_from_county()`.
      i Use `not_available = "keep"` (the default) to keep them.
    Message
      i Dropped 2 rows marked "Data not available" with no count.
        Their cases are still in the state totals, and `no_county_cases()` counts them in the remainder for any state group that still has rows.

# a state with more than 25% of its cases in no county row gets a warning (#164)

    Code
      report <- no_county_cases(data$county)
    Condition
      Warning:
      1 state has more than 25% of its published state total in no county row of `x`.
      * HIV diagnoses 2022: Alabama (26%)
      i Percentages are of the state totals CDC published, over the strata that have one.
      ! Adding up this state's county counts, or estimating smaller areas from them, leaves those cases out, so the results are too low by about that share.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.

# several states with many cases in no county row share one warning (#164)

    Code
      report <- no_county_cases(data$county)
    Condition
      Warning:
      2 states have more than 25% of their published state total in no county row of `x`.
      * HIV diagnoses 2022: Alaska (87%) and Rhode Island (73%)
      i Percentages are of the state totals CDC published, over the strata that have one.
      ! Adding up these states' county counts, or estimating smaller areas from them, leaves those cases out, so the results are too low by about that share.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.

# only the states with the largest shares are named, then how many more (#164)

    Code
      report <- no_county_cases(data$county)
    Condition
      Warning:
      7 states have more than 25% of their published state total in no county row of `x`.
      * HIV diagnoses 2022: Alabama (90%), Alaska (80%), Arizona (70%), Arkansas (60%), California (50%), and 2 more
      i Percentages are of the state totals CDC published, over the strata that have one.
      ! Adding up these states' county counts, or estimating smaller areas from them, leaves those cases out, so the results are too low by about that share.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.

# each year and indicator is assessed on its own (#164)

    Code
      report <- no_county_cases(data$county)
    Condition
      Warning:
      1 state has more than 25% of its published state total in no county row of `x`.
      * HIV diagnoses 2022: Alabama (50%)
      i Percentages are of the state totals CDC published, over the strata that have one.
      ! Adding up this state's county counts, or estimating smaller areas from them, leaves those cases out, so the results are too low by about that share.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.

