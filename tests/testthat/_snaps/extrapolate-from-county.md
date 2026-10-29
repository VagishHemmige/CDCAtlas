# empty strata can be left NA, with a warning

    Code
      tracts <- extrapolate_from_county(county, empty_strata = "none")
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 0
      * HIV prevalence 2022: 0
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      2 county strata had cases but no ACS population, so their estimates are `NA`.
      i Use `empty_strata = "total"` or `"equal"` to share them anyway.

# remainders with no county to receive them are reported

    Code
      invisible(extrapolate_from_county(county, no_county = "spread"))
    Message
      i Cases in state totals with no county, to be spread across each state's counties:
      * HIV diagnoses 2022: 342
      * HIV prevalence 2022: 542
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      4 county rows were not allocated: no matching ACS tracts.
      i Counties: "County 02001" and "County 02003".
      i See `attr(x, "not_allocated")`.
    Message
      i Cases with no county that could not be spread (no county with tracts in that state and stratum):
      * HIV diagnoses 2022: 321
      * HIV prevalence 2022: 421

# the no-county total is announced

    Code
      invisible(extrapolate_from_county(county))
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 500
      * HIV prevalence 2022: 500
        See `attr(x, "no_county_cases")` for each state.

---

    Code
      invisible(extrapolate_from_county(county, no_county = "spread"))
    Message
      i Cases in state totals with no county, to be spread across each state's counties:
      * HIV diagnoses 2022: 500
      * HIV prevalence 2022: 500
        See `attr(x, "no_county_cases")` for each state.

# county counts with no matching tracts are left out with a warning

    Code
      tracts <- extrapolate_from_county(county)
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 0
      * HIV prevalence 2022: 0
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      2 county rows were not allocated: no matching ACS tracts.
      i Counties: "County 01003".
      i See `attr(x, "not_allocated")`.

# race 'Unknown' has no Census population and is reported

    Code
      tracts <- extrapolate_from_county(county)
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 0
      * HIV prevalence 2022: 0
        See `attr(x, "no_county_cases")` for each state.
      i Cases not allocated to tracts because Census has no population for "Unknown":
      * HIV diagnoses 2022: 10
      * HIV prevalence 2022: 10
        See `attr(x, "not_allocated")`.

# extrapolate_from_county() rejects data and arguments it can't use

    Code
      extrapolate_from_county(fake_county_atlas())
    Condition
      Error in `extrapolate_from_county()`:
      ! `x` has not been through `impute_suppressed()`.
      i Run `impute_suppressed(x)` first.

---

    Code
      extrapolate_from_county(with_na)
    Condition
      Error in `extrapolate_from_county()`:
      ! `x` has 1 suppressed row with no count.
      i These are suppressed rows `impute_suppressed()` could not estimate (no state rate or county population).
      i Remove them, or use `impute_suppressed(unestimated = "zero")`.

---

    Code
      extrapolate_from_county(two_way)
    Condition
      Error in `extrapolate_from_county()`:
      ! `x` can have at most one stratification variable.
      x It is stratified by "race" and "sex".

---

    Code
      extrapolate_from_county(chlamydia)
    Condition
      Error in `extrapolate_from_county()`:
      ! Age-stratified estimates support only HIV diagnoses and prevalence.
      x `x` has "chlamydia" data.
      i CDC's age groups for other diseases aren't matched to Census ages yet.

---

    Code
      extrapolate_from_county(several_years, acs_year = 2021)
    Condition
      Error in `extrapolate_from_county()`:
      ! `acs_year` can only be set when `x` has one year.
      x `x` has years 2021 and 2022.

---

    Code
      extrapolate_from_county(county, acs_year = "2021")
    Condition
      Error in `extrapolate_from_county()`:
      ! `acs_year` must be a single year.

---

    Code
      extrapolate_from_county(county, weights = 1)
    Condition
      Error in `extrapolate_from_county()`:
      ! `weights` is not supported yet (#1).

---

    Code
      extrapolate_from_county(county, no_county = "drop")
    Condition
      Error in `extrapolate_from_county()`:
      ! `no_county` must be one of "report" or "spread", not "drop".

# a missing Census key is reported before any request

    Code
      extrapolate_from_county(county)
    Condition
      Error in `extrapolate_from_county()`:
      ! Downloading Census populations needs a Census API key.
      x `CENSUS_API_KEY` is empty.
      i Get a key at <https://api.census.gov/data/key_signup.html> and set it with `tidycensus::census_api_key()`.

# ACS years before an area's first release are refused

    Code
      extrapolate_from_county(county, to = "block group")
    Condition
      Error in `extrapolate_from_county()`:
      ! ACS 5-year estimates for block groups start in 2013.
      x No ACS data for 2011.
      i Set `acs_year` to 2013 or later.

---

    Code
      extrapolate_from_county(county, acs_year = 2008)
    Condition
      Error in `extrapolate_from_county()`:
      ! ACS 5-year estimates for tracts start in 2009.
      x No ACS data for 2008.
      i Set `acs_year` to 2009 or later.

# unknown race is shared across known races by their counts

    Code
      tracts <- extrapolate_from_county(county, unknown_race = "redistribute")
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 0
      * HIV prevalence 2022: 0
        See `attr(x, "no_county_cases")` for each state.
      i Cases of unknown race shared across known race groups:
      * HIV diagnoses 2022: 10
      * HIV prevalence 2022: 10

