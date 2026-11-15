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

# spreading stops when `x` lacks some of a state's counties (#54)

    Code
      run_extrapolation(county = one_county, county_totals = full, no_county = "spread")
    Condition
      Error in `extrapolate_from_county()`:
      ! `no_county = "spread"` needs every county CDC reports in each state of `x`.
      x `x` is missing 1 county in state "01", for example "01003".
      i Their cases would be spread to the counties in `x` as if they had no county.
      i Run `extrapolate_from_county()` on whole states and filter the result, or use `no_county = "report"`.

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

# a missing age band leaves its county out, with a warning

    Code
      tracts <- run_extrapolation(NULL, acs = acs)
    Condition
      Warning:
      2 county stratum rows in 1 county left out: the ACS population is missing for some of its areas.
      i For example "County 01001 (01001)".
      i See `attr(x, "not_allocated")`.

# a duplicated ACS tract stops rather than skewing shares (#111)

    Code
      run_extrapolation(NULL, acs = acs)
    Condition
      Error:
      ! The Census returned some ACS variables more than once for the same area.
      i Area: "01001000100".
      i This shouldn't happen; please report it at <https://github.com/VagishHemmige/CDCAtlas/issues>.

# missing_population = 'error' stops instead

    Code
      run_extrapolation(NULL, acs = acs, missing_population = "error")
    Condition
      Error in `extrapolate_from_county()`:
      ! The ACS population is missing for some areas of 1 county.
      x For example "County 01001 (01001)".
      i Use `missing_population = "leave out"` to leave those county strata out instead.

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
      ! `x` has 1 row with no count.
      i Rows with a cases_source of "published" should have counts.
      i Check how the data were changed after `impute_suppressed()`.

---

    Code
      extrapolate_from_county(two_way)
    Condition
      Error in `extrapolate_from_county()`:
      ! `x` can have at most one stratification variable.
      x It is stratified by "race" and "sex".

---

    Code
      extrapolate_from_county(congenital)
    Condition
      Error in `extrapolate_from_county()`:
      ! Census populations don't fit congenital syphilis.
      x Its rates are per 100,000 live births, not per 100,000 people.

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

# suppressed rows impute_suppressed() couldn't estimate are left out

    Code
      tracts <- extrapolate_from_county(county)
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 21
      * HIV prevalence 2022: 121
        See `attr(x, "no_county_cases")` for each state.
      i 2 suppressed rows that `impute_suppressed()` couldn't estimate were left out of the tract estimates and counted in the remainder with no county.
        County: "County 01003".

# unestimated = 'error' refuses rows impute_suppressed() couldn't estimate

    Code
      extrapolate_from_county(county, unestimated = "error")
    Condition
      Error in `extrapolate_from_county()`:
      ! `x` has 2 suppressed rows with no count.
      i These are suppressed rows `impute_suppressed()` could not estimate (no state rate or county population).
      i Remove them, use `impute_suppressed(unestimated = "zero")`, or use `unestimated = "leave out"`.

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

# unknown_age is checked

    Code
      extrapolate_from_county(county, unknown_age = "spread")
    Condition
      Error in `extrapolate_from_county()`:
      ! `unknown_age` must be one of "report" or "redistribute", not "spread".

