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
      2 states have more than 25% of their cases in no county row.
      * HIV diagnoses 2022: Alaska (97%) and Alabama (66%)
      * HIV prevalence 2022: Alaska (79%) and Alabama (52%)
      ! `no_county = "spread"` shares those cases across each state's counties by population. That places them by assumption: the county data don't say where they are.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.
      i See `attr(x, "no_county_cases")` for each state.
      Warning:
      4 county rows were not allocated: no matching ACS tracts.
      i Counties: "County 02001" and "County 02003".
      i See `attr(x, "not_allocated")`.
    Message
      i Cases with no county that could not be spread (no county with tracts in that state and stratum, or no tracts with population there):
      * HIV diagnoses 2022: 321
      * HIV prevalence 2022: 421

# spread cases that reach no area are reported as not spread (#114)

    Code
      .announce_spread(out, report, "tract")
    Message
      i Cases with no county that could not be spread (no county with tracts in that state and stratum, or no tracts with population there):
      * HIV diagnoses 2022: 10

# spreading stops when `x` lacks some of a state's counties (#54)

    Code
      run_extrapolation(county = one_county, county_totals = full, no_county = "spread")
    Condition
      Error in `extrapolate_from_county()`:
      ! `no_county = "spread"` needs every county CDC reports in each state of `x`.
      x `x` is missing 1 county in state "01", for example "01003".
      i Their cases would be spread to the counties in `x` as if they had no county.
      i Run `extrapolate_from_county()` on whole states and filter the result, or use `no_county = "report"`.

# spreading stops for HIV estimates, whose counties are EHE only (#109)

    Code
      extrapolate_from_county(county, no_county = "spread")
    Condition
      Error in `extrapolate_from_county()`:
      ! `no_county = "spread"` doesn't work for "Estimated HIV incidence".
      x CDC's county data for them cover only Ending the HIV Epidemic (EHE) counties, so the state remainder can include people in other counties and isn't necessarily cases with no county.
      i Use `no_county = "report"`.

# the no-county total is announced

    Code
      invisible(extrapolate_from_county(county))
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 500
      * HIV prevalence 2022: 500
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      1 state has more than 25% of its cases in no county row.
      * HIV diagnoses 2022: Alabama (94%)
      * HIV prevalence 2022: Alabama (68%)
      ! County and tract estimates for this state leave those cases out, so they are too low by about that share overall.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.
      i See `attr(x, "no_county_cases")` for each state.

---

    Code
      invisible(extrapolate_from_county(county, no_county = "spread"))
    Message
      i Cases in state totals with no county, to be spread across each state's counties:
      * HIV diagnoses 2022: 500
      * HIV prevalence 2022: 500
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      1 state has more than 25% of its cases in no county row.
      * HIV diagnoses 2022: Alabama (94%)
      * HIV prevalence 2022: Alabama (68%)
      ! `no_county = "spread"` shares those cases across this state's counties by population. That places them by assumption: the county data don't say where they are.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.
      i See `attr(x, "no_county_cases")` for each state.

# a missing age band leaves its county out, with a warning

    Code
      tracts <- run_extrapolation(NULL, acs = acs)
    Condition
      Warning:
      2 county stratum rows in 1 county left out: the ACS population is missing for some of its areas.
      i For example "County 01001 (01001)".
      i See `attr(x, "not_allocated")`.

# a duplicated ACS row can't stand in for a missing one (#111)

    Code
      run_extrapolation(NULL, acs = acs)
    Condition
      Error:
      ! The Census returned some ACS variables more than once for the same area.
      i Area: "01001000100".
      i This shouldn't happen; please report it at <https://github.com/VagishHemmige/CDCAtlas/issues>.

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
      ! `weights` is not supported yet.

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
    Condition
      Warning:
      1 state has more than 25% of its cases in no county row.
      * HIV diagnoses 2022: Alabama (66%)
      * HIV prevalence 2022: Alabama (52%)
      ! County and tract estimates for this state leave those cases out, so they are too low by about that share overall.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.
      i See `attr(x, "no_county_cases")` for each state.
    Message
      i 2 rows with no count that `impute_suppressed()` couldn't estimate were left out of the tract estimates and counted in the remainder with no county.
        County: "County 01003".

# unestimated = 'error' refuses rows impute_suppressed() couldn't estimate

    Code
      extrapolate_from_county(county, unestimated = "error")
    Condition
      Error in `extrapolate_from_county()`:
      ! `x` has 2 rows with no count.
      i These are rows `impute_suppressed()` could not estimate (no state rate or county population, or not marked "Data suppressed").
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

# a tract split between counties gives one warning (#152)

    Code
      invisible(run_2017())
    Condition
      Warning:
      1 tract is split between counties, so its geoid is in more than one row for the same year, indicator and stratum.
      i Each row holds the share of the tract assigned to that row's county_fips.
      i Before joining to tract boundaries, sum area_cases, area_population_acs and area_total_population by geoid within year, indicator and stratum.
      i The county columns repeat on every row of a county; don't sum them.

# the split-area warning counts areas, not rows (#152)

    Code
      invisible(run_2017(to = "block group"))
    Condition
      Warning:
      2 block groups are split between counties, so their geoid is in more than one row for the same year, indicator and stratum.
      i Each row holds the share of the block group assigned to that row's county_fips.
      i Before joining to block group boundaries, sum area_cases, area_population_acs and area_total_population by geoid within year, indicator and stratum.
      i The county columns repeat on every row of a county; don't sum them.

# Connecticut with ACS 2020 or 2021 says why it has no tracts (#112)

    Code
      invisible(run_extrapolation(county = county, acs_year = 2021))
    Condition
      Warning:
      2 county rows were not allocated: no matching ACS tracts.
      i Counties: "County 09110".
      i ACS 2021 doesn't have CDC's current codes for Connecticut's planning regions; use ACS 2010-2019 or 2022 and later.
      i See `attr(x, "not_allocated")`.

# unknown_age is checked

    Code
      extrapolate_from_county(county, unknown_age = "spread")
    Condition
      Error in `extrapolate_from_county()`:
      ! `unknown_age` must be one of "report" or "redistribute", not "spread".

# one extrapolation warns once about states with many cases in no county row (#164)

    Code
      tracts <- extrapolate_from_county(data$county)
    Message
      i Cases in state totals with no county are not allocated to tracts:
      * HIV diagnoses 2022: 26
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      1 state has more than 25% of its cases in no county row.
      * HIV diagnoses 2022: Alabama (26%)
      ! County and tract estimates for this state leave those cases out, so they are too low by about that share overall.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.
      i See `attr(x, "no_county_cases")` for each state.

# no_county = 'spread' still warns, and says the cases are placed by assumption (#164)

    Code
      tracts <- extrapolate_from_county(data$county, no_county = "spread", spread_by = "cases")
    Message
      i Cases in state totals with no county, to be spread across each state's counties:
      * HIV diagnoses 2022: 26
        See `attr(x, "no_county_cases")` for each state.
    Condition
      Warning:
      1 state has more than 25% of its cases in no county row.
      * HIV diagnoses 2022: Alabama (26%)
      ! `no_county = "spread"` shares those cases across this state's counties by cases. That places them by assumption: the county data don't say where they are.
      i This happens when a state's county counts are largely suppressed, or when `x` lacks some of its counties. CDCAtlas doesn't check the cause.
      i See `attr(x, "no_county_cases")` for each state.

