# rows impute_suppressed() couldn't estimate count towards the remainder

    Code
      report <- no_county_cases(not_estimated)
    Message
      i 2 suppressed rows that `impute_suppressed()` couldn't estimate were counted in the remainder with no county.
        County: "County 01003".

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

