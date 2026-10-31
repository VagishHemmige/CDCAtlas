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

