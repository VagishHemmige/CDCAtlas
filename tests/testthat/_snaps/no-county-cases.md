# no_county_cases() refuses imputed rows left without a count

    Code
      no_county_cases(not_estimated)
    Condition
      Error in `no_county_cases()`:
      ! `x` has 2 suppressed rows with no count.
      i These are suppressed rows `impute_suppressed()` could not estimate (no state rate or county population).
      i Remove them, or use `impute_suppressed(unestimated = "zero")`.

---

    Code
      no_county_cases(fake_state_atlas())
    Condition
      Error in `no_county_cases()`:
      ! `x` must be county data.
      i Use `get_atlas(geography = "county")`.

