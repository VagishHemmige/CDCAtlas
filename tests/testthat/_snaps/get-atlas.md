# get_atlas(extrapolate_to_tract) points to the new functions

    Code
      get_atlas("hiv", "county", 2022, extrapolate_to_tract = TRUE)
    Condition
      Error:
      ! The `extrapolate_to_tract` argument of `get_atlas()` was deprecated in CDCAtlas 0.0.0.9001 and is now defunct.
      i Please use `extrapolate_from_county()` instead.
      i Run `impute_suppressed()` on county data first.

---

    Code
      get_atlas("hiv", "county", 2022, extrapolate_to_tract = FALSE)
    Condition
      Error:
      ! The `extrapolate_to_tract` argument of `get_atlas()` was deprecated in CDCAtlas 0.0.0.9001 and is now defunct.
      i Please use `extrapolate_from_county()` instead.
      i Run `impute_suppressed()` on county data first.

