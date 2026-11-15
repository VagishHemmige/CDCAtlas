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

# an old positional extrapolate_to_tract still stops before any request

    Code
      get_atlas("hiv", "county", 2022, NULL, TRUE)
    Condition
      Error:
      ! The `extrapolate_to_tract` argument of `get_atlas()` was deprecated in CDCAtlas 0.0.0.9001 and is now defunct.
      i Please use `extrapolate_from_county()` instead.
      i Run `impute_suppressed()` on county data first.

---

    Code
      get_atlas("hiv", "county", 2022, NULL, FALSE)
    Condition
      Error:
      ! The `extrapolate_to_tract` argument of `get_atlas()` was deprecated in CDCAtlas 0.0.0.9001 and is now defunct.
      i Please use `extrapolate_from_county()` instead.
      i Run `impute_suppressed()` on county data first.

# congenital syphilis can't be stratified

    Code
      get_atlas("congenital syphilis", "state", 2022, stratify_by = "age")
    Condition
      Error in `get_atlas()`:
      ! CDC doesn't report congenital syphilis by race, sex or age.
      i Use `stratify_by = NULL`.

---

    Code
      get_atlas("congenital syphilis", "state", 2022, stratify_by = "sex")
    Condition
      Error in `get_atlas()`:
      ! CDC doesn't report congenital syphilis by race, sex or age.
      i Use `stratify_by = NULL`.

