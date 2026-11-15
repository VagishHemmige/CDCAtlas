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

# geographies CDC doesn't report for a disease are refused

    Code
      get_atlas("hepatitis", "county", 2022)
    Condition
      Error in `get_atlas()`:
      ! CDC doesn't report "hepatitis" for `geography = "county"`.
      i Available for "hepatitis": "national" or "state".

---

    Code
      get_atlas("congenital syphilis", "county", 2022)
    Condition
      Error in `get_atlas()`:
      ! CDC doesn't report "congenital syphilis" for `geography = "county"`.
      i Available for "congenital syphilis": "national" or "state".

---

    Code
      get_atlas("chlamydia", "msa", 2022)
    Condition
      Error in `get_atlas()`:
      ! CDC doesn't report "chlamydia" for `geography = "msa"`.
      i Available for "chlamydia": "national", "state", or "county".

