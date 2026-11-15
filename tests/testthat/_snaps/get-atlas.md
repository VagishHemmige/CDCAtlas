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

# years outside the code table are refused, naming the range

    Code
      get_atlas("hiv", "state", 2026)
    Condition
      Error in `get_atlas()`:
      ! `year` must contain years from 2000 through 2025.
      x You supplied 2026.
      i 2025 is the latest year in the package's copy of CDC's code table. A year CDC hasn't released for a disease returns no data.

---

    Code
      get_atlas("hiv", "state", 1999)
    Condition
      Error in `get_atlas()`:
      ! `year` must contain years from 2000 through 2025.
      x You supplied 1999.
      i 2025 is the latest year in the package's copy of CDC's code table. A year CDC hasn't released for a disease returns no data.

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

# a response missing a requested indicator warns, naming it

    Code
      out <- get_atlas("hiv", "county", 2024:2025)
    Condition
      Warning:
      CDC returned no rows for 1 requested indicator-year:
      * HIV prevalence 2025
      i CDC may not have released them yet, or may not offer them for this geography and stratification.

