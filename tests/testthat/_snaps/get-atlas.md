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

# `disease` must be one known disease name

    Code
      get_atlas(c("hiv", "gonorrhea"), "state", 2022)
    Condition
      Error in `get_atlas()`:
      ! `disease` must be a single character string.
      x You supplied a <character> of length 2.

---

    Code
      get_atlas(1, "state", 2022)
    Condition
      Error in `get_atlas()`:
      ! `disease` must be a single character string.
      x You supplied a <numeric> of length 1.

---

    Code
      get_atlas("measles", "state", 2022)
    Condition
      Error in `get_atlas()`:
      ! `disease` must be one of "chlamydia", "gonorrhea", "adult syphilis", "congenital syphilis", "tuberculosis", "hiv", "estimate", or "hepatitis".
      x You supplied "measles".

# `geography` must be one known geography

    Code
      get_atlas("hiv", c("state", "county"), 2022)
    Condition
      Error in `get_atlas()`:
      ! `geography` must be a single character string.
      x You supplied a <character> of length 2.

---

    Code
      get_atlas("hiv", "zip", 2022)
    Condition
      Error in `get_atlas()`:
      ! `geography` must be one of "national", "region", "state", "county", or "msa".
      x You supplied "zip".

# `year` must be whole numbers

    Code
      get_atlas("hiv", "state", "2022")
    Condition
      Error in `get_atlas()`:
      ! `year` must be a numeric vector, such as 2022 or 2018:2022.
      x You supplied a <character>.

---

    Code
      get_atlas("hiv", "state", numeric(0))
    Condition
      Error in `get_atlas()`:
      ! `year` must contain at least one year.

---

    Code
      get_atlas("hiv", "state", c(2021, NA))
    Condition
      Error in `get_atlas()`:
      ! `year` cannot contain missing values.

---

    Code
      get_atlas("hiv", "state", Inf)
    Condition
      Error in `get_atlas()`:
      ! `year` must contain finite years, such as 2022 or 2018:2022.

---

    Code
      get_atlas("hiv", "state", 2021.5)
    Condition
      Error in `get_atlas()`:
      ! `year` must contain whole years, such as 2022 or 2018:2022.

# `stratify_by` must be one or two of age, race and sex

    Code
      get_atlas("hiv", "state", 2022, stratify_by = 1)
    Condition
      Error in `get_atlas()`:
      ! `stratify_by` must be NULL or a character vector.
      x You supplied a <numeric>.

---

    Code
      get_atlas("hiv", "state", 2022, stratify_by = character(0))
    Condition
      Error in `get_atlas()`:
      ! `stratify_by` must be NULL or contain at least one stratification variable.

---

    Code
      get_atlas("hiv", "state", 2022, stratify_by = c("sex", NA))
    Condition
      Error in `get_atlas()`:
      ! `stratify_by` cannot contain missing values.

---

    Code
      get_atlas("hiv", "state", 2022, stratify_by = c("age", "race", "sex"))
    Condition
      Error in `get_atlas()`:
      ! `stratify_by` can currently contain at most two variables.

---

    Code
      get_atlas("hiv", "state", 2022, stratify_by = "income")
    Condition
      Error in `get_atlas()`:
      ! `stratify_by` must contain only "age", "race", and "sex".
      x Invalid value: "income".

---

    Code
      get_atlas("hiv", "state", 2022, stratify_by = c("sex", "sex"))
    Condition
      Error in `get_atlas()`:
      ! `stratify_by` cannot contain duplicate values.

