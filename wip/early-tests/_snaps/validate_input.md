# disease must be a single known value

    Code
      .validate_get_atlas_inputs("Gonorrhea", "state", 2022)
    Condition
      Error:
      ! `disease` must be one of:
      x "chlamydia", "gonorrhea", "adult syphilis", "congenital syphilis", "tuberculosis", "hiv", "estimate", and "hepatitis"
      i You supplied "Gonorrhea".

---

    Code
      .validate_get_atlas_inputs(c("hiv", "chlamydia"), "state", 2022)
    Condition
      Error:
      ! `disease` must be a single character string.

---

    Code
      .validate_get_atlas_inputs(NA_character_, "state", 2022)
    Condition
      Error:
      ! `disease` must be a single character string.

# geography must be a single known value

    Code
      .validate_get_atlas_inputs("hiv", "Region", 2022)
    Condition
      Error:
      ! `geography` must be one of:
      x "national", "region", "state", "county", and "msa"
      i You supplied "Region".

---

    Code
      .validate_get_atlas_inputs("hiv", c("state", "county"), 2022)
    Condition
      Error:
      ! `geography` must be a single character string.

# year must be whole, finite years from 2000 to 2023

    Code
      .validate_get_atlas_inputs("hiv", "state", TRUE)
    Condition
      Error:
      ! `year` must be a numeric or integer vector.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", "2022")
    Condition
      Error:
      ! `year` must be a numeric or integer vector.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", numeric(0))
    Condition
      Error:
      ! `year` must contain at least one year.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", c(2022, NA))
    Condition
      Error:
      ! `year` cannot contain missing values.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022.5)
    Condition
      Error:
      ! `year` must contain whole years, such as 2022 or 2018:2022.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 1999)
    Condition
      Error:
      ! `year` must contain years from 2000 through 2023.
      i You supplied: 1999.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2025)
    Condition
      Error:
      ! `year` must contain years from 2000 through 2023.
      i You supplied: 2025.

# Current behaviour: a non-finite year fails with an internal error

    Code
      .validate_get_atlas_inputs("hiv", "state", c(2022, Inf))
    Condition
      Error in `if (any(year%%1 != 0)) ...`:
      ! missing value where TRUE/FALSE needed

# stratify_by must be NULL or up to two known variables

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = 1)
    Condition
      Error:
      ! `stratify_by` must be NULL or a character vector.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = character(0))
    Condition
      Error:
      ! `stratify_by` must be NULL or contain at least one stratification variable.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = NA_character_)
    Condition
      Error:
      ! `stratify_by` cannot contain missing values.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = c("age", "race",
        "sex"))
    Condition
      Error:
      ! `stratify_by` can currently contain at most two variables.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = "gender")
    Condition
      Error:
      ! `stratify_by` must contain only valid stratification variables.
      x Invalid value(s): "gender".
      i Valid values are "age", "race", and "sex".

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = c("sex", "sex"))
    Condition
      Error:
      ! `stratify_by` cannot contain duplicate values.

# extrapolate_to_tract must be TRUE or FALSE and needs county data

    Code
      .validate_get_atlas_inputs("hiv", "county", 2022, extrapolate_to_tract = "yes")
    Condition
      Error:
      ! `extrapolate_to_tract` must be TRUE or FALSE.

---

    Code
      .validate_get_atlas_inputs("hiv", "county", 2022, extrapolate_to_tract = NA)
    Condition
      Error:
      ! `extrapolate_to_tract` must be TRUE or FALSE.

---

    Code
      .validate_get_atlas_inputs("hiv", "state", 2022, extrapolate_to_tract = TRUE)
    Condition
      Error:
      ! `extrapolate_to_tract` can only be used with county-level AtlasPlus data.
      x You supplied `geography = state`.
      i Use `geography = "county"`.

# errors name get_atlas() as the caller

    Code
      get_atlas("flu", "state", 2022)
    Condition
      Error in `get_atlas()`:
      ! `disease` must be one of:
      x "chlamydia", "gonorrhea", "adult syphilis", "congenital syphilis", "tuberculosis", "hiv", "estimate", and "hepatitis"
      i You supplied "flu".

