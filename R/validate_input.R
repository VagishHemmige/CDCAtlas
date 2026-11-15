.validate_get_atlas_inputs <- function(
  disease,
  geography,
  year,
  stratify_by = NULL,
  call = rlang::caller_call()
) {
  valid_diseases <- c(
    "chlamydia",
    "gonorrhea",
    "adult syphilis",
    "congenital syphilis",
    "tuberculosis",
    "hiv",
    "estimate",
    "hepatitis"
  )

  valid_geographies <- c(
    "national",
    "region",
    "state",
    "county",
    "msa"
  )

  valid_strata <- c("age", "race", "sex")

  one_of <- function(x) {
    cli::cli_vec(x, style = list("vec-sep2" = " or ", "vec-last" = ", or "))
  }

  # disease ---------------------------------------------------------------

  if (!rlang::is_string(disease)) {
    cli::cli_abort(
      c(
        "{.arg disease} must be a single character string.",
        "x" = "You supplied a {.cls {class(disease)}} of length {length(disease)}."
      ),
      call = call
    )
  }

  if (!disease %in% valid_diseases) {
    cli::cli_abort(
      c(
        "{.arg disease} must be one of {.val {one_of(valid_diseases)}}.",
        "x" = "You supplied {.val {disease}}."
      ),
      call = call
    )
  }

  # geography -------------------------------------------------------------

  if (!rlang::is_string(geography)) {
    cli::cli_abort(
      c(
        "{.arg geography} must be a single character string.",
        "x" = "You supplied a {.cls {class(geography)}} of length {length(geography)}."
      ),
      call = call
    )
  }

  if (!geography %in% valid_geographies) {
    cli::cli_abort(
      c(
        "{.arg geography} must be one of {.val {one_of(valid_geographies)}}.",
        "x" = "You supplied {.val {geography}}."
      ),
      call = call
    )
  }

  # CDC's data endpoint returns regions only for the HIV estimates. For reported
  # indicators AtlasPlus requests the member states and adds them up in the
  # browser, and a region request returns no rows.
  if (identical(geography, "region") && !identical(disease, "estimate")) {
    cli::cli_abort(
      c(
        "{.code geography = \"region\"} is only available for {.code disease = \"estimate\"}.",
        "i" = "CDC doesn't return regions for reported data such as {.val {disease}}.",
        "i" = "State data are available with {.code geography = \"state\"}; adding states up needs care with suppressed counts, and rates must be recomputed from cases and population."
      ),
      call = call
    )
  }

  # AtlasPlus has no county data for hepatitis or congenital syphilis, and
  # metropolitan areas only for HIV. From the locations CDC's set-up data
  # lists for each indicator (`combo`), checked by a live test.
  available <- .atlas_geographies(disease)
  if (!geography %in% available) {
    cli::cli_abort(
      c(
        "CDC doesn't report {.val {disease}} for {.code geography = \"{geography}\"}.",
        "i" = "Available for {.val {disease}}: {.val {one_of(available)}}."
      ),
      call = call
    )
  }

  # year ------------------------------------------------------------------

  if (!is.numeric(year)) {
    cli::cli_abort(
      c(
        "{.arg year} must be a numeric vector, such as 2022 or 2018:2022.",
        "x" = "You supplied a {.cls {class(year)}}."
      ),
      call = call
    )
  }

  if (length(year) == 0) {
    cli::cli_abort(
      "{.arg year} must contain at least one year.",
      call = call
    )
  }

  if (any(is.na(year))) {
    cli::cli_abort(
      "{.arg year} cannot contain missing values.",
      call = call
    )
  }

  if (any(!is.finite(year))) {
    cli::cli_abort(
      "{.arg year} must contain finite years, such as 2022 or 2018:2022.",
      call = call
    )
  }

  if (any(year %% 1 != 0)) {
    cli::cli_abort(
      "{.arg year} must contain whole years, such as 2022 or 2018:2022.",
      call = call
    )
  }

  latest <- .latest_atlas_year()
  if (any(year < 2000 | year > latest)) {
    cli::cli_abort(
      c(
        "{.arg year} must contain years from 2000 through {latest}.",
        "x" = "You supplied {.val {year}}.",
        "i" = "{latest} is the latest year in the package's copy of CDC's
               code table. A year CDC hasn't released for a disease returns
               no data."
      ),
      call = call
    )
  }

  # stratify_by -----------------------------------------------------------

  if (!is.null(stratify_by)) {
    if (!is.character(stratify_by)) {
      cli::cli_abort(
        c(
          "{.arg stratify_by} must be NULL or a character vector.",
          "x" = "You supplied a {.cls {class(stratify_by)}}."
        ),
        call = call
      )
    }

    if (length(stratify_by) == 0) {
      cli::cli_abort(
        "{.arg stratify_by} must be NULL or contain at least one stratification variable.",
        call = call
      )
    }

    if (any(is.na(stratify_by))) {
      cli::cli_abort(
        "{.arg stratify_by} cannot contain missing values.",
        call = call
      )
    }

    if (length(stratify_by) > 2) {
      cli::cli_abort(
        "{.arg stratify_by} can currently contain at most two variables.",
        call = call
      )
    }

    invalid_strata <- setdiff(stratify_by, valid_strata)

    if (length(invalid_strata) > 0) {
      cli::cli_abort(
        c(
          "{.arg stratify_by} must contain only {.val {valid_strata}}.",
          "x" = "Invalid value{?s}: {.val {invalid_strata}}."
        ),
        call = call
      )
    }

    if (anyDuplicated(stratify_by)) {
      cli::cli_abort(
        "{.arg stratify_by} cannot contain duplicate values.",
        call = call
      )
    }

    # AtlasPlus offers congenital syphilis only as a total: its set-up data
    # list no race, sex or age groups for it (#10).
    if (identical(disease, "congenital syphilis")) {
      cli::cli_abort(
        c(
          "CDC doesn't report congenital syphilis by race, sex or age.",
          "i" = "Use {.code stratify_by = NULL}."
        ),
        call = call
      )
    }
  }

  invisible(TRUE)
}


# The geographies AtlasPlus reports for each get_atlas() disease. Regions are
# only for the HIV estimates (see the region check above).
.atlas_geographies <- function(disease) {
  switch(
    disease,
    "hiv" = c("national", "state", "county", "msa"),
    "estimate" = c("national", "region", "state", "county"),
    "hepatitis" = ,
    "congenital syphilis" = c("national", "state"),
    c("national", "state", "county")
  )
}

# The latest year in the package's copy of CDC's code table (`varvals`, type 2
# is years, plus a few other entries such as "Change over time"). CDC lists a year's code before every disease has data for it, so
# this is the latest year that can be requested, not the latest with data
# (#16). data-raw/DATASET.R refreshes the table.
.latest_atlas_year <- function() {
  names <- varvals$name[varvals$vtid == 2]
  max(as.numeric(names[grepl("^[0-9]{4}$", names)]))
}
