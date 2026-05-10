.validate_get_atlas_inputs <- function(
    disease,
    geography,
    year,
    stratify_by = NULL,
    extrapolate_to_tract = FALSE,
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

  # disease ---------------------------------------------------------------

  if (!rlang::is_string(disease)) {
    cli::cli_abort(
      "{.arg disease} must be a single character string.",
      call = call
    )
  }

  if (!disease %in% valid_diseases) {
    cli::cli_abort(
      c(
        "{.arg disease} must be one of:",
        "x" = "{.val {valid_diseases}}",
        "i" = "You supplied {.val {disease}}."
      ),
      call = call
    )
  }

  # geography -------------------------------------------------------------

  if (!rlang::is_string(geography)) {
    cli::cli_abort(
      "{.arg geography} must be a single character string.",
      call = call
    )
  }

  if (!geography %in% valid_geographies) {
    cli::cli_abort(
      c(
        "{.arg geography} must be one of:",
        "x" = "{.val {valid_geographies}}",
        "i" = "You supplied {.val {geography}}."
      ),
      call = call
    )
  }

  # year ------------------------------------------------------------------

  if (!is.numeric(year) && !is.integer(year)) {
    cli::cli_abort(
      "{.arg year} must be a numeric or integer vector.",
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

  if (any(year %% 1 != 0)) {
    cli::cli_abort(
      "{.arg year} must contain whole years, such as 2022 or 2018:2022.",
      call = call
    )
  }

  if (any(year < 2000 | year > 2023)) {
    cli::cli_abort(
      c(
        "{.arg year} must contain years from 2000 through 2023.",
        "i" = "You supplied: {.val {year}}."
      ),
      call = call
    )
  }

  # stratify_by -----------------------------------------------------------

  if (!is.null(stratify_by)) {
    if (!is.character(stratify_by)) {
      cli::cli_abort(
        "{.arg stratify_by} must be NULL or a character vector.",
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
          "{.arg stratify_by} must contain only valid stratification variables.",
          "x" = "Invalid value(s): {.val {invalid_strata}}.",
          "i" = "Valid values are {.val {valid_strata}}."
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
  }

  # extrapolate_to_tract --------------------------------------------------

  if (!rlang::is_bool(extrapolate_to_tract)) {
    cli::cli_abort(
      "{.arg extrapolate_to_tract} must be TRUE or FALSE.",
      call = call
    )
  }

  if (isTRUE(extrapolate_to_tract) && geography != "county") {
    cli::cli_abort(
      c(
        "{.arg extrapolate_to_tract} can only be used with county-level AtlasPlus data.",
        "x" = "You supplied {.code geography = {geography}}.",
        "i" = "Use {.code geography = \"county\"}."
      ),
      call = call
    )
  }

  invisible(TRUE)
}
