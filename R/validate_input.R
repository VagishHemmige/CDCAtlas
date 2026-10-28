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

  if (identical(geography, "region")) {
    cli::cli_abort(
      c(
        "{.code geography = \"region\"} is not supported yet.",
        "i" = "Use {.val national} or {.val state} instead."
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

  if (any(year < 2000 | year > 2023)) {
    cli::cli_abort(
      c(
        "{.arg year} must contain years from 2000 through 2023.",
        "x" = "You supplied {.val {year}}."
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
  }

  # extrapolate_to_tract --------------------------------------------------

  if (!rlang::is_bool(extrapolate_to_tract)) {
    cli::cli_abort(
      "{.arg extrapolate_to_tract} must be TRUE or FALSE.",
      call = call
    )
  }

  if (isTRUE(extrapolate_to_tract)) {
    if (geography != "county") {
      cli::cli_abort(
        c(
          "{.arg extrapolate_to_tract} can only be used with county-level AtlasPlus data.",
          "x" = "You supplied {.code geography = \"{geography}\"}.",
          "i" = "Use {.code geography = \"county\"}."
        ),
        call = call
      )
    }

    if (length(stratify_by) > 1) {
      cli::cli_abort(
        c(
          "{.arg extrapolate_to_tract} supports at most one {.arg stratify_by} variable.",
          "x" = "You supplied {.val {stratify_by}}."
        ),
        call = call
      )
    }

    if ("age" %in% stratify_by && disease != "hiv") {
      cli::cli_abort(
        c(
          "Age-stratified tract extrapolation currently supports only HIV data.",
          "x" = "You supplied {.code disease = \"{disease}\"}.",
          "i" = "CDC age groups for other diseases don't yet match the Census age groups used for allocation."
        ),
        call = call
      )
    }

    rlang::check_installed(
      "tidycensus (>= 1.8.0)",
      reason = "to use `extrapolate_to_tract = TRUE`.",
      call = call
    )
  }

  invisible(TRUE)
}
