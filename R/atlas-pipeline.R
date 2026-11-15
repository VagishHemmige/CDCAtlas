# Helpers shared by impute_suppressed(), no_county_cases() and
# extrapolate_from_county(). Each step checks the columns it is given rather than a
# class, so data can be saved, filtered or combined between steps (#29).

# Columns that identify one AtlasPlus cell apart from its geography.
.atlas_keys <- c("indicator", "year", "race_ethnicity", "sex", "age")

# Labels CDC uses for "not stratified" in each stratification column.
.atlas_totals <- list(
  race = "All races/ethnicities",
  sex = "Both sexes",
  age = c("Ages 13 years and older", "All age groups")
)

.atlas_strata_columns <- c(race = "race_ethnicity", sex = "sex", age = "age")


# Diseases as get_atlas() names them, with the indicators each returns. Built
# from the indicator codes in .build_payload() so the names match CDC's labels.
.disease_indicators <- function() {
  ids <- list(
    "chlamydia" = 206,
    "gonorrhea" = 207,
    "adult syphilis" = c(208, 213, 290),
    "congenital syphilis" = 214,
    "tuberculosis" = 209,
    "hiv" = c(203, 205),
    "estimate" = 285:287,
    "hepatitis" = 210:212
  )
  data.frame(
    disease = rep(names(ids), lengths(ids)),
    indicator = varvals$name[match(unlist(ids), varvals$id)]
  )
}


# Checks --------------------------------------------------------------------

.check_atlas_columns <- function(x, needed, arg, call) {
  if (!is.data.frame(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a data frame from {.fn get_atlas}.",
        "x" = "You supplied a {.cls {class(x)}}."
      ),
      call = call
    )
  }
  missing <- setdiff(needed, names(x))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} is missing column{?s} {.field {missing}}.",
        "i" = "Use data returned by {.fn get_atlas}."
      ),
      call = call
    )
  }
  invisible(x)
}

# County data: the get_atlas() columns, with 5-digit county FIPS codes.
.check_county_data <- function(x, arg, call) {
  .check_atlas_columns(
    x,
    c(.atlas_keys, "fips", "geography", "data_status", "cases", "population"),
    arg = arg,
    call = call
  )
  if (nrow(x) == 0) {
    cli::cli_abort("{.arg {arg}} has no rows.", call = call)
  }
  if (is.numeric(x$fips)) {
    cli::cli_abort(
      c(
        "{.field fips} in {.arg {arg}} must be character, such as {.val 01001}.",
        "i" = "If you read the data from a file, read {.field fips} as
               character so leading zeros are kept."
      ),
      call = call
    )
  }
  fips <- as.character(x$fips)
  if (anyNA(fips) || any(nchar(fips) != 5)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be county data.",
        "i" = "Use {.code get_atlas(geography = \"county\")}."
      ),
      call = call
    )
  }
  duplicated_rows <- duplicated(.row_key(x, c("fips", .atlas_keys)))
  if (any(duplicated_rows)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has {sum(duplicated_rows)} duplicated county row{?s}.",
        "i" = "Each county, indicator, year and stratum must appear once.
               Check how the data were combined."
      ),
      call = call
    )
  }
  .atlas_diseases(x, arg = arg, call = call)
  .check_one_stratification_level(x, .atlas_stratify_by(x), arg, call)
  invisible(x)
}

# Stratified data must not also contain that variable's total rows (as when
# a stratified and an unstratified download are combined).
.check_one_stratification_level <- function(x, stratify_by, arg, call) {
  for (s in stratify_by) {
    values <- as.character(x[[.atlas_strata_columns[[s]]]])
    if (any(values %in% .atlas_totals[[s]])) {
      cli::cli_abort(
        c(
          "{.arg {arg}} mixes {s} totals with {s} strata.",
          "i" = "Keep either the {.val {intersect(values, .atlas_totals[[s]])}}
                 rows or the stratified rows."
        ),
        call = call
      )
    }
  }
  invisible(x)
}

# Imputed data: impute_suppressed() has added `cases_source`, and every row
# has a count except "not available" rows and, with `unestimated = "leave out"`,
# rows impute_suppressed() couldn't estimate.
.check_imputed <- function(x, arg, call, unestimated = "error") {
  if (!"cases_source" %in% names(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has not been through {.fn impute_suppressed}.",
        "i" = "Run {.code impute_suppressed({arg})} first."
      ),
      call = call
    )
  }
  no_count <- is.na(x$cases) &
    !x$cases_source %in% c("not available", "not estimated")
  if (any(no_count)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has {sum(no_count)} row{?s} with no count.",
        "i" = "Rows with a {.field cases_source} of {.val
               {unique(x$cases_source[no_count])}} should have counts.",
        "i" = "Check how the data were changed after {.fn impute_suppressed}."
      ),
      call = call
    )
  }
  not_estimated <- is.na(x$cases) & x$cases_source %in% "not estimated"
  if (unestimated == "error" && any(not_estimated)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} has {sum(not_estimated)} suppressed row{?s} with no
         count.",
        "i" = "These are suppressed rows {.fn impute_suppressed} could not
               estimate (no state rate or county population).",
        "i" = "Remove them, use {.code impute_suppressed(unestimated =
               \"zero\")}, or use {.code unestimated = \"leave out\"}."
      ),
      call = call
    )
  }
  invisible(x)
}


# What the data are ------------------------------------------------------------

# Stratification variables present, as get_atlas() names them ("race", "sex",
# "age"): a column is stratified if any value is not CDC's total label.
.atlas_stratify_by <- function(x) {
  stratified <- vapply(
    names(.atlas_strata_columns),
    function(s) {
      values <- as.character(x[[.atlas_strata_columns[[s]]]])
      any(!values %in% .atlas_totals[[s]])
    },
    logical(1)
  )
  names(.atlas_strata_columns)[stratified]
}

# The get_atlas() disease name for each indicator in `x`.
.atlas_diseases <- function(x, arg = "x", call = rlang::caller_env()) {
  lookup <- .disease_indicators()
  indicators <- unique(as.character(x$indicator))
  unknown <- setdiff(indicators, lookup$indicator)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "{.arg {arg}} has indicator{?s} CDCAtlas doesn't know: {.val {unknown}}.",
      call = call
    )
  }
  unique(lookup$disease[lookup$indicator %in% indicators])
}

# Ages the Census populations cover, for each row of `df`: 13+ for HIV
# indicators, all ages otherwise. Decided by each row's indicator, so
# combining diseases doesn't change one disease's ages (#53). `ages` other
# than "auto" sets every row, except for age strata, whose groups set the
# ages: HIV groups start at 13, other diseases' groups cover all ages (#10).
.row_ages <- function(df, stratify_by, ages = "auto") {
  if (ages != "auto" && !identical(stratify_by, "age")) {
    return(rep(ages, nrow(df)))
  }
  lookup <- .disease_indicators()
  disease <- lookup$disease[match(as.character(df$indicator), lookup$indicator)]
  ifelse(disease %in% c("hiv", "estimate"), "13+", "all")
}

# The strata that Census populations can be matched to: at most one
# stratification variable, and no congenital syphilis, whose rates are per
# 100,000 live births rather than per 100,000 people (#10). Returns the
# stratification variable, or NULL. Shared by extrapolate_from_county() and
# impute_suppressed(population = "acs") so they accept the same input (#55).
.check_census_strata <- function(x, call, hint = NULL) {
  if ("congenital syphilis" %in% .atlas_diseases(x)) {
    cli::cli_abort(
      c(
        "Census populations don't fit congenital syphilis.",
        "x" = "Its rates are per 100,000 live births, not per 100,000 people.",
        hint
      ),
      call = call
    )
  }
  stratify_by <- .atlas_stratify_by(x)
  if (length(stratify_by) > 1) {
    cli::cli_abort(
      c(
        "{.arg x} can have at most one stratification variable.",
        "x" = "It is stratified by {.val {stratify_by}}.",
        hint
      ),
      call = call
    )
  }
  if (length(stratify_by) == 0) {
    return(NULL)
  }
  stratify_by
}

# Each row's value of the stratification variable (NA when unstratified),
# matching the `stratum` column of the Census populations.
.row_stratum <- function(df, stratify_by) {
  if (is.null(stratify_by)) {
    return(rep(NA_character_, nrow(df)))
  }
  as.character(df[[.atlas_strata_columns[[stratify_by]]]])
}

# One character key per row, for matching rows across tables without factor
# level problems.
.row_key <- function(df, cols) {
  do.call(paste, c(lapply(df[cols], as.character), sep = "\r"))
}


# County and state totals --------------------------------------------------------

# CDC's full county table for the indicators, years and stratification in
# `x`, for the states in `x`. Uses the request cache, so data downloaded
# earlier with get_atlas() costs nothing.
.fetch_county_totals <- function(x) {
  stratify_by <- .atlas_stratify_by(x)
  if (length(stratify_by) == 0) {
    stratify_by <- NULL
  }
  years <- as.numeric(as.character(x$year))
  states <- unique(substr(as.character(x$fips), 1, 2))
  payloads <- .build_payloads(
    .atlas_diseases(x), "county", years, stratify_by, states = states
  )
  .request_atlas_all(unname(payloads))
}

# The state rows matching county data `x`: same diseases, years and
# stratification.
.fetch_state_totals <- function(x) {
  stratify_by <- .atlas_stratify_by(x)
  if (length(stratify_by) == 0) {
    stratify_by <- NULL
  }
  years <- as.numeric(as.character(x$year))
  payloads <- .build_payloads(.atlas_diseases(x), "state", years, stratify_by)
  state <- .request_atlas_all(payloads)
  state$state_fips <- as.character(state$fips)
  state
}
