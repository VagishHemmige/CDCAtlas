#' Cases in state totals that no county accounts for
#'
#' @description
#' A state's total in CDC AtlasPlus is usually larger than the sum of its
#' counties. `no_county_cases()` compares each state total with the county
#' counts supplied. Run it after [impute_suppressed()] for the remainder that
#' no county accounts for; on county data straight from [get_atlas()] it
#' reports only the gap left by published counts.
#'
#' For CDC's HIV estimates (`get_atlas("estimate")`), county data cover only
#' the Ending the HIV Epidemic (EHE) counties, so the remainder can include
#' people in counties outside EHE coverage and isn't necessarily cases with no county; a
#' message says so.
#'
#' The remainder is a difference, not a count of identified cases. One likely
#' explanation is cases whose county of residence is unknown or not released,
#' but CDC's documentation doesn't say so; it also includes any counties left
#' out of `x`. In 2022 the gap was far larger than suppressed counties could
#' hold at 1-4 cases each, so it can't be used to fill them in.
#' [extrapolate_from_county()] leaves the remainder out by default;
#' `no_county = "spread"` shares it across the state's counties instead. See
#' the "About CDC AtlasPlus data" article.
#'
#' @section State strata with one county row without a published count:
#' Where a state stratum has exactly one county row without a published count
#' (suppressed, "Data not available", or with no count for another reason),
#' the state total minus the published counts is that county's count plus
#' any cases with no county. CDC's documentation doesn't say that state
#' totals include cases with no county, so the figure could be that county's
#' count exactly, and `no_county_cases()` doesn't report it.
#'
#' Rows are counted both in `x` and in CDC's county table for `x`, so a
#' county filtered out of `x`, or dropped by
#' `impute_suppressed(not_available = "drop")`, still counts. CDC's table
#' comes from the cache when `x` was downloaded in this session; otherwise it
#' is downloaded. If CDC's table has no rows for a stratum, how many counties
#' lack a count can't be known, so that stratum's remainder isn't reported
#' either.
#'
#' For such a stratum `published_cases`, `unpublished_cases` and
#' `no_county_cases` are `NA` and `no_county_withheld` says why, and a
#' message says how many strata this applies to. `state_cases` (CDC's
#' published total) and `imputed_cases` (the modeled estimate) are still
#' given.
#'
#' @section States with many cases in no county row:
#' `no_county_cases()` warns, once per call, when a state has more than 25% of
#' its published state total in no county row of `x`: the remainder divided
#' by the state total, with both added up over the state's strata that are in
#' `x` and have a published state total, separately for each indicator and
#' year. The percentage is of the state totals CDC published for the strata
#' in `x`. It is not a share of every case in the state when `x` lacks a
#' stratum (data filtered to one sex, say) or CDC published no total for one.
#' The warning (class `"cdcatlas_state_unplaced_cases"`) names the states
#' with their percentages, the five largest for each indicator and year.
#' County counts for such a state, added up or shared across smaller areas,
#' leave those cases out and are too low by about that share for those
#' strata. This happens when a state's county counts are largely suppressed,
#' or when `x` lacks some of the state's counties; the package does not check
#' the cause. Nothing in the report changes.
#'
#' A state is assessed only where CDC published its total and `x` has been
#' through [impute_suppressed()]. Strata with no published state total, and
#' strata with no row in `x`, are left out of its share, and so are states
#' with no cases. CDC's HIV
#' estimates are not assessed, because their county data cover only EHE
#' counties. A stratum where the imputed counts exceed the state total
#' offsets one where they fall short. The 25% is fixed.
#'
#' @param x County data from `get_atlas(geography = "county")`, usually after
#'   [impute_suppressed()].
#'
#' @return A tibble with one row per state, indicator, year and stratum:
#'   \describe{
#'     \item{state_fips, state}{State FIPS code and name.}
#'     \item{indicator, year, race_ethnicity, sex, age}{As in `x`.}
#'     \item{state_cases}{The state total published by CDC.}
#'     \item{published_cases}{Sum of the published county counts in `x`.
#'       `NA` if the remainder is withheld (see `no_county_withheld`).}
#'     \item{unpublished_cases}{`state_cases - published_cases`: cases in
#'       suppressed counties together with the remainder no county accounts
#'       for. `NA` if withheld.}
#'     \item{imputed_cases}{Sum of the counts [impute_suppressed()] filled in;
#'       `NA` if `x` hasn't been imputed.}
#'     \item{no_county_cases}{`unpublished_cases - imputed_cases`. Can be
#'       negative if the imputed counts are too high, and is `NA` if `x` hasn't
#'       been imputed.}
#'     \item{no_county_withheld}{Why `published_cases`, `unpublished_cases`
#'       and `no_county_cases` are not reported: `"one county row without a
#'       published count"`, `"no CDC county rows to count unpublished
#'       counties"`, or `NA` otherwise. (Where CDC suppressed the state
#'       total, those columns are `NA` for that reason instead.) See the
#'       section below.}
#'   }
#'
#'   Every column computed from `state_cases` is `NA` if CDC suppressed the
#'   state total.
#'
#' @details
#' Downloads the state data matching `x` (same diseases, years and
#' stratification) from CDC AtlasPlus, and CDC's county table for `x` when it
#' isn't in the cache. The remainder also includes any county
#' without a count in `x`: "Data not available" rows, counties removed by
#' filtering `x`, and suppressed rows [impute_suppressed()] couldn't estimate
#' (marked `cases_source = "not estimated"`), which count as 0 imputed cases.
#' The report has a row only for state groups (state, indicator, year and
#' stratum) with at least one row in `x`: if
#' `impute_suppressed(not_available = "drop")` or filtering removed every row
#' of a group, its state total is not reported.
#'
#' @seealso [impute_suppressed()], [extrapolate_from_county()]; the article
#'   ["About CDC AtlasPlus data"](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html)
#'   on why state totals and county counts differ.
#'
#' @examples
#' \dontrun{
#' county <- get_atlas("hiv", "county", 2022)
#' county <- impute_suppressed(county)
#' no_county_cases(county)
#' }
#'
#' @export
no_county_cases <- function(x) {
  call <- rlang::current_env()
  .check_county_data(x, arg = "x", call = call)
  if ("cases_source" %in% names(x)) {
    .check_imputed(x, arg = "x", call = call, unestimated = "leave out")
    .announce_not_estimated(x, "counted in the remainder with no county")
  }
  .inform_ehe_remainder(x)
  report <- .no_county_cases(
    x,
    .fetch_state_totals(x),
    .fetch_county_totals(x)
  )
  .announce_withheld(report)
  .warn_state_unplaced_cases(
    report,
    c(
      "!" = "Adding up {?this state's/these states'} county counts, or
             estimating smaller areas from them, leaves those cases out, so
             the results are too low by about that share for those strata."
    )
  )
  report
}

# The report itself, given the state rows and CDC's county table for `x`
# (`cdc`; from the cache when `x` was downloaded in this session). Separate so
# extrapolate_from_county() can reuse the rows it has already fetched.
.no_county_cases <- function(x, state, cdc) {
  keys <- c("state_fips", .atlas_keys)
  source <- if ("cases_source" %in% names(x)) {
    x$cases_source
  } else {
    ifelse(is.na(x$cases), "not imputed", "published")
  }
  county <- data.frame(
    state_fips = substr(as.character(x$fips), 1, 2),
    lapply(as.data.frame(x[.atlas_keys]), as.character),
    published = ifelse(source == "published", x$cases, 0),
    unpublished = source != "published",
    imputed = dplyr::case_when(
      source %in% c("published", "not available", "not estimated") ~ 0,
      source == "not imputed" ~ NA_real_,
      .default = x$cases
    )
  )
  county$year <- as.numeric(county$year)

  report <- county |>
    dplyr::summarize(
      published_cases = sum(.data$published),
      unpublished_rows = sum(.data$unpublished),
      imputed_cases = sum(.data$imputed),
      .by = dplyr::all_of(keys)
    )
  state_cases <- state$cases[match(
    .row_key(report, keys),
    .row_key(state, keys)
  )]

  report$state <- lookup[["state"]]$name[
    match(report$state_fips, lookup[["state"]]$fips)
  ]
  report$state_cases <- state_cases
  report$unpublished_cases <- state_cases - report$published_cases
  if (!"cases_source" %in% names(x)) {
    report$imputed_cases <- NA_real_
  }
  report$no_county_cases <- report$unpublished_cases - report$imputed_cases

  # With exactly one county row without a published count, the state total
  # minus the published counts is that row plus any cases with no county, and
  # CDC's documentation doesn't say there are any. So it could be that row
  # exactly: withhold every figure that gives it (#173). Rows are counted both
  # in `x` and in CDC's county table, so filtering a county out of `x`, or
  # dropping a "Data not available" row, doesn't hide it. Where CDC's table
  # has no rows for the stratum, how many counties lack a count is unknown,
  # so it is withheld too. A gap of 0 before imputation (unpublished_cases)
  # over several rows is reported: it says those counties had no cases, which
  # identifies no one (maintainer, 22 Nov 2026).
  cdc_unpublished <- .unpublished_rows(cdc, keys)
  cdc_rows <- cdc_unpublished$rows[
    match(.row_key(report, keys), cdc_unpublished$key)
  ]
  single <- report$unpublished_rows == 1 | cdc_rows %in% 1
  unknown <- !single & is.na(cdc_rows)
  withheld <- !is.na(state_cases) & (single | unknown)
  report$no_county_withheld <- dplyr::case_when(
    !withheld ~ NA_character_,
    single ~ "one county row without a published count",
    .default = "no CDC county rows to count unpublished counties"
  )
  report[
    withheld,
    c("published_cases", "unpublished_cases", "no_county_cases")
  ] <-
    NA_real_

  tibble::as_tibble(report[c(
    "state_fips",
    "state",
    .atlas_keys,
    "state_cases",
    "published_cases",
    "unpublished_cases",
    "imputed_cases",
    "no_county_cases",
    "no_county_withheld"
  )])
}

# For each state stratum of CDC's county table, the number of county rows
# without a count, keyed as .row_key(report, keys).
.unpublished_rows <- function(cdc, keys) {
  cdc <- data.frame(
    state_fips = substr(as.character(cdc$fips), 1, 2),
    lapply(as.data.frame(cdc[.atlas_keys]), as.character),
    unpublished = is.na(cdc$cases)
  )
  cdc$year <- as.numeric(cdc$year)
  counted <- cdc |>
    dplyr::summarize(
      rows = sum(.data$unpublished),
      .by = dplyr::all_of(keys)
    )
  list(key = .row_key(counted, keys), rows = counted$rows)
}

# Message saying how many state strata had their remainder withheld (#173).
.announce_withheld <- function(report) {
  n <- sum(!is.na(report$no_county_withheld))
  if (n == 0) {
    return(invisible())
  }
  single <- sum(
    report$no_county_withheld %in% "one county row without a published count"
  )
  unknown <- n - single
  cli::cli_inform(c(
    "i" = "{cli::qty(n)}The remainder{?s} of {n} state strat{?um is/a are}
           not reported ({.code NA}):",
    "*" = if (single > 0) {
      "{single} with exactly one county row without a published count (the
       remainder could be that county's count)"
    },
    "*" = if (unknown > 0) {
      "{unknown} with no rows in CDC's county table, so it can't be known how
       many counties lack a count"
    },
    " " = "{.field no_county_withheld} says which. They are left out of any
           total or share of cases in no county row."
  ))
}

# Message naming the counties whose suppressed rows impute_suppressed() couldn't
# estimate, and what happens to their cases.
.announce_not_estimated <- function(x, what_happens) {
  rows <- is.na(x$cases) & x$cases_source %in% "not estimated"
  if (!any(rows)) {
    return(invisible())
  }
  counties <- unique(as.character(x$geography[rows]))
  cli::cli_inform(c(
    "i" = "{sum(rows)} row{?s} with no count that {.fn impute_suppressed}
           couldn't estimate {?was/were} {what_happens}.",
    " " = "{cli::qty(length(counties))}Count{?y/ies}: {.val {counties}}."
  ))
}

# States with many cases in no county row (#164) --------------------------------

# A state is reported when more than this share of its cases is in no county
# row after imputation. Decided by the maintainer on 21 Nov 2026 (#164); a
# fixed value, not an argument.
.unplaced_share_threshold <- 0.25

# The most states named for one indicator and year, and the most indicator
# and year lines, before the warning says how many more there are.
.unplaced_states_listed <- 5
.unplaced_lines_listed <- 5

# The states, by indicator and year, with more than the threshold share of
# their cases in no county row: the remainder (state total minus published and
# imputed county counts) over the state total, both summed over the state's
# strata.
# - Only strata with a published state total and a remainder are counted, so
#   a state with no published total, and county data not yet imputed, are not
#   assessed.
# - The report has rows only for strata in the county data, so a stratum the
#   caller left out is in neither sum, whatever CDC published for it.
# - The remainder is summed with its sign, so a stratum where the imputed
#   counts overshoot offsets one that falls short.
# - CDC's HIV estimates are left out: their county data cover only EHE
#   counties, so a large remainder is expected and has its own message.
.unplaced_states <- function(report) {
  assessed <- report[
    !is.na(report$state_cases) &
      !is.na(report$no_county_cases) &
      !report$indicator %in% .ehe_only_indicators(report),
  ]
  shares <- assessed |>
    dplyr::summarize(
      state = dplyr::first(.data$state),
      no_county_cases = sum(.data$no_county_cases),
      state_cases = sum(.data$state_cases),
      .by = c("state_fips", "indicator", "year")
    )
  shares$state <- dplyr::coalesce(shares$state, shares$state_fips)
  shares <- shares[shares$state_cases > 0, ]
  shares$share <- shares$no_county_cases / shares$state_cases
  shares[shares$share > .unplaced_share_threshold, ]
}

# A whole percentage for a message. A share that is not 0 or 1 is never shown
# as 0% or 100%.
.percent <- function(share) {
  percent <- round(100 * share)
  dplyr::case_when(
    percent == 0 & share > 0 ~ "under 1%",
    percent == 100 & share < 1 ~ "over 99%",
    .default = paste0(percent, "%")
  )
}

# One warning per call naming the states from .unplaced_states(), largest
# share first. `consequence` and `see` are cli bullets saying what it means
# for the caller's result and where to look; they can use cli pluralisation,
# which follows the number of states, but no variables of the caller's.
.warn_state_unplaced_cases <- function(report, consequence, see = NULL) {
  unplaced <- .unplaced_states(report)
  if (nrow(unplaced) == 0) {
    return(invisible())
  }
  unplaced <- unplaced[
    order(unplaced$indicator, unplaced$year, -unplaced$share),
  ]
  groups <- unique(unplaced[c("indicator", "year")])
  lines <- vapply(
    seq_len(min(nrow(groups), .unplaced_lines_listed)),
    function(i) {
      rows <- unplaced[
        unplaced$indicator == groups$indicator[i] &
          unplaced$year == groups$year[i],
      ]
      states <- sprintf("%s (%s)", rows$state, .percent(rows$share))
      more <- length(states) - .unplaced_states_listed
      if (more > 0) {
        states <- c(
          states[seq_len(.unplaced_states_listed)],
          paste(more, "more")
        )
      }
      paste0(
        groups$indicator[i],
        " ",
        groups$year[i],
        ": ",
        cli::ansi_collapse(states)
      )
    },
    character(1)
  )
  # State and indicator names are text, not cli markup.
  lines <- gsub("([{}])", "\\1\\1", lines)
  more_lines <- nrow(groups) - length(lines)
  if (more_lines > 0) {
    lines <- c(
      lines,
      "and {more_lines} more combination{?s} of indicator and year"
    )
  }
  n <- dplyr::n_distinct(unplaced$state_fips)
  threshold <- .percent(.unplaced_share_threshold)
  with_qty <- function(bullets) {
    rlang::set_names(paste0("{cli::qty(n)}", bullets), names(bullets))
  }
  cli::cli_warn(
    c(
      "{n} state{?s} {?has/have} more than {threshold} of
       {cli::qty(n)}{?its/their} published state total in no county row of
       {.arg x}.",
      rlang::set_names(lines, rep("*", length(lines))),
      "i" = "Percentages are of the state totals CDC published for the strata
             in {.arg x}.",
      "i" = if (any(!is.na(report$no_county_withheld))) {
        "Strata whose remainder isn't reported ({.field no_county_withheld})
         are left out of both."
      },
      with_qty(consequence),
      "i" = "This happens when a state's county counts are largely
             suppressed, or when {.arg x} lacks some of its counties.
             CDCAtlas doesn't check the cause.",
      with_qty(see)
    ),
    class = "cdcatlas_state_unplaced_cases"
  )
}
