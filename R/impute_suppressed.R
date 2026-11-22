#' Fill in suppressed county counts
#'
#' @description
#' CDC suppresses small county counts. `impute_suppressed()` gives each
#' suppressed county a count so county data can be added up or shared across
#' census tracts with [extrapolate_from_county()]. Run it on county data from
#' [get_atlas()] before [no_county_cases()] or [extrapolate_from_county()].
#'
#' Rows are handled by what CDC published:
#'
#' * A published count is kept, whatever its data status. (Connecticut's 2017
#'   planning regions are marked "Data not available" but have counts.)
#' * A row marked "Data not available" with no count has no data to estimate
#'   from; `not_available` says whether it is kept or dropped. Its cases are
#'   still in the state total, so [no_county_cases()] counts them in the
#'   remainder, as long as the state still has a row in that indicator, year
#'   and stratum.
#' * A row marked "Data suppressed" with no count is filled in by `method`.
#' * A row with no count and any other status (or none), such as "No reported
#'   cases" or a code this version doesn't know, is not filled in: it stays
#'   `NA` and is marked `"not estimated"`, with a warning.
#'
#' @param x County data from `get_atlas(geography = "county")`.
#' @param method How suppressed counts are filled in:
#'   * `"state_rate"` (default): the state's rate in the same indicator, year
#'     and stratum, times the county's population (see `population`), kept
#'     between `floor` and `cap`.
#'   * `"zero"`: 0. Useful as a sensitivity analysis.
#' @param cap The largest count `"state_rate"` can give a suppressed county.
#'   Use `Inf` for no cap. The default, 4, reflects CDC suppressing counts of
#'   1 to 4, but complementary suppression can hide larger counts, so it is a
#'   default rather than a hard limit. `cases_uncapped` in the output lets
#'   you count how often the cap sets the value. `floor = cap = 2.5` gives
#'   every row that can be estimated the same fixed value, 2.5.
#' @param floor The smallest count `"state_rate"` can give. The default, 0,
#'   allows for counties suppressed because their population is under 100,
#'   which may have no cases. Use 1 to treat every suppressed county as having
#'   at least one case. Must not be larger than `cap`.
#' @param population The county population `"state_rate"` multiplies by:
#'   * `"cdc"` (default): CDC's own county population, the same source as the
#'     state rate's denominator.
#'   * `"acs"`: the county's ACS 5-year population in the same stratum and
#'     ages, summed from its tracts as [extrapolate_from_county()] does. Needs
#'     tidycensus and a Census API key.
#'
#'   `"acs"` accepts the same input as [extrapolate_from_county()]: at most
#'   one stratification variable, and no congenital syphilis (whose rates are
#'   per 100,000 live births). It also refuses Knowledge of Status, whose
#'   denominator is people living with HIV, not residents. Other input stops
#'   before anything is downloaded; use `"cdc"` for it.
#'
#'   CDC gives Connecticut's 2017 planning regions no population, so `"cdc"`
#'   can't estimate them; `"acs"` can, from 2010 tracts assigned to the
#'   planning regions (see [extrapolate_from_county()]).
#' @param unestimated What to do with a suppressed row `"state_rate"` can't
#'   estimate, because the state rate or county population is missing:
#'   * `"na"` (default): leave it `NA`, with a warning. [no_county_cases()]
#'     counts its cases in the remainder with no county, and
#'     [extrapolate_from_county()] leaves it out (or stops, with
#'     `unestimated = "error"`).
#'   * `"zero"`: set it to 0, with a warning.
#' @param not_available Rows marked "Data not available" with no count:
#'   * `"keep"` (default): keep them with `NA` cases. The later steps leave
#'     them out of the tracts, and their cases stay in the state's remainder.
#'   * `"drop"`: remove them, with a message. If that removes every row a
#'     state has in an indicator, year and stratum, that state's remainder is
#'     no longer reported by [no_county_cases()] or spread by
#'     [extrapolate_from_county()], so this warns and names them.
#'
#' @return `x` with suppressed counts filled in and two new columns,
#'   `cases_source` and `cases_uncapped`. `cases_source` is:
#'   * `"published"`: CDC's count.
#'   * `"state_rate"` or `"zero"`: filled in by `method`.
#'   * `"not estimated"`: `"state_rate"` couldn't estimate it (`NA`, or 0
#'     with `unestimated = "zero"`), or it had no count and wasn't marked
#'     "Data suppressed" (always `NA`).
#'   * `"not available"`: kept with `not_available = "keep"`; `NA`.
#'
#'   `cases_uncapped` is, for rows filled in by `"state_rate"`, the estimate
#'   before `cap` and `floor` were applied: the state rate times the county
#'   population used (CDC's or ACS). It is a modeled number, not CDC's hidden
#'   count and not a way to recover it. It is `NA` for every other row:
#'   published counts, rows filled in by `"zero"`, and rows marked
#'   `"not estimated"` or `"not available"`. `cases` is the value to use;
#'   `cases_uncapped` shows what decided it:
#'   * `cases_uncapped > cap`: the cap set `cases`.
#'   * `cases_uncapped < floor`: the floor set `cases`.
#'   * Otherwise `cases` equals `cases_uncapped`. An estimate exactly equal
#'     to `cap` or `floor` was not changed by it.
#'
#'   With `"state_rate"`, a message says how many suppressed rows were filled
#'   in and, of those, how many had their value set by the cap
#'   (`cases_uncapped > cap`) and by the floor (`cases_uncapped < floor`), as
#'   counts and percentages. Rows that could not be estimated are not counted.
#'   A limit that can't set anything (`cap = Inf`, `floor = 0`) is not
#'   mentioned, and there is no message when no row was estimated or with
#'   `method = "zero"`. A high percentage for the cap means most estimates
#'   are the cap itself rather than the state rate.
#'
#'   `data_status` is unchanged, so suppressed rows can still be identified.
#'   `cap`, `floor`, `population` and `unestimated` apply only to
#'   `"state_rate"`. [no_county_cases()] and [extrapolate_from_county()] use
#'   `cases` and ignore `cases_uncapped`.
#'
#' @details
#' `"state_rate"` downloads the state data matching `x` (same diseases, years
#' and stratification) from CDC AtlasPlus, and with `population = "acs"` the
#' tract populations from the Census.
#'
#' @seealso [no_county_cases()] for cases in state totals that no county
#'   accounts for; [extrapolate_from_county()] for the next step; the article
#'   ["About CDC AtlasPlus data"](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html)
#'   for CDC's suppression rules.
#'
#' @examples
#' \dontrun{
#' county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
#' county <- impute_suppressed(county)
#'
#' # How many filled-in rows did the cap (4 by default) decide?
#' filled <- county[county$cases_source == "state_rate", ]
#' table(filled$indicator, capped = filled$cases_uncapped > 4)
#'
#' # Sensitivity analysis: suppressed counties have no cases
#' county_zero <- impute_suppressed(
#'   get_atlas("hiv", "county", 2022, stratify_by = "sex"),
#'   method = "zero"
#' )
#' }
#'
#' @export
impute_suppressed <- function(
  x,
  method = c("state_rate", "zero"),
  cap = 4,
  floor = 0,
  population = c("cdc", "acs"),
  unestimated = c("na", "zero"),
  not_available = c("keep", "drop")
) {
  call <- rlang::current_env()
  .check_county_data(x, arg = "x", call = call)
  if ("cases_source" %in% names(x)) {
    cli::cli_abort(
      c(
        "{.arg x} has already been through {.fn impute_suppressed}.",
        "i" = "It has a {.field cases_source} column."
      ),
      call = call
    )
  }
  if ("cases_uncapped" %in% names(x)) {
    cli::cli_abort(
      c(
        "{.arg x} already has a {.field cases_uncapped} column.",
        "i" = "{.fn impute_suppressed} adds a column of that name. Rename or
               remove yours first."
      ),
      call = call
    )
  }
  method <- rlang::arg_match(method)
  population <- rlang::arg_match(population)
  unestimated <- rlang::arg_match(unestimated)
  not_available <- rlang::arg_match(not_available)
  .check_count_limit(cap, "cap", call)
  .check_count_limit(floor, "floor", call)
  if (floor > cap) {
    cli::cli_abort(
      "{.arg floor} ({floor}) must not be larger than {.arg cap} ({cap}).",
      call = call
    )
  }

  unavailable <- is.na(x$cases) &
    as.character(x$data_status) %in% "Data not available"
  if (any(unavailable) && not_available == "drop") {
    .warn_dropped_groups(x, unavailable)
    x <- x[!unavailable, , drop = FALSE]
    cli::cli_inform(c(
      "i" = "Dropped {sum(unavailable)} row{?s} marked {.val Data not
             available} with no count.",
      " " = "Their cases are still in the state totals, and
             {.fn no_county_cases} counts them in the remainder for any state
             group that still has rows."
    ))
    unavailable <- rep(FALSE, nrow(x))
  }

  # Only rows CDC marks as suppressed are filled in. A missing count with any
  # other status is left alone rather than treated as suppression (#107).
  no_count <- is.na(x$cases) & !unavailable
  suppressed <- no_count & as.character(x$data_status) %in% "Data suppressed"
  unexplained <- no_count & !suppressed
  x$cases_source <- ifelse(suppressed, method, "published")
  x$cases_source[unavailable] <- "not available"
  x$cases_source[unexplained] <- "not estimated"
  # The "state_rate" estimate before `cap` and `floor`; NA for every row not
  # filled in that way. Shows whether a limit decided `cases` (#151).
  # Full length, not a scalar: `x` has no rows when every row was dropped.
  x$cases_uncapped <- rep(NA_real_, nrow(x))
  if (any(unexplained)) {
    statuses <- unique(as.character(x$data_status[unexplained]))
    statuses[is.na(statuses)] <- "none"
    cli::cli_warn(c(
      "{sum(unexplained)} row{?s} with no count {?is/are} not marked {.val
       Data suppressed}, so {?it is/they are} not filled in and left {.code
       NA}.",
      "i" = "Data status: {.val {statuses}}.",
      "i" = "They are marked {.code cases_source = \"not estimated\"}."
    ))
  }
  if (!any(suppressed)) {
    return(x)
  }

  if (method == "zero") {
    x$cases[suppressed] <- 0
    return(x)
  }

  if (population == "acs") {
    .check_census_strata(
      x,
      call,
      hint = c("i" = "{.code population = \"cdc\"} works for any strata.")
    )
    # The state rate for Knowledge of Status is a share of people living with
    # HIV (CDC's population for it), so it can't multiply ACS residents (#108).
    if ("Knowledge of Status" %in% as.character(x$indicator)) {
      cli::cli_abort(
        c(
          "{.code population = \"acs\"} doesn't fit Knowledge of Status.",
          "x" = "Its rate is a share of people living with HIV, not of
                 residents.",
          "i" = "Use {.code population = \"cdc\"}, CDC's estimate of people
                 living with HIV in each county."
        ),
        call = call
      )
    }
    .check_census_ready(call)
  }
  state <- .fetch_state_totals(x)
  state_rate <- ifelse(
    !is.na(state$population) & state$population > 0,
    state$cases / state$population,
    NA_real_
  )
  keys <- c("state_fips", .atlas_keys)
  x_keys <- .row_key(
    data.frame(
      state_fips = substr(as.character(x$fips), 1, 2),
      as.data.frame(x[.atlas_keys])
    ),
    keys
  )
  rate <- state_rate[match(x_keys, .row_key(state, keys))]
  county_population <- if (population == "cdc") {
    x$population
  } else {
    .acs_county_population(x)
  }
  uncapped <- rate * county_population
  estimate <- pmin(cap, pmax(floor, uncapped))
  x$cases[suppressed] <- estimate[suppressed]
  x$cases_uncapped[suppressed] <- uncapped[suppressed]

  missing <- suppressed & is.na(x$cases)
  x$cases_source[missing] <- "not estimated"
  if (any(missing)) {
    counties <- unique(as.character(x$geography[missing]))
    if (unestimated == "zero") {
      x$cases[missing] <- 0
    }
    set_to <- if (unestimated == "zero") "0" else "NA"
    cli::cli_warn(c(
      "{sum(missing)} suppressed row{?s} could not be estimated and {?is/are}
       set to {.code {set_to}}.",
      "i" = "No state rate or no county population for {.val {counties}}.",
      "i" = "They are marked {.code cases_source = \"not estimated\"}."
    ))
  }
  .inform_limits(x$cases_uncapped, cap, floor)

  x
}

# Message saying how many rows "state_rate" filled in and how many of them
# `cap` and `floor` decided, from the estimates before either was applied
# (`cases_uncapped`, NA for rows not estimated). An estimate exactly equal to
# a limit was not changed by it, so it is not counted. A limit that can't
# decide anything (no cap, or a floor of 0) is not mentioned, and nothing is
# said when no row was estimated (#164).
.inform_limits <- function(uncapped, cap, floor) {
  uncapped <- uncapped[!is.na(uncapped)]
  n <- length(uncapped)
  if (n == 0) {
    return(invisible())
  }
  capped <- sum(uncapped > cap)
  floored <- sum(uncapped < floor)
  capped_share <- .percent(capped / n)
  floored_share <- .percent(floored / n)
  has_cap <- is.finite(cap)
  has_floor <- floor > 0
  cap <- format(cap, big.mark = ",", scientific = FALSE)
  floor <- format(floor, big.mark = ",", scientific = FALSE)
  cli::cli_inform(c(
    "i" = "{n} suppressed row{?s} {?was/were} filled in from state rates.",
    "*" = if (has_cap) {
      "The cap of {cap} set the value of {capped} row{?s} ({capped_share}):
       the estimate was higher."
    },
    "*" = if (has_floor) {
      "The floor of {floor} set the value of {floored} row{?s}
       ({floored_share}): the estimate was lower."
    },
    " " = if (has_cap || has_floor) {
      "{.field cases_uncapped} holds the estimates before the cap and floor."
    }
  ))
}

# Warn when dropping "Data not available" rows removes every row of a state
# group (state, indicator, year and stratum): the remainder of that group is
# then lost to no_county_cases() and extrapolate_from_county() (#110).
.warn_dropped_groups <- function(x, unavailable) {
  keys <- c("state_fips", .atlas_keys)
  groups <- data.frame(
    state_fips = substr(as.character(x$fips), 1, 2),
    lapply(as.data.frame(x[.atlas_keys]), as.character)
  )
  key <- .row_key(groups, keys)
  lost <- unique(groups[!key %in% key[!unavailable], , drop = FALSE])
  if (nrow(lost) == 0) {
    return(invisible())
  }
  states <- unique(lookup[["state"]]$name[
    match(lost$state_fips, lookup[["state"]]$fips)
  ])
  cli::cli_warn(c(
    "Dropping rows marked {.val Data not available} removed every row of
     {nrow(lost)} state group{?s} (indicator, year and stratum).",
    "i" = "{cli::qty(length(states))}State{?s}: {.val {states}}.",
    "!" = "Their state totals are no longer counted by {.fn no_county_cases}
           or spread by {.fn extrapolate_from_county}.",
    "i" = "Use {.code not_available = \"keep\"} (the default) to keep them."
  ))
}

.check_count_limit <- function(value, arg, call) {
  if (!is.numeric(value) || length(value) != 1 || is.na(value) || value < 0) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number of 0 or more, or {.code Inf}.",
      call = call
    )
  }
}

# ACS county population for each row of `x`, in the row's stratum and ages, as
# the sum of the county's tracts (the denominator extrapolate_from_county() uses).
.acs_county_population <- function(x) {
  stratify_by <- .atlas_stratify_by(x)
  stratify_by <- if (length(stratify_by) == 0) NULL else stratify_by
  county <- .county_cases(x)
  county$stratum <- .row_stratum(county, stratify_by)
  row_ages <- .row_ages(county, stratify_by)
  states <- sort(unique(county$state_fips))

  out <- rep(NA_real_, nrow(county))
  for (year in sort(unique(county$year))) {
    for (universe in unique(row_ages[county$year == year])) {
      rows <- county$year == year & row_ages == universe
      pop <- .area_population(
        year,
        states,
        stratify_by,
        universe,
        age_groups = unique(county$age[rows])
      ) |>
        dplyr::summarize(
          population = sum(.data$area_population_acs),
          .by = c("county_fips", "stratum")
        )
      out[rows] <- pop$population[match(
        .row_key(county[rows, ], c("county_fips", "stratum")),
        .row_key(pop, c("county_fips", "stratum"))
      )]
    }
  }
  out
}
