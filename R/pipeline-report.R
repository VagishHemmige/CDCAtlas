#' What each pipeline step did to the numbers
#'
#' Counts what [impute_suppressed()], the state remainder and
#' [extrapolate_from_county()] did to one request, as one long table. Internal
#' while the table's shape settles; `data-raw/measured-effects.R` uses it to
#' build the dated summary that the website reads.
#'
#' Every count is within one indicator and year: diagnoses and prevalence are
#' never added together. Sums of published counts, modelled counts (imputed or
#' allocated) and differences are marked in `basis`, so a modelled value is
#' never presented as CDC's. Nothing here estimates a suppressed count: where
#' public data cannot show a quantity, the row has `value = NA` and a `note`.
#'
#' The cap and floor are counted from the estimate before limits (state rate
#' times CDC's county population), recomputed here from the same inputs
#' [impute_suppressed()] uses. A limit counts only when the estimate is
#' strictly beyond it, so an estimate exactly at the cap is not counted.
#'
#' Rows left out of the areas for want of a count (`"not available"`, `"not
#' estimated"`) have unknown cases that are already inside the state
#' remainder, so the remainder and `cases_not_allocated` must not be added
#' for them; `cases_not_allocated` sums only rows with a count.
#'
#' @param county County data from `get_atlas(geography = "county")`, before
#'   [impute_suppressed()].
#' @param imputed `county` after [impute_suppressed()].
#' @param areas The [extrapolate_from_county()] result for `imputed`, with its
#'   `no_county_cases` and `not_allocated` attributes. `NULL` reports the
#'   steps before allocation only, with the remainder computed from `imputed`.
#' @param cap,floor,population The values given to [impute_suppressed()]. The
#'   report stops if they don't reproduce the imputed counts. With
#'   `population = "acs"` the estimate before limits is not recomputed.
#' @param by_state Also report each state's totals and remainder?
#'
#' @return A tibble with one row per measure:
#'   \describe{
#'     \item{year, indicator}{As in the data.}
#'     \item{stratify_by, group}{The stratification (`"none"`, `"sex"`,
#'       `"age"` or `"race"`) and the stratum: `"all"` when unstratified;
#'       otherwise a stratum, or `"sum of groups"` for all strata together.}
#'     \item{state_fips}{`"all"`, or a state with `by_state = TRUE`.}
#'     \item{step}{`"download"`, `"imputation"`, `"no county"`,
#'       `"allocation"`, `"denominators"` or `"geography"`.}
#'     \item{measure, category}{What is counted, and the level of a breakdown
#'       (a data status, source, reason, allocation rule or statistic).}
#'     \item{unit}{`"county rows"`, `"state strata"`, `"area rows"`,
#'       `"areas"`, `"cases"`, `"people"` or `"proportion"`.}
#'     \item{basis}{`"count"` (rows), `"published"` (CDC's counts),
#'       `"modelled"` (imputed or allocated cases), `"acs"` (ACS populations)
#'       or `"derived"` (differences and shares).}
#'     \item{value}{The number; `NA` when it cannot be known.}
#'     \item{denominator}{For a proportion, what it is a share of.}
#'     \item{note}{Why a value is `NA`, or what it leaves out.}
#'   }
#' @noRd
.pipeline_report <- function(
  county,
  imputed,
  areas = NULL,
  cap = 4,
  floor = 0,
  population = c("cdc", "acs"),
  by_state = FALSE
) {
  call <- rlang::current_env()
  .check_county_data(county, arg = "county", call = call)
  if ("cases_source" %in% names(county)) {
    cli::cli_abort(
      c(
        "{.arg county} must be county data before {.fn impute_suppressed}.",
        "x" = "It has a {.field cases_source} column."
      ),
      call = call
    )
  }
  .check_county_data(imputed, arg = "imputed", call = call)
  .check_imputed(
    imputed,
    arg = "imputed",
    call = call,
    unestimated = "leave out"
  )
  population <- rlang::arg_match(population)
  .check_count_limit(cap, "cap", call)
  .check_count_limit(floor, "floor", call)
  stratify_by <- .atlas_stratify_by(imputed)
  if (length(stratify_by) > 1) {
    cli::cli_abort(
      c(
        "{.arg imputed} can have at most one stratification variable.",
        "x" = "It is stratified by {.val {stratify_by}}."
      ),
      call = call
    )
  }
  county_keys <- c("fips", .atlas_keys)
  if (!all(.row_key(imputed, county_keys) %in% .row_key(county, county_keys))) {
    cli::cli_abort(
      "{.arg imputed} has rows that are not in {.arg county}.",
      call = call
    )
  }
  if (!is.null(areas)) {
    .check_report_areas(areas, call)
  }

  state <- .fetch_state_totals(imputed)
  imputed <- .report_raw_estimates(imputed, state, cap, floor, population, call)
  remainder <- .no_county_cases(imputed, state)
  if (!is.null(areas)) {
    .check_report_same_run(imputed, areas, remainder, call)
    remainder <- attr(areas, "no_county_cases")
  }

  frames <- list(
    county = .report_frame(county, stratify_by),
    imputed = .report_frame(imputed, stratify_by),
    remainder = .report_frame(remainder, stratify_by)
  )
  if (!is.null(areas)) {
    frames$areas <- .report_frame(areas, stratify_by)
    frames$left_out <- .report_frame(attr(areas, "not_allocated"), stratify_by)
  }
  options <- list(
    cap = cap,
    floor = floor,
    population = population,
    crosswalk = if (!is.null(areas)) unique(.tract_crosswalk()$tract_2010)
  )

  pooled <- length(stratify_by) == 1
  national <- .report_slices(frames, options, pooled, by = NULL)
  states <- if (by_state) {
    .report_slices(frames["remainder"], options, pooled, by = "state_fips")
  }
  out <- dplyr::bind_rows(national, states)
  out$stratify_by <- rep(if (pooled) stratify_by else "none", nrow(out))
  tibble::as_tibble(out[.report_columns])
}

.report_columns <- c(
  "year",
  "indicator",
  "stratify_by",
  "group",
  "state_fips",
  "step",
  "measure",
  "category",
  "unit",
  "basis",
  "value",
  "denominator",
  "note"
)

.check_report_areas <- function(areas, call) {
  needed <- c("no_county_cases", "not_allocated")
  missing <- needed[!needed %in% names(attributes(areas))]
  if (!is.data.frame(areas) || length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg areas} must be the result of {.fn extrapolate_from_county}.",
        "x" = "It has no {.field {missing}} attribute{?s}.",
        "i" = "Filtering or binding the result can drop its attributes."
      ),
      call = call
    )
  }
}

# `areas` must have been allocated from these `imputed` rows: otherwise the
# report would join one run's imputation to another run's remainder and
# allocation. Compares the remainder attached to `areas` with the remainder
# of `imputed` (`expected`), state stratum by state stratum, and the county
# rows `areas` accounts for (allocated or left out) with the rows of
# `imputed`, count by count.
.check_report_same_run <- function(imputed, areas, expected, call) {
  tolerance <- 1e-8
  differs <- function(x, y) {
    is.na(x) != is.na(y) | (!is.na(x) & !is.na(y) & abs(x - y) > tolerance)
  }
  stop_mismatch <- function(what) {
    cli::cli_abort(
      c(
        "{.arg areas} was not allocated from {.arg imputed}.",
        "x" = what,
        "i" = "Give the {.fn extrapolate_from_county} result for these
               {.arg imputed} rows."
      ),
      call = call
    )
  }

  attached <- attr(areas, "no_county_cases")
  state_keys <- c("state_fips", .atlas_keys)
  attached_key <- .row_key(attached, state_keys)
  expected_key <- .row_key(expected, state_keys)
  if (!setequal(attached_key, expected_key)) {
    stop_mismatch(
      "Its {.field no_county_cases} attribute covers different state strata
       (state, indicator, year and stratum)."
    )
  }
  rows <- match(expected_key, attached_key)
  for (column in c("state_cases", "published_cases", "imputed_cases")) {
    bad <- differs(expected[[column]], attached[[column]][rows])
    if (any(bad)) {
      stop_mismatch(
        "{.field {column}} in its {.field no_county_cases} attribute differs
         in {sum(bad)} state strat{?um/a}."
      )
    }
  }

  county_keys <- c("county_fips", .atlas_keys)
  accounted <- dplyr::bind_rows(
    as.data.frame(areas)[c(county_keys, "county_cases")],
    as.data.frame(attr(areas, "not_allocated"))[c(county_keys, "county_cases")]
  )
  accounted <- accounted[!duplicated(.row_key(accounted, county_keys)), ]
  county <- .county_cases(imputed)
  accounted_key <- .row_key(accounted, county_keys)
  county_key <- .row_key(county, county_keys)
  if (!setequal(accounted_key, county_key)) {
    stop_mismatch("It accounts for different county rows.")
  }
  bad <- differs(
    county$county_cases,
    accounted$county_cases[match(county_key, accounted_key)]
  )
  if (any(bad)) {
    stop_mismatch(
      "{.field county_cases} differs in {sum(bad)} county row{?s}."
    )
  }
  invisible()
}

# The estimate before limits for each row of `imputed`, as impute_suppressed()
# computes it (state rate in the same indicator, year and stratum, times CDC's
# county population), with why it is missing. Stops if `cap` and `floor` don't
# turn it into the imputed counts.
.report_raw_estimates <- function(
  imputed,
  state,
  cap,
  floor,
  population,
  call
) {
  state_rate <- ifelse(
    !is.na(state$population) & state$population > 0,
    state$cases / state$population,
    NA_real_
  )
  keys <- c("state_fips", .atlas_keys)
  row_keys <- .row_key(
    data.frame(
      state_fips = substr(as.character(imputed$fips), 1, 2),
      as.data.frame(imputed[.atlas_keys])
    ),
    keys
  )
  rate <- state_rate[match(row_keys, .row_key(state, keys))]
  imputed$no_rate <- is.na(rate)
  imputed$no_population <- is.na(imputed$population)
  imputed$raw_estimate <- if (population == "cdc") {
    rate * imputed$population
  } else {
    NA_real_
  }
  if (population != "cdc") {
    return(imputed)
  }

  estimated <- imputed$cases_source %in% "state_rate"
  limited <- pmin(cap, pmax(floor, imputed$raw_estimate))
  differs <- estimated &
    (is.na(limited) | abs(limited - imputed$cases) > 1e-8)
  if (any(differs)) {
    cli::cli_abort(
      c(
        "{.arg cap}, {.arg floor} and {.arg population} don't reproduce
         {sum(differs)} imputed count{?s} in {.arg imputed}.",
        "i" = "Give the values that were given to {.fn impute_suppressed}."
      ),
      call = call
    )
  }
  imputed
}

# `df` as a plain data frame with the columns every step is sliced by:
# character `indicator`, numeric `year`, `group` (the stratum, or "all") and
# `state_fips`.
.report_frame <- function(df, stratify_by) {
  out <- as.data.frame(df)
  out$indicator <- as.character(out$indicator)
  out$year <- as.numeric(as.character(out$year))
  out$group <- if (length(stratify_by) == 0) {
    rep("all", nrow(out))
  } else {
    as.character(out[[.atlas_strata_columns[[stratify_by]]]])
  }
  if (!"state_fips" %in% names(out)) {
    out$state_fips <- substr(as.character(out$fips), 1, 2)
  }
  out
}

# Report every slice of the data: each indicator, year and group, then (when
# stratified) each indicator and year with the groups together. `by =
# "state_fips"` does the same within each state, for the remainder only.
# Indicators and years are never pooled.
.report_slices <- function(frames, options, pooled, by) {
  index <- function(frame, group) {
    key <- data.frame(
      indicator = frame$indicator,
      year = frame$year,
      group = group,
      state_fips = if (is.null(by)) {
        rep("all", nrow(frame))
      } else {
        frame$state_fips
      }
    )
    split(seq_len(nrow(frame)), .row_key(key, names(key)))
  }
  indices <- lapply(frames, function(frame) {
    c(
      index(frame, frame$group),
      if (pooled) index(frame, rep("sum of groups", nrow(frame)))
    )
  })
  # The imputed rows (or the remainder, built from them) define the slices.
  slices <- names(indices[[if (is.null(by)) "imputed" else "remainder"]])

  rows <- lapply(slices, function(slice) {
    data <- lapply(rlang::set_names(names(frames)), function(name) {
      frames[[name]][indices[[name]][[slice]] %||% integer(), , drop = FALSE]
    })
    out <- if (is.null(by)) {
      dplyr::bind_rows(
        .report_download(data$county),
        .report_imputation(data$county, data$imputed, data$remainder, options),
        .report_remainder(data$remainder),
        if (!is.null(data$areas)) {
          dplyr::bind_rows(
            .report_allocation(data$areas, data$left_out),
            .report_denominators(data$areas),
            .report_geography(data$areas, options$crosswalk)
          )
        }
      )
    } else {
      .report_state_remainder(data$remainder)
    }
    key <- strsplit(slice, "\r", fixed = TRUE)[[1]]
    out <- .report_indicator_notes(out, key[1])
    out$indicator <- rep(key[1], nrow(out))
    out$year <- rep(as.numeric(key[2]), nrow(out))
    out$group <- rep(key[3], nrow(out))
    out$state_fips <- rep(key[4], nrow(out))
    out
  })
  dplyr::bind_rows(rows)
}

# Notes that depend on the indicator. CDC's county HIV estimates cover only
# the Ending the HIV Epidemic (EHE) counties, so their remainder is not only
# cases with no county; and CDC's population for Knowledge of Status is people
# living with HIV, not residents, so it can't be compared with the ACS.
.report_indicator_notes <- function(out, indicator) {
  add <- function(rows, note) {
    out$note[rows] <<- ifelse(
      is.na(out$note[rows]),
      note,
      paste(out$note[rows], note)
    )
  }
  if (length(.ehe_only_indicators(data.frame(indicator = indicator))) > 0) {
    add(
      out$step == "no county",
      "CDC's county data for this indicator cover only EHE counties, so the remainder includes people in counties outside EHE coverage."
    )
  }
  if (indicator == "Knowledge of Status") {
    add(
      out$step == "denominators",
      "CDC's population for Knowledge of Status is people living with HIV, not residents: not comparable with the ACS."
    )
  }
  out
}

# One or more rows of the report: `value` and `category` run in parallel.
.report_rows <- function(
  step,
  measure,
  value,
  unit,
  basis,
  category = NA_character_,
  denominator = NA_character_,
  note = NA_character_
) {
  n <- length(value)
  data.frame(
    step = rep(step, n),
    measure = rep(measure, n),
    category = rep_len(as.character(category), n),
    unit = rep(unit, n),
    basis = rep(basis, n),
    value = as.numeric(value),
    denominator = rep_len(as.character(denominator), n),
    note = rep_len(as.character(note), n)
  )
}

# `values` summed within each level of `by`, in the order of `levels`.
.report_sum_by <- function(values, by, levels) {
  vapply(levels, \(level) sum(values[by %in% level]), numeric(1))
}

# Download: what CDC returned ---------------------------------------------------

.report_download <- function(county) {
  step <- "download"
  status <- as.character(county$data_status)
  status[is.na(status)] <- "none"
  statuses <- sort(unique(status))
  no_count <- is.na(county$cases)
  suppressed <- no_count & status == "Data suppressed"
  dplyr::bind_rows(
    .report_rows(
      step,
      "county_rows",
      .report_sum_by(rep(1, nrow(county)), status, statuses),
      "county rows",
      "count",
      category = statuses
    ),
    .report_rows(
      step,
      "county_rows_without_count",
      .report_sum_by(as.numeric(no_count), status, statuses),
      "county rows",
      "count",
      category = statuses
    ),
    .report_rows(
      step,
      "published_county_cases",
      if (all(no_count)) NA else sum(county$cases, na.rm = TRUE),
      "cases",
      "published",
      note = if (all(no_count)) {
        "No county row here has a published count, so this is unknown, not 0."
      } else {
        NA_character_
      }
    ),
    .report_rows(
      step,
      "observed_cases_in_suppressed_rows",
      if (any(suppressed)) NA else 0,
      "cases",
      "published",
      note = if (any(suppressed)) {
        "CDC withholds these counts. Not inferred: imputed counts are modelled, not observed."
      } else {
        NA_character_
      }
    ),
    .report_rows(
      step,
      "county_rows_expected_but_absent",
      NA,
      "county rows",
      "count",
      note = "No list of the county rows CDC should return is available, so rows missing from the response cannot be counted."
    )
  )
}

# Imputation: what impute_suppressed() filled in --------------------------------

.report_imputation <- function(county, imputed, remainder, options) {
  step <- "imputation"
  status <- as.character(imputed$data_status)
  source <- imputed$cases_source
  sources <- sort(unique(source))
  suppressed <- is.na(county$cases) &
    as.character(county$data_status) %in% "Data suppressed"
  estimated <- source %in% c("state_rate", "zero")

  no_count_note <- "Rows without a count: their cases are unknown and are inside the state remainder."
  cases_by_source <- vapply(
    sources,
    function(one) {
      cases <- imputed$cases[source == one]
      if (all(is.na(cases))) NA_real_ else sum(cases, na.rm = TRUE)
    },
    numeric(1)
  )
  source_basis <- ifelse(sources == "published", "published", "modelled")
  source_note <- ifelse(
    sources %in% c("not estimated", "not available"),
    no_count_note,
    NA_character_
  )

  not_estimated <- source %in% "not estimated"
  reason <- dplyr::case_when(
    !status %in% "Data suppressed" ~ "no count, not marked suppressed",
    options$population != "cdc" ~ "suppressed, reason not recomputed",
    imputed$no_rate & imputed$no_population ~
      "suppressed, no state rate and no county population",
    imputed$no_rate ~ "suppressed, no state rate",
    imputed$no_population ~ "suppressed, no county population",
    .default = "suppressed, other"
  )[not_estimated]
  reasons <- sort(unique(reason))

  has_total <- !is.na(remainder$state_cases)
  state_cases <- sum(remainder$state_cases[has_total])
  imputed_cases <- sum(remainder$imputed_cases[has_total])
  share_note <- if (!any(has_total)) {
    .report_no_total_note
  } else if (state_cases == 0) {
    "The published state total is 0, so there is nothing to divide by."
  } else {
    NA_character_
  }

  dplyr::bind_rows(
    .report_rows(
      step,
      "suppressed_rows",
      sum(suppressed),
      "county rows",
      "count"
    ),
    .report_rows(
      step,
      "suppressed_rows_estimated",
      sum(estimated),
      "county rows",
      "count"
    ),
    .report_limits(imputed, options),
    .report_rows(
      step,
      "county_rows_by_source",
      .report_sum_by(rep(1, nrow(imputed)), source, sources),
      "county rows",
      "count",
      category = sources
    ),
    dplyr::bind_rows(lapply(seq_along(sources), function(i) {
      .report_rows(
        step,
        "cases_by_source",
        cases_by_source[[i]],
        "cases",
        source_basis[[i]],
        category = sources[[i]],
        note = source_note[[i]]
      )
    })),
    .report_rows(
      step,
      "imputed_share_of_state_cases",
      if (is.na(share_note)) imputed_cases / state_cases else NA,
      "proportion",
      "derived",
      denominator = "state_cases: CDC's state totals, in state strata with a published total",
      note = share_note
    ),
    .report_rows(
      step,
      "county_rows_not_estimated",
      .report_sum_by(rep(1, length(reason)), reason, reasons),
      "county rows",
      "count",
      category = reasons
    )
  )
}

# How often the cap and the floor changed the estimate, and by how many cases.
.report_limits <- function(imputed, options) {
  step <- "imputation"
  by_rate <- imputed$cases_source %in% "state_rate"
  if (options$population != "cdc" && any(by_rate)) {
    measures <- c(
      "suppressed_rows_raw_above_cap",
      "suppressed_rows_raw_below_floor",
      "suppressed_rows_raw_within_limits",
      "cases_raw_estimate",
      "cases_removed_by_cap",
      "cases_added_by_floor"
    )
    return(dplyr::bind_rows(lapply(measures, function(measure) {
      .report_rows(
        step,
        measure,
        NA,
        if (startsWith(measure, "cases")) "cases" else "county rows",
        if (startsWith(measure, "cases")) "modelled" else "count",
        note = "Not recomputed for population = \"acs\": it needs the ACS county populations."
      )
    })))
  }
  raw <- imputed$raw_estimate[by_rate]
  above <- raw > options$cap
  below <- raw < options$floor
  dplyr::bind_rows(
    .report_rows(
      step,
      "suppressed_rows_raw_above_cap",
      sum(above),
      "county rows",
      "count"
    ),
    .report_rows(
      step,
      "suppressed_rows_raw_below_floor",
      sum(below),
      "county rows",
      "count"
    ),
    .report_rows(
      step,
      "suppressed_rows_raw_within_limits",
      sum(!above & !below),
      "county rows",
      "count"
    ),
    .report_rows(step, "cases_raw_estimate", sum(raw), "cases", "modelled"),
    .report_rows(
      step,
      "cases_removed_by_cap",
      sum(raw[above] - options$cap),
      "cases",
      "modelled"
    ),
    .report_rows(
      step,
      "cases_added_by_floor",
      sum(options$floor - raw[below]),
      "cases",
      "modelled"
    )
  )
}

# Cases with no county: the state remainder -----------------------------------------

# `remainder` is no_county_cases() output: one row per state stratum. Sums are
# over state strata with a published state total; the others are counted, not
# treated as 0.
.report_remainder <- function(remainder) {
  step <- "no county"
  has_total <- !is.na(remainder$state_cases)
  with_total <- remainder[has_total, , drop = FALSE]
  left <- with_total$no_county_cases
  # With no published state total here, nothing below is known: an empty sum
  # would be 0, which would read as a published zero.
  known <- any(has_total)
  coverage <- if (!known) {
    .report_no_total_note
  } else if (any(!has_total)) {
    paste(
      sum(!has_total),
      "state strata have no published state total and are left out, not counted as 0."
    )
  } else {
    NA_character_
  }
  state_cases <- sum(with_total$state_cases)
  share <- function(measure, value) {
    note <- if (!known) {
      coverage
    } else if (state_cases == 0) {
      "The published state total is 0, so there is nothing to divide by."
    } else {
      coverage
    }
    .report_rows(
      step,
      measure,
      if (known && state_cases != 0) value / state_cases else NA,
      "proportion",
      "derived",
      denominator = "state_cases: CDC's state totals, in state strata with a published total",
      note = note
    )
  }
  count <- function(measure, value, note = NA_character_) {
    .report_rows(step, measure, value, "state strata", "count", note = note)
  }
  # A sum, or a count, over the state strata with a published total.
  cases <- function(measure, value, basis) {
    .report_rows(
      step,
      measure,
      if (known) value else NA,
      "cases",
      basis,
      note = coverage
    )
  }
  with_remainder <- function(measure, value) {
    count(measure, if (known) value else NA, coverage)
  }
  dplyr::bind_rows(
    count("state_strata", nrow(remainder)),
    count("state_strata_without_total", sum(!has_total)),
    cases("state_cases", state_cases, "published"),
    cases("published_cases", sum(with_total$published_cases), "published"),
    cases("unpublished_cases", sum(with_total$unpublished_cases), "derived"),
    cases("imputed_cases", sum(with_total$imputed_cases), "modelled"),
    cases("remainder_signed", sum(left), "derived"),
    cases("remainder_positive", sum(pmax(left, 0)), "derived"),
    cases("remainder_negative", sum(pmin(left, 0)), "derived"),
    with_remainder("state_strata_remainder_positive", sum(left > 0)),
    with_remainder("state_strata_remainder_negative", sum(left < 0)),
    with_remainder("state_strata_remainder_zero", sum(left == 0)),
    share("remainder_signed_share_of_state_cases", sum(left)),
    share("remainder_positive_share_of_state_cases", sum(pmax(left, 0))),
    .report_rows(
      step,
      "cases_with_unknown_county_of_residence",
      NA,
      "cases",
      "published",
      note = "The remainder is a difference (state total minus published and imputed county counts), not a count of identified cases. Public data do not say what it consists of."
    )
  )
}

.report_no_total_note <- "Unknown, not 0: the state strata here have no published state total."

# One state's totals and remainder, with the number of its strata that have no
# published total. A single state stratum has one remainder, so its positive
# and negative parts are given only for groups together.
.report_state_remainder <- function(remainder) {
  out <- .report_remainder(remainder)
  keep <- c(
    "state_strata_without_total",
    "state_cases",
    "published_cases",
    "imputed_cases",
    "remainder_signed"
  )
  if (nrow(remainder) > 1) {
    keep <- c(keep, "remainder_positive", "remainder_negative")
  }
  out[out$measure %in% keep, , drop = FALSE]
}

# Allocation: what reached the areas --------------------------------------------

.report_allocation <- function(areas, left_out) {
  step <- "allocation"
  first <- !duplicated(.row_key(areas, c("county_fips", "group")))
  rules <- sort(unique(areas$allocation))
  exceeds <- !is.na(areas$area_cases) &
    areas$area_cases > areas$area_population_acs
  area_cases <- dplyr::coalesce(areas$area_cases, 0)
  by_rule <- function(measure, values, unit, basis, rows = TRUE) {
    .report_rows(
      step,
      measure,
      .report_sum_by(values[rows], areas$allocation[rows], rules),
      unit,
      basis,
      category = rules
    )
  }

  # Rows left out, by reason. Published and imputed counts are summed
  # separately; a reason whose rows all lack a count has no known cases.
  reasons <- sort(unique(left_out$reason))
  no_count <- is.na(left_out$county_cases)
  published <- left_out$cases_source %in% "published" & !no_count
  imputed <- !published & !no_count
  cases_left_out <- .report_sum_by(
    dplyr::coalesce(left_out$county_cases, 0) * published,
    left_out$reason,
    reasons
  )
  cases_left_out[!reasons %in% left_out$reason[!no_count]] <- NA
  imputed_reasons <- sort(unique(left_out$reason[imputed]))
  rows_without_count <- .report_sum_by(
    as.numeric(no_count),
    left_out$reason,
    reasons
  )

  dplyr::bind_rows(
    by_rule(
      "county_rows_allocated",
      rep(1, nrow(areas)),
      "county rows",
      "count",
      first
    ),
    by_rule(
      "county_rows_allocated_with_cases",
      as.numeric(areas$county_cases_allocated > 0),
      "county rows",
      "count",
      first
    ),
    by_rule("area_rows", rep(1, nrow(areas)), "area rows", "count"),
    by_rule("cases_allocated", area_cases, "cases", "modelled"),
    by_rule(
      "area_rows_cases_exceed_population",
      as.numeric(exceeds),
      "area rows",
      "count"
    ),
    by_rule(
      "cases_in_area_rows_exceeding_population",
      area_cases,
      "cases",
      "modelled",
      exceeds
    ),
    by_rule(
      "people_in_area_rows_exceeding_population",
      areas$area_population_acs,
      "people",
      "acs",
      exceeds
    ),
    .report_rows(
      step,
      "county_rows_not_allocated",
      .report_sum_by(rep(1, nrow(left_out)), left_out$reason, reasons),
      "county rows",
      "count",
      category = reasons
    ),
    .report_rows(
      step,
      "county_rows_not_allocated_without_count",
      rows_without_count,
      "county rows",
      "count",
      category = reasons
    ),
    .report_rows(
      step,
      "cases_not_allocated",
      cases_left_out,
      "cases",
      "published",
      category = reasons,
      note = ifelse(
        rows_without_count > 0,
        "Rows without a count are not in this sum: their cases are unknown and are inside the state remainder, so do not add the two.",
        NA_character_
      )
    ),
    .report_rows(
      step,
      "imputed_cases_not_allocated",
      .report_sum_by(
        dplyr::coalesce(left_out$county_cases, 0) * imputed,
        left_out$reason,
        imputed_reasons
      ),
      "cases",
      "modelled",
      category = imputed_reasons
    )
  )
}

# Denominators: CDC's county population against the ACS sum of areas ------------

.report_denominators <- function(areas) {
  step <- "denominators"
  county <- areas[!duplicated(.row_key(areas, c("county_fips", "group"))), ]
  cdc <- county$county_population_atlas
  acs <- county$county_population_acs
  compared <- !is.na(cdc) & !is.na(acs)
  difference <- (acs - cdc)[compared]
  relative <- ((acs - cdc) / cdc)[compared & cdc > 0]
  distribution <- function(measure, values, unit, denominator = NA_character_) {
    if (length(values) == 0) {
      return(NULL)
    }
    quantiles <- stats::quantile(
      values,
      c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
      names = FALSE
    )
    .report_rows(
      step,
      measure,
      c(quantiles, mean(values)),
      unit,
      "derived",
      category = c("min", "p05", "p25", "median", "p75", "p95", "max", "mean"),
      denominator = denominator
    )
  }
  dplyr::bind_rows(
    .report_rows(
      step,
      "county_rows_populations_compared",
      sum(compared),
      "county rows",
      "count"
    ),
    .report_rows(
      step,
      "county_rows_without_cdc_population",
      sum(is.na(cdc)),
      "county rows",
      "count"
    ),
    distribution("population_difference_acs_minus_cdc", difference, "people"),
    distribution(
      "population_relative_difference_acs_vs_cdc",
      relative,
      "proportion",
      "county_population_atlas: CDC's county population in the stratum, allocated county rows where it is above 0"
    )
  )
}

# Geography: crosswalked 2010 tracts and areas in two counties ------------------

.report_geography <- function(areas, crosswalk) {
  step <- "geography"
  area_key <- .row_key(areas, c("geoid", "group"))
  in_two <- area_key %in% area_key[duplicated(area_key)]
  moved <- areas$acs_year %in%
    .acs_years_2010_tracts &
    substr(areas$geoid, 1, 11) %in% crosswalk
  area_cases <- dplyr::coalesce(areas$area_cases, 0)
  counties <- function(rows) {
    length(unique(.row_key(areas[rows, ], c("county_fips", "group"))))
  }
  count <- function(measure, value, unit) {
    .report_rows(step, measure, value, unit, "count")
  }
  dplyr::bind_rows(
    count("area_rows_total", nrow(areas), "area rows"),
    count("areas", length(unique(areas$geoid)), "areas"),
    count("area_rows_geoid_in_two_counties", sum(in_two), "area rows"),
    count(
      "areas_in_two_counties",
      length(unique(areas$geoid[in_two])),
      "areas"
    ),
    .report_rows(
      step,
      "cases_in_areas_in_two_counties",
      sum(area_cases[in_two]),
      "cases",
      "modelled"
    ),
    .report_rows(
      step,
      "people_in_areas_in_two_counties",
      sum(areas$area_population_acs[in_two]),
      "people",
      "acs"
    ),
    count("area_rows_crosswalk", sum(moved), "area rows"),
    count("areas_crosswalk", length(unique(areas$geoid[moved])), "areas"),
    count("county_rows_crosswalk", counties(moved), "county rows"),
    .report_rows(
      step,
      "cases_in_crosswalk_areas",
      sum(area_cases[moved]),
      "cases",
      "modelled"
    ),
    .report_rows(
      step,
      "people_in_crosswalk_areas",
      sum(areas$area_population_acs[moved]),
      "people",
      "acs"
    )
  )
}


# The compact summary -----------------------------------------------------------

#' The figures the articles plot or quote, from the detail table
#'
#' Reduces the detail table that `data-raw/measured-effects.R` builds from
#' [.pipeline_report()] (with `by_state = TRUE` and a `scenario` column) to a
#' few hundred rows per year. Every figure is copied from, or computed from,
#' rows of `detail`; shares say what they divide by what in `numerator` and
#' `denominator`. A share whose denominator is 0 or missing is `NA` with a
#' note, never `Inf` or 0. Quantiles are `stats::quantile(type = 7)`.
#'
#' Within each year, indicator and stratification:
#'
#' * for every group, the remainder (signed, positive and negative) and its
#'   share of state cases, the share of county rows suppressed, the imputed
#'   share of state cases, the share of estimated rows above the cap, the
#'   share of allocated county rows that used a fallback rule, and the area
#'   rows with more cases than people;
#' * for the groups together (`"all"`, or `"sum of groups"`), the rest:
#'   limits, rows not estimated, each alternative against the default,
#'   allocation and rows left out, denominators, geography, and the rows that
#'   say what public data cannot show;
#' * across states, for the groups together: the remainder and the imputed
#'   cases as shares of each state's cases (minimum, quartiles and maximum,
#'   with the states at the extremes named), and counts of states;
#' * by state, for the groups together, two series only, for plots: the
#'   remainder and the imputed cases as shares of the state's cases.
#'
#' A state is left out of the across-state figures, and counted, when its
#' state cases are 0 or missing. Ties at an extreme name the first state in
#' FIPS order.
#'
#' @param detail The detail table: [.pipeline_report()] output with a
#'   `scenario` column, `"default"` for the default pipeline.
#' @return A tibble like `detail`, with `state`, `numerator` and
#'   `extreme_state` columns added.
#' @noRd
.pipeline_summary <- function(detail) {
  call <- rlang::current_env()
  needed <- c(.report_columns, "scenario")
  missing <- setdiff(needed, names(detail))
  if (!is.data.frame(detail) || length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg detail} must be {.fn .pipeline_report} output with a
         {.field scenario} column.",
        "x" = "It has no {.field {missing}} column{?s}."
      ),
      call = call
    )
  }
  detail <- as.data.frame(detail)
  detail$state_fips <- as.character(detail$state_fips)
  # A detail table read back from its CSV file can have "" for a missing text.
  for (column in c("category", "denominator", "note")) {
    text <- as.character(detail[[column]])
    detail[[column]] <- ifelse(text %in% "", NA_character_, text)
  }

  run_keys <- c("year", "indicator", "stratify_by")
  runs <- split(detail, .row_key(detail, run_keys))
  out <- dplyr::bind_rows(lapply(runs, function(run) {
    rows <- .summary_run(run)
    for (key in run_keys) {
      rows[[key]] <- rep(run[[key]][1], nrow(rows))
    }
    rows
  }))
  out <- out[
    order(
      out$year,
      out$indicator,
      match(out$stratify_by, c("none", "sex", "age", "race"))
    ),
  ]
  tibble::as_tibble(out[.summary_columns])
}

.summary_columns <- c(
  "year",
  "indicator",
  "stratify_by",
  "scenario",
  "group",
  "state_fips",
  "state",
  "step",
  "measure",
  "category",
  "unit",
  "basis",
  "value",
  "numerator",
  "denominator",
  "extreme_state",
  "note"
)

.summary_quantile_type <- 7

# One year, indicator and stratification.
.summary_run <- function(run) {
  default <- run[run$scenario == "default", , drop = FALSE]
  national <- default[default$state_fips == "all", , drop = FALSE]
  groups <- unique(national$group)
  together <- if ("sum of groups" %in% groups) "sum of groups" else "all"

  by_group <- lapply(groups, function(group) {
    rows <- .summary_group(
      national[national$group == group, , drop = FALSE],
      full = group == together
    )
    rows$group <- rep(group, nrow(rows))
    rows
  })
  states <- default[
    default$state_fips != "all" & default$group == together,
    ,
    drop = FALSE
  ]
  across <- dplyr::bind_rows(
    .summary_states(states),
    .summary_sensitivity(run[run$group == together, , drop = FALSE])
  )
  across$group <- rep(together, nrow(across))
  dplyr::bind_rows(by_group, across)
}

# One or more rows of the summary.
.summary_rows <- function(
  step,
  measure,
  value,
  unit,
  basis,
  category = NA_character_,
  numerator = NA_character_,
  denominator = NA_character_,
  note = NA_character_,
  scenario = "default",
  state_fips = "all",
  state = NA_character_,
  extreme_state = NA_character_
) {
  n <- length(value)
  each <- function(x) rep_len(as.character(x), n)
  data.frame(
    scenario = each(scenario),
    state_fips = each(state_fips),
    state = each(state),
    step = each(step),
    measure = each(measure),
    category = each(category),
    unit = each(unit),
    basis = each(basis),
    value = as.numeric(value),
    numerator = each(numerator),
    denominator = each(denominator),
    extreme_state = each(extreme_state),
    note = each(note)
  )
}

# `numerator / denominator` (times `scale`), or NA where the denominator is 0
# or either is missing, with the reason. Vectorised.
.summary_divide <- function(numerator, denominator, scale = 1) {
  missing <- is.na(numerator) | is.na(denominator)
  zero <- !missing & denominator == 0
  value <- rep(NA_real_, length(missing))
  ok <- !missing & !zero
  value[ok] <- scale * numerator[ok] / denominator[ok]
  note <- rep(NA_character_, length(missing))
  note[
    missing
  ] <- "Numerator or denominator is not in the detail table, or is NA there."
  note[zero] <- "Denominator is 0."
  list(value = value, note = note)
}

# A share row: `numerator_text` and `denominator_text` name the detail rows.
.summary_share <- function(
  step,
  measure,
  numerator,
  denominator,
  numerator_text,
  denominator_text,
  ...
) {
  share <- .summary_divide(numerator, denominator)
  .summary_rows(
    step,
    measure,
    share$value,
    "proportion",
    "derived",
    numerator = numerator_text,
    denominator = denominator_text,
    note = share$note,
    ...
  )
}

# The state cases every share of state cases divides by.
.summary_state_cases <- "state_cases: CDC's state totals, in state strata with a published total"

# One group's national rows of the default pipeline. `full` adds the figures
# given only for the groups together.
.summary_group <- function(slice, full) {
  # The one value of a measure (NA if it isn't there), and the sum of a
  # measure over its categories (NA if its step isn't there at all).
  one <- function(measure) {
    value <- slice$value[slice$measure == measure]
    if (length(value) == 1) value else NA_real_
  }
  total <- function(measure, step, keep = TRUE) {
    if (!any(slice$step == step)) {
      return(NA_real_)
    }
    values <- slice$value[slice$measure == measure & keep]
    # A measure whose every row is NA is unknown; one with no rows is 0.
    if (length(values) > 0 && all(is.na(values))) {
      return(NA_real_)
    }
    sum(values, na.rm = TRUE)
  }
  copy <- function(measures, categories = NULL, note = NULL) {
    rows <- slice[slice$measure %in% measures, , drop = FALSE]
    if (!is.null(categories)) {
      rows <- rows[rows$category %in% categories, , drop = FALSE]
    }
    rows <- rows[order(match(rows$measure, measures)), , drop = FALSE]
    if (!is.null(note)) {
      rows$note <- ifelse(is.na(rows$note), note, paste(rows$note, note))
    }
    .summary_rows(
      rows$step,
      rows$measure,
      rows$value,
      rows$unit,
      rows$basis,
      category = rows$category,
      denominator = rows$denominator,
      note = rows$note
    )
  }

  state_cases <- one("state_cases")
  remainder_share <- function(sign) {
    measure <- paste0("remainder_", sign)
    .summary_share(
      "no county",
      paste0(measure, "_share_of_state_cases"),
      one(measure),
      state_cases,
      measure,
      .summary_state_cases
    )
  }
  estimated <- one("suppressed_rows_raw_above_cap") +
    one("suppressed_rows_raw_below_floor") +
    one("suppressed_rows_raw_within_limits")
  estimated_text <- "suppressed rows estimated by the state rate: suppressed_rows_raw_above_cap + _below_floor + _within_limits"
  limit_share <- function(limit) {
    measure <- paste0("suppressed_rows_raw_", limit)
    .summary_share(
      "imputation",
      paste0("raw_", limit, "_share_of_estimated_rows"),
      one(measure),
      estimated,
      measure,
      estimated_text
    )
  }
  fallback <- slice$category != "stratum population"
  allocated <- total("county_rows_allocated", "allocation")
  allocated_text <- "county_rows_allocated, all rules"

  rows <- list(
    copy(c("remainder_signed", "remainder_positive", "remainder_negative")),
    remainder_share("signed"),
    remainder_share("positive"),
    .summary_share(
      "imputation",
      "suppressed_rows_share_of_county_rows",
      one("suppressed_rows"),
      total("county_rows", "download"),
      "suppressed_rows",
      "county_rows, all data statuses"
    ),
    .summary_share(
      "imputation",
      "imputed_share_of_state_cases",
      one("imputed_cases"),
      state_cases,
      "imputed_cases, in state strata with a published total",
      .summary_state_cases
    ),
    limit_share("above_cap"),
    .summary_share(
      "allocation",
      "county_rows_fallback_share",
      total("county_rows_allocated", "allocation", fallback),
      allocated,
      "county_rows_allocated, rules other than stratum population",
      allocated_text,
      category = "any fallback rule"
    ),
    .summary_rows(
      "allocation",
      "area_rows_cases_exceed_population",
      total("area_rows_cases_exceed_population", "allocation"),
      "area rows",
      "count",
      note = if (any(slice$step == "allocation")) {
        NA_character_
      } else {
        "The detail table has no allocation step."
      }
    )
  )
  if (!full) {
    return(dplyr::bind_rows(rows))
  }

  rules <- sort(unique(slice$category[
    slice$measure == "county_rows_allocated" & fallback
  ]))
  cases_allocated <- total("cases_allocated", "allocation")
  county_cases <- total("cases_by_source", "imputation")
  by_rule <- function(measure, source, denominator, denominator_text) {
    dplyr::bind_rows(lapply(rules, function(rule) {
      .summary_share(
        "allocation",
        measure,
        total(source, "allocation", slice$category %in% rule),
        denominator,
        paste0(source, ", this rule"),
        denominator_text,
        category = rule
      )
    }))
  }
  quantiles <- c("min", "p05", "p25", "median", "p75", "p95", "max")

  dplyr::bind_rows(
    rows,
    remainder_share("negative"),
    copy(c(
      "state_cases",
      "state_strata_remainder_negative",
      "state_strata_without_total"
    )),
    limit_share("below_floor"),
    copy("cases_removed_by_cap"),
    .summary_share(
      "imputation",
      "cases_removed_by_cap_share_of_raw_estimate",
      one("cases_removed_by_cap"),
      one("cases_raw_estimate"),
      "cases_removed_by_cap",
      "cases_raw_estimate: the estimates before limits, summed"
    ),
    copy("county_rows_not_estimated"),
    .summary_share(
      "allocation",
      "county_rows_allocated_share",
      allocated,
      total("county_rows_by_source", "imputation"),
      allocated_text,
      "county_rows_by_source, all sources: county rows after imputation"
    ),
    .summary_share(
      "allocation",
      "cases_allocated_share_of_county_cases",
      cases_allocated,
      county_cases,
      "cases_allocated, all rules",
      "cases_by_source, sources with a count: published and imputed county cases"
    ),
    by_rule(
      "county_rows_fallback_share",
      "county_rows_allocated",
      allocated,
      allocated_text
    ),
    by_rule(
      "cases_fallback_share",
      "cases_allocated",
      cases_allocated,
      "cases_allocated, all rules"
    ),
    if (any(slice$step == "allocation")) {
      .summary_rows(
        "allocation",
        "cases_in_area_rows_exceeding_population",
        total("cases_in_area_rows_exceeding_population", "allocation"),
        "cases",
        "modelled"
      )
    },
    copy(c(
      "county_rows_not_allocated",
      "cases_not_allocated",
      "imputed_cases_not_allocated"
    )),
    copy(
      "population_relative_difference_acs_vs_cdc",
      quantiles,
      note = paste0("Quantile type ", .summary_quantile_type, ".")
    ),
    copy(c(
      "area_rows_crosswalk",
      "areas_crosswalk",
      "cases_in_crosswalk_areas",
      "people_in_crosswalk_areas",
      "area_rows_geoid_in_two_counties",
      "areas_in_two_counties",
      "cases_in_areas_in_two_counties",
      "people_in_areas_in_two_counties"
    )),
    copy(c(
      "observed_cases_in_suppressed_rows",
      "county_rows_expected_but_absent",
      "cases_with_unknown_county_of_residence"
    ))
  )
}

# "Name (FIPS)" for each state code.
.summary_state_label <- function(state_fips) {
  name <- .summary_state_name(state_fips)
  paste0(dplyr::coalesce(name, "Unknown state"), " (", state_fips, ")")
}

.summary_state_name <- function(state_fips) {
  lookup[["state"]]$name[match(state_fips, lookup[["state"]]$fips)]
}

# One value per state of a measure in by-state rows, in the order of `states`.
.summary_state_values <- function(rows, measure, states) {
  rows <- rows[rows$measure == measure, , drop = FALSE]
  rows$value[match(states, rows$state_fips)]
}

# Minimum, quartiles and maximum of one value per state, with the states at
# the extremes named, and the number of states with no value.
.summary_distribution <- function(
  step,
  measure,
  values,
  states,
  numerator,
  denominator
) {
  known <- !is.na(values)
  type <- paste0(
    "Quantile type ",
    .summary_quantile_type,
    ", over ",
    sum(known),
    " states."
  )
  if (!any(known)) {
    return(.summary_rows(
      step,
      measure,
      rep(NA, 5),
      "proportion",
      "derived",
      category = c("min", "p25", "median", "p75", "max"),
      numerator = numerator,
      denominator = denominator,
      note = "No state has a value."
    ))
  }
  quantiles <- stats::quantile(
    values[known],
    c(0, 0.25, 0.5, 0.75, 1),
    names = FALSE,
    type = .summary_quantile_type
  )
  # Named after rounding away arithmetic noise, so ties name the first state.
  at <- function(extreme) {
    .summary_state_label(states[known][which(
      abs(values[known] - extreme) < 1e-12
    )[1]])
  }
  .summary_rows(
    step,
    measure,
    quantiles,
    "proportion",
    "derived",
    category = c("min", "p25", "median", "p75", "max"),
    numerator = numerator,
    denominator = denominator,
    extreme_state = c(at(quantiles[1]), NA, NA, NA, at(quantiles[5])),
    note = type
  )
}

# Across states, for the groups together: `states` holds the default
# pipeline's by-state rows. Each state's remainder and imputed cases as shares
# of its state cases; the two series are also kept state by state, for plots.
.summary_states <- function(states) {
  if (nrow(states) == 0) {
    return(NULL)
  }
  fips <- sort(unique(states$state_fips))
  state_cases <- .summary_state_values(states, "state_cases", fips)
  signed <- .summary_state_values(states, "remainder_signed", fips)
  # A state whose total is unknown keeps the detail table's reason.
  total_rows <- states[states$measure == "state_cases", , drop = FALSE]
  unknown <- total_rows$note[match(fips, total_rows$state_fips)]
  with_reason <- function(share) {
    explained <- is.na(state_cases) & !is.na(unknown)
    share$note[explained] <- unknown[explained]
    share
  }
  remainder <- with_reason(.summary_divide(signed, state_cases))
  imputed <- with_reason(.summary_divide(
    .summary_state_values(states, "imputed_cases", fips),
    state_cases
  ))
  denominator <- "the state's state_cases, groups together"
  count <- function(step, measure, value, note = NA_character_) {
    .summary_rows(step, measure, value, "states", "count", note = note)
  }
  series <- function(step, measure, share, numerator) {
    .summary_rows(
      step,
      measure,
      share$value,
      "proportion",
      "derived",
      numerator = numerator,
      denominator = denominator,
      note = share$note,
      state_fips = fips,
      state = .summary_state_name(fips)
    )
  }
  above <- function(threshold) sum(remainder$value > threshold, na.rm = TRUE)
  remainder_text <- "the state's remainder_signed, groups together"
  imputed_text <- "the state's imputed_cases, groups together"
  without_note <- "States whose state cases are 0 or unknown (no published state total); they are left out of the shares across states, not counted as 0."

  dplyr::bind_rows(
    .summary_distribution(
      "no county",
      "state_remainder_share",
      remainder$value,
      fips,
      remainder_text,
      denominator
    ),
    count(
      "no county",
      "states_remainder_negative",
      sum(signed < 0, na.rm = TRUE),
      "States whose signed remainder is below 0, including any whose state cases are 0."
    ),
    count("no county", "states_remainder_share_above_5pct", above(0.05)),
    count("no county", "states_remainder_share_above_10pct", above(0.10)),
    count(
      "no county",
      "states_without_remainder_share",
      sum(is.na(remainder$value)),
      without_note
    ),
    .summary_distribution(
      "imputation",
      "state_imputed_share",
      imputed$value,
      fips,
      imputed_text,
      denominator
    ),
    count(
      "imputation",
      "states_without_imputed_share",
      sum(is.na(imputed$value)),
      without_note
    ),
    series(
      "no county",
      "state_remainder_share_of_state_cases",
      remainder,
      remainder_text
    ),
    series(
      "imputation",
      "state_imputed_share_of_state_cases",
      imputed,
      imputed_text
    )
  )
}

# Each alternative imputation against the default, for the groups together:
# published plus imputed county cases, nationally and in the state that
# changes most. Published cases are the same under every alternative.
.summary_sensitivity <- function(run) {
  step <- "sensitivity"
  value <- function(rows, measure) {
    out <- rows$value[rows$measure == measure & rows$state_fips == "all"]
    if (length(out) == 1) out else NA_real_
  }
  default <- run[run$scenario == "default", , drop = FALSE]
  default_states <- default[default$state_fips != "all", , drop = FALSE]
  default_total <- value(default, "published_cases") +
    value(default, "imputed_cases")
  total_text <- "published_cases + imputed_cases under the scenario, minus the same under the default"
  default_text <- "published_cases + imputed_cases under the default"

  scenarios <- setdiff(unique(run$scenario), "default")
  dplyr::bind_rows(lapply(scenarios, function(scenario) {
    rows <- run[run$scenario == scenario, , drop = FALSE]
    difference <- value(rows, "published_cases") +
      value(rows, "imputed_cases") -
      default_total
    change <- .summary_divide(difference, default_total, scale = 100)
    out <- dplyr::bind_rows(
      .summary_rows(
        step,
        "cases_difference_from_default",
        difference,
        "cases",
        "modelled",
        numerator = total_text,
        note = if (is.na(difference)) {
          "Published or imputed cases are not in the detail table."
        } else {
          NA_character_
        },
        scenario = scenario
      ),
      .summary_rows(
        step,
        "cases_percent_change_from_default",
        change$value,
        "percent",
        "derived",
        numerator = total_text,
        denominator = default_text,
        note = change$note,
        scenario = scenario
      )
    )

    by_state <- rows[rows$state_fips != "all", , drop = FALSE]
    if (nrow(by_state) == 0 || nrow(default_states) == 0) {
      return(out)
    }
    fips <- sort(unique(default_states$state_fips))
    imputed <- .summary_state_values(default_states, "imputed_cases", fips)
    state_total <- .summary_state_values(
      default_states,
      "published_cases",
      fips
    ) +
      imputed
    state_change <- .summary_divide(
      .summary_state_values(by_state, "imputed_cases", fips) - imputed,
      state_total,
      scale = 100
    )
    largest <- which.max(abs(state_change$value))
    # No state is named when no state changes at all.
    changed <- length(largest) == 1 && state_change$value[largest] != 0
    left_out <- sum(is.na(state_change$value))
    dplyr::bind_rows(
      out,
      .summary_rows(
        step,
        "largest_state_percent_change_from_default",
        if (length(largest) == 1) state_change$value[largest] else NA,
        "percent",
        "derived",
        numerator = "the state's imputed_cases under the scenario, minus the same under the default",
        denominator = "the state's published_cases + imputed_cases under the default",
        extreme_state = if (changed) {
          .summary_state_label(fips[largest])
        } else {
          NA_character_
        },
        note = if (length(largest) != 1) {
          "No state has a value."
        } else if (!changed) {
          "No state changes, so none is named."
        } else if (left_out > 0) {
          paste(
            left_out,
            "states with no published or imputed cases under the default are left out."
          )
        } else {
          NA_character_
        },
        scenario = scenario
      )
    )
  }))
}
