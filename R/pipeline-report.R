#' What each pipeline step did to the numbers
#'
#' Counts what [impute_suppressed()], the state remainder and
#' [extrapolate_from_county()] did to one request, as one long table. Internal
#' while the table's shape settles.
#'
#' Every count is within one indicator and year: diagnoses and prevalence are
#' never added together. Sums of published counts, modelled counts (imputed or
#' allocated) and differences are marked in `basis`, so a modelled value is
#' never presented as CDC's. Nothing here estimates a suppressed count: where
#' public data cannot show a quantity, the row has `value = NA` and a `note`.
#'
#' The cap and floor are counted from the estimate before limits (state rate
#' times county population): the `cases_uncapped` column [impute_suppressed()]
#' records, whichever population it used. For input with no such column the
#' estimate is recomputed from the same inputs, which is possible only with
#' CDC's population; with ACS populations those measures are then `NA` with a
#' note. A limit counts only when the estimate is strictly beyond it, so an
#' estimate exactly at the cap is not counted.
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
#'   The report stops if `areas` was not allocated from these `imputed` rows
#'   (different rows, counts or CDC county populations), or was made with
#'   `no_county = "spread"`, `unknown_race = "redistribute"` or `unknown_age =
#'   "redistribute"`, which it cannot account for yet.
#' @param cap,floor,population The values given to [impute_suppressed()]. The
#'   report stops if `cap` and `floor` don't reproduce the imputed counts.
#'   `population` matters only for input with no `cases_uncapped` column, and
#'   for the reasons rows could not be estimated.
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
  remainder <- .no_county_cases(imputed, state, .fetch_county_totals(imputed))
  # The remainder computed here, not the one attached to `areas`: once the
  # check shows they're from the same run they agree, and an allocation saved
  # by an older version may lack `no_county_withheld` (#173).
  if (!is.null(areas)) {
    .check_report_same_run(imputed, areas, remainder, call)
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
    raw_known = "cases_uncapped" %in% names(imputed) || population == "cdc",
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

  # The report accounts for the default allocation only. Cases moved between
  # rows would be counted wrongly: a spread remainder both as the remainder
  # and as allocated cases, and a redistributed Unknown row as neither
  # allocated nor left out (extrapolate_from_county() drops the row and keeps
  # no record of it on its result).
  moved <- function(column) {
    any(dplyr::coalesce(areas[[column]], 0) != 0)
  }
  unsupported <- function(what, argument) {
    cli::cli_abort(
      c(
        "The report can't account for {what} yet.",
        "x" = "{.arg areas} was made with {.code {argument}}.",
        "i" = "Use the {.fn extrapolate_from_county} result with the default
               for that argument."
      ),
      call = call
    )
  }
  if (moved("county_cases_spread")) {
    unsupported(
      "a remainder spread across counties",
      "no_county = \"spread\""
    )
  }
  if (moved("county_cases_unknown")) {
    unsupported(
      "cases of unknown race or age shared across the known groups",
      "unknown_race = \"redistribute\" or unknown_age = \"redistribute\""
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
  # A withheld stratum's published_cases is NA here (#173), and may not be in
  # an allocation saved by an older version, so it isn't compared.
  shown <- is.na(expected$no_county_withheld)
  for (column in c("state_cases", "published_cases", "imputed_cases")) {
    compare <- if (column == "published_cases") shown else TRUE
    bad <- compare & differs(expected[[column]], attached[[column]][rows])
    if (any(bad)) {
      stop_mismatch(
        "{.field {column}} in its {.field no_county_cases} attribute differs
         in {sum(bad)} state strat{?um/a}."
      )
    }
  }

  county_keys <- c("county_fips", .atlas_keys)
  columns <- c(county_keys, "county_cases", "county_population_atlas")
  accounted <- dplyr::bind_rows(
    as.data.frame(areas)[columns],
    as.data.frame(attr(areas, "not_allocated"))[columns]
  )
  accounted <- accounted[!duplicated(.row_key(accounted, county_keys)), ]
  county <- .county_cases(imputed)
  accounted_key <- .row_key(accounted, county_keys)
  county_key <- .row_key(county, county_keys)
  if (!setequal(accounted_key, county_key)) {
    # An Unknown row that was redistributed with nothing to share leaves no
    # trace but its absence.
    absent <- county[!county_key %in% accounted_key, , drop = FALSE]
    if (
      nrow(absent) > 0 &&
        all(absent$race_ethnicity == "Unknown" | absent$age == "Unknown")
    ) {
      unsupported(
        "cases of unknown race or age shared across the known groups",
        "unknown_race = \"redistribute\" or unknown_age = \"redistribute\""
      )
    }
    stop_mismatch("It accounts for different county rows.")
  }
  rows <- match(county_key, accounted_key)
  # The counts, and CDC's county population, which the report compares with
  # the ACS: a stale population would not change any count.
  for (column in c("county_cases", "county_population_atlas")) {
    bad <- differs(county[[column]], accounted[[column]][rows])
    if (any(bad)) {
      stop_mismatch(
        "{.field {column}} differs in {sum(bad)} county row{?s}."
      )
    }
  }
  invisible()
}

# The estimate before limits for each row of `imputed` (the state rate in the
# same indicator, year and stratum, times the county population), with why it
# is missing. Stops if `cap` and `floor` don't turn it into the imputed counts.
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
  # impute_suppressed() records the estimate before limits in
  # `cases_uncapped`, with whichever population it used. Input imputed before
  # that column existed has it recomputed here, which is possible only for
  # CDC's population: the ACS one would need another download.
  recorded <- "cases_uncapped" %in% names(imputed)
  imputed$raw_estimate <- if (recorded) {
    imputed$cases_uncapped
  } else if (population == "cdc") {
    rate * imputed$population
  } else {
    NA_real_
  }
  if (!recorded && population != "cdc") {
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
  if (!options$raw_known && any(by_rate)) {
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
        note = "Not known: `imputed` has no `cases_uncapped` column, and recomputing the estimate for population = \"acs\" would need the ACS county populations."
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
# over state strata with a published state total and a remainder that isn't
# withheld; the others are counted, not treated as 0. A withheld remainder
# (see no_county_cases(), #173) is left out of
# every sum here, not only its own row: a sum that included it, beside the
# other states' figures, would give it back by subtraction.
.report_remainder <- function(remainder) {
  step <- "no county"
  no_total <- is.na(remainder$state_cases)
  withheld <- !no_total & !is.na(remainder$no_county_withheld)
  has_total <- !no_total & !withheld
  with_total <- remainder[has_total, , drop = FALSE]
  left <- with_total$no_county_cases
  # With no published state total here, nothing below is known: an empty sum
  # would be 0, which would read as a published zero.
  known <- any(has_total)
  left_out <- paste(
    c(
      if (any(no_total)) {
        paste(
          sum(no_total),
          "state strata have no published state total and are left out, not counted as 0."
        )
      },
      if (any(withheld)) {
        paste(
          sum(withheld),
          "state strata have a withheld remainder (one county row without a published count, or no CDC county rows to count them) and are left out."
        )
      }
    ),
    collapse = " "
  )
  coverage <- if (!known && !any(withheld)) {
    .report_no_total_note
  } else if (!known && !any(no_total)) {
    .report_withheld_note
  } else if (!known) {
    paste("Unknown, not 0:", left_out)
  } else if (any(!has_total)) {
    left_out
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
      denominator = "state_cases: CDC's state totals, in state strata with a published total and a remainder that isn't withheld",
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
    count("state_strata_without_total", sum(no_total)),
    count("state_strata_remainder_withheld", sum(withheld)),
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

.report_withheld_note <- "Not reported: the remainder of the state strata here is withheld (one county row without a published count, or no CDC county rows to count them)."

# One state's totals and remainder, with the number of its strata that have no
# published total. A single state stratum has one remainder, so its positive
# and negative parts are given only for groups together.
.report_state_remainder <- function(remainder) {
  out <- .report_remainder(remainder)
  keep <- c(
    "state_strata_without_total",
    "state_strata_remainder_withheld",
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
