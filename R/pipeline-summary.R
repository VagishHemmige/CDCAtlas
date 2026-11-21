#' The figures the articles plot or quote, from the detail table
#'
#' Reduces a detail table, which is [.pipeline_report()] output for one or
#' more runs (with `by_state = TRUE` and a `scenario` column), to a few
#' hundred rows per year. Every figure is copied from, or computed from,
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
