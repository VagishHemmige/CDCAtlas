# Fitting stratified county estimates to published county totals: the
# method behind impute_suppressed(fit_to = "county_total") (#164 step 2; the
# design is in #179).
#
# For one county, indicator and year, R is the county's published
# unstratified total minus its published stratum cells, and the suppressed
# cells share R in proportion to their seeds: the state-rate estimates
# before the cap (`cases_uncapped`). Fitting modeled estimates to a public
# total is estimation, not recovery, only when R spans two or more suppressed
# cells (AGENTS.md; #169, #176). Every case where the public figures could
# pin down one cell, or where the cells and the total might not cover the
# same people, is therefore left unfitted, with a reason. The reasons are
# checked in a fixed order, structural ones first, so the residual is looked
# at only when nothing else stops the fit, and residuals that are zero,
# negative or otherwise unusable share one generic reason that exposes no
# bound on a cell.

.fit_reasons <- c(
  "race not fitted",
  "county total not available",
  "county total suppressed",
  "strata incomplete or inconsistent",
  "one suppressed cell",
  "a suppressed cell has no seed",
  "fewer than two positive seeds",
  "published total not usable for fitting"
)

.fit_done <- "fitted to county total"

# The stratum labels get_atlas() requests for HIV, from the codes
# .build_payload() sends, so a stratum absent from the whole download still
# counts as missing.
.fit_strata <- function(stratify_by) {
  codes <- switch(
    stratify_by,
    sex = c(602, 603),
    age = .atlas_age_codes("hiv", TRUE),
    race = .atlas_race_codes("hiv", TRUE)
  )
  varvals$name[match(codes, varvals$id)]
}

# CDC's label for HIV's age total (code 650), the age universe a county total
# must have: HIV age groups start at 13 and sum to it.
.fit_universe <- function() {
  varvals$name[match(650, varvals$id)]
}

# Fit the suppressed cells of HIV county data stratified by one variable to
# each county's published total.
#
# `x`: county rows from get_atlas(), stratified by `stratify_by` ("sex",
#   "age" or "race"), before impute_suppressed() drops or fills anything,
#   so eligibility is decided on what CDC returned.
# `seed`: one value per row of `x`, the state-rate estimate before the cap
#   (`cases_uncapped`); NA where there is none.
# `totals`: CDC's unstratified county rows for the same states, diseases and
#   years. Rows for counties not in `x` are ignored.
#
# Returns a data frame with one row per row of `x`, in the same order:
# `cases`, the fitted count on fitted rows and NA elsewhere, and
# `fit_reason`, NA on rows with a published count and otherwise
# `.fit_done` or the first of `.fit_reasons` that applies to the row's
# county, indicator and year. Warns, with counts of groups only, when the
# strata are incomplete or inconsistent and when published strata add up to
# more than the county's total.
.fit_county_total <- function(x, seed, totals, stratify_by) {
  n <- nrow(x)
  out <- data.frame(
    cases = rep(NA_real_, n),
    fit_reason = rep(NA_character_, n)
  )
  cases <- x$cases
  status <- as.character(x$data_status)
  needs_reason <- is.na(cases)
  if (!any(needs_reason)) {
    return(out)
  }
  if (stratify_by == "race") {
    out$fit_reason[needs_reason] <- .fit_reasons[[1]]
    return(out)
  }

  group_cols <- c("fips", "indicator", "year")
  column <- .atlas_strata_columns[[stratify_by]]
  requested <- .fit_strata(stratify_by)
  universe <- .fit_universe()
  # Every column other than the stratification variable must hold its total
  # label, with HIV's age universe.
  fixed <- c(
    race_ethnicity = .atlas_totals$race,
    sex = .atlas_totals$sex,
    age = universe
  )
  fixed <- fixed[names(fixed) != column]
  fixed_ok <- Reduce(
    `&`,
    lapply(names(fixed), \(col) as.character(x[[col]]) %in% fixed[[col]])
  )

  is_total <- as.character(totals$race_ethnicity) %in%
    .atlas_totals$race &
    as.character(totals$sex) %in% .atlas_totals$sex &
    as.character(totals$age) %in% universe
  totals <- totals[is_total, , drop = FALSE]
  total_rows <- split(seq_len(nrow(totals)), .row_key(totals, group_cols))
  total_status <- as.character(totals$data_status)

  group <- .row_key(x, group_cols)
  negative <- 0L
  unfittable <- 0L
  for (rows in split(seq_len(n), group)) {
    if (!any(needs_reason[rows])) {
      next
    }
    reason <- NULL
    total <- total_rows[[group[rows[1]]]]
    if (length(total) != 1) {
      # No row, or more than one: no single published total to fit to.
      reason <- .fit_reasons[[2]]
    } else if (is.na(totals$cases[total])) {
      reason <- if (total_status[total] %in% "Data suppressed") {
        .fit_reasons[[3]]
      } else {
        .fit_reasons[[2]]
      }
    } else if (total_status[total] %in% "Data suppressed") {
      # A count marked suppressed: contradictory, so not usable.
      reason <- .fit_reasons[[2]]
    }

    labels <- as.character(x[[column]][rows])
    has_count <- !is.na(cases[rows])
    marked <- status[rows] %in% "Data suppressed"
    if (is.null(reason)) {
      complete <- setequal(labels, requested) &&
        !anyDuplicated(labels) &&
        all(fixed_ok[rows])
      consistent <- !any(has_count & marked) && !any(!has_count & !marked)
      if (!complete || !consistent) {
        unfittable <- unfittable + 1L
        reason <- .fit_reasons[[4]]
      }
    }

    cells <- rows[!has_count]
    if (is.null(reason) && length(cells) == 1) {
      reason <- .fit_reasons[[5]]
    }
    if (is.null(reason) && anyNA(seed[cells])) {
      reason <- .fit_reasons[[6]]
    }
    if (is.null(reason) && sum(seed[cells] > 0) < 2) {
      reason <- .fit_reasons[[7]]
    }
    if (is.null(reason)) {
      residual <- totals$cases[total] - sum(cases[rows[has_count]])
      if (residual <= 0) {
        negative <- negative + (residual < 0)
        reason <- .fit_reasons[[8]]
      }
    }

    if (is.null(reason)) {
      out$cases[cells] <- residual * seed[cells] / sum(seed[cells])
      out$fit_reason[cells] <- .fit_done
    } else {
      out$fit_reason[rows[!has_count]] <- reason
    }
  }

  if (unfittable > 0) {
    cli::cli_warn(
      c(
        "In {unfittable} group{?s} of county, indicator and year, the strata
         are incomplete or inconsistent, so {?it was/they were} not fitted.",
        "i" = "Check that each requested stratum appears once, the other
               columns match the requested population (HIV, ages 13 and
               over), and each count agrees with its data status."
      ),
      class = "cdcatlas_unfittable_county_strata"
    )
  }
  if (negative > 0) {
    cli::cli_warn(
      c(
        "In {negative} group{?s} of county, indicator and year, the published
         strata add up to more than the county's published total, so
         {?it was/they were} not fitted.",
        "i" = "The stratified and unstratified county data may come from
               different CDC releases."
      ),
      class = "cdcatlas_negative_county_residual"
    )
  }
  out
}
