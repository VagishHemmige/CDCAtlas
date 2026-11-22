# Shared downloads for the live tests (test-live-*.R), so each county
# request and pipeline run is made at most once per test run. Nothing here
# makes a request until a live test calls it.

skip_unless_census <- function() {
  skip_if_not_installed("tidycensus")
  skip_if(
    Sys.getenv("CENSUS_API_KEY") == "",
    "No Census API key (CENSUS_API_KEY)."
  )
}

live_counties <- new.env()
live_states <- new.env()
live_runs <- new.env()

# The state a county payload asks for (county requests are split by state),
# as "Name (FIPS)"; NA for any other payload.
live_payload_state <- function(payload) {
  ids <- as.numeric(strsplit(payload$VariableIDs, ",")[[1]])
  counties <- lookup[["counties"]]
  fips <- unique(substr(counties$fips[counties$geo_id %in% ids], 1, 2))
  if (length(fips) != 1) {
    return(NA_character_)
  }
  states <- lookup[["state"]]
  paste0(states$name[match(fips, states$fips)], " (", fips, ")")
}

# One uncached request to CDC. A failure keeps its own message and also
# carries the state the request was for (`live_state`), so a county download
# that fails part-way can say where.
live_request <- function(payload) {
  tryCatch(
    .request_atlas_internal(payload),
    error = function(e) {
      e$live_state <- live_payload_state(payload)
      stop(e)
    }
  )
}

# Runs `code` once per `key` in `store`, keeping its value, its warnings, or
# its error. Later calls get the same value and see the same warnings, or the
# same error, without running `code` again, so a failed download is not
# retried by every test that needs it.
live_once <- function(store, key, code) {
  if (is.null(store[[key]])) {
    warnings <- character()
    value <- tryCatch(
      withCallingHandlers(
        code,
        warning = function(w) {
          warnings <<- c(warnings, cli::ansi_strip(conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )
    store[[key]] <- list(value = value, warnings = warnings)
  }
  run <- store[[key]]
  for (w in run$warnings) {
    warning(w, call. = FALSE)
  }
  if (inherits(run$value, "error")) {
    stop(run$value)
  }
  run$value
}

# get_atlas(disease, "county", year, stratify_by), fresh from CDC (bypassing
# the on-disk cache) the first time, then from memory.
live_county <- function(year, stratify_by = NULL, disease = "hiv") {
  local_mocked_bindings(.request_atlas = live_request)
  live_once(
    live_counties,
    paste(disease, year, stratify_by %||% "none"),
    suppressMessages(get_atlas(
      disease,
      "county",
      year,
      stratify_by = stratify_by
    ))
  )
}

# The indicators of `disease` CDC has released for a year and stratification,
# from one small state-level request (made once per run): the requested
# indicators that have state rows. Never none: nothing is inferred from a
# response until it is shown to be an answer to the request.
#
# An indicator counts as unreleased only on positive evidence: the state
# response holds only indicators that were asked for, at least one other of
# them has rows, and this one has none, as for HIV 2025, which came with
# diagnoses but not prevalence (#149). get_atlas() warns about that; the
# warning is expected here and dropped.
#
# Everything else stops with the year and stratification:
# - the state request fails, "CDC AtlasPlus returned no data" included (the
#   package says that whenever a response has no `sourcedata`, whatever the
#   reason);
# - the response has an indicator that wasn't asked for (chlamydia rows for
#   an HIV request are a wrong response, not a sign HIV is unreleased);
# - the response has none of the indicators asked for.
#
# Two consequences, both deliberate: if CDC ever has no state rows at all for
# the disease in the package's latest year, a test using this fails rather
# than moving to an earlier year, and a person needs to look. And for a
# disease with a single indicator (chlamydia, say), its absence can never be
# told from a wrong or empty response, so there is no stepping back at all:
# an unreleased year always fails. The latest-years check uses HIV, which has
# two.
live_released_indicators <- function(
  year,
  stratify_by = NULL,
  disease = "hiv"
) {
  local_mocked_bindings(.request_atlas = live_request)
  state <- live_without_unreleased_warning(tryCatch(
    live_once(
      live_states,
      paste(disease, year, stratify_by %||% "none"),
      suppressMessages(get_atlas(
        disease,
        "state",
        year,
        stratify_by = stratify_by
      ))
    ),
    error = function(e) {
      rlang::abort(
        c(
          paste0(
            "Couldn't establish whether CDC has released ",
            disease,
            " ",
            year,
            ", ",
            if (is.null(stratify_by)) {
              "unstratified"
            } else {
              paste("by", stratify_by)
            },
            ": the state request failed."
          ),
          "i" = paste0(
            "A failed request is not evidence that the year is unreleased, ",
            "so an earlier year is not used instead."
          )
        ),
        parent = e
      )
    }
  ))
  indicators <- .disease_indicators()
  requested <- indicators$indicator[indicators$disease == disease]
  returned <- unique(as.character(state$indicator))
  what <- paste0(
    disease,
    " ",
    year,
    ", ",
    if (is.null(stratify_by)) "unstratified" else paste("by", stratify_by)
  )
  not_relevant <- "so nothing is inferred from it and an earlier year is not used instead."
  unexpected <- setdiff(returned, requested)
  if (length(unexpected) > 0) {
    rlang::abort(c(
      paste0(
        "Couldn't establish whether CDC has released ",
        what,
        ": the state response has indicators that weren't asked for."
      ),
      "x" = paste0("Returned: ", paste(sort(returned), collapse = ", "), "."),
      "x" = paste0("Asked for: ", paste(requested, collapse = ", "), "."),
      "i" = paste("The response doesn't match the request,", not_relevant)
    ))
  }
  released <- intersect(requested, returned)
  if (length(released) == 0) {
    rlang::abort(c(
      paste0(
        "Couldn't establish whether CDC has released ",
        what,
        ": the state response has none of the indicators asked for."
      ),
      "x" = paste0("Asked for: ", paste(requested, collapse = ", "), "."),
      "i" = paste("The response has no rows for the request,", not_relevant)
    ))
  }
  released
}

live_without_unreleased_warning <- function(code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      if (
        grepl("CDC returned no rows for", conditionMessage(w), fixed = TRUE)
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# live_county() for a year CDC may have released only in part. It never gives
# up on a year: live_released_indicators() must first show which indicators
# CDC has released for states (and stops if it can't), then the county
# download must work and must have rows for every one of those. The result
# can lack an indicator only when the state response lacked it too, beside
# another that was there. A county download is one request per
# state and stops at the first that fails, so its failing, "returned no data"
# included, says nothing about the year as a whole: that stops with the year,
# stratification and state rather than letting a test fall back to an earlier
# year.
live_county_if_released <- function(year, stratify_by = NULL, disease = "hiv") {
  released <- live_released_indicators(year, stratify_by, disease)
  what <- paste0(
    disease,
    " ",
    year,
    ", ",
    if (is.null(stratify_by)) "unstratified" else paste("by", stratify_by)
  )
  county <- live_without_unreleased_warning(tryCatch(
    live_county(year, stratify_by, disease),
    error = function(e) {
      rlang::abort(
        c(
          paste0("The county download for ", what, " failed."),
          "x" = paste0(
            "Failing request: ",
            if (is.null(e$live_state) || is.na(e$live_state)) {
              "state not known"
            } else {
              e$live_state
            },
            "."
          ),
          "i" = paste0(
            "CDC has state rows for ",
            paste(released, collapse = " and "),
            " in ",
            year,
            ", so the year is released and an earlier year is not used ",
            "instead."
          )
        ),
        parent = e
      )
    }
  ))
  absent <- setdiff(released, unique(as.character(county$indicator)))
  if (length(absent) > 0) {
    rlang::abort(c(
      paste0(
        "The county download for ",
        what,
        " has no rows for ",
        paste(absent, collapse = " and "),
        "."
      ),
      "i" = paste0(
        "CDC has state rows for it in ",
        year,
        ", so an earlier year is not used instead."
      )
    ))
  }
  county
}

# Expects that CDC gave no rate where it gave no count (#157): every row of a
# county response from get_atlas() marked "Data suppressed" with `cases` NA
# also has NA rate100000, lowerci_rate and upperci_rate. impute_suppressed()
# fills in `cases` on those rows and leaves the rate columns alone, so a rate
# there would sit beside an estimate it doesn't describe. A failure names the
# rows (the first `max_shown`). A response with no such row fails too: the
# check would say nothing about it. One expectation per call. Returns the
# number of rows checked.
expect_no_rate_on_suppressed <- function(
  county,
  label = "county response",
  max_shown = 20
) {
  rate_columns <- c("rate100000", "lowerci_rate", "upperci_rate")
  needed <- c(
    rate_columns,
    c("fips", "indicator", "year", "data_status", "cases"),
    c("race_ethnicity", "sex", "age")
  )
  absent <- setdiff(needed, names(county))
  if (length(absent) > 0) {
    fail(paste0(label, ": no column ", paste(absent, collapse = ", "), "."))
    return(invisible(0L))
  }

  suppressed <- is.na(county$cases) &
    as.character(county$data_status) %in% "Data suppressed"
  if (!any(suppressed)) {
    fail(paste0(
      label,
      ": no row is marked \"Data suppressed\" with no count, so nothing was ",
      "checked."
    ))
    return(invisible(0L))
  }

  rates <- as.data.frame(county[rate_columns])
  has_rate <- suppressed & rowSums(!is.na(rates)) > 0
  rows <- county[has_rate, , drop = FALSE]
  given <- vapply(
    which(has_rate),
    function(i) {
      values <- unlist(rates[i, ])
      paste0(
        rate_columns[!is.na(values)],
        " = ",
        values[!is.na(values)],
        collapse = ", "
      )
    },
    character(1)
  )
  described <- paste0(
    "* ",
    as.character(rows$fips),
    " | ",
    as.character(rows$indicator),
    " | ",
    as.character(rows$year),
    " | ",
    as.character(rows$race_ethnicity),
    ", ",
    as.character(rows$sex),
    ", ",
    as.character(rows$age),
    ": ",
    given,
    recycle0 = TRUE
  )
  more <- length(described) - max_shown
  expect(
    !any(has_rate),
    paste0(
      label,
      ": ",
      sum(has_rate),
      " of ",
      sum(suppressed),
      " suppressed rows with no count have a rate or a rate bound ",
      "(fips | indicator | year | stratum):\n",
      paste(utils::head(described, max_shown), collapse = "\n"),
      if (more > 0) paste0("\n... and ", more, " more")
    )
  )
  invisible(sum(suppressed))
}

# Counts of counties by how many of their stratum cells CDC suppressed, from
# two get_atlas() county responses for the same year: `stratified` (by one of
# race, sex and age) and `unstratified`. The premise of fitting stratified
# estimates to a county's published total (#164) is that a county with a
# published total never has exactly one suppressed stratum cell.
#
# COUNTS ONLY. One row per indicator and year, every other column a whole
# number of counties (or `strata`, the number of strata). Nothing here
# returns, prints, stores or attaches which counties are in any group, and a
# county's total minus its published strata is never computed: the published
# strata are summed and that sum is only compared with the published total
# (equal, less, more). Makes no request.
#
# A stratum cell is one row of `stratified`: a county, indicator, year and
# stratum value ("Unknown" included, where a disease has it). The strata of
# an indicator and year are the values that appear in any county's rows.
# - published: a count, and not marked "Data suppressed";
# - suppressed: marked "Data suppressed" with no count, as impute_suppressed()
#   identifies the rows it fills in;
# - anything else ("Data not available" with no count, a count marked
#   suppressed, an unknown status) is neither, and is counted in
#   `other_cells`;
# - a stratum with no row at all for a county is absent, not suppressed, and
#   is counted in `incomplete_strata`.
# The unstratified count is classified the same way (`total_published`,
# `total_suppressed`, `total_other`).
#
# Counties are matched on fips, indicator and year. Those in one response
# only are counted (`counties_stratified_only`, `counties_unstratified_only`)
# and left out. The rest are `counties_in_both`, and every column from
# `none_suppressed` on is about those of them with a published unstratified
# count:
# - `none_suppressed`, `one_suppressed`, `two_or_more_suppressed`: by the
#   number of suppressed cells, whatever the other cells are. They add up to
#   `total_published`.
# - `incomplete_strata`: fewer rows than the indicator has strata.
# - `other_cells`: at least one cell that is neither published nor
#   suppressed.
# - `all_published`: a row for every stratum, each published; and of those,
#   `sum_equal`, `sum_less`, `sum_more`: the strata add up to the
#   unstratified count, to less, or to more.
# - `published_exceed`: the published cells alone add up to more than the
#   unstratified count, however many cells are suppressed.
#
# Stops, giving numbers of rows only, if a county is in a response twice, so
# no county can be counted twice.
count_suppressed_strata <- function(stratified, unstratified) {
  needed <- c(
    "fips",
    "indicator",
    "year",
    "data_status",
    "cases",
    unname(.atlas_strata_columns)
  )
  for (arg in c("stratified", "unstratified")) {
    absent <- setdiff(needed, names(get(arg)))
    if (length(absent) > 0) {
      rlang::abort(paste0(
        "`",
        arg,
        "` has no column ",
        paste(absent, collapse = ", "),
        "."
      ))
    }
  }
  by <- .atlas_stratify_by(stratified)
  if (length(by) != 1) {
    rlang::abort(paste0(
      "`stratified` must be stratified by one of race, sex and age; it is ",
      if (length(by) == 0) "not stratified" else paste("by", toString(by)),
      "."
    ))
  }
  if (length(.atlas_stratify_by(unstratified)) > 0) {
    rlang::abort(paste0(
      "`unstratified` must not be stratified; it is by ",
      toString(.atlas_stratify_by(unstratified)),
      "."
    ))
  }

  # One row per cell, with nothing but what the counts need.
  cells <- function(rows) {
    status <- as.character(rows$data_status)
    cases <- as.numeric(rows$cases)
    data.frame(
      group = .row_key(rows, c("indicator", "year")),
      county = .row_key(rows, c("fips", "indicator", "year")),
      cases = cases,
      published = !is.na(cases) & !status %in% "Data suppressed",
      suppressed = is.na(cases) & status %in% "Data suppressed"
    )
  }
  total <- cells(unstratified)
  strata <- cells(stratified)
  strata$stratum <- as.character(stratified[[.atlas_strata_columns[[by]]]])
  twice <- sum(duplicated(total$county))
  if (twice > 0) {
    rlang::abort(paste0(
      "`unstratified` has ",
      twice,
      " repeated county rows (same fips, indicator and year), so a county ",
      "would be counted twice."
    ))
  }
  twice <- sum(duplicated(strata[c("county", "stratum")]))
  if (twice > 0) {
    rlang::abort(paste0(
      "`stratified` has ",
      twice,
      " repeated stratum rows (same fips, indicator, year and stratum), so ",
      "a county would be counted twice."
    ))
  }

  groups <- sort(union(total$group, strata$group))
  out <- lapply(groups, function(group) {
    total <- total[total$group == group, , drop = FALSE]
    strata <- strata[strata$group == group, , drop = FALSE]
    n_strata <- length(unique(strata$stratum))
    # Per county: its rows, its published and suppressed cells, and the sum
    # of its published cells only, which is never subtracted from anything.
    sums <- rowsum(
      cbind(
        rows = rep(1, nrow(strata)),
        n_published = as.numeric(strata$published),
        n_suppressed = as.numeric(strata$suppressed),
        published_sum = ifelse(strata$published, strata$cases, 0)
      ),
      strata$county
    )
    county <- data.frame(county = rownames(sums), sums, row.names = NULL)
    in_both <- merge(total, county, by = "county")
    if (nrow(in_both) != length(intersect(total$county, county$county))) {
      rlang::abort("Matching the two responses changed the number of rows.")
    }
    with_total <- in_both[in_both$published, , drop = FALSE]
    complete <- with_total$rows == n_strata
    all_published <- complete & with_total$n_published == n_strata
    # Counts are whole numbers; the tolerance only allows for storage.
    same <- abs(with_total$published_sum - with_total$cases) < 1e-8
    less <- !same & with_total$published_sum < with_total$cases
    more <- !same & with_total$published_sum > with_total$cases
    key <- strsplit(group, "\r", fixed = TRUE)[[1]]
    data.frame(
      indicator = key[[1]],
      year = key[[2]],
      stratified_by = by,
      strata = n_strata,
      counties_stratified_only = sum(!county$county %in% total$county),
      counties_unstratified_only = sum(!total$county %in% county$county),
      counties_in_both = nrow(in_both),
      total_suppressed = sum(in_both$suppressed),
      total_other = sum(!in_both$suppressed & !in_both$published),
      total_published = nrow(with_total),
      none_suppressed = sum(with_total$n_suppressed == 0),
      one_suppressed = sum(with_total$n_suppressed == 1),
      two_or_more_suppressed = sum(with_total$n_suppressed >= 2),
      incomplete_strata = sum(!complete),
      other_cells = sum(
        with_total$n_published + with_total$n_suppressed < with_total$rows
      ),
      all_published = sum(all_published),
      sum_equal = sum(all_published & same),
      sum_less = sum(all_published & less),
      sum_more = sum(all_published & more),
      published_exceed = sum(more)
    )
  })
  out <- do.call(rbind, out)
  counts <- setdiff(names(out), c("indicator", "year", "stratified_by"))
  out[counts] <- lapply(out[counts], as.integer)
  rownames(out) <- NULL
  out
}

# One line per row of count_suppressed_strata()'s result, for message(): every
# count, in words. `label` says which responses were compared.
describe_suppressed_strata <- function(counts, label) {
  n <- function(column) format(counts[[column]], big.mark = ",", trim = TRUE)
  paste0(
    label,
    ", ",
    counts$indicator,
    " ",
    counts$year,
    " by ",
    counts$stratified_by,
    " (",
    n("strata"),
    " strata). Counties in both responses: ",
    n("counties_in_both"),
    " (left out: ",
    n("counties_stratified_only"),
    " only stratified, ",
    n("counties_unstratified_only"),
    " only unstratified); unstratified count published in ",
    n("total_published"),
    ", suppressed in ",
    n("total_suppressed"),
    ", neither in ",
    n("total_other"),
    ". Of those with it published, suppressed stratum cells: none ",
    n("none_suppressed"),
    ", exactly one ",
    n("one_suppressed"),
    ", two or more ",
    n("two_or_more_suppressed"),
    "; missing a stratum row ",
    n("incomplete_strata"),
    "; with a cell neither published nor suppressed ",
    n("other_cells"),
    ". Every stratum published: ",
    n("all_published"),
    " (strata sum equal to the unstratified count ",
    n("sum_equal"),
    ", less ",
    n("sum_less"),
    ", more ",
    n("sum_more"),
    "). Published strata alone exceed the unstratified count: ",
    n("published_exceed"),
    "."
  )
}

# For each of `indicators`, the most recent year with rows for it among
# `responses`, a list of get_atlas() results named by year. NA when no
# response has rows for the indicator.
latest_year_with_rows <- function(responses, indicators) {
  years <- sort(as.numeric(names(responses)), decreasing = TRUE)
  out <- rlang::set_names(rep(NA_real_, length(indicators)), indicators)
  for (indicator in indicators) {
    for (year in years) {
      response <- responses[[as.character(year)]]
      if (any(as.character(response$indicator) == indicator)) {
        out[[indicator]] <- year
        break
      }
    }
  }
  out
}

# Full default pipeline on real data: live_county() -> impute_suppressed() ->
# extrapolate_from_county(), every option at its default, as HOPE runs it. Each
# run is made once and shared by the tests that use it. `warnings` includes
# warnings from the county download, even if an earlier test made it.
live_pipeline <- function(
  year,
  stratify_by = NULL,
  to = "tract",
  disease = "hiv"
) {
  skip_unless_census()
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  run <- NULL
  warnings <- capture_warnings(
    run <- live_once(
      live_runs,
      paste(disease, year, stratify_by %||% "none", to),
      suppressMessages({
        county <- live_county(year, stratify_by, disease)
        imputed <- impute_suppressed(county)
        list(
          imputed = imputed,
          areas = extrapolate_from_county(imputed, to = to)
        )
      })
    )
  )
  c(run, list(warnings = cli::ansi_strip(warnings)))
}
