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

# get_atlas(disease, "state", year, stratify_by), fresh from CDC the first
# time, then from memory: the one small state request
# live_released_indicators() makes, kept so the state rows can be used again
# without another request.
live_state <- function(year, stratify_by = NULL, disease = "hiv") {
  local_mocked_bindings(.request_atlas = live_request)
  live_once(
    live_states,
    paste(disease, year, stratify_by %||% "none"),
    suppressMessages(get_atlas(
      disease,
      "state",
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
    live_state(year, stratify_by, disease),
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

# `x` as a comma-separated list, or "none".
or_none <- function(x) if (length(x) == 0) "none" else toString(x)

# Counts of counties by how many of their stratum cells CDC suppressed, from
# two get_atlas() county responses for the same year: `stratified`, asked for
# by `stratify_by` (one of "race", "sex", "age"), and `unstratified`.
# `strata` is the stratum labels that request asks for. The counts monitor
# CDC's responses (#164): they say how often each pattern occurs, not whether
# any particular cell could be worked out.
#
# COUNTS ONLY. One row per indicator and year, every other column a whole
# number of counties (or `strata`, the number of strata). Nothing here
# returns, prints, stores or attaches which counties are in any group.
# NO SUBTRACTION. No count is ever subtracted from another, for any county:
# the published strata are added up, and that sum is compared with the
# published unstratified count only by `==`, `<` and `>`. A test checks that
# this function contains no subtraction at all. Makes no request.
#
# What was asked for is checked against what came back: `stratified` must be
# stratified by `stratify_by` and nothing else, and the stratum labels of
# each indicator and year in it must be exactly `strata`. Otherwise a
# response by another variable could pass for the one asked for, or a
# stratum absent from every county could go unnoticed and leave counties
# looking complete. Both stop.
#
# A stratum cell is one row of `stratified`: a county, indicator, year and
# stratum value ("Unknown" included, where a disease has it).
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
# - `incomplete_strata`: fewer rows than `strata`.
# - `other_cells`: at least one cell that is neither published nor
#   suppressed.
# - `all_published`: a row for every stratum, each published; and of those
#   only, `sum_equal`, `sum_less`, `sum_more`: the strata add up to the
#   unstratified count, to less, or to more.
# - `published_exceed`: the published cells alone add up to more than the
#   unstratified count, however many cells are suppressed.
#
# Stops, giving numbers of rows only, if a county is in a response twice, so
# no county can be counted twice.
count_suppressed_strata <- function(
  stratified,
  unstratified,
  stratify_by,
  strata
) {
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
  stratify_by <- rlang::arg_match0(stratify_by, names(.atlas_strata_columns))
  by <- .atlas_stratify_by(stratified)
  if (!identical(by, stratify_by)) {
    rlang::abort(paste0(
      "`stratified` must be stratified by ",
      stratify_by,
      " only, as asked for; it is ",
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
  if (!is.character(strata) || length(strata) == 0 || anyNA(strata)) {
    rlang::abort("`strata` must be the stratum labels asked for.")
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
  cell <- cells(stratified)
  cell$stratum <- as.character(stratified[[.atlas_strata_columns[[by]]]])
  twice <- sum(duplicated(total$county))
  if (twice > 0) {
    rlang::abort(paste0(
      "`unstratified` has ",
      twice,
      " repeated county rows (same fips, indicator and year), so a county ",
      "would be counted twice."
    ))
  }
  twice <- sum(duplicated(cell[c("county", "stratum")]))
  if (twice > 0) {
    rlang::abort(paste0(
      "`stratified` has ",
      twice,
      " repeated stratum rows (same fips, indicator, year and stratum), so ",
      "a county would be counted twice."
    ))
  }
  # The strata each indicator and year came back with are the ones asked for.
  for (group in unique(cell$group)) {
    returned <- unique(cell$stratum[cell$group == group])
    if (!setequal(returned, strata)) {
      rlang::abort(c(
        paste0(
          "The ",
          stratify_by,
          " strata of ",
          gsub("\r", " ", group, fixed = TRUE),
          " are not the ones asked for."
        ),
        "x" = paste0(
          "Asked for and absent from every county: ",
          or_none(setdiff(strata, returned)),
          "."
        ),
        "x" = paste0(
          "Returned and not asked for: ",
          or_none(setdiff(returned, strata)),
          "."
        )
      ))
    }
  }
  n_strata <- length(unique(strata))

  groups <- sort(union(total$group, cell$group))
  out <- lapply(groups, function(group) {
    total <- total[total$group == group, , drop = FALSE]
    cell <- cell[cell$group == group, , drop = FALSE]
    # Per county: its rows, its published and suppressed cells, and the sum
    # of its published cells, which is only ever compared, never subtracted.
    sums <- rowsum(
      cbind(
        rows = rep(1, nrow(cell)),
        n_published = as.numeric(cell$published),
        n_suppressed = as.numeric(cell$suppressed),
        published_sum = ifelse(cell$published, cell$cases, 0)
      ),
      cell$county
    )
    county <- data.frame(county = rownames(sums), sums, row.names = NULL)
    in_both <- merge(total, county, by = "county")
    if (nrow(in_both) != length(intersect(total$county, county$county))) {
      rlang::abort("Matching the two responses changed the number of rows.")
    }
    with_total <- in_both[in_both$published, , drop = FALSE]
    complete <- with_total$rows == n_strata
    all_published <- complete & with_total$n_published == n_strata
    # Counts are whole numbers, so they are compared directly. The three-way
    # comparison is of fully published counties only.
    full <- with_total[all_published, , drop = FALSE]
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
      all_published = nrow(full),
      sum_equal = sum(full$published_sum == full$cases),
      sum_less = sum(full$published_sum < full$cases),
      sum_more = sum(full$published_sum > full$cases),
      published_exceed = sum(with_total$published_sum > with_total$cases)
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

# Counts of state strata by how many of their county rows CDC suppressed,
# from one get_atlas() county response `county` (stratified or not) and,
# optionally, the matching state response `state`. A state stratum is a
# state, indicator, year and stratum: for unstratified data, the state as a
# whole. Like count_suppressed_strata(), a monitor of CDC's responses (#164):
# it says how often a state stratum has exactly one suppressed county row,
# not whether that row could be worked out.
#
# COUNTS ONLY: one row per indicator and year, whole numbers of state strata,
# no state or county named, returned or stored. NO SUBTRACTION, and no counts
# are added up either: only rows are counted. Makes no request.
#
# What was asked for is checked against what came back, as in
# count_suppressed_strata(): `county`, and `state` when given, must be for
# `year` only and stratified by `stratify_by` only (NULL: not stratified),
# with every stratum label one of `strata`, the labels that request asks for
# (needed when `stratify_by` is given).
# Otherwise a response for another year or by another variable could pass
# for the one asked for. Both stop.
#
# A county row is suppressed when it is marked "Data suppressed" with no
# count. A county belongs to the state its FIPS code starts with.
# - `state_strata`: the state strata that have county rows.
# - With `state`: `state_total_published` of them have a state row with a
#   count not marked "Data suppressed", `state_total_other` a state row
#   without, and `no_state_row` none. Only the first are counted further.
#   Without `state` these three are NA and every state stratum is counted.
# - `counted`, and of those `none_suppressed`, `one_suppressed` and
#   `two_or_more_suppressed`: by the number of suppressed county rows.
#
# Stops, giving numbers of rows only, if a county or state row is repeated.
count_suppressed_counties <- function(
  county,
  state = NULL,
  year,
  stratify_by = NULL,
  strata = NULL
) {
  keys <- c("indicator", "year", unname(.atlas_strata_columns))
  needed <- c("fips", "data_status", "cases", keys)
  for (arg in c("county", if (!is.null(state)) "state")) {
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
  fips <- as.character(county$fips)
  if (anyNA(fips) || any(nchar(fips) != 5)) {
    rlang::abort("`county` must be county rows, with 5-digit `fips`.")
  }
  twice <- sum(duplicated(.row_key(county, c("fips", keys))))
  if (twice > 0) {
    rlang::abort(paste0(
      "`county` has ",
      twice,
      " repeated rows (same fips, indicator, year and stratum), so a county ",
      "would be counted twice."
    ))
  }
  rows <- data.frame(
    group = .row_key(county, c("indicator", "year")),
    stratum = paste(substr(fips, 1, 2), .row_key(county, keys), sep = "\r"),
    suppressed = is.na(as.numeric(county$cases)) &
      as.character(county$data_status) %in% "Data suppressed"
  )
  if (!is.null(state)) {
    state_fips <- as.character(state$fips)
    if (anyNA(state_fips) || any(nchar(state_fips) != 2)) {
      rlang::abort("`state` must be state rows, with 2-digit `fips`.")
    }
    state_stratum <- paste(state_fips, .row_key(state, keys), sep = "\r")
    twice <- sum(duplicated(state_stratum))
    if (twice > 0) {
      rlang::abort(paste0(
        "`state` has ",
        twice,
        " repeated rows (same fips, indicator, year and stratum), so a ",
        "state would be counted twice."
      ))
    }
    state_published <- !is.na(as.numeric(state$cases)) &
      !as.character(state$data_status) %in% "Data suppressed"
  }
  check_counted_request(county, state, year, stratify_by, strata)

  out <- lapply(sort(unique(rows$group)), function(group) {
    rows <- rows[rows$group == group, , drop = FALSE]
    n_suppressed <- rowsum(as.numeric(rows$suppressed), rows$stratum)
    strata <- rownames(n_suppressed)
    n_suppressed <- as.vector(n_suppressed)
    if (is.null(state)) {
      counted <- rep(TRUE, length(strata))
      totals <- rep(NA_integer_, 3)
    } else {
      at <- match(strata, state_stratum)
      counted <- !is.na(at) & state_published[at]
      totals <- c(sum(counted), sum(!is.na(at) & !counted), sum(is.na(at)))
    }
    key <- strsplit(group, "\r", fixed = TRUE)[[1]]
    data.frame(
      indicator = key[[1]],
      year = key[[2]],
      stratified_by = or_none(.atlas_stratify_by(county)),
      state_strata = length(strata),
      state_total_published = totals[[1]],
      state_total_other = totals[[2]],
      no_state_row = totals[[3]],
      counted = sum(counted),
      none_suppressed = sum(counted & n_suppressed == 0),
      one_suppressed = sum(counted & n_suppressed == 1),
      two_or_more_suppressed = sum(counted & n_suppressed >= 2)
    )
  })
  out <- do.call(rbind, out)
  counts <- setdiff(names(out), c("indicator", "year", "stratified_by"))
  out[counts] <- lapply(out[counts], as.integer)
  rownames(out) <- NULL
  out
}

# One line per row of count_suppressed_counties()'s result, for message().
describe_suppressed_counties <- function(counts, label) {
  n <- function(column) format(counts[[column]], big.mark = ",", trim = TRUE)
  paste0(
    label,
    ", ",
    counts$indicator,
    " ",
    counts$year,
    ifelse(
      counts$stratified_by == "none",
      ", unstratified",
      paste0(" by ", counts$stratified_by)
    ),
    ". State strata with county rows: ",
    n("state_strata"),
    ifelse(
      is.na(counts$state_total_published),
      " (state totals not used: every state stratum is counted)",
      paste0(
        " (state total published in ",
        n("state_total_published"),
        ", not published in ",
        n("state_total_other"),
        ", no state row for ",
        n("no_state_row"),
        "; only the first are counted)"
      )
    ),
    ". Of the ",
    n("counted"),
    " counted, suppressed county rows: none ",
    n("none_suppressed"),
    ", exactly one ",
    n("one_suppressed"),
    ", two or more ",
    n("two_or_more_suppressed"),
    "."
  )
}

# Stops unless `county`, and `state` when given, are for `year` only and
# stratified by `stratify_by` only (NULL: not stratified), checked on every
# row: each row has a year, and each row's label is one of `strata` for
# `stratify_by` and the total for the other variables. Used by
# count_suppressed_counties(); the messages give years, variables and numbers
# of rows only.
check_counted_request <- function(
  county,
  state,
  year,
  stratify_by,
  strata,
  call = rlang::caller_env()
) {
  if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
    rlang::abort("`year` must be one year.", call = call)
  }
  if (!is.null(stratify_by)) {
    stratify_by <- rlang::arg_match0(
      stratify_by,
      names(.atlas_strata_columns),
      error_call = call
    )
    if (!is.character(strata) || length(strata) == 0 || anyNA(strata)) {
      rlang::abort(
        "`strata` must be the stratum labels the request asks for.",
        call = call
      )
    }
  }
  for (arg in c("county", if (!is.null(state)) "state")) {
    x <- get(arg)
    if (anyNA(x$year)) {
      rlang::abort(
        paste0(
          "`",
          arg,
          "` has ",
          sum(is.na(x$year)),
          " rows with no year, so they can't be matched to the year asked ",
          "for."
        ),
        call = call
      )
    }
    years <- sort(unique(as.character(x$year)))
    if (!identical(years, as.character(year))) {
      rlang::abort(
        paste0(
          "`",
          arg,
          "` must be for ",
          year,
          " only, as asked for; it has ",
          if (length(years) == 0) "no rows" else toString(years),
          "."
        ),
        call = call
      )
    }
    by <- .atlas_stratify_by(x)
    asked <- if (is.null(stratify_by)) character() else stratify_by
    if (!identical(by, asked)) {
      rlang::abort(
        paste0(
          "`",
          arg,
          "` must ",
          if (is.null(stratify_by)) {
            "not be stratified"
          } else {
            paste0("be stratified by ", stratify_by, " only")
          },
          ", as asked for; it is ",
          if (length(by) == 0) "not stratified" else paste("by", toString(by)),
          "."
        ),
        call = call
      )
    }
    # Row by row, since a response is called stratified when any one row
    # has a subgroup label: a total row among subgroup rows would match no
    # stratum and drop out unseen.
    for (s in names(.atlas_strata_columns)) {
      labels <- as.character(x[[.atlas_strata_columns[[s]]]])
      odd <- if (identical(s, stratify_by)) {
        !labels %in% strata
      } else {
        !labels %in% .atlas_totals[[s]]
      }
      if (any(odd)) {
        rlang::abort(
          paste0(
            "`",
            arg,
            "` has ",
            sum(odd),
            " rows ",
            if (identical(s, stratify_by)) {
              paste0(
                "whose ",
                s,
                " is not one of the labels asked for (a total, missing or ",
                "another label)"
              )
            } else {
              paste0("whose ", s, " is not the total")
            },
            ", though it is ",
            if (is.null(stratify_by)) {
              "not stratified"
            } else {
              paste("stratified by", stratify_by)
            },
            "."
          ),
          call = call
        )
      }
    }
  }
  invisible()
}

# Expects, for each of `indicators` in one year's county response `county`
# (asked for by `stratify_by`, NULL: not stratified, with stratum labels
# `strata`), no state stratum with exactly one suppressed county row, and
# reports every count. A count only,
# as in count_suppressed_counties(): no state is named, and no state total has
# its county counts subtracted. Like count_suppressed_strata(), a monitor of
# CDC's responses (#164), not a judgment that any row could be worked out.
#
# Asserted only where it was observed to hold: unstratified, among state
# strata whose state total is published, which needs the matching state
# response `state`. Live run 45017608026 (22 Nov 2026 UTC) found none
# unstratified, for both HIV indicators in every year counted, 2017 to 2025.
# Stratified, it found 1 to 12 in every year and stratification counted but
# one (HIV diagnoses 2025 by sex, 0), so those counts are reported only. Without `state` (2017 and 2022, for which no state response
# is kept), every state stratum with county rows is counted, including any
# whose state total CDC did not publish, so the counts are reported only.
# Either way the counts must cover some state stratum.
expect_no_single_suppressed_county <- function(
  county,
  state,
  indicators,
  year,
  stratify_by = NULL,
  strata = NULL
) {
  counts <- count_suppressed_counties(county, state, year, stratify_by, strata)
  for (line in describe_suppressed_counties(counts, "Suppressed county rows")) {
    message(line)
  }
  label <- paste0(
    year,
    ", ",
    if (is.null(stratify_by)) "unstratified" else paste("by", stratify_by)
  )
  asserted <- is.null(stratify_by) && !is.null(state)
  for (indicator in indicators) {
    what <- paste0(indicator, " ", label, ": ")
    row <- counts[counts$indicator == indicator, ]
    if (nrow(row) != 1 || row$counted == 0) {
      fail(paste0(
        what,
        "no state stratum with county rows",
        if (!is.null(state)) " and a published state total",
        ", so nothing was counted."
      ))
      next
    }
    # One expectation per indicator, so expect_success() and
    # expect_failure() can check it (testthat 3.3 requires exactly one).
    adds_up <- row$none_suppressed +
      row$one_suppressed +
      row$two_or_more_suppressed ==
      row$counted
    none_single <- !asserted || row$one_suppressed == 0
    expect(
      adds_up && none_single,
      paste0(
        what,
        if (!adds_up) {
          paste0(
            "state strata by suppressed county rows add up to ",
            row$none_suppressed +
              row$one_suppressed +
              row$two_or_more_suppressed,
            ", not ",
            row$counted,
            ". "
          )
        },
        if (!none_single) {
          paste0(
            row$one_suppressed,
            " of ",
            row$counted,
            " state strata have exactly one suppressed county row; none ",
            "expected (none observed unstratified, 2017 to 2025)."
          )
        }
      )
    )
  }
  invisible(counts)
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

# Whether a warning about states with many cases in no county row (#164)
# names `state` in its entry for `entry`, such as "HIV prevalence 2022". The
# warning has one entry per indicator and year, each a line starting with a
# bullet, so a state named only under another indicator doesn't count. A
# line that starts with a space continues the entry above it. No percentage
# is matched.
unplaced_warning_names <- function(warning, entry, state) {
  lines <- strsplit(warning, "\n", fixed = TRUE)[[1]]
  continues <- grepl("^\\s", lines)
  entries <- vapply(
    split(lines, cumsum(!continues)),
    function(x) paste(trimws(x), collapse = " "),
    character(1)
  )
  entries <- sub("^[^[:alnum:]]+", "", entries)
  this <- entries[startsWith(entries, paste0(entry, ":"))]
  length(this) == 1 && grepl(paste0(state, " ("), this, fixed = TRUE)
}

# impute_suppressed(fit_to = "county_total") on real data (#179), sharing the
# county downloads the other live tests make: the unstratified county table
# the fit uses is exactly the unstratified get_atlas() county download (the
# same payloads, tested offline), and CDC's county table for the state check
# is the stratified download itself. So no county request is added; the
# state totals are one small request per year and stratification, kept for
# the run. Returns the imputed rows, the county data and totals they came
# from, and every warning's text.
live_fit <- function(year, stratify_by) {
  county <- live_county(year, stratify_by)
  totals <- live_county(year)
  state <- live_state(year, stratify_by)
  state$state_fips <- as.character(state$fips)
  local_mocked_bindings(
    .fetch_county_unstratified = function(x) totals,
    .fetch_county_totals = function(x) county,
    .fetch_state_totals = function(x) state
  )
  imputed <- NULL
  warnings <- capture_warnings(
    imputed <- suppressMessages(
      impute_suppressed(county, fit_to = "county_total")
    )
  )
  list(
    county = county,
    totals = totals,
    state = state,
    imputed = imputed,
    warnings = cli::ansi_strip(warnings)
  )
}
