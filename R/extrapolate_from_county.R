#' Share county counts across smaller Census areas
#'
#' @description
#' `extrapolate_from_county()` shares each county's count across the county's
#' census tracts or block groups, in proportion to each area's population in
#' the same stratum, from the American Community Survey (ACS) 5-year estimates.
#' Run it on county data that has been through [impute_suppressed()].
#'
#' The estimates are modelled, not observed: they assume the same rate in every
#' area of a county within a stratum.
#'
#' @param x County data from [get_atlas()] that has been through
#'   [impute_suppressed()], with at most one stratification variable.
#' @param to The areas to share county counts across. Both nest inside
#'   counties, so the estimates add up to the county counts exactly:
#'   * `"tract"` (default): census tracts. ACS 5-year estimates from 2009.
#'   * `"block group"`: census block groups. ACS 5-year estimates from 2013;
#'     smaller areas have larger ACS margins of error.
#'
#'   Areas that cross county lines, such as ZCTAs, are not supported yet.
#' @param no_county Cases in state totals with no county (see
#'   [no_county_cases()]):
#'   * `"report"` (default): left out of the estimates, with a message giving
#'     the total. The report is kept in `attr(result, "no_county_cases")`.
#'   * `"spread"`: shared across the state's counties in `x` by their ACS
#'     population in the same stratum, then across areas as usual. `x` must
#'     hold every county CDC reports in each of its states, for each
#'     indicator, year and stratum it has (rows CDC lists as "Data not
#'     available" with no count aside), or the counties left out would have
#'     their cases spread too; to study part of a state, spread first and
#'     filter the result. Checking needs CDC's county table, which comes from
#'     the cache when `x` was downloaded in this session. Negative
#'     remainders are not spread, nor are remainders in a state with no
#'     allocated county in that stratum; the message says how many cases
#'     could not be spread.
#' @param spread_by How `no_county = "spread"` shares a state's remainder
#'   among its counties:
#'   * `"population"` (default): by each county's ACS population in the
#'     stratum, as if cases with no county lived anywhere in the state.
#'   * `"cases"`: in proportion to each county's own count in the stratum, as
#'     if cases with no county resembled cases with one. A state stratum with
#'     no county cases falls back to population.
#'
#'   Ignored for `no_county = "report"`.
#' @param empty_strata What to do when CDC reports cases in a county stratum
#'   but no area in the county has ACS population in it (for example a small
#'   county with no residents of one age group):
#'   * `"total"` (default): share by the areas' total population.
#'   * `"equal"`: share equally across the county's areas.
#'   * `"none"`: leave the estimates `NA`, with a warning.
#' @param ages Ages the Census populations cover:
#'   * `"auto"` (default): 13 and older for HIV indicators (as CDC reports
#'     them), all ages otherwise, decided indicator by indicator, so `x` can
#'     combine HIV with other diseases.
#'   * `"13+"` or `"all"`: set it yourself.
#'
#'   When `x` is stratified by age, the age groups set the ages and `ages` is
#'   ignored: HIV groups start at 13, so the total population (the `"total"`
#'   fallback for empty strata) is ages 13 and older; other diseases' groups
#'   cover all ages.
#' @param race_table The ACS table for race populations, used when `x` is
#'   stratified by race:
#'   * `"B03002"` (default): Hispanic or Latino origin by race. Its groups
#'     match AtlasPlus exactly (Hispanic, and non-Hispanic for each race), but
#'     it has no ages, so for ages 13 and older each area's populations are
#'     scaled by the area's share of residents aged 13 and older.
#'   * `"B01001"`: the sex-by-age tables for each race (B01001B to B01001I),
#'     which give ages 13 and older directly. Only White (B01001H) and
#'     Hispanic (B01001I) match AtlasPlus; the other groups include Hispanic
#'     people of that race.
#' @param unknown_race Cases CDC reports with race "Unknown", when `x` is
#'   stratified by race:
#'   * `"report"` (default): left out, with a message, and listed in
#'     `attr(result, "not_allocated")`.
#'   * `"redistribute"`: shared across the county's known race groups in
#'     proportion to their counts, within each indicator and year, then
#'     across areas as usual. A county with no cases of known race keeps its
#'     unknown cases unallocated.
#' @param unknown_age Cases CDC reports with age "Unknown" (chlamydia,
#'   gonorrhea and syphilis), when `x` is stratified by age:
#'   * `"report"` (default): left out, with a message, and listed in
#'     `attr(result, "not_allocated")`.
#'   * `"redistribute"`: shared across the county's known age groups in
#'     proportion to their counts, within each indicator and year, then
#'     across areas as usual. A county with no cases of known age keeps its
#'     unknown cases unallocated.
#' @param unestimated Suppressed rows [impute_suppressed()] couldn't estimate
#'   (`cases_source = "not estimated"`, with no count), such as a county CDC
#'   still lists after it was abolished:
#'   * `"leave out"` (default): left out, with a message naming the counties,
#'     and listed in `attr(result, "not_allocated")`. Their cases count towards
#'     the state's remainder with no county.
#'   * `"error"`: stop with an error.
#' @param missing_population What to do when the ACS population is missing
#'   (an `NA` estimate, or a variable the Census didn't return) for some
#'   area of a county:
#'   * `"leave out"` (default): leave that county stratum out, with a warning,
#'     and list it in `attr(result, "not_allocated")` with reason `"missing
#'     ACS population"`. Sharing over the other areas would move cases.
#'   * `"error"`: stop instead.
#'
#'   A population of 0 is not missing; see `empty_strata`.
#' @param acs_year The last year of the ACS 5-year estimates. `NULL` (default)
#'   uses each data year. A single year can be given only if `x` has one year.
#' @param weights Not yet supported; for sub-county weights later (#1).
#'
#' @return A tibble with one row per area, indicator, year and stratum:
#'   \describe{
#'     \item{year, acs_year}{Data year and ACS year used.}
#'     \item{area_type, geoid, area_name}{`to`, and the area's Census GEOID
#'       and name.}
#'     \item{county_fips, county_name, state_fips}{The county and state the
#'       area is in.}
#'     \item{indicator, race_ethnicity, sex, age, data_status}{From `x`.}
#'     \item{county_cases, cases_source, county_population_atlas}{The county's
#'       count, where it came from (see [impute_suppressed()]) and CDC's
#'       county population.}
#'     \item{county_cases_spread}{Cases with no county added to this county
#'       (`no_county = "spread"`); 0 otherwise.}
#'     \item{county_cases_unknown}{Cases of unknown race or age added to this
#'       group (`unknown_race` or `unknown_age = "redistribute"`); 0
#'       otherwise.}
#'     \item{county_cases_allocated}{`county_cases + county_cases_spread +
#'       county_cases_unknown`: what the county's areas add up to.}
#'     \item{area_population_acs, county_population_acs}{ACS population in
#'       the stratum, for the area and the sum of the county's areas.}
#'     \item{area_total_population, county_total_population}{The same for the
#'       whole population at the same ages.}
#'     \item{allocation}{How the county's count was shared: `"stratum
#'       population"`, `"total population"`, `"equal shares"` or `"none"` (see
#'       `empty_strata`).}
#'     \item{area_cases}{Estimated cases in the area.}
#'     \item{area_noncases}{`area_population_acs - area_cases`, floored at 0.}
#'   }
#'
#'   Two attributes record what was not allocated:
#'   `attr(result, "no_county_cases")`, the [no_county_cases()] report, and
#'   `attr(result, "not_allocated")`, county rows left out, with a `reason`.
#'
#' @details
#' The denominator is the sum of the county's areas, so the estimates add up
#' to `county_cases_allocated` in every county stratum.
#'
#' Census populations come from ACS tables requested by variable code:
#' B01001 (sex by age) for all ages, sex and age, and for race the table set
#' by `race_table`. Each CDC age group counts the share of each B01001 age
#' band that its ages cover, assuming ages are spread evenly within a band:
#' for ages 13 and older (and HIV's 13-24 group), 2/5 of the 10 to 14 band.
#' [get_atlas()] requests each disease's own age groups, which cover its ages
#' once.
#'
#' County rows with no Census denominator are left out and listed in
#' `attr(result, "not_allocated")`: race "Other", race or age "Unknown"
#' (unless redistributed), suppressed rows with no estimate (see
#' `unestimated`), rows kept by
#' `impute_suppressed(not_available = "keep")`, and counties with no ACS areas
#' (such as Valdez-Cordova, Alaska, which CDC still lists with no population).
#'
#' CDC reports every year on its current county map. ACS releases from
#' 2006-2010 to 2015-2019 use 2010 tracts, so in Connecticut (now planning
#' regions) and the old Valdez-Cordova Census Area, Alaska (now Chugach and
#' Copper River), each 2010 tract and its block groups are assigned to the
#' current county they fall in. The one 2010 tract split between two planning
#' regions is shared by land area. The crosswalk ships in
#' `inst/extdata/tract-2010-crosswalk.csv`.
#'
#' CDC's current county codes the package knows an ACS release lacks:
#' * Connecticut's planning regions: handled by the crosswalk for ACS 2010 to
#'   2019; missing from ACS 2009, 2020 and 2021, which code Connecticut by its
#'   old counties (#121); in ACS 2022 on.
#' * Chugach and Copper River, Alaska: handled by the crosswalk for ACS 2010
#'   to 2019; missing from ACS 2009; in ACS 2020 on.
#' * Kusilvak, Alaska (02158, formerly Wade Hampton 02270) and Oglala Lakota,
#'   South Dakota (46102, formerly Shannon 46113), renamed in 2015: missing
#'   from ACS releases before 2015 (#121).
#'
#' This list is not a full check of every ACS vintage against CDC's map.
#' Counties a release doesn't match are left out with a warning that says
#' why, for the cases above. In 2010-2019 a block group follows its tract: the block groups of the
#' one split tract (09013890201) are shared between Capitol and Northeastern
#' Connecticut planning regions by the tract's land area, not by where each
#' block group lies.
#'
#' Tract and block group boundaries change every ten years, so estimates from
#' ACS years on different sides of a decennial census (2017 and 2022, say)
#' can't be compared area by area.
#'
#' Needs the tidycensus package and a Census API key in the `CENSUS_API_KEY`
#' environment variable (see `tidycensus::census_api_key()`). Also downloads
#' the matching state data from CDC AtlasPlus for the no-county report.
#'
#' Congenital syphilis isn't supported: its rates are per 100,000 live
#' births, so Census populations are the wrong denominator.
#'
#' @seealso [impute_suppressed()], [no_county_cases()]; the article
#'   ["About CDC AtlasPlus data"](https://vagishhemmige.github.io/CDCAtlas/articles/about-atlasplus-data.html)
#'   on CDC's county map and the Census denominators.
#'
#' @examples
#' \dontrun{
#' county <- get_atlas("hiv", "county", 2022, stratify_by = "sex")
#' county <- impute_suppressed(county)
#' tracts <- extrapolate_from_county(county)
#' block_groups <- extrapolate_from_county(county, to = "block group")
#'
#' # Cases left out because they have no county
#' attr(tracts, "no_county_cases")
#' }
#'
#' @export
extrapolate_from_county <- function(
  x,
  to = c("tract", "block group"),
  no_county = c("report", "spread"),
  spread_by = c("population", "cases"),
  empty_strata = c("total", "equal", "none"),
  ages = c("auto", "13+", "all"),
  race_table = c("B03002", "B01001"),
  unknown_race = c("report", "redistribute"),
  unknown_age = c("report", "redistribute"),
  unestimated = c("leave out", "error"),
  missing_population = c("leave out", "error"),
  acs_year = NULL,
  weights = NULL
) {
  call <- rlang::current_env()
  .check_county_data(x, arg = "x", call = call)
  unestimated <- rlang::arg_match(unestimated)
  missing_population <- rlang::arg_match(missing_population)
  .check_imputed(x, arg = "x", call = call, unestimated = unestimated)
  to <- rlang::arg_match(to)
  no_county <- rlang::arg_match(no_county)
  spread_by <- rlang::arg_match(spread_by)
  empty_strata <- rlang::arg_match(empty_strata)
  ages <- rlang::arg_match(ages)
  race_table <- rlang::arg_match(race_table)
  unknown_race <- rlang::arg_match(unknown_race)
  unknown_age <- rlang::arg_match(unknown_age)

  if (!is.null(weights)) {
    cli::cli_abort("{.arg weights} is not supported yet (#1).", call = call)
  }

  stratify_by <- .check_census_strata(x, call)

  years <- sort(unique(as.numeric(as.character(x$year))))
  if (!is.null(acs_year)) {
    if (!rlang::is_scalar_integerish(acs_year) || is.na(acs_year)) {
      cli::cli_abort("{.arg acs_year} must be a single year.", call = call)
    }
    if (length(years) > 1) {
      cli::cli_abort(
        c(
          "{.arg acs_year} can only be set when {.arg x} has one year.",
          "x" = "{.arg x} has years {.val {years}}."
        ),
        call = call
      )
    }
  }
  .check_acs_years(acs_year %||% years, to, call)

  .check_census_ready(call)

  if (no_county == "spread") {
    .check_whole_states(x, call)
  }
  report <- .no_county_cases(x, .fetch_state_totals(x))

  county <- .county_cases(x)
  county$county_cases_unknown <- 0
  redistributed <- NULL
  if (identical(stratify_by, "race") && unknown_race == "redistribute") {
    county <- .redistribute_unknown(county, "race_ethnicity", other = "Other")
    redistributed <- attr(county, "redistributed")
  }
  if (identical(stratify_by, "age") && unknown_age == "redistribute") {
    county <- .redistribute_unknown(county, "age")
    redistributed <- attr(county, "redistributed")
  }
  states <- sort(unique(county$state_fips))
  row_ages <- .row_ages(county, stratify_by, ages)
  pieces <- list()
  for (year in years) {
    acs <- acs_year %||% year
    for (universe in unique(row_ages[county$year == year])) {
      rows <- county$year == year & row_ages == universe
      pop <- .area_population(
        acs,
        states,
        stratify_by,
        universe,
        to,
        race_table,
        age_groups = unique(county$age[rows])
      )
      pop$acs_year <- acs
      piece <- .join_areas(county[rows, ], pop, stratify_by, to)
      piece$not_allocated$acs_year <- rep(acs, nrow(piece$not_allocated))
      pieces[[length(pieces) + 1]] <- piece
    }
  }
  areas <- dplyr::bind_rows(lapply(pieces, `[[`, "areas"))
  not_allocated <- dplyr::bind_rows(lapply(pieces, `[[`, "not_allocated"))
  missing_pop <- .leave_out_missing_population(
    areas,
    missing_population,
    call
  )
  areas <- missing_pop$areas
  not_allocated <- dplyr::bind_rows(not_allocated, missing_pop$not_allocated)

  areas$county_cases_spread <- rep(0, nrow(areas))
  if (no_county == "spread") {
    areas <- .spread_no_county(areas, report, empty_strata, spread_by)
  }
  areas <- .allocate(areas, empty_strata)
  areas$area_type <- rep(to, nrow(areas))

  # fmt: skip
  out <- tibble::as_tibble(areas[c(
    "year", "acs_year", "area_type", "geoid", "area_name", "county_fips",
    "county_name", "state_fips", "indicator", "race_ethnicity", "sex", "age",
    "data_status", "county_cases", "cases_source", "county_population_atlas",
    "county_cases_spread", "county_cases_unknown", "county_cases_allocated",
    "area_population_acs", "county_population_acs",
    "area_total_population", "county_total_population", "allocation",
    "area_cases", "area_noncases"
  )])
  attr(out, "no_county_cases") <- report
  attr(out, "not_allocated") <- tibble::as_tibble(not_allocated)

  .announce_extrapolation(
    out,
    report,
    not_allocated,
    no_county,
    empty_strata,
    to
  )
  .announce_not_estimated(
    x,
    paste(
      "left out of the",
      to,
      "estimates and counted in the remainder",
      "with no county"
    )
  )
  if (!is.null(redistributed) && nrow(redistributed) > 0) {
    what <- if (identical(stratify_by, "age")) "age" else "race"
    cli::cli_inform(c(
      "i" = "Cases of unknown {what} shared across known {what} groups:",
      .lines_by_indicator(redistributed, "county_cases")
    ))
  }
  if (no_county == "spread") {
    .announce_spread(areas, report, to)
  }
  out
}


# Checks --------------------------------------------------------------------------

# Say what is missing before any request is made.
# no_county = "spread" shares each state's remainder (state total minus the
# counties in `x`) across the counties in `x`. If `x` leaves out counties CDC
# reports, their own cases would be counted in the remainder and moved to the
# counties kept (#54). Checked within each state, indicator, year and stratum
# that `x` has; rows CDC lists as "Data not available" with no count don't
# count, as impute_suppressed() drops those.
.check_whole_states <- function(x, call) {
  with_state <- function(df) {
    df$state_fips <- substr(as.character(df$fips), 1, 2)
    df
  }
  x <- with_state(x)
  cdc <- with_state(.fetch_county_totals(x))
  groups <- c("state_fips", .atlas_keys)
  no_count <- is.na(cdc$cases) &
    as.character(cdc$data_status) == "Data not available"
  cdc <- cdc[!no_count & .row_key(cdc, groups) %in% .row_key(x, groups), ]
  keys <- c("fips", .atlas_keys)
  missing <- cdc[!.row_key(cdc, keys) %in% .row_key(x, keys), ]
  if (nrow(missing) == 0) {
    return(invisible())
  }
  counties <- unique(as.character(missing$fips))
  states <- unique(substr(counties, 1, 2))
  cli::cli_abort(
    c(
      "{.arg no_county = \"spread\"} needs every county CDC reports in each
       state of {.arg x}.",
      "x" = "{.arg x} is missing {length(counties)} count{?y/ies} in state{?s}
             {.val {states}}, for example {.val {utils::head(counties, 3)}}.",
      "i" = "Their cases would be spread to the counties in {.arg x} as if
             they had no county.",
      "i" = "Run {.fn extrapolate_from_county} on whole states and filter the
             result, or use {.code no_county = \"report\"}."
    ),
    call = call
  )
}

.check_census_ready <- function(call) {
  if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) {
    cli::cli_abort(
      c(
        "Downloading Census populations needs a Census API key.",
        "x" = "{.envvar CENSUS_API_KEY} is empty.",
        "i" = "Get a key at {.url https://api.census.gov/data/key_signup.html}
               and set it with {.code tidycensus::census_api_key()}."
      ),
      call = call
    )
  }
  rlang::check_installed(
    "tidycensus (>= 1.8.0)",
    reason = "to download Census populations.",
    call = call
  )
}

# First ACS 5-year release with each area: 2005-2009 for tracts; 2009-2013 for
# block groups through the Census API.
.first_acs_year <- c("tract" = 2009, "block group" = 2013)

.check_acs_years <- function(acs_years, to, call) {
  first <- .first_acs_year[[to]]
  too_early <- acs_years[acs_years < first]
  if (length(too_early) > 0) {
    cli::cli_abort(
      c(
        "ACS 5-year estimates for {to}s start in {first}.",
        "x" = "No ACS data for {.val {too_early}}.",
        "i" = "Set {.arg acs_year} to {first} or later."
      ),
      call = call
    )
  }
}

.area_plural <- function(to) paste0(to, "s")


# Census variables --------------------------------------------------------------

# ACS table B01001 (sex by age) age bands, in table order. Codes 003-025 are
# male and 027-049 female. `lower` and `upper` are the band's first and last
# age (`Inf` for 85 and over); `weight_13` counts 2/5 of the 10-14 band (ages
# 13 and 14), assuming ages are spread evenly within the band. `band` is the
# end of the Census label, checked by a live test.
.acs_age_bands <- function() {
  # fmt: skip
  data.frame(
    band = c(
      "Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years",
      "18 and 19 years", "20 years", "21 years", "22 to 24 years",
      "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years",
      "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 and 61 years",
      "62 to 64 years", "65 and 66 years", "67 to 69 years", "70 to 74 years",
      "75 to 79 years", "80 to 84 years", "85 years and over"
    ),
    lower = c(
      0, 5, 10, 15, 18, 20, 21, 22, 25, 30, 35, 40, 45, 50, 55, 60, 62, 65,
      67, 70, 75, 80, 85
    ),
    upper = c(
      4, 9, 14, 17, 19, 20, 21, 24, 29, 34, 39, 44, 49, 54, 59, 61, 64, 66,
      69, 74, 79, 84, Inf
    ),
    weight_13 = c(0, 0, 0.4, rep(1, 20)),
    male = sprintf("B01001_%03d", 3:25),
    female = sprintf("B01001_%03d", 27:49)
  )
}

# ACS table B03002 (Hispanic or Latino origin by race), matched to AtlasPlus's
# race/ethnicity categories. "Some other race alone" (B03002_008) has no
# AtlasPlus category. `label` is the end of the Census label without colons
# (2020 onwards adds them), checked by a live test.
.acs_race_variables <- function() {
  data.frame(
    variable = sprintf("B03002_%03d", c(3, 4, 5, 6, 7, 9, 12)),
    race_ethnicity = c(
      "White",
      "Black/African American",
      "American Indian/Alaska Native",
      "Asian",
      "Native Hawaiian/Other Pacific Islander",
      "Multiracial",
      "Hispanic/Latino"
    ),
    label = c(
      "Not Hispanic or Latino!!White alone",
      "Not Hispanic or Latino!!Black or African American alone",
      "Not Hispanic or Latino!!American Indian and Alaska Native alone",
      "Not Hispanic or Latino!!Asian alone",
      "Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone",
      "Not Hispanic or Latino!!Two or more races",
      "Hispanic or Latino"
    )
  )
}

# ACS sex-by-age tables for each race (B01001B to B01001I), matched to
# AtlasPlus's categories. Only H (White alone, not Hispanic) and I (Hispanic)
# match exactly; the others include Hispanic people of that race. `concept` is
# the table title, checked by a live test.
.acs_race_age_tables <- function() {
  data.frame(
    table = c(
      "B01001H",
      "B01001B",
      "B01001C",
      "B01001D",
      "B01001E",
      "B01001G",
      "B01001I"
    ),
    race_ethnicity = c(
      "White",
      "Black/African American",
      "American Indian/Alaska Native",
      "Asian",
      "Native Hawaiian/Other Pacific Islander",
      "Multiracial",
      "Hispanic/Latino"
    ),
    concept = c(
      "White Alone, Not Hispanic or Latino",
      "Black or African American Alone",
      "American Indian and Alaska Native Alone",
      "Asian Alone",
      "Native Hawaiian and Other Pacific Islander Alone",
      "Two or More Races",
      "Hispanic or Latino"
    )
  )
}

# Age bands of the B01001B-I tables, in table order: codes 003-016 male and
# 018-031 female. `weight_13` as in .acs_age_bands().
.acs_race_age_bands <- function() {
  # fmt: skip
  data.frame(
    band = c(
      "Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years",
      "18 and 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years",
      "35 to 44 years", "45 to 54 years", "55 to 64 years", "65 to 74 years",
      "75 to 84 years", "85 years and over"
    ),
    weight_13 = c(0, 0, 0.4, rep(1, 11))
  )
}

# First and last age of a CDC age-group label ("13-24", "05-14", "13\u201319",
# "65+"); NA for labels with no ages, such as "Unknown".
.age_group_bounds <- function(label) {
  label <- gsub("\u2013", "-", enc2utf8(label), fixed = TRUE, useBytes = TRUE)
  if (grepl("^[0-9]+-[0-9]+$", label)) {
    return(as.numeric(strsplit(label, "-", fixed = TRUE)[[1]]))
  }
  if (grepl("^[0-9]+\\+$", label)) {
    return(c(as.numeric(sub("+", "", label, fixed = TRUE)), Inf))
  }
  c(NA_real_, NA_real_)
}

# Share of each ACS age band that falls in ages `lower` to `upper`, assuming
# ages are spread evenly within a band (so ages 13-14 are 2/5 of the 10-14
# band). The open 85+ band counts in full when the group reaches 85 and has no
# upper limit.
.band_weights <- function(bands, lower, upper) {
  closed <- is.finite(bands$upper)
  overlap <- pmin(bands$upper, upper) - pmax(bands$lower, lower) + 1
  weight <- ifelse(
    closed,
    pmax(overlap, 0) / (bands$upper - bands$lower + 1),
    NA
  )
  open_in <- !closed & is.infinite(upper) & lower <= bands$lower
  open_out <- !closed & upper < bands$lower
  weight[open_in] <- 1
  weight[open_out] <- 0
  if (anyNA(weight)) {
    cli::cli_abort(
      "Age group {lower}-{upper} splits the ACS 85 and over band.",
      .internal = TRUE
    )
  }
  weight
}

# One row per Census variable and stratum it counts towards, with its weight
# (`stratum` is NA when unstratified). `ages` is "13+" or "all". For age
# strata, `age_groups` are CDC's labels; each group counts the share of each
# ACS band its ages cover, so groups starting at 13 count 2/5 of the 10-14
# band. Labels with no ages (such as "Unknown") have no rows.
.census_denominators <- function(
  stratify_by,
  ages,
  race_table = "B03002",
  age_groups = NULL
) {
  if (identical(stratify_by, "age")) {
    bands <- .acs_age_bands()
    rows <- lapply(unique(age_groups), function(group) {
      bounds <- .age_group_bounds(group)
      if (anyNA(bounds)) {
        return(NULL)
      }
      weight <- .band_weights(bands, bounds[1], bounds[2])
      out <- data.frame(
        variable = c(bands$male, bands$female),
        weight = rep(weight, 2),
        stratum = group
      )
      out[out$weight > 0, ]
    })
    out <- do.call(rbind, rows)
    if (is.null(out)) {
      out <- data.frame(
        variable = character(),
        weight = numeric(),
        stratum = character()
      )
    }
    rownames(out) <- NULL
    return(out)
  }
  if (identical(stratify_by, "race") && race_table == "B03002") {
    race <- .acs_race_variables()
    return(data.frame(
      variable = race$variable,
      weight = 1,
      stratum = race$race_ethnicity
    ))
  }
  if (identical(stratify_by, "race")) {
    tables <- .acs_race_age_tables()
    bands <- .acs_race_age_bands()
    weight <- if (ages == "13+") bands$weight_13 else rep(1, nrow(bands))
    codes <- c(3:16, 18:31)
    return(data.frame(
      variable = sprintf(
        "%s_%03d",
        rep(tables$table, each = length(codes)),
        codes
      ),
      weight = rep(rep(weight, 2), nrow(tables)),
      stratum = rep(tables$race_ethnicity, each = length(codes))
    )[rep(rep(weight, 2), nrow(tables)) > 0, ])
  }

  bands <- .acs_age_bands()
  weight <- if (ages == "13+") bands$weight_13 else rep(1, nrow(bands))
  by_band <- data.frame(
    variable = c(bands$male, bands$female),
    weight = rep(weight, 2),
    sex = rep(c("Male", "Female"), each = nrow(bands))
  )
  by_band <- by_band[by_band$weight > 0, ]
  by_band$stratum <- switch(
    stratify_by %||% "none",
    none = NA_character_,
    sex = by_band$sex
  )
  by_band[c("variable", "weight", "stratum")]
}

# Stands in front of tidycensus so tests can replace it.
.get_acs_areas <- function(variables, year, states, to) {
  tidycensus::get_acs(
    geography = to,
    variables = variables,
    year = year,
    survey = "acs5",
    state = states
  )
}

# Area populations for one ACS year: one row per area and stratum (every area
# appears in every stratum, with 0 if it has no population in it), with the
# stratum population and the area's total population at the same ages.
.area_population <- function(
  year,
  states,
  stratify_by,
  ages,
  to = "tract",
  race_table = "B03002",
  age_groups = NULL
) {
  denominators <- .census_denominators(
    stratify_by,
    ages,
    race_table,
    age_groups
  )
  total <- .census_denominators(NULL, ages)
  total_13 <- .census_denominators(NULL, "13+")
  variables <- unique(c(
    denominators$variable,
    total$variable,
    total_13$variable,
    "B01001_001"
  ))

  acs <- .get_acs_areas(variables, year, states, to)
  acs <- data.frame(
    geoid = acs$GEOID,
    area_name = acs$NAME,
    variable = acs$variable,
    estimate = acs$estimate
  )

  # Sum of each area's estimates for the variables in `table`, weighted. A
  # variable can count towards more than one stratum. A missing estimate, or
  # a variable row the area lacks, makes the sum NA rather than an undercount
  # (#52).
  weighted_sum <- function(table, by_stratum = FALSE) {
    table <- table[table$weight != 0, c("variable", "weight", "stratum")]
    if (!by_stratum) {
      table$stratum <- NA_character_
    }
    needed <- table |>
      dplyr::summarize(n_needed = dplyr::n(), .by = "stratum")
    sums <- dplyr::inner_join(
      acs[c("geoid", "variable", "estimate")],
      table,
      by = "variable",
      relationship = "many-to-many"
    ) |>
      dplyr::summarize(
        value = sum(.data$estimate * .data$weight),
        n_found = dplyr::n(),
        .by = c("geoid", "stratum")
      )
    n_needed <- needed$n_needed[match(sums$stratum, needed$stratum)]
    sums$value[sums$n_found < n_needed] <- NA_real_
    sums[c("geoid", "stratum", "value")]
  }
  # An area with no rows at all for these variables has a missing value too.
  lookup_value <- function(sums, geoid) {
    sums$value[match(geoid, sums$geoid)]
  }

  areas <- unique(acs[c("geoid", "area_name")])
  areas$county_fips <- substr(areas$geoid, 1, 5)
  areas$area_total_population <- lookup_value(
    weighted_sum(total),
    areas$geoid
  )

  in_stratum <- weighted_sum(denominators, by_stratum = TRUE)
  strata <- unique(denominators["stratum"])
  out <- merge(areas, strata, by = NULL)
  out$area_population_acs <- in_stratum$value[match(
    .row_key(out, c("geoid", "stratum")),
    .row_key(in_stratum, c("geoid", "stratum"))
  )]

  if (
    identical(stratify_by, "race") && race_table == "B03002" && ages == "13+"
  ) {
    total_13 <- lookup_value(weighted_sum(total_13), out$geoid)
    all_ages <- acs[acs$variable == "B01001_001", c("geoid", "estimate")]
    names(all_ages)[2] <- "value"
    total_all <- lookup_value(all_ages, out$geoid)
    share_13 <- dplyr::if_else(total_all > 0, total_13 / total_all, 0)
    out$area_population_acs <- out$area_population_acs * share_13
  }

  out <- out[c(
    "geoid",
    "area_name",
    "county_fips",
    "stratum",
    "area_population_acs",
    "area_total_population"
  )]
  if (year %in% .acs_years_2010_tracts) {
    out <- .apply_tract_crosswalk(out)
  }
  out
}

# Why a current CDC county has no areas in an ACS release, for the known
# cases (#112), or NA:
# - Connecticut's planning regions (09110-09190) first appear in ACS 2022.
#   ACS 2010-2019 (2010 tracts) reach them through the crosswalk; ACS 2020
#   and 2021 (2020 tracts) still code Connecticut by its old counties, and
#   ACS 2009 uses 2000 tracts.
# - Chugach (02063) and Copper River (02066), split from Valdez-Cordova in
#   2019, are reached through the crosswalk for ACS 2010-2019 and are in ACS
#   2020 on; ACS 2009 uses 2000 tracts.
# - Kusilvak (02158) and Oglala Lakota (46102) were renamed from Wade Hampton
#   (02270) and Shannon (46113) in Census's 2015 geography changes, so earlier
#   releases use the old codes.
# Recoding these is #121.
.acs_vintage_note <- function(county_fips, acs_year) {
  connecticut <- county_fips %in%
    sprintf("09%d0", 11:19) &
    acs_year %in% c(2009, 2020, 2021)
  alaska <- county_fips %in% c("02063", "02066") & acs_year == 2009
  renamed <- county_fips %in% c("02158", "46102") & acs_year < 2015
  out <- dplyr::case_when(
    connecticut ~ "Connecticut's planning regions",
    alaska ~ "Chugach and Copper River, Alaska",
    renamed ~ "Kusilvak, AK and Oglala Lakota, SD (renamed in 2015)",
    .default = NA_character_
  )
  rlang::set_names(out, county_fips)
}

# ACS 5-year releases on 2010 tracts and block groups (2006-2010 to 2015-2019).
.acs_years_2010_tracts <- 2010:2019

# CDC reports every year on its current county map (#30). For ACS releases on
# 2010 tracts, areas in Connecticut (now planning regions) and in the old
# Valdez-Cordova Census Area, Alaska (now Chugach and Copper River) are moved
# to the current county, using inst/extdata/tract-2010-crosswalk.csv (built by
# data-raw/tract-crosswalk.R). A block group follows its tract. An area split
# between two current counties appears once in each, with its populations
# multiplied by its share of the tract's land area.
.apply_tract_crosswalk <- function(areas) {
  crosswalk <- .tract_crosswalk()
  tract <- substr(areas$geoid, 1, 11)
  moved <- tract %in% crosswalk$tract_2010
  if (!any(moved)) {
    return(areas)
  }
  shifted <- dplyr::inner_join(
    areas[moved, setdiff(names(areas), "county_fips")] |>
      dplyr::mutate(tract_2010 = tract[moved]),
    crosswalk,
    by = "tract_2010",
    relationship = "many-to-many"
  )
  shifted$area_population_acs <- shifted$area_population_acs * shifted$share
  shifted$area_total_population <- shifted$area_total_population * shifted$share
  dplyr::bind_rows(areas[!moved, ], shifted[names(areas)])
}

# The shipped crosswalk from 2010 tracts to CDC's current counties.
.tract_crosswalk <- function() {
  utils::read.csv(
    system.file("extdata", "tract-2010-crosswalk.csv", package = "CDCAtlas"),
    colClasses = c("character", "character", "numeric")
  )
}


# Allocation --------------------------------------------------------------------

# County rows in the shape the allocation needs, with character keys.
.county_cases <- function(x) {
  out <- data.frame(
    lapply(as.data.frame(x[.atlas_keys]), as.character),
    county_fips = as.character(x$fips),
    county_name = as.character(x$geography),
    state_fips = substr(as.character(x$fips), 1, 2),
    data_status = as.character(x$data_status),
    county_cases = x$cases,
    cases_source = x$cases_source,
    county_population_atlas = x$population
  )
  out$year <- as.numeric(out$year)
  out
}

# Join county rows to their areas. County rows with no areas, or in a stratum
# with no Census denominator, are returned separately with a reason.
.join_areas <- function(county, pop, stratify_by, to) {
  county$stratum <- .row_stratum(county, stratify_by)

  not_available <- county$cases_source == "not available"
  not_estimated <- county$cases_source == "not estimated" &
    is.na(county$county_cases)
  no_data <- not_available | not_estimated
  no_areas <- !no_data & !county$county_fips %in% pop$county_fips
  no_denominator <- !no_data & !no_areas & !county$stratum %in% pop$stratum
  left_out <- no_data | no_areas | no_denominator

  not_allocated <- county[left_out, setdiff(names(county), "stratum")]
  not_allocated$reason <- dplyr::case_when(
    not_available[left_out] ~ "no CDC data for this county",
    not_estimated[left_out] ~ "suppressed, not estimated",
    no_areas[left_out] ~ paste("no ACS", .area_plural(to), "for this county"),
    .default = "no Census population for this stratum"
  )

  areas <- dplyr::inner_join(
    pop,
    county[!left_out, ],
    by = c("county_fips", "stratum"),
    relationship = "many-to-many"
  )
  list(areas = areas, not_allocated = not_allocated)
}

.county_groups <- c("county_fips", .atlas_keys)

# A county stratum where any area's ACS population is missing (an NA
# estimate or a missing variable row) can't be shared fairly: sharing over the
# other areas would move cases. It is left out and listed, or stops the run
# (#52). Real zeros are not missing.
.leave_out_missing_population <- function(areas, missing_population, call) {
  missing <- is.na(areas$area_population_acs) |
    is.na(areas$area_total_population)
  if (!any(missing)) {
    return(list(areas = areas, not_allocated = NULL))
  }
  keys <- .row_key(areas, .county_groups)
  affected <- keys %in% keys[missing]
  rows <- areas[affected & !duplicated(keys), ]
  counties <- unique(paste0(rows$county_name, " (", rows$county_fips, ")"))
  if (missing_population == "error") {
    cli::cli_abort(
      c(
        "The ACS population is missing for some areas of
         {length(counties)} count{?y/ies}.",
        "x" = "For example {.val {utils::head(counties, 3)}}.",
        "i" = "Use {.code missing_population = \"leave out\"} to leave
               those county strata out instead."
      ),
      call = call
    )
  }
  cli::cli_warn(c(
    "{nrow(rows)} county stratum row{?s} in {length(counties)}
     count{?y/ies} left out: the ACS population is missing for some of
     {?its/their} areas.",
    "i" = "For example {.val {utils::head(counties, 3)}}.",
    "i" = "See {.code attr(x, \"not_allocated\")}."
  ))
  pop_columns <- c(
    "geoid",
    "area_name",
    "stratum",
    "area_population_acs",
    "area_total_population",
    "acs_year"
  )
  not_allocated <- rows[setdiff(names(rows), pop_columns)]
  not_allocated$reason <- "missing ACS population"
  list(areas = areas[!affected, ], not_allocated = not_allocated)
}

# Share each county's "Unknown" cases in `column` (race or age) across its
# known groups in proportion to their counts, within indicator and year.
# Groups in `other` (race "Other") are neither known nor shared to. Unknown
# rows that are shared are removed; the rest stay (and are reported as not
# allocated). The shared rows are kept in attr(, "redistributed").
.redistribute_unknown <- function(county, column, other = character()) {
  keys <- c("county_fips", "indicator", "year")
  unknown <- county[[column]] == "Unknown" & !is.na(county$county_cases)
  known <- !county[[column]] %in% c("Unknown", other) &
    !is.na(county$county_cases)

  known_total <- county[known, ] |>
    dplyr::summarize(total = sum(.data$county_cases), .by = dplyr::all_of(keys))
  unknown_rows <- county[unknown, ]
  unknown_rows$known_total <- known_total$total[match(
    .row_key(unknown_rows, keys),
    .row_key(known_total, keys)
  )]
  shared <- unknown_rows[dplyr::coalesce(unknown_rows$known_total, 0) > 0, ]

  rows <- match(.row_key(county, keys), .row_key(shared, keys))
  share <- dplyr::if_else(
    known & !is.na(rows),
    county$county_cases / shared$known_total[rows],
    0
  )
  county$county_cases_unknown <- dplyr::coalesce(
    shared$county_cases[rows] * share,
    0
  )

  out <- county[
    !(unknown &
      .row_key(county, keys) %in%
        .row_key(shared, keys)),
  ]
  attr(out, "redistributed") <- shared
  out
}

# Which rule shares a group's count: its stratum population if it has any,
# otherwise as `empty_strata` says.
.allocation_rule <- function(stratum, total, empty_strata) {
  dplyr::case_when(
    stratum > 0 ~ "stratum population",
    empty_strata == "none" ~ "none",
    empty_strata == "total" & total > 0 ~ "total population",
    .default = "equal shares"
  )
}

# Shares within one group, by that group's rule.
.shares <- function(rule, stratum, total) {
  dplyr::case_when(
    rule == "stratum population" ~ stratum / sum(stratum),
    rule == "total population" ~ total / sum(total),
    rule == "equal shares" ~ 1 / length(stratum),
    .default = NA_real_
  )
}

# Share each state's positive no-county remainder across its counties in the
# data, by the counties' ACS population in the stratum.
.spread_no_county <- function(
  areas,
  report,
  empty_strata,
  spread_by = "population"
) {
  state_keys <- c("state_fips", .atlas_keys)
  county_keys <- c(state_keys, "county_fips")

  counties <- areas |>
    dplyr::summarize(
      stratum_pop = sum(.data$area_population_acs),
      total_pop = sum(.data$area_total_population),
      cases = dplyr::first(.data$county_cases + .data$county_cases_unknown),
      .by = dplyr::all_of(county_keys)
    )
  remainder <- report$no_county_cases[
    match(.row_key(counties, state_keys), .row_key(report, state_keys))
  ]
  counties$remainder <- pmax(0, dplyr::coalesce(remainder, 0))

  counties <- counties |>
    dplyr::mutate(
      rule = if (spread_by == "cases" && sum(.data$cases) > 0) {
        "county cases"
      } else {
        .allocation_rule(
          sum(.data$stratum_pop),
          sum(.data$total_pop),
          empty_strata
        )
      },
      county_cases_spread = dplyr::coalesce(
        .data$remainder *
          dplyr::if_else(
            .data$rule == "county cases",
            .data$cases / sum(.data$cases),
            .shares(.data$rule, .data$stratum_pop, .data$total_pop)
          ),
        0
      ),
      .by = dplyr::all_of(state_keys)
    )

  areas$county_cases_spread <- counties$county_cases_spread[
    match(.row_key(areas, county_keys), .row_key(counties, county_keys))
  ]
  areas
}

.allocate <- function(areas, empty_strata) {
  areas |>
    dplyr::mutate(
      county_population_acs = sum(.data$area_population_acs),
      county_total_population = sum(.data$area_total_population),
      allocation = .allocation_rule(
        .data$county_population_acs,
        .data$county_total_population,
        empty_strata
      ),
      county_cases_allocated = .data$county_cases +
        .data$county_cases_spread +
        .data$county_cases_unknown,
      area_cases = dplyr::if_else(
        .data$county_cases_allocated == 0,
        0,
        .data$county_cases_allocated *
          .shares(
            .data$allocation,
            .data$area_population_acs,
            .data$area_total_population
          )
      ),
      area_noncases = pmax(0, .data$area_population_acs - .data$area_cases),
      .by = dplyr::all_of(.county_groups)
    )
}


# Messages ------------------------------------------------------------------------

# One "* indicator year: n" line per indicator and year.
.lines_by_indicator <- function(df, value) {
  df <- data.frame(
    indicator = df$indicator,
    year = df$year,
    cases = df[[value]]
  )
  by_indicator <- df |>
    dplyr::summarize(
      cases = sum(.data$cases, na.rm = TRUE),
      .by = c("indicator", "year")
    )
  lines <- sprintf(
    "%s %s: %s",
    by_indicator$indicator,
    by_indicator$year,
    format(round(by_indicator$cases), big.mark = ",", trim = TRUE)
  )
  rlang::set_names(lines, rep("*", length(lines)))
}

.announce_extrapolation <- function(
  out,
  report,
  not_allocated,
  no_county,
  empty_strata,
  to
) {
  areas <- .area_plural(to)
  positive <- report
  positive$no_county_cases <- pmax(0, positive$no_county_cases)
  heading <- if (no_county == "report") {
    "Cases in state totals with no county are not allocated to {areas}:"
  } else {
    "Cases in state totals with no county, to be spread across each state's
     counties:"
  }
  odd <- c(
    if (any(report$no_county_cases < 0, na.rm = TRUE)) {
      "{sum(report$no_county_cases < 0, na.rm = TRUE)} state strat{?um/a}
       with a negative remainder (counted as 0)"
    },
    if (anyNA(report$no_county_cases)) {
      "{sum(is.na(report$no_county_cases))} state strat{?um/a} with no state
       total (left out)"
    }
  )
  cli::cli_inform(c(
    "i" = heading,
    .lines_by_indicator(positive, "no_county_cases"),
    if (length(odd) > 0) rlang::set_names(odd, rep("!", length(odd))),
    " " = "See {.code attr(x, \"no_county_cases\")} for each state."
  ))

  no_stratum <- not_allocated[
    not_allocated$reason == "no Census population for this stratum",
  ]
  if (nrow(no_stratum) > 0) {
    strata <- setdiff(
      unique(c(no_stratum$race_ethnicity, no_stratum$sex, no_stratum$age)),
      unlist(.atlas_totals)
    )
    cli::cli_inform(c(
      "i" = "Cases not allocated to {areas} because Census has no population
             for {.val {strata}}:",
      .lines_by_indicator(no_stratum, "county_cases"),
      " " = "See {.code attr(x, \"not_allocated\")}."
    ))
  }

  no_areas <- not_allocated[
    not_allocated$reason == paste("no ACS", areas, "for this county"),
  ]
  if (nrow(no_areas) > 0) {
    counties <- unique(no_areas$county_name)
    notes <- unique(stats::na.omit(data.frame(
      what = .acs_vintage_note(no_areas$county_fips, no_areas$acs_year),
      acs_year = no_areas$acs_year
    )))
    use <- dplyr::case_when(
      notes$what == "Connecticut's planning regions" ~
        "ACS 2010-2019 or 2022 and later",
      startsWith(notes$what, "Kusilvak") ~ "ACS 2015 or later",
      .default = "ACS 2010 or later"
    )
    notes <- sprintf(
      "ACS %s doesn't have CDC's current codes for %s; use %s.",
      notes$acs_year,
      notes$what,
      use
    )
    cli::cli_warn(c(
      "{nrow(no_areas)} county row{?s} {?was/were} not allocated: no matching
       ACS {areas}.",
      "i" = "Counties: {.val {counties}}.",
      rlang::set_names(notes, rep("i", length(notes))),
      "i" = "See {.code attr(x, \"not_allocated\")}."
    ))
  }

  if (empty_strata == "none" && anyNA(out$area_cases)) {
    groups <- unique(out[is.na(out$area_cases), .county_groups])
    cli::cli_warn(c(
      "{nrow(groups)} county strat{?um/a} had cases but no ACS population, so
       their estimates are {.code NA}.",
      "i" = "Use {.code empty_strata = \"total\"} or {.code \"equal\"} to share
             them anyway."
    ))
  }
}

# How much of the positive remainder was spread, and how much could not be
# (no allocated county in that state stratum, or rule "none").
.announce_spread <- function(areas, report, to) {
  state_keys <- c("state_fips", .atlas_keys)
  spread <- unique(areas[c(
    state_keys,
    "county_fips",
    "county_cases_spread"
  )]) |>
    dplyr::summarize(
      spread = sum(.data$county_cases_spread),
      .by = dplyr::all_of(state_keys)
    )
  wanted <- report
  wanted$wanted <- pmax(0, dplyr::coalesce(wanted$no_county_cases, 0))
  wanted$spread <- dplyr::coalesce(
    spread$spread[
      match(.row_key(wanted, state_keys), .row_key(spread, state_keys))
    ],
    0
  )
  wanted$unspread <- wanted$wanted - wanted$spread
  if (sum(wanted$unspread) > 0.5) {
    plural <- .area_plural(to)
    cli::cli_inform(c(
      "i" = "Cases with no county that could not be spread (no county with
             {plural} in that state and stratum):",
      .lines_by_indicator(wanted[wanted$unspread > 0, ], "unspread")
    ))
  }
}
