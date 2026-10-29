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
#'     population in the same stratum, then across areas as usual. Negative
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
#'     them), all ages otherwise.
#'   * `"13+"` or `"all"`: set it yourself.
#'
#'   When `x` is stratified by age, the age groups set the ages, so 13 and
#'   older is used for the total population (the `"total"` fallback for
#'   empty strata).
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
#'     \item{county_cases_unknown}{Cases of unknown race added to this race
#'       group (`unknown_race = "redistribute"`); 0 otherwise.}
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
#' by `race_table`. For ages 13 and older, 2/5 of the 10 to 14 band is
#' counted, assuming ages are spread evenly within it.
#'
#' County rows with no Census denominator are left out and listed in
#' `attr(result, "not_allocated")`: race "Other", race "Unknown" (unless
#' redistributed), age groups
#' other than CDC's HIV age groups, rows kept by
#' `impute_suppressed(not_available = "keep")`, and counties with no ACS areas
#' (such as Connecticut's planning regions with 2010 tracts, until #30 adds a
#' crosswalk).
#'
#' Tract and block group boundaries change every ten years, so estimates from
#' ACS years on different sides of a decennial census (2017 and 2022, say)
#' can't be compared area by area.
#'
#' Needs the tidycensus package and a Census API key in the `CENSUS_API_KEY`
#' environment variable (see `tidycensus::census_api_key()`). Also downloads
#' the matching state data from CDC AtlasPlus for the no-county report.
#'
#' Age-stratified extrapolation supports only HIV diagnoses and prevalence,
#' because CDC's age groups for other diseases aren't matched to Census ages
#' yet.
#'
#' @seealso [impute_suppressed()], [no_county_cases()].
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
  acs_year = NULL,
  weights = NULL
) {
  call <- rlang::current_env()
  .check_county_data(x, arg = "x", call = call)
  .check_imputed(x, arg = "x", call = call)
  to <- rlang::arg_match(to)
  no_county <- rlang::arg_match(no_county)
  spread_by <- rlang::arg_match(spread_by)
  empty_strata <- rlang::arg_match(empty_strata)
  ages <- rlang::arg_match(ages)
  race_table <- rlang::arg_match(race_table)
  unknown_race <- rlang::arg_match(unknown_race)

  if (!is.null(weights)) {
    cli::cli_abort("{.arg weights} is not supported yet (#1).", call = call)
  }

  stratify_by <- .atlas_stratify_by(x)
  if (length(stratify_by) > 1) {
    cli::cli_abort(
      c(
        "{.arg x} can have at most one stratification variable.",
        "x" = "It is stratified by {.val {stratify_by}}."
      ),
      call = call
    )
  }
  if (length(stratify_by) == 0) {
    stratify_by <- NULL
  }

  diseases <- .atlas_diseases(x)
  if (identical(stratify_by, "age") && !identical(diseases, "hiv")) {
    cli::cli_abort(
      c(
        "Age-stratified estimates support only HIV diagnoses and prevalence.",
        "x" = "{.arg x} has {.val {setdiff(diseases, 'hiv')}} data.",
        "i" = "CDC's age groups for other diseases aren't matched to Census
               ages yet."
      ),
      call = call
    )
  }
  ages <- .default_ages(x, stratify_by, ages)

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

  report <- .no_county_cases(x, .fetch_state_totals(x))

  county <- .county_cases(x)
  county$county_cases_unknown <- 0
  redistributed <- NULL
  if (identical(stratify_by, "race") && unknown_race == "redistribute") {
    county <- .redistribute_unknown_race(county)
    redistributed <- attr(county, "redistributed")
  }
  states <- sort(unique(county$state_fips))
  pieces <- lapply(years, function(year) {
    acs <- acs_year %||% year
    pop <- .area_population(acs, states, stratify_by, ages, to, race_table)
    pop$acs_year <- acs
    .join_areas(county[county$year == year, ], pop, stratify_by, to)
  })
  areas <- dplyr::bind_rows(lapply(pieces, `[[`, "areas"))
  not_allocated <- dplyr::bind_rows(lapply(pieces, `[[`, "not_allocated"))

  areas$county_cases_spread <- 0
  if (no_county == "spread") {
    areas <- .spread_no_county(areas, report, empty_strata, spread_by)
  }
  areas <- .allocate(areas, empty_strata)
  areas$area_type <- to

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
    out, report, not_allocated, no_county, empty_strata, to
  )
  if (!is.null(redistributed) && nrow(redistributed) > 0) {
    cli::cli_inform(c(
      "i" = "Cases of unknown race shared across known race groups:",
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
# male and 027-049 female. `hiv_age` is the AtlasPlus HIV age group each band
# belongs to; `weight_13` counts 2/5 of the 10-14 band (ages 13 and 14),
# assuming ages are spread evenly within the band. `band` is the end of the
# Census label, checked by a live test.
.acs_age_bands <- function() {
  data.frame(
    band = c(
      "Under 5 years", "5 to 9 years", "10 to 14 years", "15 to 17 years",
      "18 and 19 years", "20 years", "21 years", "22 to 24 years",
      "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years",
      "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 and 61 years",
      "62 to 64 years", "65 and 66 years", "67 to 69 years", "70 to 74 years",
      "75 to 79 years", "80 to 84 years", "85 years and over"
    ),
    hiv_age = c(
      NA, NA, rep("13-24", 6), rep("25-34", 2), rep("35-44", 2),
      rep("45-54", 2), rep("55-64", 3), rep("65+", 6)
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
      "White", "Black/African American", "American Indian/Alaska Native",
      "Asian", "Native Hawaiian/Other Pacific Islander", "Multiracial",
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
      "B01001H", "B01001B", "B01001C", "B01001D", "B01001E", "B01001G",
      "B01001I"
    ),
    race_ethnicity = c(
      "White", "Black/African American", "American Indian/Alaska Native",
      "Asian", "Native Hawaiian/Other Pacific Islander", "Multiracial",
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

# One row per Census variable, with its weight and the stratum it counts
# towards (`stratum` is NA when unstratified). `ages` is "13+" or "all"; age
# groups always use the 13+ weights, as CDC's HIV age groups start at 13.
.census_denominators <- function(stratify_by, ages, race_table = "B03002") {
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
  weight <- if (ages == "13+" || identical(stratify_by, "age")) {
    bands$weight_13
  } else {
    rep(1, nrow(bands))
  }
  by_band <- data.frame(
    variable = c(bands$male, bands$female),
    weight = rep(weight, 2),
    sex = rep(c("Male", "Female"), each = nrow(bands)),
    hiv_age = rep(bands$hiv_age, 2)
  )
  by_band <- by_band[by_band$weight > 0, ]
  by_band$stratum <- switch(
    stratify_by %||% "none",
    none = NA_character_,
    sex = by_band$sex,
    age = by_band$hiv_age
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
.area_population <- function(year, states, stratify_by, ages, to = "tract",
                             race_table = "B03002") {
  denominators <- .census_denominators(stratify_by, ages, race_table)
  total <- .census_denominators(NULL, ages)
  total_13 <- .census_denominators(NULL, "13+")
  variables <- unique(c(
    denominators$variable, total$variable, total_13$variable, "B01001_001"
  ))

  acs <- .get_acs_areas(variables, year, states, to)
  acs <- data.frame(
    geoid = acs$GEOID,
    area_name = acs$NAME,
    variable = acs$variable,
    estimate = acs$estimate
  )

  # Sum of each area's estimates for the variables in `table`, weighted.
  weighted_sum <- function(table, by_stratum = FALSE) {
    rows <- match(acs$variable, table$variable)
    keep <- !is.na(rows)
    data.frame(
      geoid = acs$geoid[keep],
      stratum = if (by_stratum) table$stratum[rows[keep]] else NA_character_,
      value = acs$estimate[keep] * table$weight[rows[keep]]
    ) |>
      dplyr::summarize(
        value = sum(value, na.rm = TRUE),
        .by = c(geoid, stratum)
      )
  }
  lookup_value <- function(sums, geoid) {
    dplyr::coalesce(sums$value[match(geoid, sums$geoid)], 0)
  }

  areas <- unique(acs[c("geoid", "area_name")])
  areas$county_fips <- substr(areas$geoid, 1, 5)
  areas$area_total_population <- lookup_value(
    weighted_sum(total), areas$geoid
  )

  in_stratum <- weighted_sum(denominators, by_stratum = TRUE)
  strata <- unique(denominators["stratum"])
  out <- merge(areas, strata, by = NULL)
  out$area_population_acs <- dplyr::coalesce(
    in_stratum$value[match(
      .row_key(out, c("geoid", "stratum")),
      .row_key(in_stratum, c("geoid", "stratum"))
    )],
    0
  )

  if (identical(stratify_by, "race") && race_table == "B03002" &&
    ages == "13+") {
    total_13 <- lookup_value(weighted_sum(total_13), out$geoid)
    all_ages <- acs[acs$variable == "B01001_001", c("geoid", "estimate")]
    names(all_ages)[2] <- "value"
    total_all <- lookup_value(all_ages, out$geoid)
    share_13 <- ifelse(total_all > 0, total_13 / total_all, 0)
    out$area_population_acs <- out$area_population_acs * share_13
  }

  out[c(
    "geoid", "area_name", "county_fips", "stratum",
    "area_population_acs", "area_total_population"
  )]
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
  no_areas <- !not_available & !county$county_fips %in% pop$county_fips
  no_denominator <- !not_available & !no_areas &
    !county$stratum %in% pop$stratum
  left_out <- not_available | no_areas | no_denominator

  not_allocated <- county[left_out, setdiff(names(county), "stratum")]
  not_allocated$reason <- dplyr::case_when(
    not_available[left_out] ~ "no CDC data for this county",
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

# Share each county's race "Unknown" cases across its known race groups in
# proportion to their counts, within indicator and year. Unknown rows that are
# shared are removed; the rest stay (and are reported as not allocated). The
# shared rows are kept in attr(, "redistributed").
.redistribute_unknown_race <- function(county) {
  keys <- c("county_fips", "indicator", "year")
  unknown <- county$race_ethnicity == "Unknown" & !is.na(county$county_cases)
  known <- !county$race_ethnicity %in% c("Unknown", "Other") &
    !is.na(county$county_cases)

  known_total <- county[known, ] |>
    dplyr::summarize(total = sum(county_cases), .by = dplyr::all_of(keys))
  unknown_rows <- county[unknown, ]
  unknown_rows$known_total <- known_total$total[match(
    .row_key(unknown_rows, keys), .row_key(known_total, keys)
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

  out <- county[!(unknown & .row_key(county, keys) %in%
    .row_key(shared, keys)), ]
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
.spread_no_county <- function(areas, report, empty_strata,
                              spread_by = "population") {
  state_keys <- c("state_fips", .atlas_keys)
  county_keys <- c(state_keys, "county_fips")

  counties <- areas |>
    dplyr::summarize(
      stratum_pop = sum(area_population_acs),
      total_pop = sum(area_total_population),
      cases = dplyr::first(county_cases + county_cases_unknown),
      .by = dplyr::all_of(county_keys)
    )
  remainder <- report$no_county_cases[
    match(.row_key(counties, state_keys), .row_key(report, state_keys))
  ]
  counties$remainder <- pmax(0, dplyr::coalesce(remainder, 0))

  counties <- counties |>
    dplyr::mutate(
      rule = if (spread_by == "cases" && sum(cases) > 0) {
        "county cases"
      } else {
        .allocation_rule(sum(stratum_pop), sum(total_pop), empty_strata)
      },
      county_cases_spread = dplyr::coalesce(
        remainder * dplyr::if_else(
          rule == "county cases",
          cases / sum(cases),
          .shares(rule, stratum_pop, total_pop)
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
      county_population_acs = sum(area_population_acs),
      county_total_population = sum(area_total_population),
      allocation = .allocation_rule(
        county_population_acs, county_total_population, empty_strata
      ),
      county_cases_allocated = county_cases + county_cases_spread +
        county_cases_unknown,
      area_cases = dplyr::if_else(
        county_cases_allocated == 0,
        0,
        county_cases_allocated *
          .shares(allocation, area_population_acs, area_total_population)
      ),
      area_noncases = pmax(0, area_population_acs - area_cases),
      .by = dplyr::all_of(.county_groups)
    )
}


# Messages ------------------------------------------------------------------------

# One "* indicator year: n" line per indicator and year.
.lines_by_indicator <- function(df, value) {
  df <- data.frame(indicator = df$indicator, year = df$year, cases = df[[value]])
  by_indicator <- df |>
    dplyr::summarize(cases = sum(cases, na.rm = TRUE), .by = c(indicator, year))
  lines <- sprintf(
    "%s %s: %s",
    by_indicator$indicator,
    by_indicator$year,
    format(round(by_indicator$cases), big.mark = ",", trim = TRUE)
  )
  rlang::set_names(lines, rep("*", length(lines)))
}

.announce_extrapolation <- function(out, report, not_allocated, no_county,
                                    empty_strata, to) {
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
    cli::cli_warn(c(
      "{nrow(no_areas)} county row{?s} {?was/were} not allocated: no matching
       ACS {areas}.",
      "i" = "Counties: {.val {counties}}.",
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
  spread <- unique(areas[c(state_keys, "county_fips", "county_cases_spread")]) |>
    dplyr::summarize(
      spread = sum(county_cases_spread),
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
