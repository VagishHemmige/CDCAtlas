# Live checks of county downloads and the full pipeline on real CDC and Census
# data. Each county download is about 56 CDC requests (one per state), and each
# extrapolation downloads tract populations for every state, so a full run
# takes 35 to 45 minutes. Downloads are shared through helper-live.R, so each
# year and stratification is fetched once per run. Skipped unless switched on:
#
#   Sys.setenv(CDCATLAS_LIVE_TESTS = "true")
#   devtools::test(filter = "live-pipeline")
#
# They bypass the on-disk cache, so they test what CDC returns today. The years
# are the ones HOPE uses, plus 2024 onward for the check of CDC's rates on
# suppressed rows (#157), which adds county downloads (and a state request
# for each) but no Census requests. The counts of suppressed cells (#164)
# reuse those downloads and add none.

skip_if_not(
  identical(Sys.getenv("CDCATLAS_LIVE_TESTS"), "true"),
  "Live CDC tests are off. Set CDCATLAS_LIVE_TESTS=true to run them."
)
skip_if_offline("gis.cdc.gov")

for (year in c(2017, 2022)) {
  test_that(paste("county HIV request returns every state's counties,", year), {
    out <- live_county(year)

    prevalence <- out[out$indicator == "HIV prevalence", ]
    expect_gt(nrow(prevalence), 3000)
    expect_equal(sum(duplicated(prevalence$fips)), 0)
    county_states <- unique(substr(prevalence$fips, 1, 2))
    expect_contains(county_states, c("01", "06", "36", "48"))
    expect_gte(length(county_states), 51)
  })
}

test_that("no-county remainders match #27 (Pennsylvania HIV prevalence)", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  expected <- c("2017" = 6292, "2022" = 1140)

  for (year in names(expected)) {
    county <- live_county(as.numeric(year))
    report <- no_county_cases(county)
    # Count only (#173): unstratified state strata whose remainder is
    # withheld, by reason, among those that can be checked (a published state
    # total), and how many can't be checked (no published state total). The
    # provisional article lists 2022 unstratified state shares on the basis
    # that none is withheld; read a zero only over the strata checked here.
    for (indicator in c("HIV diagnoses", "HIV prevalence")) {
      rows <- report[report$indicator == indicator, ]
      checked <- !is.na(rows$state_cases)
      reasons <- table(rows$no_county_withheld[checked])
      message(
        "Withheld remainders, ",
        indicator,
        " ",
        year,
        ", unstratified: ",
        if (nrow(rows) == 0) {
          "no state strata returned."
        } else {
          paste0(
            sum(!is.na(rows$no_county_withheld[checked])),
            " of ",
            sum(checked),
            " state strata with a published state total",
            if (length(reasons) > 0) {
              paste0(
                " (",
                paste(names(reasons), reasons, sep = ": ", collapse = "; "),
                ")"
              )
            },
            "; ",
            sum(!checked),
            " without one, not checked."
          )
        }
      )
    }
    pa <- report[
      report$state_fips == "42" & report$indicator == "HIV prevalence",
    ]
    expect_equal(pa$unpublished_cases, expected[[year]])
  }
})

test_that("imputation respects the cap and keeps published counts, 2022", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  county <- live_county(2022, "sex")
  # Valdez-Cordova (02261) has no CDC population in 2022 (see below).
  expect_warning(
    imputed <- impute_suppressed(county, cap = 2),
    "Valdez-Cordova"
  )

  estimated <- imputed$cases[imputed$cases_source == "state_rate"]
  expect_gt(length(estimated), 0)
  expect_lte(max(estimated, na.rm = TRUE), 2)
  expect_gt(max(estimated, na.rm = TRUE), 0)

  published <- imputed[imputed$cases_source == "published", ]
  original <- county$cases[match(
    paste(published$fips, published$indicator, published$sex),
    paste(county$fips, county$indicator, county$sex)
  )]
  # `[` drops the column's variable label from `original`; compare the counts.
  expect_equal(published$cases, original, ignore_attr = "label")
})

# County rows the default pipeline is known to leave out. Anything else in
# attr(, "not_allocated") is new and fails the test below. Found by running
# these tests on 30 October 2026.
#   - "Data not available" with no count: CDC has no data for the county;
#     kept by impute_suppressed() since #110 and left out here.
#   - Race or age "Unknown": no Census population for it.
#   - CDC reports every year on its current county map. Since #30, 2010
#     tracts are moved to the current counties by a crosswalk, so 2017
#     Connecticut planning regions and Alaska's Chugach (02063) and Copper
#     River (02066) are allocated. Still left out in 2017:
#     - Connecticut: suppressed planning regions, which have no CDC
#       population, so can't be estimated.
#     - Alaska: Valdez-Cordova (02261), listed with no CDC population; its
#       2010 tracts now belong to Chugach and Copper River.
#   - 2022, Alaska: CDC still lists Valdez-Cordova (02261) beside its
#     successors; it has no CDC population and no 2020 tracts.
#   - Puerto Rico, by race: suppressed municipio rows can't be estimated
#     (no state rate or municipio population by race from CDC).
#   - Chlamydia 2022 by age (found 15 Nov 2026, #87):
#     - suppressed rows of age "Unknown" have no CDC population, so can't be
#       estimated;
#     - CDC lists Connecticut's old counties (09001-09015) as "Data
#       suppressed" with no population in every age group, beside the
#       planning regions.
no_area_or_estimate <- c(
  "suppressed, not estimated",
  "no ACS tracts for this county"
)
is_known_left_out <- function(left_out, year) {
  unknown_race <- left_out$reason == "no Census population for this stratum" &
    (left_out$race_ethnicity == "Unknown" | left_out$age == "Unknown")
  connecticut <- year == 2017 &
    left_out$reason == "suppressed, not estimated" &
    substr(left_out$county_fips, 1, 2) == "09"
  valdez_cordova_split <- left_out$reason %in%
    no_area_or_estimate &
    left_out$county_fips == "02261"
  puerto_rico_race <- left_out$reason == "suppressed, not estimated" &
    substr(left_out$county_fips, 1, 2) == "72" &
    left_out$race_ethnicity != "All races/ethnicities"
  unknown_age_suppressed <- left_out$reason == "suppressed, not estimated" &
    left_out$age == "Unknown"
  old_connecticut <- year == 2022 &
    left_out$reason == "suppressed, not estimated" &
    left_out$county_fips %in% sprintf("090%02d", seq(1, 15, 2))
  no_cdc_data <- left_out$reason == "no CDC data for this county"
  no_cdc_data |
    unknown_race |
    connecticut |
    valdez_cordova_split |
    puerto_rico_race |
    unknown_age_suppressed |
    old_connecticut
}

# Warnings the default pipeline is known to give, by year. 2017 also warns
# that one 2010 tract is split between two planning regions (#152). Chlamydia
# by age also warns that suppressed rows couldn't be estimated (unknown age
# and Connecticut's old counties; the rows themselves are checked above).
# Every run is national, so it can warn that some states have more than 25%
# of their cases in no county row (#164): in the first measured run (HIV
# prevalence 2022, 21 Nov 2026) Alaska, Rhode Island, Nebraska and Idaho did
# unstratified, and New Hampshire and Maine by sex. The pattern allows that
# warning in every run; it doesn't require it, and which states it names is
# not checked here.
known_warnings <- c(
  "2017" = "no matching ACS tracts|Planning Region, CT|Valdez-Cordova|Municipio, PR|split between counties|in no county row of",
  "2022" = "Valdez-Cordova Census Area, AK|no matching ACS tracts|Municipio, PR|in no county row of"
)
known_disease_warnings <- c(
  chlamydia = "suppressed rows? could not be estimated"
)

county_groups <- c("county_fips", "indicator", "race_ethnicity", "sex", "age")

# Fails listing every item in `x`, so a live failure says what was unexpected.
expect_none <- function(x, what) {
  x <- unique(x)
  expect(
    length(x) == 0,
    paste0(
      length(x),
      " unexpected ",
      what,
      ":\n",
      paste("*", x, collapse = "\n")
    )
  )
}

# The default pipeline's runs: HIV for both years and every stratification,
# and chlamydia by age, whose age groups differ from HIV's (#10).
pipeline_runs <- c(
  lapply(
    list(
      list(2017, NULL),
      list(2017, "sex"),
      list(2017, "age"),
      list(2017, "race"),
      list(2022, NULL),
      list(2022, "sex"),
      list(2022, "age"),
      list(2022, "race")
    ),
    function(run) list(year = run[[1]], strat = run[[2]], disease = "hiv")
  ),
  list(list(year = 2022, strat = "age", disease = "chlamydia"))
)

for (pipeline_run in pipeline_runs) {
  local({
    year <- pipeline_run$year
    strat <- pipeline_run$strat
    disease <- pipeline_run$disease
    label <- paste0(
      if (disease == "hiv") "" else paste0(disease, " "),
      year,
      ", ",
      if (is.null(strat)) "unstratified" else paste("by", strat)
    )

    test_that(paste("suppressed rows with no count have no rate,", label), {
      # The county download the tests below share: no extra request (#157).
      expect_no_rate_on_suppressed(
        live_county(year, strat, disease),
        paste(disease, label)
      )
    })

    test_that(
      paste("tract estimates add up to the counts they share,", label),
      {
        areas <- live_pipeline(year, strat, disease = disease)$areas
        check <- areas |>
          dplyr::summarize(
            county_values = dplyr::n_distinct(county_cases_allocated),
            county_cases = dplyr::first(county_cases_allocated),
            area_cases_sum = sum(area_cases),
            .by = dplyr::all_of(county_groups)
          )
        expect_equal(check$county_values, rep(1, nrow(check)))
        expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-6)
        expect_equal(sum(is.na(areas$area_cases)), 0)
        expect_gte(min(areas$area_cases), 0)
        # An area appears once per county and stratum. The one 2010 tract
        # the crosswalk splits between two counties (#30) appears in each.
        expect_equal(sum(duplicated(areas[c("geoid", county_groups)])), 0)
        keys <- areas[c("geoid", county_groups[-1])]
        in_two <- unique(areas$geoid[duplicated(keys)])
        crosswalk <- .tract_crosswalk()
        split <- crosswalk$tract_2010[duplicated(crosswalk$tract_2010)]
        expect_none(
          setdiff(substr(in_two, 1, 11), split),
          "areas in two counties"
        )
      }
    )

    test_that(
      paste("every county row is allocated or listed as left out,", label),
      {
        run <- live_pipeline(year, strat, disease = disease)
        county_rows <- .row_key(
          data.frame(
            county_fips = as.character(run$imputed$fips),
            lapply(as.data.frame(run$imputed[county_groups[-1]]), as.character)
          ),
          county_groups
        )
        allocated <- unique(.row_key(run$areas, county_groups))
        left_out <- .row_key(attr(run$areas, "not_allocated"), county_groups)

        readable <- function(keys) gsub("\r", " | ", keys)
        expect_none(
          readable(intersect(allocated, left_out)),
          "rows both allocated and left out"
        )
        expect_none(
          readable(setdiff(county_rows, c(allocated, left_out))),
          "county rows missing"
        )
        expect_none(
          readable(setdiff(c(allocated, left_out), county_rows)),
          "rows not in the county data"
        )
      }
    )

    test_that(paste("only known county rows are left out,", label), {
      left_out <- attr(
        live_pipeline(year, strat, disease = disease)$areas,
        "not_allocated"
      )
      unexpected <- left_out[!is_known_left_out(left_out, year), ]
      expect_none(
        paste0(
          unexpected$county_fips,
          " ",
          unexpected$county_name,
          ": ",
          unexpected$reason,
          recycle0 = TRUE
        ),
        "county rows left out"
      )
    })

    test_that(paste("only known warnings are given,", label), {
      warnings <- live_pipeline(year, strat, disease = disease)$warnings
      expect_none(
        warnings[
          !grepl(
            paste(
              c(
                known_warnings[[as.character(year)]],
                unname(known_disease_warnings[intersect(
                  disease,
                  names(known_disease_warnings)
                )])
              ),
              collapse = "|"
            ),
            warnings
          )
        ],
        "warnings"
      )
    })
  })
}

# A monitor of CDC's suppression in county data (#164). A planned method
# would fit stratified county estimates to public totals. These tests do not
# establish that fitting is safe for any cell, and they assess no cell's
# recoverability. They count how often certain patterns occur in CDC's
# responses and fail when a count moves from what was last observed: a drift
# alert, so that a change in what CDC publishes is noticed before anything
# relies on the earlier pattern. They use only the downloads this file
# already makes (no request is added), and nothing is imputed.
#
# COUNTS ONLY, NO SUBTRACTION. count_suppressed_strata() and
# count_suppressed_counties() (helper-live.R) return numbers of counties or
# state strata and nothing else. No area is named, and no count is ever
# subtracted from another: sums of published cells are compared with a
# published total by `==`, `<` and `>` only. Every count is reported with
# message().
#
# For each HIV indicator, among counties in both the stratified and the
# unstratified response whose unstratified count is published:
# 1. Counties with exactly one suppressed stratum cell. Asserted: none, for
#    every indicator and stratification, in 2022 and every later year; in
#    2017 no more than was observed (below).
# 2. Where every stratum is published: by sex and by age the strata add up
#    to the unstratified count in every county (asserted: none less, none
#    more). By race none add up to more (asserted); some add up to less, and
#    how many is reported, not asserted.
# 3. Counties whose published strata alone exceed the unstratified count.
#    Asserted: none.
# So that none of this passes on an empty set, each indicator must have
# counties with a published unstratified count, some of them with two or
# more suppressed cells and some with every stratum published. The response
# must be by the variable asked for and have exactly the strata the request
# asks for (the helper stops otherwise). A stratum with no row for a county
# is absent, not suppressed; counties in only one response are left out;
# both are counted and reported.
#
# Observed on 22 Nov 2026 UTC (live run 45017608026, whose code reported
# numbers of counties only: for the sum checks it added a county's published
# stratum counts and compared the sum directly with the published county
# total, without computing a difference; the earlier run 44985849638 printed
# the same counts), HIV diagnoses and HIV prevalence, by sex, age and race:
# - 2022, 2024 and 2025 (2025: diagnoses only; CDC had not released
#   prevalence): no county with exactly one suppressed stratum cell.
# - 2017, the spot-check year: exactly one in 1 county for diagnoses by age,
#   2 for diagnoses by race and 5 for prevalence by race; none in the other
#   three. These are pinned below as upper bounds. They were counted only,
#   and whether any of those cells could be worked out was not assessed.
#   2017 is also the only year with stratum cells that are neither published
#   nor suppressed: in 3 counties (diagnoses by age), 7 (diagnoses by race),
#   1 (prevalence by age) and 8 (prevalence by race).
# - By sex and by age, every fully published county's strata added up to its
#   unstratified count. By race the same for diagnoses; for prevalence 47
#   (2017), 51 (2022) and 54 (2024) of about 240 fully published counties
#   added up to less, and none to more.
# - No county's published strata exceeded its unstratified count.
# - Prevalence, counties by suppressed stratum cells, as none / exactly one
#   / two or more (not pinned; CDC revises its data):
#     2017: sex 1,790 / 0 / 934; age 751 / 0 / 1,973; race 234 / 5 / 2,485
#     2022: sex 1,826 / 0 / 950; age 793 / 0 / 1,983; race 244 / 0 / 2,532
#     2024: sex 1,868 / 0 / 945; age 806 / 0 / 2,007; race 244 / 0 / 2,569
#
# A year CDC releases later is held to none as well. That is the drift
# alert for a year not yet measured, not an observation: if it fails, the
# claim is corrected to what CDC returns.
observed_one_suppressed_2017 <- c(
  "HIV diagnoses age" = 1,
  "HIV diagnoses race" = 2,
  "HIV prevalence race" = 5
)
one_suppressed_allowed <- function(indicator, year, strat) {
  key <- paste(indicator, strat)
  if (year == 2017 && key %in% names(observed_one_suppressed_2017)) {
    return(observed_one_suppressed_2017[[key]])
  }
  0
}

# The stratum labels a request for `disease` by `strat` asks CDC for, from
# the request the package builds: its race, sex or age codes, as labels.
requested_strata <- function(strat, year, disease = "hiv") {
  ids <- .build_payload(disease, "national", year, strat)$VariableIDs
  ids <- as.numeric(strsplit(ids, ",")[[1]])
  type <- c(race = 5, sex = 6, age = 7)[[strat]]
  varvals$name[varvals$id %in% ids & varvals$vtid %in% type]
}

# Expects the counts of suppressed stratum cells for each of `indicators`, in
# one year's stratified and unstratified county responses, to be as last
# observed, and reports every count. A failure gives counts only.
expect_suppressed_strata_as_observed <- function(
  stratified,
  unstratified,
  indicators,
  year,
  strat
) {
  counts <- count_suppressed_strata(
    stratified,
    unstratified,
    strat,
    requested_strata(strat, year)
  )
  for (line in describe_suppressed_strata(counts, "Suppressed stratum cells")) {
    message(line)
  }
  for (indicator in indicators) {
    what <- paste0(indicator, " ", year, " by ", strat, ": ")
    row <- counts[counts$indicator == indicator & counts$year == year, ]
    if (nrow(row) != 1 || row$total_published == 0) {
      fail(paste0(
        what,
        "no county in both responses has a published unstratified count, ",
        "so nothing was checked."
      ))
      next
    }
    of_indicator <- function(rows) {
      as.character(rows$fips)[as.character(rows$indicator) == indicator]
    }

    # Matching the responses neither repeated nor lost a county.
    expect_equal(
      row$counties_in_both + row$counties_unstratified_only,
      length(of_indicator(unstratified)),
      label = paste0(what, "unstratified counties counted")
    )
    expect_equal(
      row$counties_in_both + row$counties_stratified_only,
      length(unique(of_indicator(stratified))),
      label = paste0(what, "stratified counties counted")
    )
    expect_equal(
      row$total_published + row$total_suppressed + row$total_other,
      row$counties_in_both,
      label = paste0(what, "counties by unstratified count")
    )
    expect_equal(
      row$none_suppressed + row$one_suppressed + row$two_or_more_suppressed,
      row$total_published,
      label = paste0(what, "counties by suppressed cells")
    )
    expect_equal(
      row$sum_equal + row$sum_less + row$sum_more,
      row$all_published,
      label = paste0(what, "counties with every stratum published")
    )

    # 1. Counties with exactly one suppressed stratum cell.
    expect(
      row$two_or_more_suppressed > 0,
      paste0(
        what,
        "no county with a published unstratified count has two or more ",
        "suppressed stratum cells, so there was nothing to count."
      )
    )
    allowed <- one_suppressed_allowed(indicator, year, strat)
    expect(
      row$one_suppressed <= allowed,
      paste0(
        what,
        row$one_suppressed,
        " of ",
        row$total_published,
        " counties with a published unstratified count have exactly one ",
        "suppressed stratum cell; at most ",
        allowed,
        " when last observed. CDC's response has changed."
      )
    )

    # 2. Fully published counties: the strata against the unstratified count.
    expect(
      row$all_published > 0,
      paste0(
        what,
        "no county with a published unstratified count has every stratum ",
        "published, so there were no sums to compare."
      )
    )
    expect(
      row$sum_more == 0,
      paste0(
        what,
        row$sum_more,
        " of ",
        row$all_published,
        " fully published counties have strata that add up to more than ",
        "the unstratified count; none when last observed."
      )
    )
    if (strat != "race") {
      expect(
        row$sum_less == 0,
        paste0(
          what,
          row$sum_less,
          " of ",
          row$all_published,
          " fully published counties have strata that add up to less than ",
          "the unstratified count; none when last observed."
        )
      )
    }

    # 3. Published strata alone above the unstratified count.
    expect(
      row$published_exceed == 0,
      paste0(
        what,
        row$published_exceed,
        " of ",
        row$total_published,
        " counties have published strata that alone exceed the ",
        "unstratified count; none when last observed."
      )
    )
  }
  invisible(counts)
}

# 4. State strata with exactly one suppressed county row, a state stratum
# being a state, indicator, year and stratum (unstratified: the state as a
# whole). expect_no_single_suppressed_county() (helper-live.R) asserts none
# only unstratified with published state totals, as observed, and reports
# the rest; it says why there.

for (year in c(2017, 2022)) {
  for (strat in list(NULL, "sex", "age", "race")) {
    local({
      year <- year
      strat <- strat
      label <- paste0(
        year,
        ", ",
        if (is.null(strat)) "unstratified" else paste("by", strat)
      )
      indicators <- .disease_indicators()
      indicators <- indicators$indicator[indicators$disease == "hiv"]

      # Every download here is shared with the pipeline runs above.
      if (!is.null(strat)) {
        test_that(
          paste(
            "CDC's suppressed stratum cells are as last observed, HIV",
            label
          ),
          {
            expect_suppressed_strata_as_observed(
              live_county(year, strat),
              live_county(year),
              indicators,
              year,
              strat
            )
          }
        )
      }

      test_that(
        paste(
          "suppressed county rows are counted for every state stratum, HIV",
          label
        ),
        {
          # No state totals: they would need a new request for this year, so
          # every state stratum is counted and the counts are reported only.
          expect_no_single_suppressed_county(
            live_county(year, strat),
            NULL,
            indicators,
            year,
            strat,
            if (!is.null(strat)) requested_strata(strat, year)
          )
        }
      )
    })
  }
}

# CDC's rates on suppressed rows in its latest years (#157). The runs above
# check 2017 and 2022; these check every year from 2024 to the latest year in
# the package's code table, so the set grows as CDC adds years. CDC lists a
# year before every indicator has data for it (HIV prevalence 2025, #149), so
# each indicator must have county rows in the latest year or one of the two
# before it, and the year used is reported. A year before 2024 is downloaded
# only if an indicator still has no rows. An earlier year is used for an
# indicator only when CDC answers a state request for the year with HIV rows
# and nothing else, some of them for the other indicator and none for this
# one (one small state request per year and stratification, see
# live_released_indicators()). A state request that fails or doesn't match
# the request, a county download that fails, or a latest year with no state
# rows at all for HIV fails the test rather than moving it to an earlier
# year. Nothing is imputed and the Census isn't asked.
for (strat in list(NULL, "sex", "age", "race")) {
  local({
    strat <- strat
    label <- if (is.null(strat)) "unstratified" else paste("by", strat)

    test_that(
      paste("suppressed rows have no rate in CDC's latest HIV years,", label),
      {
        latest <- .latest_atlas_year()
        window <- latest - 0:2
        indicators <- .disease_indicators()
        indicators <- indicators$indicator[indicators$disease == "hiv"]

        responses <- list()
        in_window <- function() responses[names(responses) %in% window]
        for (year in seq(latest, min(2024, latest - 2))) {
          found <- latest_year_with_rows(in_window(), indicators)
          if (year < 2024 && !anyNA(found)) {
            next
          }
          county <- live_county_if_released(year, strat)
          responses[[as.character(year)]] <- county
          expect_no_rate_on_suppressed(county, paste("hiv", year, label))
        }

        used <- latest_year_with_rows(in_window(), indicators)
        for (indicator in indicators) {
          message(
            indicator,
            ", ",
            label,
            ": latest year with county rows is ",
            used[[indicator]],
            " (package's latest year: ",
            latest,
            ")."
          )
          expect(
            !is.na(used[[indicator]]),
            paste0(
              "CDC returned no county rows for ",
              indicator,
              " (",
              label,
              ") in ",
              min(window),
              " to ",
              latest,
              "."
            )
          )
        }
      }
    )
  })
}

# The same monitor of CDC's suppression (#164) in its latest years: every
# year from 2024 to the latest in the package's code table, from the
# downloads the check above makes for those years (no request is added; the
# state rows are the ones its release check asked for). An indicator CDC has
# not released for a year (HIV prevalence 2025, #149) has no county rows in
# either response: that is reported, and the indicator must still be counted
# in at least one of these years.
for (strat in list(NULL, "sex", "age", "race")) {
  local({
    strat <- strat
    label <- if (is.null(strat)) "unstratified" else paste("by", strat)

    test_that(
      paste(
        "CDC's suppressed cells are as last observed in its latest HIV years,",
        label
      ),
      {
        indicators <- .disease_indicators()
        indicators <- indicators$indicator[indicators$disease == "hiv"]
        counted <- rlang::set_names(
          rep(NA_real_, length(indicators)),
          indicators
        )

        for (year in seq(.latest_atlas_year(), 2024)) {
          county <- live_county_if_released(year, strat)
          unstratified <- live_county_if_released(year)
          # Already made, and shown to have rows, by the line above.
          state <- live_without_unreleased_warning(live_state(year, strat))
          # An indicator with rows in only one response is not unreleased:
          # it is checked, and fails for having nothing to compare.
          released <- intersect(
            indicators,
            c(
              as.character(county$indicator),
              as.character(unstratified$indicator)
            )
          )
          for (indicator in setdiff(indicators, released)) {
            message(
              indicator,
              ", ",
              label,
              ": CDC has no county rows for ",
              year,
              ", so its suppressed cells were not counted for that year."
            )
          }
          if (!is.null(strat)) {
            expect_suppressed_strata_as_observed(
              county,
              unstratified,
              released,
              year,
              strat
            )
          }
          expect_no_single_suppressed_county(
            county,
            state,
            released,
            year,
            strat,
            if (!is.null(strat)) requested_strata(strat, year)
          )
          for (indicator in released) {
            counted[[indicator]] <- max(
              counted[[indicator]],
              year,
              na.rm = TRUE
            )
          }
        }

        for (indicator in indicators) {
          message(
            indicator,
            ", ",
            label,
            ": latest year with suppressed cells counted is ",
            counted[[indicator]],
            "."
          )
          expect(
            !is.na(counted[[indicator]]),
            paste0(
              "CDC returned no county rows for ",
              indicator,
              " (",
              label,
              ") in 2024 to ",
              .latest_atlas_year(),
              ", so its suppressed cells were not counted."
            )
          )
        }
      }
    )
  })
}

# The check of known warnings above only shows that the warning about states
# with many cases in no county row is allowed, not that it is given (#164).
# In the first measured run (HIV prevalence 2022, 21 Nov 2026) Alaska had 87%
# of its state total in no county row, so the national unstratified 2022 run
# must give the warning and name Alaska under HIV prevalence 2022. The
# warning also has an entry for diagnoses, so Alaska is looked for in the
# prevalence entry only (see unplaced_warning_names()); its percentage is
# not checked. Uses the run the tests above share: no new request.
# live_once() keeps each warning's message but not its class, so the warning
# is found by its text.
test_that("the 2022 run warns about states with many cases in no county row, naming Alaska (#164)", {
  warnings <- live_pipeline(2022)$warnings
  unplaced <- warnings[grepl(
    "published state total in no county row",
    warnings
  )]
  expect_length(unplaced, 1)
  expect_true(
    unplaced_warning_names(unplaced, "HIV prevalence 2022", "Alaska")
  )
})

test_that("2017 allocates Connecticut's planning regions, Chugach and Copper River (#30)", {
  run <- live_pipeline(2017)
  planning_regions <- sprintf("091%d0", 1:9)
  expect_contains(unique(run$areas$county_fips), c("02063", "02066"))
  expect_gt(
    length(intersect(unique(run$areas$county_fips), planning_regions)),
    0
  )

  left_out <- attr(run$areas, "not_allocated")
  no_tracts <- left_out$county_fips[
    left_out$reason == "no ACS tracts for this county"
  ]
  expect_none(setdiff(no_tracts, "02261"), "counties with no 2010 tracts")
})

test_that("2022 leaves out Valdez-Cordova (02261) and allocates its successors", {
  run <- live_pipeline(2022)
  left_out <- attr(run$areas, "not_allocated")
  expect_contains(left_out$county_fips, "02261")
  expect_contains(unique(run$areas$county_fips), c("02063", "02066"))
})

test_that("2022 block group estimates add up to county counts, by sex", {
  areas <- live_pipeline(2022, "sex", to = "block group")$areas
  check <- areas |>
    dplyr::summarize(
      county_cases = dplyr::first(county_cases_allocated),
      area_cases_sum = sum(area_cases),
      .by = dplyr::all_of(county_groups)
    )
  expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-6)
  expect_equal(unique(nchar(areas$geoid)), 12)
})

# Fitting to county totals (#179) ------------------------------------------------
# impute_suppressed(fit_to = "county_total") on real data, 2017 and 2022, by
# sex and by age, using the county downloads above (no new county request).
# Checks what the fit guarantees whatever CDC returns, and logs aggregate
# counts only: county groups by reason, and warnings by class. Comparisons
# are counts of mismatches, so even a failure prints no county's values.

for (year in c(2017, 2022)) {
  for (strat in c("sex", "age")) {
    local({
      year <- year
      strat <- strat
      label <- paste(year, "by", strat)
      test_that(
        paste("fitting to county totals keeps its guarantees,", label),
        {
          run <- live_fit(year, strat)
          imputed <- run$imputed
          key <- paste(imputed$fips, imputed$indicator, imputed$year)

          groups <- unique(data.frame(
            key = key,
            indicator = as.character(imputed$indicator),
            reason = imputed$fit_reason
          )[!is.na(imputed$fit_reason), ])
          counts <- table(groups$indicator, groups$reason)
          message(
            "Fit, HIV ",
            label,
            " (",
            Sys.getenv("GITHUB_SHA", "local"),
            ", retrieved ",
            format(Sys.Date()),
            "): county groups by reason\n",
            paste(utils::capture.output(print(counts)), collapse = "\n"),
            "\nWarnings by class: ",
            live_warning_counts(run$warnings)
          )

          # Both HIV indicators are released for these years, and each has
          # fitted groups, so the checks below aren't vacuous.
          expect_setequal(
            unique(as.character(imputed$indicator)),
            c("HIV diagnoses", "HIV prevalence")
          )
          expect_equal(unique(as.numeric(as.character(imputed$year))), year)
          for (indicator in c("HIV diagnoses", "HIV prevalence")) {
            fitted <- groups$reason == "fitted to county total" &
              groups$indicator == indicator
            expect_gt(sum(fitted), 0, label = paste(indicator, label))
          }

          # Published counts unchanged; every unpublished row has a reason.
          published <- !is.na(run$county$cases)
          expect_equal(
            live_mismatches(
              imputed$cases[published],
              run$county$cases[published]
            ),
            0
          )
          expect_equal(sum(is.na(imputed$fit_reason) & !published), 0)
          expect_equal(
            sum(imputed$cases_source == "county_fit"),
            sum(imputed$fit_reason %in% "fitted to county total")
          )

          # A fitted group has two or more suppressed cells, and its cells
          # add up to the county's published total.
          fit_rows <- imputed$cases_source == "county_fit"
          fit_keys <- unique(key[fit_rows])
          cells <- table(key[fit_rows])
          expect_equal(sum(cells < 2), 0)
          sums <- as.numeric(tapply(imputed$cases, key, sum)[fit_keys])
          totals <- run$totals
          total <- totals$cases[match(
            fit_keys,
            paste(totals$fips, totals$indicator, totals$year)
          )]
          expect_equal(live_mismatches(sums, as.numeric(total)), 0)
        }
      )
    })
  }
}

test_that("fitted counts reach the tracts, with spread cases kept apart, 2022 by sex", {
  skip_unless_census()
  run <- live_fit(2022, "sex")
  imputed <- run$imputed
  # Three states with fitted rows, to keep the Census downloads small.
  states <- utils::head(
    sort(unique(substr(
      imputed$fips[imputed$cases_source == "county_fit"],
      1,
      2
    ))),
    3
  )
  imputed <- imputed[substr(imputed$fips, 1, 2) %in% states, ]
  county <- run$county
  state <- run$state
  local_mocked_bindings(
    .fetch_county_totals = function(x) county,
    .fetch_state_totals = function(x) state
  )
  result <- live_collect(
    extrapolate_from_county(imputed, no_county = "spread"),
    "allocating HIV 2022 by sex"
  )
  areas <- result$value
  fitted <- areas[areas$cases_source == "county_fit", ]
  spread <- unique(fitted[fitted$county_cases_spread > 0, county_groups])
  message(
    "Fit then spread, HIV 2022 by sex, states ",
    paste(states, collapse = ", "),
    ": fitted county strata receiving spread cases: ",
    nrow(spread),
    "; warnings by class: ",
    live_warning_counts(result$warnings)
  )
  expect_gt(nrow(fitted), 0)
  expect_equal(
    live_mismatches(
      fitted$county_cases_allocated,
      fitted$county_cases +
        fitted$county_cases_spread +
        fitted$county_cases_unknown
    ),
    0
  )
  check <- areas |>
    dplyr::summarize(
      county_cases = dplyr::first(county_cases_allocated),
      area_cases_sum = sum(area_cases),
      .by = dplyr::all_of(county_groups)
    )
  expect_equal(
    live_mismatches(check$area_cases_sum, check$county_cases, 1e-6),
    0
  )
  # The checks above also pass when nothing was spread; say so rather than
  # report a pass for a path the data didn't exercise.
  if (nrow(spread) == 0) {
    skip("No fitted county stratum received spread cases in these states.")
  }
})
