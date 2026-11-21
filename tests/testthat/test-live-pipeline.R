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
# for each) but no Census requests.

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

# Warnings the default pipeline is known to give, by year. Chlamydia by age
# also warns that suppressed rows couldn't be estimated (unknown age and
# Connecticut's old counties; the rows themselves are checked above).
known_warnings <- c(
  "2017" = "no matching ACS tracts|Planning Region, CT|Valdez-Cordova|Municipio, PR",
  "2022" = "Valdez-Cordova Census Area, AK|no matching ACS tracts|Municipio, PR"
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

# CDC's rates on suppressed rows in its latest years (#157). The runs above
# check 2017 and 2022; these check every year from 2024 to the latest year in
# the package's code table, so the set grows as CDC adds years. CDC lists a
# year before every indicator has data for it (HIV prevalence 2025, #149), so
# each indicator must have county rows in the latest year or one of the two
# before it, and the year used is reported. A year before 2024 is downloaded
# only if an indicator still has no rows. An earlier year is used only when
# CDC answers a state request for the year with rows, none of them for the
# indicator (one small state request per year and stratification, see
# live_county_if_released()). A state request that fails, a county download
# that fails, or a latest year with no state rows at all for HIV fails the
# test rather than moving it to an earlier year. Nothing is imputed and the
# Census isn't asked.
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
          responses[as.character(year)] <- list(county)
          if (is.null(county)) {
            message(
              "HIV ",
              year,
              ", ",
              label,
              ": no HIV rows among CDC's state rows."
            )
          } else {
            expect_no_rate_on_suppressed(county, paste("hiv", year, label))
          }
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
