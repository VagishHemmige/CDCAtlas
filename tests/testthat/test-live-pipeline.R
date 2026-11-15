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
# are the ones HOPE uses.

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
    pa <- report[report$state_fips == "42" & report$indicator == "HIV prevalence", ]
    expect_equal(pa$unpublished_cases, expected[[year]])
  }
})

test_that("imputation respects the cap and keeps published counts, 2022", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  county <- live_county(2022, "sex")
  # Valdez-Cordova (02261) has no CDC population in 2022 (see below).
  expect_warning(imputed <- impute_suppressed(county, cap = 2), "Valdez-Cordova")

  estimated <- imputed$cases[imputed$cases_source == "state_rate"]
  expect_gt(length(estimated), 0)
  expect_lte(max(estimated, na.rm = TRUE), 2)
  expect_gt(max(estimated, na.rm = TRUE), 0)

  published <- imputed[imputed$cases_source == "published", ]
  original <- county$cases[match(
    paste(published$fips, published$indicator, published$sex),
    paste(county$fips, county$indicator, county$sex)
  )]
  expect_equal(published$cases, original)
})

# County rows the default pipeline is known to leave out. Anything else in
# attr(, "not_allocated") is new and fails the test below. Found by running
# these tests on 30 October 2026.
#   - Race "Unknown": no Census population for it.
#   - CDC reports every year on its current county map, so older years don't
#     match their ACS tract vintage (#30):
#     - 2017, Connecticut: planning regions have no 2010 tracts and no CDC
#       population, so suppressed ones can't be estimated either.
#     - 2017, Alaska: Chugach (02063) and Copper River (02066), created in
#       2019, have counts but no 2010 tracts; Valdez-Cordova (02261), which
#       the 2010 tracts belong to, has no CDC population.
#   - 2022, Alaska: CDC still lists Valdez-Cordova (02261) beside its
#     successors; it has no CDC population and no 2020 tracts.
#   - Puerto Rico, by race: suppressed municipio rows can't be estimated
#     (no state rate or municipio population by race from CDC).
no_area_or_estimate <- c(
  "suppressed, not estimated", "no ACS tracts for this county"
)
is_known_left_out <- function(left_out, year) {
  unknown_race <- left_out$reason == "no Census population for this stratum" &
    left_out$race_ethnicity == "Unknown"
  connecticut <- year == 2017 & left_out$reason %in% no_area_or_estimate &
    substr(left_out$county_fips, 1, 2) == "09"
  valdez_cordova_split <- left_out$reason %in% no_area_or_estimate & (
    (year == 2017 & left_out$county_fips %in% c("02063", "02066", "02261")) |
      (year == 2022 & left_out$county_fips == "02261")
  )
  puerto_rico_race <- left_out$reason == "suppressed, not estimated" &
    substr(left_out$county_fips, 1, 2) == "72" &
    left_out$race_ethnicity != "All races/ethnicities"
  unknown_race | connecticut | valdez_cordova_split | puerto_rico_race
}

# Warnings the default pipeline is known to give, by year.
known_warnings <- c(
  "2017" = "no matching ACS tracts|Planning Region, CT|Valdez-Cordova|Municipio, PR",
  "2022" = "Valdez-Cordova Census Area, AK|no matching ACS tracts|Municipio, PR"
)

county_groups <- c("county_fips", "indicator", "race_ethnicity", "sex", "age")

# Fails listing every item in `x`, so a live failure says what was unexpected.
expect_none <- function(x, what) {
  x <- unique(x)
  expect(
    length(x) == 0,
    paste0(length(x), " unexpected ", what, ":\n", paste("*", x, collapse = "\n"))
  )
}

for (year in c(2017, 2022)) {
  for (strat in list(NULL, "sex", "age", "race")) {
    label <- paste0(year, ", ", if (is.null(strat)) "unstratified" else paste("by", strat))

    test_that(paste("tract estimates add up to the counts they share,", label), {
      areas <- live_pipeline(year, strat)$areas
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
      expect_equal(sum(duplicated(areas[c("geoid", county_groups[-1])])), 0)
    })

    test_that(paste("every county row is allocated or listed as left out,", label), {
      run <- live_pipeline(year, strat)
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
      expect_none(readable(intersect(allocated, left_out)), "rows both allocated and left out")
      expect_none(readable(setdiff(county_rows, c(allocated, left_out))), "county rows missing")
      expect_none(readable(setdiff(c(allocated, left_out), county_rows)), "rows not in the county data")
    })

    test_that(paste("only known county rows are left out,", label), {
      left_out <- attr(live_pipeline(year, strat)$areas, "not_allocated")
      unexpected <- left_out[!is_known_left_out(left_out, year), ]
      expect_none(
        paste0(unexpected$county_fips, " ", unexpected$county_name, ": ",
               unexpected$reason, recycle0 = TRUE),
        "county rows left out"
      )
    })

    test_that(paste("only known warnings are given,", label), {
      warnings <- live_pipeline(year, strat)$warnings
      expect_none(
        warnings[!grepl(known_warnings[[as.character(year)]], warnings)],
        "warnings"
      )
    })
  }
}

test_that("2017 leaves out Connecticut and the Valdez-Cordova split, and only them", {
  left_out <- attr(live_pipeline(2017)$areas, "not_allocated")
  no_tracts <- left_out[left_out$reason %in% no_area_or_estimate, ]
  expect_setequal(unique(substr(no_tracts$county_fips, 1, 2)), c("02", "09"))
  expect_setequal(
    unique(no_tracts$county_fips[substr(no_tracts$county_fips, 1, 2) == "02"]),
    c("02063", "02066", "02261")
  )
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
