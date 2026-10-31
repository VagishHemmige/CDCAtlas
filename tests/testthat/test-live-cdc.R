# Live checks against CDC AtlasPlus. These call CDC for real, so they are
# skipped unless switched on:
#
#   Sys.setenv(CDCATLAS_LIVE_TESTS = "true")
#   devtools::test(filter = "live-cdc")
#
# They bypass the on-disk cache, so they test what CDC returns today. The years
# are the ones HOPE uses.

skip_if_not(
  identical(Sys.getenv("CDCATLAS_LIVE_TESTS"), "true"),
  "Live CDC tests are off. Set CDCATLAS_LIVE_TESTS=true to run them."
)
skip_if_offline("gis.cdc.gov")

for (year in c(2017, 2022)) {
  test_that(paste("state HIV request returns every state,", year), {
    local_mocked_bindings(.request_atlas = .request_atlas_internal)
    out <- get_atlas("hiv", "state", year)

    expect_setequal(as.character(unique(out$indicator)), c("HIV diagnoses", "HIV prevalence"))
    states_and_dc <- c(sprintf("%02d", c(1:2, 4:6, 8:13, 15:42, 44:51, 53:56)))
    expect_contains(out$fips, states_and_dc)
    expect_equal(sum(duplicated(out[c("fips", "indicator")])), 0)
  })

  test_that(paste("county HIV request returns every state's counties,", year), {
    local_mocked_bindings(.request_atlas = .request_atlas_internal)
    out <- suppressMessages(get_atlas("hiv", "county", year))

    prevalence <- out[out$indicator == "HIV prevalence", ]
    expect_gt(nrow(prevalence), 3000)
    expect_equal(sum(duplicated(prevalence$fips)), 0)
    county_states <- unique(substr(prevalence$fips, 1, 2))
    expect_contains(county_states, c("01", "06", "36", "48"))
    expect_gte(length(county_states), 51)
  })
}

test_that("national HIV request matches AtlasPlus, 2022", {
  # Values shown by AtlasPlus on 28 October 2026. If CDC revises its 2022
  # figures, check the new values in AtlasPlus and update these.
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  out <- get_atlas("hiv", "national", 2022)

  expect_equal(nrow(out), 2)
  expect_equal(as.character(unique(out$geography)), "United States")
  expect_equal(out$cases[out$indicator == "HIV diagnoses"], 37685)
  expect_equal(out$cases[out$indicator == "HIV prevalence"], 1087524)
})

test_that("national HIV request returns both indicators, 2017", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  out <- get_atlas("hiv", "national", 2017)

  expect_setequal(as.character(out$indicator), c("HIV diagnoses", "HIV prevalence"))
  expect_gt(out$cases[out$indicator == "HIV prevalence"], 900000)
})

# Tract pipeline with real CDC and Census data. Needs tidycensus and a Census
# API key (CENSUS_API_KEY); each extrapolation downloads tract populations for
# every state, so expect several minutes per test.

skip_unless_census <- function() {
  skip_if_not_installed("tidycensus")
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "No Census API key (CENSUS_API_KEY).")
}

for (year in c(2017, 2022)) {
  test_that(paste("Census variable codes have the expected labels,", year), {
    skip_if_not_installed("tidycensus")
    labels <- tidycensus::load_variables(year, "acs5")
    label_of <- function(code) {
      gsub(":", "", labels$label[match(code, labels$name)])
    }

    bands <- .acs_age_bands()
    expect_equal(
      label_of(bands$male),
      paste0("Estimate!!Total!!Male!!", bands$band)
    )
    expect_equal(
      label_of(bands$female),
      paste0("Estimate!!Total!!Female!!", bands$band)
    )
    expect_equal(label_of("B01001_001"), "Estimate!!Total")

    race <- .acs_race_variables()
    expect_equal(label_of(race$variable), paste0("Estimate!!Total!!", race$label))

    tables <- .acs_race_age_tables()
    race_bands <- .acs_race_age_bands()
    for (i in seq_len(nrow(tables))) {
      codes <- sprintf("%s_%03d", tables$table[i], c(3:16, 18:31))
      expect_equal(
        label_of(codes),
        paste0(
          "Estimate!!Total!!",
          rep(c("Male!!", "Female!!"), each = 14),
          race_bands$band
        )
      )
      concept <- labels$concept[match(codes[1], labels$name)]
      expect_equal(
        toupper(concept),
        toupper(paste0("Sex by Age (", tables$concept[i], ")"))
      )
    }
  })
}

test_that("no-county remainders match #27 (Pennsylvania HIV prevalence)", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  expected <- c("2017" = 6292, "2022" = 1140)

  for (year in names(expected)) {
    county <- suppressMessages(get_atlas("hiv", "county", as.numeric(year)))
    report <- no_county_cases(county)
    pa <- report[report$state_fips == "42" & report$indicator == "HIV prevalence", ]
    expect_equal(pa$unpublished_cases, expected[[year]])
  }
})

test_that("imputation respects the cap and keeps published counts, 2022", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  county <- suppressMessages(get_atlas("hiv", "county", 2022, stratify_by = "sex"))
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

# Full default pipeline on real data: get_atlas() -> impute_suppressed() ->
# extrapolate_from_county(), every option at its default, as HOPE runs it. Each
# run is downloaded once and shared by the tests below.

live_runs <- new.env()

live_pipeline <- function(year, stratify_by = NULL, to = "tract") {
  key <- paste(year, stratify_by %||% "none", to)
  if (is.null(live_runs[[key]])) {
    skip_unless_census()
    local_mocked_bindings(.request_atlas = .request_atlas_internal)
    warnings <- character()
    keep_warning <- function(w) {
      warnings <<- c(warnings, cli::ansi_strip(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
    withCallingHandlers(
      suppressMessages({
        county <- get_atlas("hiv", "county", year, stratify_by = stratify_by)
        imputed <- impute_suppressed(county)
        areas <- extrapolate_from_county(imputed, to = to)
      }),
      warning = keep_warning
    )
    live_runs[[key]] <- list(imputed = imputed, areas = areas, warnings = warnings)
  }
  live_runs[[key]]
}

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
