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
  imputed <- impute_suppressed(county, cap = 2)

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

live_tracts <- function(year, stratify_by = NULL, to = "tract") {
  skip_unless_census()
  local_mocked_bindings(.request_atlas = .request_atlas_internal, .env = parent.frame())
  county <- suppressMessages(
    get_atlas("hiv", "county", year, stratify_by = stratify_by)
  )
  suppressMessages(extrapolate_from_county(impute_suppressed(county), to = to))
}

check_adds_up <- function(tracts) {
  check <- tracts |>
    dplyr::summarize(
      county_values = dplyr::n_distinct(county_cases),
      county_cases = dplyr::first(county_cases),
      area_cases_sum = sum(area_cases),
      .by = c(county_fips, indicator, race_ethnicity, sex, age)
    )
  expect_equal(check$county_values, rep(1, nrow(check)))
  expect_equal(check$area_cases_sum, check$county_cases, tolerance = 1e-6)
  expect_equal(sum(is.na(tracts$area_cases)), 0)
  expect_gte(min(tracts$area_cases), 0)
  expect_equal(sum(duplicated(tracts[c(
    "geoid", "indicator", "race_ethnicity", "sex", "age"
  )])), 0)
}

for (strat in list(NULL, "sex", "age", "race")) {
  label <- if (is.null(strat)) "unstratified" else paste("by", strat)

  test_that(paste("2022 tract estimates add up to county counts,", label), {
    check_adds_up(live_tracts(2022, strat))
  })

  test_that(paste("2017 tract estimates add up, except Connecticut,", label), {
    expect_warning(tracts <- live_tracts(2017, strat), "no matching\\s+ACS tracts")
    check_adds_up(tracts)
    left_out <- attr(tracts, "not_allocated")
    no_tracts <- left_out[left_out$reason == "no ACS tracts for this county", ]
    expect_equal(unique(substr(no_tracts$county_fips, 1, 2)), "09")
  })
}

test_that("2022 block group estimates add up to county counts, by sex", {
  areas <- live_tracts(2022, "sex", to = "block group")
  check_adds_up(areas)
  expect_equal(unique(nchar(areas$geoid)), 12)
})
