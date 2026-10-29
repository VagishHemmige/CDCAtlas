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
