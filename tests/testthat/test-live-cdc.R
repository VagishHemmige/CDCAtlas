# Quick live checks against CDC AtlasPlus and the Census API: a few small
# requests (state, national, CDC's code table, Census variable labels). They
# take about a minute. The county downloads and the full pipeline are in
# test-live-pipeline.R. Both are skipped unless switched on:
#
#   Sys.setenv(CDCATLAS_LIVE_TESTS = "true")
#   devtools::test(filter = "live-cdc")       # these checks
#   devtools::test(filter = "live")           # every live check
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

test_that("region HIV estimates return the four regions and the territories, 2021", {
  # The 2021 Northeast estimated incidence was 4,100 when checked on
  # 15 November 2026.
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  out <- get_atlas("estimate", "region", 2021)
  regions <- c("Northeast", "Midwest", "South", "West")

  expect_setequal(
    as.character(unique(out$geography)),
    c(regions, "U.S. territories and freely associated states")
  )
  expect_equal(unique(out$year), 2021)
  expect_equal(sum(duplicated(out[c("geography", "indicator")])), 0)
  for (region in regions) {
    expect_equal(
      sort(as.character(out$indicator[out$geography == region])),
      sort(c(
        "Estimated HIV incidence",
        "Estimated HIV prevalence (undiagnosed and diagnosed)",
        "Knowledge of Status"
      )),
      label = paste("indicators for", region)
    )
  }
  northeast <- out[out$geography == "Northeast" & out$indicator == "Estimated HIV incidence", ]
  expect_equal(northeast$cases, 4100, ignore_attr = "label")
})

test_that("national HIV request returns both indicators, 2017", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  out <- get_atlas("hiv", "national", 2017)

  expect_setequal(as.character(out$indicator), c("HIV diagnoses", "HIV prevalence"))
  expect_gt(out$cases[out$indicator == "HIV prevalence"], 900000)
})

# The help for get_atlas() describes these indicators; these checks keep it
# true.
test_that("each disease returns the indicators its help describes, 2021", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  indicators <- function(disease) {
    as.character(unique(get_atlas(disease, "national", 2021)$indicator))
  }

  expect_setequal(
    indicators("adult syphilis"),
    c(
      "Primary and Secondary Syphilis",
      "Early Non-Primary, Non-Secondary Syphilis",
      "Unknown Duration or Late Syphilis"
    )
  )
  expect_setequal(
    indicators("hepatitis"),
    c("Hepatitis  A", "Acute Viral Hepatitis B", "Acute Viral Hepatitis C")
  )
})

test_that("reported counts have no confidence intervals and estimates do, 2021", {
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  reported <- get_atlas("hiv", "national", 2021)
  estimated <- get_atlas("estimate", "national", 2021)
  ci <- c("lowerci_rate", "upperci_rate", "lowerci_cases", "upperci_cases")

  expect_equal(sum(!is.na(as.matrix(reported[ci]))), 0)
  expect_setequal(
    as.character(unique(estimated$indicator)),
    c(
      "Estimated HIV incidence",
      "Estimated HIV prevalence (undiagnosed and diagnosed)",
      "Knowledge of Status"
    )
  )
  counts <- estimated[estimated$indicator != "Knowledge of Status", ]
  expect_equal(sum(is.na(as.matrix(counts[c("lowerci_cases", "upperci_cases")]))), 0)
  expect_equal(unique(estimated$year), 2021)
  expect_equal(as.character(unique(estimated$geography)), "United States")

  # Knowledge of Status: the percentage is in rate100000, with its bounds in
  # the rate CI columns; cases are those who know their status, out of
  # estimated prevalence (population); there are no count CIs.
  status <- estimated[estimated$indicator == "Knowledge of Status", ]
  expect_equal(nrow(status), 1)
  expect_gt(status$rate100000, 50)
  expect_lte(status$rate100000, 100)
  expect_lt(status$lowerci_rate, status$rate100000)
  expect_gt(status$upperci_rate, status$rate100000)
  expect_equal(status$rate100000, 100 * status$cases / status$population, tolerance = 0.01)
  expect_equal(sum(!is.na(c(status$lowerci_cases, status$upperci_cases))), 0)
})

test_that("stored codes still match CDC's code table", {
  # R/sysdata.rda is built by data-raw/DATASET.R. CDC may add codes (a new
  # year, say); that is fine. A stored code that CDC dropped or relabelled
  # means get_atlas() would send or label something wrong: rerun DATASET.R.
  current <- tibble::as_tibble(.atlas_init_data()$varvals)
  cols <- c("id", "name", "vtid", "fips")

  expect_contains(current$id, varvals$id)
  stored <- varvals[cols]
  expect_equal(
    current[match(stored$id, current$id), cols],
    stored,
    ignore_attr = TRUE
  )

  for (level in names(lookup)) {
    expect_contains(current$id, lookup[[level]]$geo_id)
    missing_fips <- sum(is.na(lookup[[level]]$fips))
    expect_equal(missing_fips, 0, label = paste(level, "missing FIPS"))
    rows <- match(lookup[[level]]$geo_id, current$id)
    expect_equal(current$fips[rows], lookup[[level]]$fips, label = level)
  }
})

# CDC's set-up data list, for each indicator, every combination of codes
# AtlasPlus offers; `data_structure` names the variable type in each column
# (7 is age, 50 is the location).
cdc_offered <- function(variable_type) {
  combo <- .atlas_init_data()$combo
  column <- match(variable_type, combo$data_structure)
  do.call(rbind, lapply(combo$diseases$combinations, function(m) {
    m <- if (is.matrix(m)) m else do.call(rbind, m)
    data.frame(indicator = m[, 1], code = m[, column])
  }))
}

indicator_ids <- function(disease) {
  lookup <- .disease_indicators()
  varvals$id[varvals$vtid == 1 &
    varvals$name %in% lookup$indicator[lookup$disease == disease]]
}

test_that("each disease's age codes are ones CDC offers for its indicators (#11)", {
  offered <- cdc_offered("7")
  for (disease in unique(.disease_indicators()$disease)) {
    codes <- .atlas_age_codes(disease, by_age = FALSE)
    if (disease != "congenital syphilis") {
      codes <- c(codes, .atlas_age_codes(disease, by_age = TRUE))
    }
    for (id in indicator_ids(disease)) {
      expect_contains(offered$code[offered$indicator == id], codes)
    }
  }
})

test_that("each disease's geographies are the ones CDC offers for its indicators", {
  offered <- cdc_offered("50")
  geography <- c(
    "1001" = "national", "1002" = "state", "1003" = "county",
    "1004" = "msa", "1005" = "region"
  )
  for (disease in unique(.disease_indicators()$disease)) {
    for (id in indicator_ids(disease)) {
      cdc <- unname(geography[as.character(unique(offered$code[offered$indicator == id]))])
      expect_setequal(.atlas_geographies(disease), cdc)
    }
  }
})

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

test_that("HIV diagnoses are released for the latest stored year", {
  # Checks data availability only: CDC listing a year before releasing HIV
  # diagnoses for it would fail here. Whether CDC now lists a later year than
  # the stored code table is a separate question this test does not answer.
  local_mocked_bindings(.request_atlas = .request_atlas_internal)
  latest <- .latest_atlas_year()
  out <- get_atlas("hiv", "national", latest)
  diagnoses <- out[out$indicator == "HIV diagnoses", ]

  expect_equal(nrow(diagnoses), 1)
  expect_equal(diagnoses$year, latest)
  expect_equal(as.character(diagnoses$geography), "United States")
})
