# Made-up CDC for data-raw/measured-effects.R: used by
# test-measured-effects-script.R and by the script's own synthetic mode, which
# sources this file. The made-up tables it answers with are in
# helper-pipeline.R.

# Stands in for CDC in the test of data-raw/measured-effects.R: answers state
# requests with state rows and one state's county request with county rows,
# for 2017, 2022 and 2023, in any one stratification. For any other year the
# states have diagnoses but not prevalence, as CDC answered for HIV 2025 in
# November 2026: a year released in part.
report_fake_cdc <- function(payload) {
  ids <- as.integer(strsplit(payload$VariableIDs, ",")[[1]])
  stratify_by <- if (602 %in% ids) {
    "sex"
  } else if (656 %in% ids) {
    "age"
  } else if (553 %in% ids) {
    "race"
  }
  years <- c(2017, 2022, 2023)
  county_ids <- lookup[["counties"]]$geo_id
  if (any(ids %in% county_ids)) {
    fips <- lookup[["counties"]]$fips[match(
      ids[ids %in% county_ids],
      county_ids
    )]
    rows <- fake_county_atlas(stratify_by, suppressed = "01003", years = years)
    rows <- rows[substr(rows$fips, 1, 2) %in% substr(fips, 1, 2), ]
  } else {
    asked <- ids[ids %in% varvals$id[varvals$vtid == 2]] + 1478
    part <- setdiff(asked, years)
    rows <- fake_state_atlas(
      no_county = 50,
      stratify_by = stratify_by,
      years = c(years, part)
    )
    rows <- rows[!(rows$year %in% part & rows$indicator == "HIV prevalence"), ]
  }
  fake_state_response(rows, payload)
}
