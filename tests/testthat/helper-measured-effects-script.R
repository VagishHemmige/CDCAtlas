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

# Made-up imputed county data for the script's consistency check: one
# unstratified table and one by sex, as impute_suppressed() returns them (the
# columns the check reads). `strata` gives each county's Male and Female
# cases, and which were suppressed cells.
#
# | County | Unstratified | Male        | Female        | What it shows            |
# |--------|--------------|-------------|---------------|--------------------------|
# | 01001  | 100          | 60          | 40            | all published, ratio 1   |
# | 01003  | 50           | supp. -> 4  | supp. -> 4    | two capped, ratio 0.16   |
# | 01005  | 20           | 15          | supp. -> 3    | exactly one suppressed   |
# | 01007  | supp. -> 4   | 2           | 2             | total not published      |
# | 01009  | 30           | supp. -> 4  | supp., no est.| a stratum has no count   |
# | 01011  | 10           | 5           | 4             | short by 1, none supp.   |
# | 01013  | 7            | (no rows)   | (no rows)     | only in the unstratified |
# | 01015  | (no row)     | 3           | 3             | only in the stratified   |
# | 01017  | 0            | 0           | 0             | published zero           |
# | 01019  | 5            | supp. -> 4  | supp. -> 4    | two capped, ratio 1.6    |
consistency_fixture <- function() {
  rows <- function(fips, sex, cases, source) {
    suppressed <- source != "published"
    data.frame(
      indicator = "HIV prevalence",
      year = 2022,
      fips = fips,
      geography = paste("County", fips),
      race_ethnicity = "All races/ethnicities",
      sex = sex,
      age = "Ages 13 years and older",
      data_status = ifelse(suppressed, "Data suppressed", "Not Suppressed"),
      cases = cases,
      cases_source = source,
      population = 1000
    )
  }
  pub <- "published"
  est <- "state_rate"
  unstratified <- rows(
    c(
      "01001",
      "01003",
      "01005",
      "01007",
      "01009",
      "01011",
      "01013",
      "01017",
      "01019"
    ),
    "Both sexes",
    c(100, 50, 20, 4, 30, 10, 7, 0, 5),
    c(pub, pub, pub, est, pub, pub, pub, pub, pub)
  )
  counties <- c(
    "01001",
    "01003",
    "01005",
    "01007",
    "01009",
    "01011",
    "01015",
    "01017",
    "01019"
  )
  stratified <- rbind(
    rows(
      counties,
      "Male",
      c(60, 4, 15, 2, 4, 5, 3, 0, 4),
      c(pub, est, pub, pub, est, pub, pub, pub, est)
    ),
    rows(
      counties,
      "Female",
      c(40, 4, 3, 2, NA, 4, 3, 0, 4),
      c(pub, est, est, pub, "not estimated", pub, pub, pub, est)
    )
  )
  list(unstratified = unstratified, stratified = stratified)
}
