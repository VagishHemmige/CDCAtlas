# .build_payload() turns one request into the comma-separated CDC variable
# codes sent to AtlasPlus, and .build_payloads() splits a request into one
# payload per disease and year, and for counties per state. Each check of codes
# compares the exact set of codes of one CDC variable type (`varvals$vtid`),
# so a code sent by mistake fails as well as one left out. The tests of
# indicators, years, states, MSAs, sex and transmission were ported from the
# early drafts on claude/wip-early-tests (#23).

payload_codes <- function(..., vtid) {
  ids <- as.integer(strsplit(.build_payload(...)$VariableIDs, ",")[[1]])
  sort(intersect(ids, varvals$id[varvals$vtid == vtid]))
}

test_that("a payload is a list holding one comma-separated string of codes", {
  payload <- .build_payload("gonorrhea", "national", 2022)
  expect_type(payload, "list")
  expect_named(payload, "VariableIDs")
  expect_identical(payload$VariableIDs, "207,5000,544,651,551,601,801")
})

test_that("each disease asks for its own indicators", {
  expected <- list(
    "chlamydia" = 206,
    "gonorrhea" = 207,
    "adult syphilis" = c(208, 213, 290),
    "congenital syphilis" = 214,
    "tuberculosis" = 209,
    "hiv" = c(203, 205),
    "estimate" = 285:287,
    "hepatitis" = 210:212
  )
  for (disease in names(expected)) {
    expect_equal(
      payload_codes(disease, "national", 2022, vtid = 1),
      expected[[disease]],
      label = disease
    )
  }
})

test_that("the year code is the calendar year minus 1478, as in CDC's table", {
  expect_equal(payload_codes("hiv", "national", 2022, vtid = 2), 544)

  # Every calendar year CDC lists has the code its name minus 1478. The year
  # table also holds "Change over time" (1) and "5-year estimates (2019-2023)"
  # (3000), which get_atlas() never requests.
  years <- varvals[varvals$vtid == 2 & grepl("^[0-9]{4}$", varvals$name), ]
  expect_equal(years$id, as.integer(years$name) - 1478)
})

test_that("state and MSA requests ask for every state or every MSA, and nothing else", {
  expect_equal(
    payload_codes("hiv", "state", 2022, vtid = 3),
    sort(lookup[["state"]]$geo_id)
  )
  expect_equal(
    payload_codes("hiv", "msa", 2022, vtid = 3),
    sort(lookup[["msa"]]$geo_id)
  )
})

test_that("sex-stratified requests ask for male and female, others for both", {
  expect_equal(payload_codes("hiv", "state", 2022, vtid = 6), 601)
  expect_equal(
    payload_codes("hiv", "state", 2022, stratify_by = "sex", vtid = 6),
    c(602, 603)
  )
  expect_equal(
    payload_codes("chlamydia", "state", 2022, stratify_by = "race", vtid = 6),
    601
  )
})

test_that("every request asks for all transmission categories", {
  for (stratify_by in list(NULL, "sex", "age", "race")) {
    expect_equal(
      payload_codes("hiv", "state", 2022, stratify_by = stratify_by, vtid = 8),
      801,
      label = paste("stratify_by =", format(stratify_by %||% "none"))
    )
  }
})

# Splitting -------------------------------------------------------------------

test_that("county requests are split into one payload per state", {
  payloads <- .build_payloads("hiv", "county", 2017)

  expect_length(payloads, length(unique(county_states)))
  expect_named(payloads, sort(unique(county_states)))
})

test_that("together the state payloads request every county exactly once", {
  payloads <- .build_payloads("hiv", "county", 2017)
  requested <- unlist(lapply(payloads, function(p) {
    intersect(payload_ids(p), county_ids)
  }))

  expect_setequal(requested, county_ids)
  expect_equal(anyDuplicated(requested), 0)
})

test_that("each state payload asks for that state's counties only", {
  payloads <- .build_payloads("hiv", "county", 2017)
  al <- intersect(payload_ids(payloads[["01"]]), county_ids)

  expect_setequal(al, county_ids[county_states == "01"])
})

test_that("state payloads keep the same indicator, year and category codes", {
  full <- payload_ids(.build_payload(
    "hiv",
    "county",
    2017,
    stratify_by = "sex"
  ))
  non_geo <- setdiff(full, county_ids)

  payloads <- .build_payloads("hiv", "county", 2017, stratify_by = "sex")
  for (state in names(payloads)) {
    expect_equal(
      setdiff(payload_ids(payloads[[state]]), county_ids),
      non_geo,
      label = paste("non-county codes for state", state)
    )
  }
})

test_that("requests are split into one per disease and year", {
  payloads <- .build_payloads(c("hiv", "chlamydia"), "state", c(2022, 2017))
  expected <- list(
    .build_payload("hiv", "state", 2017),
    .build_payload("hiv", "state", 2022),
    .build_payload("chlamydia", "state", 2017),
    .build_payload("chlamydia", "state", 2022)
  )
  expect_equal(payloads, expected)
})

test_that("county requests are split by disease, year and state", {
  payloads <- .build_payloads("hiv", "county", 2017:2018)
  expect_length(payloads, 2 * length(unique(county_states)))
})

test_that("the order and repeats of years don't change the requests", {
  expect_identical(
    .build_payloads("hiv", "county", c(2022, 2017, 2022)),
    .build_payloads("hiv", "county", c(2017, 2022))
  )
})

age_ids <- varvals$id[varvals$vtid == 7]

# Age, race and geography codes ------------------------------------------------


test_that("age-stratified requests ask for each disease's own age groups (#11)", {
  hiv <- c(656, 662, 665, 668, 669, 672)
  sti <- c(654, 657, 660, 661, 663, 664, 666, 668, 669, 672)
  expected <- list(
    "hiv" = hiv,
    "estimate" = hiv,
    "chlamydia" = c(sti, 673),
    "gonorrhea" = c(sti, 673),
    "adult syphilis" = c(sti, 673),
    "tuberculosis" = sti,
    "hepatitis" = sti
  )
  for (disease in names(expected)) {
    ids <- payload_ids(.build_payload(
      disease,
      "state",
      2022,
      stratify_by = "age"
    ))
    expect_setequal(intersect(ids, age_ids), expected[[disease]])
  }
})

race_ids <- varvals$id[varvals$vtid == 5]

test_that("race-stratified requests ask for each disease's own race groups (#105)", {
  groups <- c(552, 553, 554, 555, 556, 557, 562)
  expected <- list(
    "hiv" = groups,
    "estimate" = groups,
    "chlamydia" = c(groups, 559),
    "gonorrhea" = c(groups, 559),
    "adult syphilis" = c(groups, 559),
    "tuberculosis" = c(groups, 559),
    "hepatitis" = c(552, 554, 555, 557, 559, 560, 561)
  )
  for (disease in names(expected)) {
    ids <- payload_ids(.build_payload(
      disease,
      "state",
      2022,
      stratify_by = "race"
    ))
    expect_equal(
      sort(intersect(ids, race_ids)),
      sort(expected[[disease]]),
      label = disease
    )
  }
})

test_that("unstratified requests ask for the race total", {
  for (disease in c("hiv", "hepatitis", "congenital syphilis")) {
    ids <- payload_ids(.build_payload(disease, "state", 2022))
    expect_equal(intersect(ids, race_ids), 551, label = disease)
  }
})

test_that("unstratified requests ask for each disease's own age total", {
  expected <- c(
    "hiv" = 650,
    "estimate" = 650,
    "chlamydia" = 651,
    "gonorrhea" = 651,
    "adult syphilis" = 651,
    "congenital syphilis" = 651,
    "tuberculosis" = 651,
    "hepatitis" = 651
  )
  for (disease in names(expected)) {
    ids <- payload_ids(.build_payload(disease, "state", 2022))
    expect_equal(intersect(ids, age_ids), expected[[disease]], label = disease)
  }
})

test_that("national requests use CDC's United States code, 5000", {
  ids <- payload_ids(.build_payload("hiv", "national", 2022))
  expect_contains(ids, 5000)
  expect_equal(intersect(ids, 500), integer(0))
  expect_identical(varvals$name[varvals$id == 5000], "United States")
})

test_that("region requests use CDC's four Census regions and the territories", {
  ids <- payload_ids(.build_payload("estimate", "region", 2022))
  regions <- c(8453:8457)

  expect_contains(ids, regions)
  expect_equal(intersect(ids, c(5000, lookup[["state"]]$geo_id)), integer(0))
  expect_identical(
    varvals$name[match(regions, varvals$id)],
    c(
      "Northeast",
      "Midwest",
      "South",
      "West",
      "U.S. territories and freely associated states"
    )
  )
})

test_that("other geographies are still sent as one payload", {
  for (geography in c("national", "state", "msa")) {
    payloads <- .build_payloads("hiv", geography, 2022)
    expect_equal(length(payloads), 1, label = paste("payloads for", geography))
    expect_identical(
      payloads[[1]],
      .build_payload("hiv", geography, 2022),
      label = paste("the", geography, "payload")
    )
  }
})
