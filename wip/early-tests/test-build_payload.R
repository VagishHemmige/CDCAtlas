# .build_payload() turns a request into the comma-separated CDC variable codes
# sent to AtlasPlus. These tests record the codes sent on master today.

payload_ids <- function(...) {
  as.integer(strsplit(.build_payload(...)$VariableIDs, ",")[[1]])
}

test_that("payload is a list with one comma-separated string of codes", {
  payload <- .build_payload("gonorrhea", "national", 2022)
  expect_type(payload, "list")
  expect_named(payload, "VariableIDs")
  expect_identical(payload$VariableIDs, "207,500,544,651,551,601,801")
})

test_that("each disease sends its indicator codes", {
  indicator_codes <- list(
    "chlamydia" = 206,
    "gonorrhea" = 207,
    "adult syphilis" = c(208, 213, 290),
    "congenital syphilis" = 214,
    "tuberculosis" = 209,
    "hiv" = c(203, 205),
    "estimate" = 285:287,
    "hepatitis" = 210:212
  )
  for (disease in names(indicator_codes)) {
    ids <- payload_ids(disease, "national", 2022)
    expect_contains(ids, indicator_codes[[disease]])
  }
})

test_that("geography sends national, state, county or MSA codes", {
  expect_contains(payload_ids("hiv", "national", 2022), 500)
  expect_contains(payload_ids("hiv", "state", 2022), lookup[["state"]]$geo_id)
  expect_contains(payload_ids("hiv", "county", 2022), lookup[["counties"]]$geo_id)
  expect_contains(payload_ids("hiv", "msa", 2022), lookup[["msa"]]$geo_id)

  expect_length(lookup[["state"]]$geo_id, 57)
  expect_length(lookup[["counties"]]$geo_id, 3231)
  expect_length(lookup[["msa"]]$geo_id, 158)
})

test_that("Current behaviour: geography = 'region' sends no geography code", {
  expect_identical(
    .build_payload("gonorrhea", "region", 2022)$VariableIDs,
    "207,544,651,551,601,801"
  )
})

test_that("year code is the calendar year minus 1478", {
  expect_contains(payload_ids("hiv", "national", 2022), 544)
  expect_contains(payload_ids("hiv", "national", 2018:2020), 540:542)
})

test_that("unstratified requests send the all-ages, all-races and both-sexes codes", {
  sti <- payload_ids("chlamydia", "national", 2022)
  expect_contains(sti, c(651, 551, 601, 801))
  expect_none_of(sti, 650)

  hiv <- payload_ids("hiv", "national", 2022)
  expect_contains(hiv, c(650, 551, 601, 801))
  expect_none_of(hiv, 651)

  est <- payload_ids("estimate", "national", 2022)
  expect_contains(est, 650)
})

test_that("age stratification sends disease-specific age groups", {
  sti_ages <- c(654, 657, 660, 661, 663, 664, 666, 668, 669, 672, 673)
  hiv_ages <- c(656, 662, 665, 668, 669, 672)

  sti <- payload_ids("chlamydia", "national", 2022, stratify_by = "age")
  expect_contains(sti, sti_ages)
  expect_none_of(sti, c(650, 651))

  hiv <- payload_ids("hiv", "national", 2022, stratify_by = "age")
  expect_contains(hiv, hiv_ages)
  expect_none_of(hiv, c(650, 651, 654, 673))
})

test_that("Current behaviour: estimate with age stratification sends STI age groups", {
  ids <- payload_ids("estimate", "national", 2022, stratify_by = "age")
  expect_contains(ids, c(654, 657, 660, 673))
  expect_none_of(ids, 656)
})

test_that("race and sex stratification send their category codes", {
  ids <- payload_ids("chlamydia", "national", 2022, stratify_by = c("sex", "race"))
  expect_contains(ids, c(552, 553, 554, 555, 556, 557, 559, 562))
  expect_contains(ids, c(602, 603))
  expect_none_of(ids, c(551, 601))
})

test_that("transmission category is always all categories", {
  expect_contains(payload_ids("hiv", "state", 2022, stratify_by = "sex"), 801)
})
