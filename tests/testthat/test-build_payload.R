# .build_payload() turns one request into the comma-separated CDC variable
# codes sent to AtlasPlus. Age and race codes, national and region codes and
# the county split are tested in test-county-requests.R; this file covers the
# rest of the payload. Ported from the early drafts on
# claude/wip-early-tests (#23), keeping only cases still true on master.
# Each check compares the exact set of codes of one CDC variable type
# (`varvals$vtid`), so a code sent by mistake fails as well as one left out.

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

test_that("state and MSA requests ask for every state and every MSA", {
  state_ids <- as.integer(strsplit(
    .build_payload("hiv", "state", 2022)$VariableIDs,
    ","
  )[[1]])
  expect_contains(state_ids, lookup[["state"]]$geo_id)
  expect_equal(intersect(state_ids, lookup[["msa"]]$geo_id), integer(0))

  msa_ids <- as.integer(strsplit(
    .build_payload("hiv", "msa", 2022)$VariableIDs,
    ","
  )[[1]])
  expect_contains(msa_ids, lookup[["msa"]]$geo_id)
  expect_equal(intersect(msa_ids, lookup[["state"]]$geo_id), integer(0))
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
