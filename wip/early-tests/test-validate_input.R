# These tests record what the validator does on master today. Tests marked
# "Current behaviour" describe something a later branch is expected to change;
# that branch should update the test on purpose.

test_that("valid requests pass validation", {
  expect_no_error(.validate_get_atlas_inputs("gonorrhea", "state", 2022))
  expect_no_error(.validate_get_atlas_inputs("hiv", "county", 2018:2022, stratify_by = "race"))
  expect_no_error(.validate_get_atlas_inputs("chlamydia", "national", 2022L, stratify_by = c("sex", "age")))
  expect_no_error(.validate_get_atlas_inputs("hiv", "county", 2022, extrapolate_to_tract = TRUE))
  expect_invisible(.validate_get_atlas_inputs("tuberculosis", "msa", 2010))
})

test_that("disease must be a single known value", {
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("Gonorrhea", "state", 2022))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs(c("hiv", "chlamydia"), "state", 2022))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs(NA_character_, "state", 2022))
})

test_that("geography must be a single known value", {
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "Region", 2022))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", c("state", "county"), 2022))
})

test_that("Current behaviour: geography = 'region' passes validation", {
  # .build_payload() adds no geography codes for "region".
  expect_no_error(.validate_get_atlas_inputs("hiv", "region", 2022))
})

test_that("year must be whole, finite years from 2000 to 2023", {
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", TRUE))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", "2022"))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", numeric(0)))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", c(2022, NA)))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022.5))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 1999))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2025))
})

test_that("Current behaviour: a non-finite year fails with an internal error", {
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", c(2022, Inf)))
})

test_that("stratify_by must be NULL or up to two known variables", {
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = 1))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = character(0)))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = NA_character_))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = c("age", "race", "sex")))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = "gender"))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, stratify_by = c("sex", "sex")))
})

test_that("extrapolate_to_tract must be TRUE or FALSE and needs county data", {
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "county", 2022, extrapolate_to_tract = "yes"))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "county", 2022, extrapolate_to_tract = NA))
  expect_snapshot(error = TRUE, .validate_get_atlas_inputs("hiv", "state", 2022, extrapolate_to_tract = TRUE))
})

test_that("Current behaviour: extrapolation combinations with no Census match pass validation", {
  # Two stratification variables: the Census lookup has no matching variables.
  expect_no_error(
    .validate_get_atlas_inputs("hiv", "county", 2022, stratify_by = c("sex", "age"), extrapolate_to_tract = TRUE)
  )
  # STI age groups don't match the Census age groups in the lookup.
  expect_no_error(
    .validate_get_atlas_inputs("chlamydia", "county", 2022, stratify_by = "age", extrapolate_to_tract = TRUE)
  )
})

test_that("errors name get_atlas() as the caller", {
  local_mocked_bindings(.request_atlas = function(...) stop("CDC should not be called"))
  expect_snapshot(error = TRUE, get_atlas("flu", "state", 2022))
})
