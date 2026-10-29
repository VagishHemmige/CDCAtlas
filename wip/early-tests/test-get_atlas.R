# get_atlas() with the CDC request and tract extrapolation replaced, so these
# check the wiring between steps without the network.

test_that("get_atlas() sends the built payload and returns the CDC data", {
  sent <- NULL
  local_mocked_bindings(.request_atlas = function(payload) {
    sent <<- payload
    data.frame(indicator = "Gonorrhea", cases = 10)
  })

  out <- get_atlas("gonorrhea", "state", 2022, stratify_by = "sex")

  expect_identical(sent, .build_payload("gonorrhea", "state", 2022, stratify_by = "sex"))
  expect_identical(out, data.frame(indicator = "Gonorrhea", cases = 10))
})

test_that("get_atlas() validates before calling CDC", {
  local_mocked_bindings(.request_atlas = function(...) stop("CDC should not be called"))
  expect_error(get_atlas("gonorrhea", "state", 1999), "2000 through 2023")
})

test_that("extrapolation passes county data and a state payload to the tract step", {
  received <- NULL
  local_mocked_bindings(
    .request_atlas = function(payload) data.frame(source = "county request"),
    .extrapolate_to_tract = function(df, payload_state, stratify_by) {
      received <<- list(df = df, payload_state = payload_state, stratify_by = stratify_by)
      data.frame(source = "tract estimates")
    }
  )

  out <- get_atlas("hiv", "county", 2022, stratify_by = "sex", extrapolate_to_tract = TRUE)

  expect_identical(out, data.frame(source = "tract estimates"))
  expect_identical(received$df, data.frame(source = "county request"))
  expect_identical(received$payload_state, .build_payload("hiv", "state", 2022, stratify_by = "sex"))
  expect_identical(received$stratify_by, "sex")
})

test_that("without extrapolation the tract step is not called", {
  local_mocked_bindings(
    .request_atlas = function(payload) data.frame(source = "county request"),
    .extrapolate_to_tract = function(...) stop("tract step should not be called")
  )
  expect_no_error(get_atlas("hiv", "county", 2022))
})
