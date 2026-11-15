fake_init_response <- function(json, status = 200L) {
  structure(
    list(
      url = "https://gis.cdc.gov/api/AtlasPlus/getInitData/00",
      status_code = status,
      headers = list(`content-type` = "application/json; charset=utf-8"),
      content = charToRaw(json)
    ),
    class = "response"
  )
}

test_that(".atlas_init_data() returns CDC's code table", {
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_get = function(url) {
    fake_init_response(
      '{"varvals": [{"id": 5001, "name": "Alabama", "vtid": 3, "fips": "01"}],
        "combo": {}}'
    )
  })

  init <- .atlas_init_data()

  expect_named(init, c("varvals", "combo"), ignore.order = TRUE)
  expect_equal(init$varvals$id, 5001)
  expect_equal(init$varvals$fips, "01")
})

test_that(".atlas_init_data() errors when CDC refuses the request", {
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_get = function(url) {
    fake_init_response('{"message": "Server busy"}', status = 503L)
  })

  expect_snapshot(.atlas_init_data(), error = TRUE)
})

test_that(".atlas_init_data() errors when the code table is missing", {
  local_mocked_bindings(client_rate = function() invisible(NULL))
  local_mocked_bindings(.atlas_get = function(url) {
    fake_init_response('{"combo": {}}')
  })

  expect_snapshot(.atlas_init_data(), error = TRUE)
})
