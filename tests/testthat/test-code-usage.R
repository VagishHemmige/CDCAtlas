test_that("package code refers to no undefined names", {
  skip_if_not_installed("codetools")
  notes <- character()
  codetools::checkUsageEnv(
    asNamespace("CDCAtlas"),
    report = function(x) notes <<- c(notes, x),
    suppressLocalUnused = TRUE
  )
  expect_equal(grep("no visible", notes, value = TRUE), character())
})
