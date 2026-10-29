# Fails with the codes that were found, so a payload change is easy to read.
expect_none_of <- function(ids, codes) {
  found <- intersect(codes, ids)
  expect(
    length(found) == 0,
    sprintf("Payload contains codes that should be absent: %s", paste(found, collapse = ", "))
  )
  invisible(ids)
}
