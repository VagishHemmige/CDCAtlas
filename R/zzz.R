# Null-coalescing helper. Base R has `%||%` only from R 4.4.0, and CDCAtlas
# depends on R >= 4.1.0, so the package defines its own.
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
