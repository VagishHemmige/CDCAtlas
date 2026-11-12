#' Internal null-coalescing helper
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
