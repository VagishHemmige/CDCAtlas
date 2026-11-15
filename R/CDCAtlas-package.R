#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom rlang .data
## usethis namespace: end
NULL

# memoise and ratelimitr are used only when the package is built (the cached,
# rate-limited request functions in internal-request.R), and digest only by
# memoise::cache_filesystem(), which memoise merely suggests. Importing one
# function from each records that they are needed, so R CMD check doesn't
# report them as unused Imports (#21).
#' @importFrom digest digest
#' @importFrom memoise memoise
#' @importFrom ratelimitr limit_rate
NULL
