#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom rlang .data
## usethis namespace: end
NULL

# ratelimitr is used only when the package is built (the rate limiter in
# internal-request.R). Importing one function records that it is needed, so
# R CMD check doesn't report it as an unused Import (#21). memoise and cachem
# are called when a request is made, so need no import.
#' @importFrom ratelimitr limit_rate
NULL
