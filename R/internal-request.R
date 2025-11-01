#' Internal AtlasPlus request helper
#'
#' @description
#' `.request_atlas()` is a low-level helper that sends a POST JSON payload to the
#' AtlasPlus `qtOutputData` endpoint. It is not user-facing.  All politeness
#' (user-agent, rate limiting, retries, caching) is handled upstream by
#' `.request_json()`.
#'
#' @param payload A named list representing the JSON POST body.
#' @param ttl Optional override for caching time-to-live (seconds). Defaults to
#'   package options. Usually `0` (no caching) for raw data requests.
#'
#' @return A tibble. If no data present, returns an empty tibble.
#' @keywords internal
.request_atlas <- function(payload, ttl = NULL) {

  # Validate payload
  if (is.null(payload) || !is.list(payload) || length(payload) == 0) {
    cli::cli_abort("Internal error: `.request_atlas()` received an invalid payload.")
  }

  # Perform the POST request via the internal HTTP client
  json <- .request_json(
    path   = "/qtOutputData",
    method = "POST",
    body   = payload,
    ttl    = ttl %||% 0  # Usually don't cache raw data responses
  )

  # AtlasPlus responses are inconsistent: sometimes `data`, sometimes `sourcedata`
  raw <- json$data %||% json$sourcedata

  if (is.null(raw)) {
    cli::cli_warn("AtlasPlus returned no data for this request.")
    return(tibble::tibble())
  }

  # Convert to tibble safely
  out <- tryCatch(
    tibble::as_tibble(raw),
    error = function(e) {
      cli::cli_abort(
        "AtlasPlus returned JSON that could not be converted to a tibble."
      )
    }
  )

  out
}



#' Internal rate limiter
#'
#' Uses ratelimitr to ensure we don't overwhelm AtlasPlus.
#' @keywords internal
client_rate <- ratelimitr::limit_rate(
  function() Sys.sleep(0),
  ratelimitr::rate(n = 5, period = 1)  # max 5 requests per second
)

