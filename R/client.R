#' Internal HTTP request helper for CDCAtlas
#'
#' @keywords internal
.request_json <- function(path,
                          method = c("GET", "POST"),
                          body   = NULL,
                          ttl    = NULL) {

  method <- match.arg(method)
  opts <- ._cdcAtlas_env$opts
  ttl  <- ttl %||% opts$cache_ttl

  # Full URL
  url <- paste0(opts$base_url, path)

  # ---- Caching key ----------------------------------------------------------
  key <- digest::digest(list(url = url, method = method, body = body), algo = "sha256")
  cache_file <- file.path(opts$cache_dir, paste0(key, ".json"))

  # Return cached result if fresh
  if (file.exists(cache_file)) {
    mtime <- file.info(cache_file)$mtime
    age   <- as.numeric(Sys.time()) - as.numeric(mtime)

    if (age < ttl) {
      return(jsonlite::read_json(cache_file, simplifyVector = TRUE))
    }
  }

  # ---- Politeness -----------------------------------------------------------
  # Rate limiting & delay
  client_rate()                    # ratelimitr function defined elsewhere
  Sys.sleep(opts$min_delay_ms / 1000)

  # ---- Construct request ----------------------------------------------------
  req <- httr2::request(url) |>
    httr2::req_user_agent(opts$user_agent) |>
    httr2::req_headers(
      Accept = "application/json, text/javascript, */*; q=0.01",
      `Content-Type` = "application/json; charset=utf-8",
      `X-Requested-With` = "XMLHttpRequest"
    ) |>
    httr2::req_retry(
      max_tries = 5,
      backoff = ~ .x + runif(1, 0, 0.5)
    )

  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }

  if (method == "POST") req <- httr2::req_method(req, "POST")

  # ---- Perform request ------------------------------------------------------
  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)

  # ---- Parse JSON -----------------------------------------------------------
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # ---- Write cache ----------------------------------------------------------
  jsonlite::write_json(out, cache_file, auto_unbox = TRUE)

  out
}
