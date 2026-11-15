#' Delete saved CDC downloads
#'
#' @description
#' [get_atlas()], and the functions that compare county data with CDC's state
#' totals, save each response from CDC AtlasPlus and reuse it for 30 days.
#' `clear_atlas_cache()` deletes every saved response, so the next request for
#' any data downloads it again. To download one request again, use
#' `get_atlas(refresh = TRUE)` instead.
#'
#' The responses are saved in the folder given by
#' `tools::R_user_dir("CDCAtlas", "cache")`.
#'
#' @returns The number of saved responses deleted, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Deletes your saved downloads, so not run here
#' clear_atlas_cache()
#' }
clear_atlas_cache <- function() {
  dir <- .atlas_cache_dir()
  n <- length(list.files(dir, all.files = TRUE, recursive = TRUE, no.. = TRUE))
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  if (n == 0) {
    cli::cli_inform("No saved CDC downloads to delete.")
  } else {
    cli::cli_inform("Deleted {n} saved CDC download{?s}.")
  }
  invisible(n)
}
