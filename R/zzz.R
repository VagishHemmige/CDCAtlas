# Internal environment to store CDCAtlas options
._cdcAtlas_env <- new.env(parent = emptyenv())

# Default options
._cdcAtlas_env$opts <- list(
  base_url     = "https://gis.cdc.gov/grasp/AtlasPlus",
  user_agent   = "CDCAtlas R package (contact: your_email@domain.edu)",
  min_delay_ms = 600L,
  cache_dir    = rappdirs::user_cache_dir("CDCAtlas"),
  cache_ttl    = 3600L   # seconds
)

# Make sure cache directory exists when package loads
.onLoad <- function(libname, pkgname) {
  dir.create(._cdcAtlas_env$opts$cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (!exists("varvals")) {
    warning("Internal data 'varvals' not found. Package may not function correctly.")
  }
}


#' Internal null-coalescing helper
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
