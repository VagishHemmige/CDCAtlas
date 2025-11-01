#' Internal: fetch AtlasPlus metadata from getInitData/00
#'
#' @keywords internal
.fetch_metadata <- function(ttl = NULL) {

  ttl <- ttl %||% ._cdcAtlas_env$opts$cache_ttl

  json <- .request_json(
    path   = "/getInitData/00",
    method = "GET",
    body   = NULL,
    ttl    = ttl
  )

  # varvals contains the lookup table: id, name, vtid (type)
  if (is.null(json$varvals)) {
    cli::cli_abort("Metadata structure changed: no `varvals` field returned by AtlasPlus.")
  }

  tibble::as_tibble(json$varvals)
}

#' Internal: decode codes using varvals table
#'
#' @keywords internal
.decode_var <- function(ids, type, varvals) {

  # ids = coded integers (e.g., 5001, 205)
  # type = vtid category (300 = disease, 400 = year, 500 = measure)

  out <- dplyr::filter(varvals, vtid == type, id %in% ids)

  if (nrow(out) == 0) {
    return(tibble::tibble(id = ids, desc = NA_character_))
  }

  tibble::tibble(
    code = out$id,
    desc = out$name
  )
}




.decode_atlas <- function(df) {

  if (nrow(df) == 0) return(df)

  varvals <- .fetch_metadata()
  names(df) <- paste0("V", seq_along(df))

  out <- df

  # Base fallbacks
  out$year    <- out$V4
  out$disease <- out$V1
  out$measure <- out$V2

  # --- Decode disease (always V1) ---
  disease_lookup <- varvals |>
    dplyr::filter(id %in% unique(out$V1)) |>
    dplyr::select(id, name)

  if (nrow(disease_lookup) > 0) {
    out <- out |>
      dplyr::left_join(disease_lookup, by = c("V1" = "id")) |>
      dplyr::mutate(disease = dplyr::coalesce(name, as.character(disease))) |>
      dplyr::select(-name)
  }

  # --- Candidate labels for V2 and V4 ---
  lab_v2 <- varvals |>
    dplyr::filter(id %in% unique(out$V2)) |>
    dplyr::select(id, name)

  lab_v4 <- varvals |>
    dplyr::filter(id %in% unique(out$V4)) |>
    dplyr::select(id, name)

  looks_like_year <- function(x) {
    any(grepl("^\\d{4}$", x %||% character()))
  }

  v2_is_year <- nrow(lab_v2) > 0 && looks_like_year(lab_v2$name)
  v4_is_year <- nrow(lab_v4) > 0 && looks_like_year(lab_v4$name)

  # --- Decode year and measure robustly ---
  if (v2_is_year && !v4_is_year) {
    # V2 = year, V4 = measure
    out <- out |>
      dplyr::left_join(lab_v2, by = c("V2" = "id")) |>
      dplyr::mutate(year = dplyr::coalesce(name, as.character(year))) |>
      dplyr::select(-name)

    if (nrow(lab_v4) > 0) {
      out <- out |>
        dplyr::left_join(lab_v4, by = c("V4" = "id")) |>
        dplyr::mutate(measure = dplyr::coalesce(name, as.character(measure))) |>
        dplyr::select(-name)
    }

  } else {
    # Default: V4 = year, V2 = measure
    if (nrow(lab_v4) > 0) {
      out <- out |>
        dplyr::left_join(lab_v4, by = c("V4" = "id")) |>
        dplyr::mutate(year = dplyr::coalesce(name, as.character(year))) |>
        dplyr::select(-name)
    }
    if (nrow(lab_v2) > 0) {
      out <- out |>
        dplyr::left_join(lab_v2, by = c("V2" = "id")) |>
        dplyr::mutate(measure = dplyr::coalesce(name, as.character(measure))) |>
        dplyr::select(-name)
    }
  }

  # --- Decode state ---
  out <- out |>
    dplyr::left_join(.state_lookup, by = c("V3" = "geo_id")) |>
    dplyr::rename(state = state)

  # --- Final clean table ---
  out |>
    dplyr::transmute(
      state,
      year,
      disease,
      measure,
      rate       = V9,
      cases      = V10,
      population = V11
    )
}
