#' Aggregate Mosquito Alert + GBIF occurrence counts by GADM administrative level
#'
#' Spatially joins Mosquito Alert (MA) and GBIF occurrence points to GADM polygons
#' and returns counts by admin name at the chosen level, plus totals.
#'
#' If the requested GADM level is not available for the given ISO3, the function
#' falls back to the highest available GADM level.
#'
#' @param iso3 Character ISO3 code (e.g., "MEX", "JAM").
#' @param level Integer requested GADM level (0 = country, 1 = region, 2 = district, ...).
#' @param gadm Optional sf polygon layer for the chosen GADM level. If NULL, fetched via
#'   \code{vectormodelR::get_gadm_data(iso3, level)}.
#' @param gbif_tbl Optional data.frame/tibble of GBIF occurrences. If NULL, fetched via
#'   \code{vectormodelR::get_gbif_data()}.
#' @param malert_sf Optional sf POINT layer for Mosquito Alert occurrences. If NULL, fetched via
#'   \code{vectormodelR::get_malert_data()}.
#' @param malert_source Character passed to \code{vectormodelR::get_malert_data(source = ...)}.
#'   Default: "github".
#' @param taxon_key Optional vector of GBIF taxon key passed to \code{vectormodelR::get_gbif_data()}.
#' @param gbif_clip_to_perimeter Logical passed to \code{vectormodelR::get_gbif_data()}.
#' @param gbif_save_outputs Logical passed to \code{vectormodelR::get_gbif_data()}.
#' @param crs Integer EPSG for point creation / joining. Default 4326 (WGS84).
#' @param join_predicate Spatial predicate for join. Default \code{sf::st_within}.
#'   You can set \code{sf::st_intersects} if you prefer edge-inclusion.
#' @param keep_unmatched Logical. If TRUE, includes an "UNMATCHED" bucket for points
#'   that do not fall inside polygons. Default FALSE (drops NAs).
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{admin_name} : the selected name column at the level (e.g., NAME_2)
#'     \item \code{malert_count}
#'     \item \code{gbif_count}
#'     \item \code{total}
#'     \item \code{iso3}
#'     \item \code{level_used} : actual level used after fallback
#'     \item \code{name_col} : which NAME_* column was used
#'   }
#'
#' @examples
#' \dontrun{
#' counts <- get_vector_counts(iso3 = "MEX", level = 2)
#' counts <- get_vector_counts(iso3 = "JAM", level = 4) # falls back if 4 unavailable
#' }
#'
#' @export
get_vector_counts <- function(
  iso3,
  level = 2,
  gadm = NULL,
  gbif_tbl = NULL,
  malert_sf = NULL,
  malert_source = "github",
  taxon_key = NULL,
  gbif_clip_to_perimeter = FALSE,
  gbif_save_outputs = FALSE,
  crs = 4326,
  join_predicate = sf::st_within,
  keep_unmatched = FALSE
) {
  # Dependencies (avoid attaching; use ::)
  stopifnot(is.character(iso3), length(iso3) == 1)
  iso3 <- toupper(iso3)
  if (!is.numeric(level) || length(level) != 1 || is.na(level) || level < 0) {
    stop("`level` must be a single non-negative integer.")
  }
  level <- as.integer(level)

  # ---- helper: attempt to get the highest available gadm level ----
  get_gadm_with_fallback <- function(iso3, level) {
    # First try requested level
    gadm_try <- try(vectormodelR::get_gadm_data(iso3 = iso3, level = level), silent = TRUE)
    if (!inherits(gadm_try, "try-error") && inherits(gadm_try, "sf")) {
      return(list(gadm = gadm_try, level_used = level))
    }

    # If it fails, search downward for highest available
    # (We don't know max ahead of time, so probe down from requested level, then up to some sane cap)
    # Strategy:
    # 1) If requested level fails, try levels from (level-1) down to 0
    for (lv in seq.int(from = level - 1L, to = 0L, by = -1L)) {
      gadm_try <- try(vectormodelR::get_gadm_data(iso3 = iso3, level = lv), silent = TRUE)
      if (!inherits(gadm_try, "try-error") && inherits(gadm_try, "sf")) {
        return(list(gadm = gadm_try, level_used = lv))
      }
    }

    # 2) If even 0 fails (rare, but handle), try a small upward probe (some datasets might not support low?)
    for (lv in seq.int(from = 0L, to = 6L, by = 1L)) {
      gadm_try <- try(vectormodelR::get_gadm_data(iso3 = iso3, level = lv), silent = TRUE)
      if (!inherits(gadm_try, "try-error") && inherits(gadm_try, "sf")) {
        return(list(gadm = gadm_try, level_used = lv))
      }
    }

    stop("Could not retrieve GADM data for iso3='", iso3, "' at any level (0..6 tried).")
  }

  # ---- get gadm polygons (or validate provided) ----
  level_used <- level
  if (is.null(gadm)) {
    got <- get_gadm_with_fallback(iso3, level)
    gadm <- got$gadm
    level_used <- got$level_used
  } else {
    if (!inherits(gadm, "sf")) stop("`gadm` must be an sf object when provided.")
  }

  # Determine name column for this level
  name_col <- paste0("NAME_", level_used)
  if (!name_col %in% names(gadm)) {
    # fallback: choose the highest NAME_* present
    name_cols <- grep("^NAME_[0-9]+$", names(gadm), value = TRUE)
    if (length(name_cols) == 0) stop("No NAME_* columns found in `gadm`.")
    # pick max numeric suffix
    lv_max <- max(as.integer(sub("^NAME_", "", name_cols)), na.rm = TRUE)
    name_col <- paste0("NAME_", lv_max)
  }

  gadm <- gadm |>
    sf::st_transform(crs) |>
    dplyr::select(dplyr::all_of(name_col))

  # ---- get gbif / malert data if needed ----
  if (is.null(gbif_tbl)) {
    gbif_tbl <- vectormodelR::get_gbif_data(
      taxon_key = taxon_key,
      iso3 = iso3,
      clip_to_perimeter = gbif_clip_to_perimeter,
      save_outputs = gbif_save_outputs
    )
  }

  if (is.null(malert_sf)) {
    malert_sf <- vectormodelR::get_malert_data(source = malert_source)
  }

  # ---- make sf points (GBIF) ----
  if (!inherits(gbif_tbl, "sf")) {
    if (!all(c("decimalLongitude", "decimalLatitude") %in% names(gbif_tbl))) {
      stop("`gbif_tbl` must include columns decimalLongitude and decimalLatitude (or be an sf object).")
    }

    gbif_sf <- gbif_tbl |>
      dplyr::filter(!is.na(.data$decimalLatitude), !is.na(.data$decimalLongitude)) |>
      sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = crs,
        remove = FALSE
      )
  } else {
    gbif_sf <- sf::st_transform(gbif_tbl, crs)
  }

  # ---- make sf points (Mosquito Alert) ----
  # Support both common naming schemes: (lon, lat) or (longitude, latitude)
  if (!inherits(malert_sf, "sf")) {
    lon_col <- dplyr::case_when(
      "lon" %in% names(malert_sf) ~ "lon",
      "longitude" %in% names(malert_sf) ~ "longitude",
      TRUE ~ NA_character_
    )
    lat_col <- dplyr::case_when(
      "lat" %in% names(malert_sf) ~ "lat",
      "latitude" %in% names(malert_sf) ~ "latitude",
      TRUE ~ NA_character_
    )
    if (is.na(lon_col) || is.na(lat_col)) {
      stop("`malert_sf` must be an sf object or contain lon/lat (or longitude/latitude) columns.")
    }

    malert_pts <- malert_sf |>
      dplyr::filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) |>
      sf::st_as_sf(
        coords = c(lon_col, lat_col),
        crs = crs,
        remove = FALSE
      )
  } else {
    malert_pts <- sf::st_transform(malert_sf, crs)
  }

  # ---- spatial join ----
  gbif_joined <- sf::st_join(gbif_sf, gadm, join = join_predicate)
  malert_joined <- sf::st_join(malert_pts, gadm, join = join_predicate)

  # ---- counts ----
  gbif_counts <- gbif_joined |>
    sf::st_drop_geometry() |>
    dplyr::mutate(admin_name = .data[[name_col]]) |>
    dplyr::mutate(admin_name = dplyr::if_else(is.na(.data$admin_name) & keep_unmatched,
                                              "UNMATCHED", .data$admin_name)) |>
    dplyr::filter(!is.na(.data$admin_name)) |>
    dplyr::group_by(.data$admin_name) |>
    dplyr::summarise(gbif_count = dplyr::n(), .groups = "drop")

  malert_counts <- malert_joined |>
    sf::st_drop_geometry() |>
    dplyr::mutate(admin_name = .data[[name_col]]) |>
    dplyr::mutate(admin_name = dplyr::if_else(is.na(.data$admin_name) & keep_unmatched,
                                              "UNMATCHED", .data$admin_name)) |>
    dplyr::filter(!is.na(.data$admin_name)) |>
    dplyr::group_by(.data$admin_name) |>
    dplyr::summarise(malert_count = dplyr::n(), .groups = "drop")

  out <- dplyr::full_join(malert_counts, gbif_counts, by = "admin_name") |>
    dplyr::mutate(
      malert_count = tidyr::replace_na(.data$malert_count, 0L),
      gbif_count   = tidyr::replace_na(.data$gbif_count, 0L),
      total        = .data$malert_count + .data$gbif_count,
      iso3         = iso3,
      level_used   = level_used,
      name_col     = name_col
    ) |>
    dplyr::arrange(dplyr::desc(.data$total))

  out
}