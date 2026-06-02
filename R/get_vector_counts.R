#' Aggregate Mosquito Alert + GBIF occurrence counts by GADM administrative level
#'
#' Spatially joins Mosquito Alert (MA) and GBIF occurrence points to GADM polygons
#' and returns counts by admin name at the chosen level, plus parent admin names
#' and totals.
#'
#' If the requested GADM level is not available for the given ISO3, the function
#' falls back to the highest available GADM level.
#'
#' Parent admin names are included for levels above 1. For example:
#' \itemize{
#'   \item If \code{level = 2}, output includes \code{admin_level_1} and \code{admin_name}.
#'   \item If \code{level = 3}, output includes \code{admin_level_1}, \code{admin_level_2}, and \code{admin_name}.
#'   \item If \code{level = 1}, output includes only \code{admin_name}.
#' }
#'
#' Country-level names are not included as parent columns.
#'
#' @param iso3 Character ISO3 code, e.g. \code{"MEX"} or \code{"JAM"}.
#' @param level Integer requested GADM level.
#'   \code{0 = country}, \code{1 = region}, \code{2 = district}, etc.
#' @param gadm Optional sf polygon layer for the chosen GADM level. If NULL, fetched via
#'   \code{vectormodelR::get_gadm_data(iso3, level)}.
#' @param gbif_tbl Optional data.frame/tibble of GBIF occurrences. If NULL, fetched via
#'   \code{vectormodelR::get_gbif_data()}.
#' @param malert_sf Optional sf POINT layer for Mosquito Alert occurrences. If NULL, fetched via
#'   \code{vectormodelR::get_malert_data()}.
#' @param malert_source Character passed to
#'   \code{vectormodelR::get_malert_data(source = ...)}.
#'   Default: \code{"github"}.
#' @param taxon_key Optional vector of GBIF taxon keys passed to
#'   \code{vectormodelR::get_gbif_data()}.
#' @param gbif_clip_to_perimeter Logical passed to
#'   \code{vectormodelR::get_gbif_data()}.
#' @param gbif_save_outputs Logical passed to
#'   \code{vectormodelR::get_gbif_data()}.
#' @param crs Integer EPSG for point creation and spatial joining.
#'   Default: \code{4326}.
#' @param join_predicate Spatial predicate for join.
#'   Default: \code{sf::st_within}.
#'   You can set \code{sf::st_intersects} if you prefer edge inclusion.
#' @param keep_unmatched Logical. If TRUE, includes an \code{"UNMATCHED"} bucket for points
#'   that do not fall inside polygons. Default FALSE.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item Parent admin columns, where relevant, e.g. \code{admin_level_1}, \code{admin_level_2}
#'     \item \code{admin_name}: the selected admin name at the requested/fallback level
#'     \item \code{malert_count}
#'     \item \code{gbif_count}
#'     \item \code{total}
#'     \item \code{iso3}
#'     \item \code{level_used}: actual level used after fallback
#'     \item \code{name_col}: which GADM \code{NAME_*} column was used as \code{admin_name}
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
  # ---- validate inputs ----
  stopifnot(is.character(iso3), length(iso3) == 1)

  iso3 <- toupper(iso3)

  if (!is.numeric(level) || length(level) != 1 || is.na(level) || level < 0) {
    stop("`level` must be a single non-negative integer.")
  }

  level <- as.integer(level)

  # ---- helper: attempt to get GADM with fallback ----
  get_gadm_with_fallback <- function(iso3, level) {
    gadm_try <- try(
      vectormodelR::get_gadm_data(iso3 = iso3, level = level),
      silent = TRUE
    )

    if (!inherits(gadm_try, "try-error") && inherits(gadm_try, "sf")) {
      return(list(gadm = gadm_try, level_used = level))
    }

    # If requested level fails, search downward.
    if (level > 0) {
      for (lv in seq.int(from = level - 1L, to = 0L, by = -1L)) {
        gadm_try <- try(
          vectormodelR::get_gadm_data(iso3 = iso3, level = lv),
          silent = TRUE
        )

        if (!inherits(gadm_try, "try-error") && inherits(gadm_try, "sf")) {
          return(list(gadm = gadm_try, level_used = lv))
        }
      }
    }

    # Final upward probe, just in case low levels are not available but others are.
    for (lv in seq.int(from = 0L, to = 6L, by = 1L)) {
      gadm_try <- try(
        vectormodelR::get_gadm_data(iso3 = iso3, level = lv),
        silent = TRUE
      )

      if (!inherits(gadm_try, "try-error") && inherits(gadm_try, "sf")) {
        return(list(gadm = gadm_try, level_used = lv))
      }
    }

    stop("Could not retrieve GADM data for iso3='", iso3, "' at any level 0..6.")
  }

  # ---- get GADM polygons ----
  level_used <- level

  if (is.null(gadm)) {
    got <- get_gadm_with_fallback(iso3, level)
    gadm <- got$gadm
    level_used <- got$level_used
  } else {
    if (!inherits(gadm, "sf")) {
      stop("`gadm` must be an sf object when provided.")
    }
  }

  # ---- determine current admin name column ----
  name_col <- paste0("NAME_", level_used)

  if (!name_col %in% names(gadm)) {
    name_cols <- grep("^NAME_[0-9]+$", names(gadm), value = TRUE)

    if (length(name_cols) == 0) {
      stop("No NAME_* columns found in `gadm`.")
    }

    lv_max <- max(as.integer(sub("^NAME_", "", name_cols)), na.rm = TRUE)
    name_col <- paste0("NAME_", lv_max)
  }

  # ---- determine parent columns to keep ----
  #
  # Examples:
  # level 1 -> NAME_1 only, later renamed to admin_name
  # level 2 -> NAME_1 + NAME_2, where NAME_2 becomes admin_name
  # level 3 -> NAME_1 + NAME_2 + NAME_3, where NAME_3 becomes admin_name
  if (level_used >= 1) {
    admin_name_cols <- paste0("NAME_", seq.int(1L, level_used))
  } else {
    admin_name_cols <- name_col
  }

  admin_name_cols <- admin_name_cols[admin_name_cols %in% names(gadm)]

  if (!name_col %in% admin_name_cols) {
    admin_name_cols <- unique(c(admin_name_cols, name_col))
  }

  parent_name_cols <- setdiff(admin_name_cols, name_col)

  # ---- prepare GADM for join ----
  gadm <- gadm |>
    sf::st_transform(crs) |>
    dplyr::select(dplyr::all_of(admin_name_cols))

  # ---- get GBIF data if needed ----
  if (is.null(gbif_tbl)) {
    gbif_tbl <- vectormodelR::get_gbif_data(
      taxon_key = taxon_key,
      iso3 = iso3,
      clip_to_perimeter = gbif_clip_to_perimeter,
      save_outputs = gbif_save_outputs
    )
  }

  # ---- get Mosquito Alert data if needed ----
  if (is.null(malert_sf)) {
    malert_sf <- vectormodelR::get_malert_data(source = malert_source)
  }

  # ---- make sf points: GBIF ----
  if (!inherits(gbif_tbl, "sf")) {
    if (!all(c("decimalLongitude", "decimalLatitude") %in% names(gbif_tbl))) {
      stop(
        "`gbif_tbl` must include columns decimalLongitude and decimalLatitude ",
        "or be an sf object."
      )
    }

    gbif_sf <- gbif_tbl |>
      dplyr::filter(
        !is.na(.data$decimalLatitude),
        !is.na(.data$decimalLongitude)
      ) |>
      sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = crs,
        remove = FALSE
      )
  } else {
    gbif_sf <- sf::st_transform(gbif_tbl, crs)
  }

  # ---- make sf points: Mosquito Alert ----
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
      stop(
        "`malert_sf` must be an sf object or contain lon/lat ",
        "or longitude/latitude columns."
      )
    }

    malert_pts <- malert_sf |>
      dplyr::filter(
        !is.na(.data[[lon_col]]),
        !is.na(.data[[lat_col]])
      ) |>
      sf::st_as_sf(
        coords = c(lon_col, lat_col),
        crs = crs,
        remove = FALSE
      )
  } else {
    malert_pts <- sf::st_transform(malert_sf, crs)
  }

  # ---- spatial join ----
  gbif_joined <- sf::st_join(
    gbif_sf,
    gadm,
    join = join_predicate
  )

  malert_joined <- sf::st_join(
    malert_pts,
    gadm,
    join = join_predicate
  )

  # ---- helper: count joined points by admin hierarchy ----
  count_joined_points <- function(joined_sf, count_col) {
    joined_df <- joined_sf |>
      sf::st_drop_geometry() |>
      dplyr::mutate(
        admin_name = .data[[name_col]]
      )

    if (isTRUE(keep_unmatched)) {
      joined_df <- joined_df |>
        dplyr::mutate(
          admin_name = dplyr::if_else(
            is.na(.data$admin_name),
            "UNMATCHED",
            .data$admin_name
          )
        )

      # For unmatched rows, parent columns will also be NA.
      # This makes the unmatched bucket clearer.
      if (length(parent_name_cols) > 0) {
        for (parent_col in parent_name_cols) {
          joined_df[[parent_col]] <- dplyr::if_else(
            is.na(joined_df[[parent_col]]) & joined_df$admin_name == "UNMATCHED",
            "UNMATCHED",
            joined_df[[parent_col]]
          )
        }
      }
    }

    joined_df |>
      dplyr::filter(!is.na(.data$admin_name)) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(parent_name_cols)),
        .data$admin_name
      ) |>
      dplyr::summarise(
        "{count_col}" := dplyr::n(),
        .groups = "drop"
      )
  }

  # ---- counts ----
  gbif_counts <- count_joined_points(
    joined_sf = gbif_joined,
    count_col = "gbif_count"
  )

  malert_counts <- count_joined_points(
    joined_sf = malert_joined,
    count_col = "malert_count"
  )

  # ---- join counts ----
  join_cols <- c(parent_name_cols, "admin_name")

  out <- dplyr::full_join(
    malert_counts,
    gbif_counts,
    by = join_cols
  ) |>
    dplyr::mutate(
      malert_count = tidyr::replace_na(.data$malert_count, 0L),
      gbif_count = tidyr::replace_na(.data$gbif_count, 0L),
      total = .data$malert_count + .data$gbif_count,
      iso3 = iso3,
      level_used = level_used,
      name_col = name_col
    ) |>
    dplyr::arrange(dplyr::desc(.data$total))

  # ---- rename parent columns to cleaner names ----
  # NAME_1 -> admin_level_1
  # NAME_2 -> admin_level_2
  # The selected level itself is already called admin_name.
  if (length(parent_name_cols) > 0) {
    out <- out |>
      dplyr::rename_with(
        .fn = ~ paste0("admin_level_", sub("^NAME_", "", .x)),
        .cols = dplyr::all_of(parent_name_cols)
      )
  }

  out
}