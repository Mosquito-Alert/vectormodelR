#' Summarise vector observations by administrative unit
#'
#' Retrieves GADM boundaries for a country, counts Mosquito Alert and GBIF
#' occurrences within each administrative unit, and returns a tidy summary
#' table. The helper gracefully downgrades the requested administrative level
#' when it is not available in GADM (for example, requesting level 4 in a
#' country where level 3 is the finest resolution).
#'
#' @param iso3 Three-letter ISO3 code identifying the country.
#' @param admin_level Integer administrative level to aggregate by. If the
#'   requested level is unavailable, the highest existing level is used instead.
#' @param gbif_data Optional pre-loaded GBIF tibble. When supplied, the GBIF
#'   download step is skipped.
#' @param malert_data Optional pre-loaded Mosquito Alert tibble. When supplied,
#'   the Mosquito Alert download step is skipped.
#' @param gbif_lon_col Character name of the longitude column in the GBIF
#'   dataset. Defaults to "decimalLongitude".
#' @param gbif_lat_col Character name of the latitude column in the GBIF
#'   dataset. Defaults to "decimalLatitude".
#' @param malert_lon_col Character name of the longitude column in the Mosquito
#'   Alert dataset. Defaults to "lon".
#' @param malert_lat_col Character name of the latitude column in the Mosquito
#'   Alert dataset. Defaults to "lat".
#' @param gbif_args List of additional arguments passed to [get_gbif_data()].
#'   Values supplied here override the defaults used by this helper.
#' @param malert_args List of additional arguments passed to
#'   [get_malert_data()]. Values supplied here override the defaults used by this
#'   helper. The `source` argument defaults to "github" when Mosquito Alert data
#'   must be downloaded.
#' @param clip_to_perimeter Logical; forwarded to [get_gbif_data()]. Defaults to
#'   `FALSE`, allowing a full-country download that is intersected with the GADM
#'   polygons inside this helper.
#' @param save_gbif_outputs Logical; forwarded to [get_gbif_data()]. Defaults to
#'   `FALSE` so intermediate GBIF artefacts are not persisted.
#' @param gadm_path Directory where GADM downloads are cached. Passed to
#'   [get_gadm_data()]. Defaults to `"data/gadm"`.
#' @param verbose Logical; print progress messages. Default `TRUE`.
#'
#' @return A tibble with columns `admin_name`, `malert_count`, `gbif_count`, and
#'   `total`, ordered by descending total records. Attributes include the
#'   requested and resolved administrative levels (`admin_level_requested` and
#'   `admin_level_used`) and the boundary object used (`boundary`).
#'
#' @examples
#' \dontrun{
#' counts <- get_vector_counts(
#'   iso3 = "MEX",
#'   admin_level = 2,
#'   clip_to_perimeter = FALSE,
#'   save_gbif_outputs = FALSE,
#'   gbif_args = list(taxon_key = c(1651430, 1651891))
#' )
#' head(counts)
#' }
#'
#' @export
get_vector_counts <- function(
  iso3,
  admin_level,
  gbif_data = NULL,
  malert_data = NULL,
  gbif_lon_col = "decimalLongitude",
  gbif_lat_col = "decimalLatitude",
  malert_lon_col = "lon",
  malert_lat_col = "lat",
  gbif_args = list(),
  malert_args = list(),
  clip_to_perimeter = FALSE,
  save_gbif_outputs = FALSE,
  gadm_path = "data/gadm",
  verbose = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install it with install.packages('sf').", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install it with install.packages('dplyr').", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Install it with install.packages('tibble').", call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required. Install it with install.packages('tidyr').", call. = FALSE)
  }

  iso3_code <- toupper(as.character(iso3)[1])
  target_level <- suppressWarnings(as.integer(admin_level))
  if (is.na(target_level) || target_level < 0) {
    stop("`admin_level` must be a non-negative integer.", call. = FALSE)
  }

  fetch_boundary <- function(level) {
    tryCatch(
      {
        get_gadm_data(
          iso3 = iso3_code,
          level = level,
          path = gadm_path,
          rds = FALSE,
          perimeter = FALSE,
          union = FALSE,
          verbose = verbose
        )
      },
      error = function(e) NULL
    )
  }

  current_level <- target_level
  boundary <- NULL
  name_col <- NULL
  while (current_level >= 0) {
    boundary <- fetch_boundary(current_level)
    if (is.null(boundary)) {
      current_level <- current_level - 1L
      next
    }
    candidate_col <- paste0("NAME_", current_level)
    if (!candidate_col %in% names(boundary)) {
      boundary <- NULL
      current_level <- current_level - 1L
      next
    }
    name_col <- candidate_col
    break
  }

  if (is.null(boundary)) {
    stop(
      "Unable to retrieve GADM data for ", iso3_code,
      " at or below level ", target_level, ".",
      call. = FALSE
    )
  }

  used_level <- current_level
  if (used_level != target_level && isTRUE(verbose)) {
    message(
      "Requested level ", target_level,
      " unavailable; using level ", used_level, " instead."
    )
  }

  boundary <- sf::st_make_valid(boundary)
  boundary <- boundary[!sf::st_is_empty(boundary), , drop = FALSE]
  boundary <- sf::st_transform(boundary, 4326)

  if (is.null(gbif_data)) {
    default_gbif_args <- list(
      iso3 = iso3_code,
      admin_level = used_level,
      admin_name = "country",
      clip_to_perimeter = clip_to_perimeter,
      save_outputs = save_gbif_outputs,
      verbose = verbose
    )
    gbif_args <- base::modifyList(default_gbif_args, gbif_args, keep.null = TRUE)
    if (isTRUE(gbif_args$clip_to_perimeter) && identical(gbif_args$admin_name, "country")) {
      warning(
        "`clip_to_perimeter = TRUE` requested without specifying `admin_name` in `gbif_args`; ensure a matching perimeter exists.",
        call. = FALSE
      )
    }
    gbif_data <- do.call(get_gbif_data, gbif_args)
  }

  if (is.null(malert_data)) {
    default_malert_args <- list(source = "github")
    malert_args <- base::modifyList(default_malert_args, malert_args, keep.null = TRUE)
    malert_data <- do.call(get_malert_data, malert_args)
  }

  if (!is.data.frame(gbif_data)) {
    stop("`gbif_data` must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.data.frame(malert_data)) {
    stop("`malert_data` must be a data frame or tibble.", call. = FALSE)
  }

  count_points <- function(data, lon_col, lat_col, count_col) {
    make_empty <- function() {
      out <- tibble::tibble(admin_name = character())
      out[[count_col]] <- integer()
      out
    }

    if (!all(c(lon_col, lat_col) %in% names(data))) {
      warning(
        "Skipping count for ", count_col, ": columns ", lon_col,
        " and/or ", lat_col, " not found.",
        call. = FALSE
      )
      return(make_empty())
    }

    coord_idx <- stats::complete.cases(data[, c(lon_col, lat_col)])
    if (!any(coord_idx)) {
      warning(
        "Skipping count for ", count_col,
        ": no records with complete coordinates.",
        call. = FALSE
      )
      return(make_empty())
    }

    pts_sf <- sf::st_as_sf(
      data[coord_idx, , drop = FALSE],
      coords = c(lon_col, lat_col),
      crs = 4326,
      remove = FALSE
    )

    joined <- sf::st_join(
      pts_sf,
      boundary[, c(name_col), drop = FALSE],
      join = sf::st_within
    )

    if (!nrow(joined)) {
      return(make_empty())
    }

    joined_tbl <- sf::st_drop_geometry(joined)
    joined_tbl <- joined_tbl[!is.na(joined_tbl[[name_col]]), , drop = FALSE]
    if (!nrow(joined_tbl)) {
      return(make_empty())
    }

    counts <- dplyr::count(joined_tbl, .data[[name_col]], name = count_col)
    counts <- tibble::as_tibble(counts)
    names(counts)[names(counts) == name_col] <- "admin_name"
    counts
  }

  gbif_counts <- count_points(gbif_data, gbif_lon_col, gbif_lat_col, "gbif_count")
  malert_counts <- count_points(malert_data, malert_lon_col, malert_lat_col, "malert_count")

  combined <- dplyr::full_join(malert_counts, gbif_counts, by = "admin_name")
  if (!nrow(combined)) {
    combined <- tibble::tibble(
      admin_name = character(),
      malert_count = integer(),
      gbif_count = integer()
    )
  }

  combined <- combined |>
    dplyr::mutate(
      malert_count = tidyr::replace_na(.data$malert_count, 0L),
      gbif_count = tidyr::replace_na(.data$gbif_count, 0L)
    ) |>
    dplyr::mutate(total = .data$malert_count + .data$gbif_count) |>
    dplyr::arrange(dplyr::desc(.data$total))

  attr(combined, "admin_level_requested") <- target_level
  attr(combined, "admin_level_used") <- used_level
  attr(combined, "boundary") <- boundary

  combined
}