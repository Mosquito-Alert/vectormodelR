#' Add high-NDVI proximity metrics to Mosquito Alert model inputs
#'
#' Loads the land-cover enriched model-preparation dataset produced by
#' [add_landcover_features()], identifies raster cells whose NDVI exceeds a
#' supplied threshold, and measures each report's distance to the nearest
#' qualifying cell. Results are saved as `model_prep_*_wx_lc_ndvi.Rds`.
#'
#' @param dataset Either the in-memory land-cover enriched dataset (output of
#'   [add_landcover_features()]) or a path to the corresponding RDS file. When a
#'   data object is supplied it must carry an `output_path` attribute naming the
#'   most recently saved file; the NDVI-enriched output is written to `data_dir`
#'   with `_ndvi.Rds` appended to that stem when `write_output` is `TRUE`.
#' @param data_dir Directory containing processed rasters and where the output
#'   dataset will be written. Defaults to `"data/proc"`.
#' @param ndvi_threshold Numeric cutoff applied to the NDVI raster. Defaults to
#'   `0.3`.
#' @param decay_alpha Optional exponential decay coefficient used when
#'   deriving proximity; set to `NULL` to skip. Defaults to `0.01`.
#' @param decay_beta Exponent applied to the distance term in the decay
#'   function. Ignored when `decay_alpha` is `NULL`. Defaults to `1`.
#' @param verbose Logical; if `TRUE`, prints progress messages.
#' @param write_output Logical flag; when `TRUE` (default) the enriched dataset
#'   is written to disk. Set to `FALSE` to skip writing while still returning the
#'   augmented object and updating its metadata.
#'
#' @return A tibble/data frame with the augmented variables. Attributes from
#'   the source dataset are preserved and augmented with `ndvi_source`,
#'   `ndvi_threshold`, and `output_path` metadata. ndvi_distance_m – meters from each report to the closest raster cell whose NDVI ≥ threshold.
#'   ndvi_value_nearest – NDVI value at that nearest qualifying cell.
#'   ndvi_ddf_proximity – optional exponential decay score exp(-α · distance^β); higher values mean nearer high-NDVI areas.
#' @export
#' @importFrom terra rast ifel as.points
#' @importFrom sf st_as_sf st_nearest_feature st_distance
add_ndvi_features <- function(
  dataset,
  data_dir = "data/proc",
  ndvi_threshold = 0.3,
  decay_alpha = 0.01,
  decay_beta = 1,
  verbose = TRUE,
  write_output = TRUE
) {
  infer_slug <- function(path) {
    fname <- basename(path)
    matches <- regexec("^model_prep_(.+?)_base", fname)
    parts <- regmatches(fname, matches)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }

  dataset_is_path <- is.character(dataset) && length(dataset) == 1L
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Land-cover enriched dataset not found at ", dataset_path, call. = FALSE)
    }
    enriched <- readRDS(dataset_path)
  } else {
    enriched <- dataset
    dataset_path <- attr(enriched, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop(
        "Input dataset must carry an `output_path` attribute or be provided as a file path.",
        call. = FALSE
      )
    }
  }

  location_slug <- attr(enriched, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- infer_slug(dataset_path)
  }
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine location slug from dataset; ensure it carries a `location_slug` attribute.", call. = FALSE)
  }
  attr(enriched, "location_slug") <- location_slug

  ndvi_path <- file.path(data_dir, paste0("spatial_", location_slug, "_ndvi.tif"))
  if (!file.exists(ndvi_path)) {
    stop("NDVI raster not found at ", ndvi_path, call. = FALSE)
  }

  if (isTRUE(verbose)) message("Using land-cover dataset from ", dataset_path)
  base_attrs <- attributes(enriched)

  if (!is.null(decay_alpha)) {
    if (!is.numeric(decay_alpha) || length(decay_alpha) != 1 || is.na(decay_alpha)) {
      stop("`decay_alpha` must be a single numeric value or NULL.", call. = FALSE)
    }
    if (!is.numeric(decay_beta) || length(decay_beta) != 1 || is.na(decay_beta)) {
      stop("`decay_beta` must be a single numeric value.", call. = FALSE)
    }
  }

  if (!all(c("lon", "lat") %in% names(enriched))) {
    stop("Input dataset must contain `lon` and `lat` columns.", call. = FALSE)
  }

  valid_points <- stats::complete.cases(enriched[c("lon", "lat")])
  if (!any(valid_points)) {
    stop("No valid lon/lat coordinates available to compute NDVI distances.", call. = FALSE)
  }

  if (isTRUE(verbose)) message("Loading NDVI raster from ", ndvi_path)
  ndvi_raster <- terra::rast(ndvi_path)

  if (isTRUE(verbose)) {
    message("Filtering NDVI values using threshold >= ", format(ndvi_threshold))
  }
  ndvi_filtered <- terra::ifel(ndvi_raster >= ndvi_threshold, ndvi_raster, NA_real_)

  ndvi_points <- terra::as.points(ndvi_filtered, na.rm = TRUE)
  if (terra::nrow(ndvi_points) == 0L) {
    stop("No NDVI pixels met the threshold of ", ndvi_threshold, ".", call. = FALSE)
  }

  ndvi_points_sf <- sf::st_as_sf(ndvi_points)

  points_sf <- sf::st_as_sf(enriched[valid_points, , drop = FALSE], coords = c("lon", "lat"), crs = terra::crs(ndvi_raster), remove = FALSE)

  nearest_idx <- sf::st_nearest_feature(points_sf, ndvi_points_sf)
  nearest_points <- ndvi_points_sf[nearest_idx, , drop = FALSE]

  distances <- sf::st_distance(points_sf, nearest_points, by_element = TRUE)
  distances_numeric <- as.numeric(distances)

  ndvi_col <- names(nearest_points)[1]
  ndvi_values <- as.numeric(nearest_points[[ndvi_col]])

  ndvi_distance_full <- rep(NA_real_, nrow(enriched))
  ndvi_distance_full[valid_points] <- distances_numeric

  ndvi_value_full <- rep(NA_real_, nrow(enriched))
  ndvi_value_full[valid_points] <- ndvi_values

  enriched$ndvi_distance_m <- ndvi_distance_full
  enriched$ndvi_value_nearest <- ndvi_value_full

  if (!is.null(decay_alpha)) {
    decay_full <- rep(NA_real_, nrow(enriched))
    decay_full[valid_points] <- exp(-decay_alpha * (distances_numeric ^ decay_beta))
    enriched$ndvi_ddf_proximity <- decay_full
  }

  enriched_out <- enriched

  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_filename <- paste0(stem, "_ndvi.Rds")
  output_path <- file.path(data_dir, output_filename)

  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched_out, nm) <- preserve[[nm]]
  }

  attr(enriched_out, "ndvi_source") <- ndvi_path
  attr(enriched_out, "ndvi_threshold") <- ndvi_threshold
  attr(enriched_out, "output_path") <- output_path
  attr(enriched_out, "location_slug") <- location_slug

  if (isTRUE(write_output)) {
    if (isTRUE(verbose)) message("Saving NDVI-enriched dataset to ", output_path)
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(enriched_out, output_path)
  } else if (isTRUE(verbose)) {
    message("NDVI features added (not written to disk).")
  }

  enriched_out
}
