#' Add elevation to Mosquito Alert model inputs
#'
#' Loads the NDVI-enriched model-preparation dataset produced by
#' [add_ndvi_features()], extracts elevation values from a corresponding raster,
#' and writes a new `model_prep_*_wx_lc_ndvi_elev.Rds` file with an
#' `elevation_m` column.
#'
#' The elevation raster is expected at
#' `file.path(data_dir, paste0("spatial_", slug, "_elevation.tif"))`.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when sourcing the inputs.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param data_dir Directory holding the processed datasets. Defaults to
#'   `"data/proc"`.
#' @param verbose Logical; if `TRUE`, prints status messages while processing.
#'
#' @return A tibble/data frame containing the augmented dataset. Attributes
#'   from the input object are preserved, and two additional attributes are set:
#'   `elevation_source` (the raster path) and `output_path` (the saved file).
#' @export
#' @importFrom terra rast crs extract vect
#' @importFrom sf st_as_sf st_transform st_make_valid
add_elevation_features <- function(
  iso3,
  admin_level,
  admin_name,
  data_dir = "data/proc",
  verbose  = TRUE
) {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug

  # ---- Input (NDVI-enriched dataset) ----
  dataset_filename <- paste0("model_prep_", location_slug, "_wx_lc_ndvi.Rds")
  dataset_path     <- file.path(data_dir, dataset_filename)
  if (!file.exists(dataset_path)) {
    stop("NDVI-enriched dataset not found at ", dataset_path, call. = FALSE)
  }

  if (isTRUE(verbose)) message("Reading NDVI-enriched dataset from ", dataset_path)
  enriched <- readRDS(dataset_path)
  base_attrs <- attributes(enriched)

  if (!all(c("lon", "lat") %in% names(enriched))) {
    stop("Input dataset must contain `lon` and `lat` columns.", call. = FALSE)
  }

  valid_points <- stats::complete.cases(enriched[c("lon", "lat")])
  if (!any(valid_points)) {
    stop("No valid lon/lat coordinates available to extract elevation.", call. = FALSE)
  }

  # ---- Elevation raster ----
  elev_filename <- paste0("spatial_", location_slug, "_elevation.tif")
  elev_path     <- file.path(data_dir, elev_filename)
  if (!file.exists(elev_path)) {
    stop("Elevation raster not found at ", elev_path, call. = FALSE)
  }

  if (isTRUE(verbose)) message("Loading elevation raster from ", elev_path)
  elev_raster <- terra::rast(elev_path)

  # ---- Prepare points in raster CRS ----
  points_sf <- sf::st_as_sf(
    enriched[valid_points, , drop = FALSE],
    coords = c("lon", "lat"),
    crs    = 4326,
    remove = FALSE
  )
  points_sf <- sf::st_make_valid(points_sf)
  points_sf <- sf::st_transform(points_sf, terra::crs(elev_raster))

  # ---- Extract elevation ----
  elev_vals <- terra::extract(
    elev_raster,
    terra::vect(points_sf),
    ID = FALSE
  )

  if (ncol(elev_vals) == 0L) {
    stop("No elevation values were extracted; check coordinate reference systems.",
         call. = FALSE)
  }

  elev_full <- rep(NA_real_, nrow(enriched))
  elev_full[valid_points] <- as.numeric(elev_vals[[1]])

  # ---- Attach and save ----
  enriched$elevation_m <- elev_full

  output_filename <- paste0("model_prep_", location_slug, "_wx_lc_ndvi_elev.Rds")
  output_path     <- file.path(data_dir, output_filename)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # restore non-structural attributes
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched, nm) <- preserve[[nm]]
  }

  attr(enriched, "elevation_source") <- elev_path
  attr(enriched, "output_path")      <- output_path

  if (isTRUE(verbose)) {
    message("Saving elevation-enriched dataset to ", output_path)
  }
  saveRDS(enriched, output_path)

  enriched
}