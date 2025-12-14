#' Add population density to Mosquito Alert model inputs
#'
#' Loads the elevation-enriched model-preparation dataset produced by
#' [add_elevation_features()], extracts population density values from the
#' corresponding raster created by [process_popdensity_data()], and writes a
#' new `model_prep_*_wx_lc_ndvi_elev_pd.Rds` file with a `popdensity_km2`
#' column (people per square kilometre).
#'
#' The population density raster is expected at
#' `file.path(data_dir, paste0("spatial_", slug, "_popdensity.tif"))`.
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
#'   `popdensity_source` (the raster path) and `output_path` (the saved file).
#' @export
#' @importFrom terra rast crs extract vect
#' @importFrom sf st_as_sf st_transform st_make_valid
add_popdensity_features <- function(
	iso3,
	admin_level,
	admin_name,
	data_dir = "data/proc",
	verbose  = TRUE
) {
	ids <- build_location_identifiers(iso3, admin_level, admin_name)
	location_slug <- ids$slug

	# ---- Input (elevation-enriched dataset) ----
	dataset_filename <- paste0("model_prep_", location_slug, "_wx_lc_ndvi_elev.Rds")
	dataset_path     <- file.path(data_dir, dataset_filename)
	if (!file.exists(dataset_path)) {
		stop("Elevation-enriched dataset not found at ", dataset_path, call. = FALSE)
	}

	if (isTRUE(verbose)) message("Reading elevation-enriched dataset from ", dataset_path)
	enriched <- readRDS(dataset_path)
	base_attrs <- attributes(enriched)

	if (!all(c("lon", "lat") %in% names(enriched))) {
		stop("Input dataset must contain `lon` and `lat` columns.", call. = FALSE)
	}

	valid_points <- stats::complete.cases(enriched[c("lon", "lat")])
	if (!any(valid_points)) {
		stop("No valid lon/lat coordinates available to extract population density.", call. = FALSE)
	}

	# ---- Population density raster ----
	pd_filename <- paste0("spatial_", location_slug, "_popdensity.tif")
	pd_path     <- file.path(data_dir, pd_filename)
	if (!file.exists(pd_path)) {
		stop("Population density raster not found at ", pd_path, call. = FALSE)
	}

	if (isTRUE(verbose)) message("Loading population density raster from ", pd_path)
	pd_raster <- terra::rast(pd_path)

	# ---- Prepare points in raster CRS ----
	points_sf <- sf::st_as_sf(
		enriched[valid_points, , drop = FALSE],
		coords = c("lon", "lat"),
		crs    = 4326,
		remove = FALSE
	)
	points_sf <- sf::st_make_valid(points_sf)
	points_sf <- sf::st_transform(points_sf, terra::crs(pd_raster))

	# ---- Extract population density ----
	pd_vals <- terra::extract(
		pd_raster,
		terra::vect(points_sf),
		ID = FALSE
	)

	if (ncol(pd_vals) == 0L) {
		stop("No population density values were extracted; check coordinate reference systems.",
				 call. = FALSE)
	}

	pd_full <- rep(NA_real_, nrow(enriched))
	pd_full[valid_points] <- as.numeric(pd_vals[[1]])

	# ---- Attach and save ----
	enriched$popdensity_km2 <- pd_full

	output_filename <- paste0("model_prep_", location_slug, "_wx_lc_ndvi_elev_pd.Rds")
	output_path     <- file.path(data_dir, output_filename)
	dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

	# restore non-structural attributes
	preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
	for (nm in names(preserve)) {
		attr(enriched, nm) <- preserve[[nm]]
	}

	attr(enriched, "popdensity_source") <- pd_path
	attr(enriched, "output_path")      <- output_path

	if (isTRUE(verbose)) {
		message("Saving population-density-enriched dataset to ", output_path)
	}
	saveRDS(enriched, output_path)

	enriched
}
