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
#' @param dataset Either the in-memory elevation-enriched dataset (output of
#'   [add_elevation_features()]) or a path to the corresponding RDS file. When a
#'   data object is supplied it must carry an `output_path` attribute naming the
#'   most recently saved file; the population-density enriched dataset is
#'   written to `data_dir` with `_pd.Rds` appended to that stem when
#'   `write_output` is `TRUE`.
#' @param data_dir Directory holding the population density raster and where the
#'   enriched dataset will be written. Defaults to `"data/proc"`.
#' @param verbose Logical; if `TRUE`, prints status messages while processing.
#' @param write_output Logical flag; when `TRUE` (default) the enriched dataset
#'   is written to disk. Set to `FALSE` to skip writing while still returning the
#'   augmented object and updating its metadata.
#'
#' @return A tibble/data frame containing the augmented dataset. Attributes
#'   from the input object are preserved, and two additional attributes are set:
#'   `popdensity_source` (the raster path) and `output_path` (the saved file).
#' @export
#' @importFrom terra rast crs extract vect
#' @importFrom sf st_as_sf st_transform st_make_valid
add_popdensity_features <- function(
	dataset,
	data_dir = "data/proc",
	verbose  = TRUE,
	write_output = TRUE
) {
	infer_slug <- function(path) {
		fname <- basename(path)
		matches <- regexec("^model_prep_(.+?)_base", fname)
		parts <- regmatches(fname, matches)[[1]]
		if (length(parts) >= 2) parts[2] else NA_character_
	}

	# ---- Input (elevation-enriched dataset) ----
	dataset_is_path <- is.character(dataset) && length(dataset) == 1L
	if (dataset_is_path) {
		dataset_path <- dataset
		if (!file.exists(dataset_path)) {
			stop("Elevation-enriched dataset not found at ", dataset_path, call. = FALSE)
		}
		if (isTRUE(verbose)) message("Reading elevation-enriched dataset from ", dataset_path)
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
		if (isTRUE(verbose)) message("Using elevation-enriched dataset from ", dataset_path)
	}
	base_attrs <- attributes(enriched)
	location_slug <- attr(enriched, "location_slug", exact = TRUE)
	if (is.null(location_slug) || !nzchar(location_slug)) {
		location_slug <- infer_slug(dataset_path)
	}
	if (is.na(location_slug) || !nzchar(location_slug)) {
		stop("Could not determine location slug from dataset; ensure it carries a `location_slug` attribute.", call. = FALSE)
	}
	attr(enriched, "location_slug") <- location_slug

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

	stem <- tools::file_path_sans_ext(basename(dataset_path))
	output_filename <- paste0(stem, "_pd.Rds")
	output_path     <- file.path(data_dir, output_filename)

	# restore non-structural attributes
	preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
	for (nm in names(preserve)) {
		attr(enriched, nm) <- preserve[[nm]]
	}

	attr(enriched, "popdensity_source") <- pd_path
	attr(enriched, "output_path")      <- output_path
  attr(enriched, "location_slug")    <- location_slug

	if (isTRUE(write_output)) {
		if (isTRUE(verbose)) {
			message("Saving population-density-enriched dataset to ", output_path)
		}
		dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
		saveRDS(enriched, output_path)
	} else if (isTRUE(verbose)) {
		message("Population density features added (not written to disk).")
	}

	enriched
}
