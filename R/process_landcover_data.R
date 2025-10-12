#' Process land-cover raster by clipping to a GADM boundary
#'
#' This helper loads a land-cover raster (e.g., ESA WorldCover GeoTIFF), crops it
#' to the extent of a GADM administrative boundary, masks pixels outside the
#' boundary, and returns the masked `terra::SpatRaster`. Optionally, the masked
#' layer can be written to disk via [terra::writeRaster()].
#'
#' @param landcover A `terra::SpatRaster` or path to a land-cover raster file.
#' @param boundary An `sf` polygon layer (e.g., the output of
#'   [get_gadm_data()]) defining the target administrative unit.
#' @param write_raster Logical. If `TRUE`, write the masked raster using
#'   [terra::writeRaster()].
#' @param output_filename Optional file name for the raster output. If `NULL`
#'   and `landcover` is supplied as a file path, the base name of the raster is
#'   reused with `"_processed.tif"` appended. Otherwise defaults to
#'   `"landcover_processed.tif"`.
#' @param proc_dir Directory used when `write_raster = TRUE`. Defaults to
#'   `"data/proc"`.
#' @param datatype GDAL datatype passed to [terra::writeRaster()]. Defaults to
#'   `"INT1U"`, which preserves the categorical land-cover codes.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A `terra::SpatRaster` representing the cropped and masked land-cover
#'   layer.
#' @export
#' @importFrom terra rast crop mask writeRaster
#' @importFrom sf st_transform
process_landcover_data <- function(
  landcover,
  boundary,
  write_raster = TRUE,
  output_filename = NULL,
  proc_dir = "data/proc",
  datatype = "INT1U",
  verbose = TRUE
) {
  if (!inherits(boundary, "sf")) {
    stop("`boundary` must be an sf object. Did you call get_gadm_data()?" )
  }

  lc <- if (inherits(landcover, "SpatRaster")) {
    landcover
  } else if (is.character(landcover) && length(landcover) == 1) {
    if (!file.exists(landcover)) {
      stop("Landcover file does not exist: ", landcover)
    }
    terra::rast(landcover)
  } else {
    stop("`landcover` must be a terra::SpatRaster or a file path to a raster.")
  }

  if (!inherits(lc, "SpatRaster")) {
    stop("Unable to create a SpatRaster from `landcover`.")
  }

  if (isTRUE(verbose)) message("Aligning CRS between raster and boundary ...")
  boundary_aligned <- sf::st_transform(boundary, terra::crs(lc))

  if (isTRUE(verbose)) message("Cropping raster to boundary extent ...")
  lc_crop <- terra::crop(lc, boundary_aligned)

  if (isTRUE(verbose)) message("Masking raster to boundary ...")
  lc_masked <- terra::mask(lc_crop, boundary_aligned)

  if (isTRUE(write_raster)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

    if (is.null(output_filename)) {
      if (is.character(landcover) && length(landcover) == 1) {
        base_name <- tools::file_path_sans_ext(basename(landcover))
        output_filename <- paste0(base_name, "_processed.tif")
      } else {
        output_filename <- "landcover_processed.tif"
      }
    }

    if (!nzchar(output_filename)) {
      stop("`output_filename` must be a non-empty string when provided.")
    }

    output_path <- file.path(proc_dir, output_filename)
    if (is.null(datatype)) {
      terra::writeRaster(lc_masked, output_path, overwrite = TRUE)
    } else {
      terra::writeRaster(lc_masked, output_path, datatype = datatype, overwrite = TRUE)
    }
    if (isTRUE(verbose)) message("Saved masked land-cover raster to ", output_path)
  }

  lc_masked
}