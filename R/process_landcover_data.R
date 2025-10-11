#' Process land-cover raster by clipping to a GADM boundary
#'
#' This helper loads a land-cover raster (e.g., ESA WorldCover GeoTIFF), crops it
#' to the extent of a GADM administrative boundary, masks pixels outside the
#' boundary, converts the result to polygons, assigns descriptive land-cover
#' labels, and returns an `sf` object. Optionally, the processed layer can be
#' saved to disk as an RDS file.
#'
#' @param landcover A `terra::SpatRaster` or path to a land-cover raster file.
#' @param boundary An `sf` polygon layer (e.g., the output of
#'   [get_gadm_data()]) defining the target administrative unit.
#' @param dissolve Logical. Passed to [terra::as.polygons()] to control whether
#'   adjacent pixels of the same class should be merged. Default `FALSE` to
#'   preserve the original pixel grid.
#' @param class_lookup Optional named character vector mapping land-cover codes
#'   (e.g., ESA WorldCover codes) to descriptive labels. When `NULL`, a default
#'   lookup based on ESA WorldCover 2021 is used.
#' @param write_rds Logical. If `TRUE`, write the processed layer as an RDS file.
#' @param output_filename Optional file name for the RDS output. If `NULL` and
#'   `landcover` is supplied as a file path, the base name of the raster is
#'   reused with `"_processed.rds"` appended. Otherwise defaults to
#'   `"landcover_processed.rds"`.
#' @param proc_dir Directory used when `write_rds = TRUE`. Defaults to
#'   `"data/proc"`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return An `sf` polygon layer with columns `class_code` and `class_name`
#'   describing the land-cover class of each polygon.
#' @export
#' @importFrom terra rast crop mask as.polygons
#' @importFrom sf st_as_sf st_transform st_crs
#' @importFrom dplyr mutate coalesce
#' @importFrom rlang .data
#' @importFrom readr write_rds
process_landcover_data <- function(
  landcover,
  boundary,
  dissolve = FALSE,
  class_lookup = NULL,
  write_rds = FALSE,
  output_filename = NULL,
  proc_dir = "data/proc",
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

  if (isTRUE(verbose)) message("Converting raster cells to polygons ...")
  lc_polygons <- terra::as.polygons(lc_masked, dissolve = dissolve)
  lc_sf <- sf::st_as_sf(lc_polygons)

  value_col <- setdiff(names(lc_sf), attr(lc_sf, "sf_column"))[1]
  if (is.na(value_col)) {
    stop("Unable to identify the land-cover value column in the polygon layer.")
  }

  if (is.null(class_lookup)) {
    class_lookup <- c(
      `10`  = "Tree cover",
      `20`  = "Shrubland",
      `30`  = "Grassland",
      `40`  = "Cropland",
      `50`  = "Built-up",
      `60`  = "Bare/sparse",
      `70`  = "Snow/ice",
      `80`  = "Water",
      `90`  = "Herbaceous wetland",
      `95`  = "Mangroves",
      `100` = "Moss/lichen"
    )
  }

  lc_sf <- dplyr::mutate(
    lc_sf,
    class_code = as.integer(.data[[value_col]]),
    class_name = class_lookup[as.character(.data$class_code)],
    class_name = dplyr::coalesce(.data$class_name, "Unknown")
  )

  if (isTRUE(write_rds)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

    if (is.null(output_filename)) {
      if (is.character(landcover) && length(landcover) == 1) {
        base_name <- tools::file_path_sans_ext(basename(landcover))
        output_filename <- paste0(base_name, "_processed.rds")
      } else {
        output_filename <- "landcover_processed.rds"
      }
    }

    if (!nzchar(output_filename)) {
      stop("`output_filename` must be a non-empty string when provided.")
    }

    output_path <- file.path(proc_dir, output_filename)
    readr::write_rds(lc_sf, output_path)
    if (isTRUE(verbose)) message("Saved processed land-cover RDS to ", output_path)
  }

  lc_sf
}