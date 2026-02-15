#' Process NDVI raster by clipping to an administrative boundary
#'
#' Loads an NDVI raster (e.g., a Sentinel-2 composite), aligns it with a target
#' boundary, crops and masks pixels outside the polygon, writes the masked
#' output if requested, and computes simple global summary statistics.
#'
#' @param ndvi A `terra::SpatRaster` or path to an NDVI GeoTIFF file.
#' @param iso3 Three-letter ISO3 country code used to locate the boundary file.
#' @param admin_level Administrative level associated with the boundary.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param write_raster Logical. If `TRUE`, save the masked raster via
#'   [terra::writeRaster()].
#' @param write_stats Logical. If `TRUE`, persist the summary statistics to
#'   disk (RDS).
#' @param proc_dir Directory used for any outputs. Created when missing.
#' @param datatype GDAL datatype passed to [terra::writeRaster()]. Defaults to
#'   `"FLT4S"` for floating-point NDVI values.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A list with the masked NDVI raster (`raster`) and a tibble of
#'   summary statistics (`stats`).
#' @export
#' @importFrom terra rast crop mask writeRaster global
#' @importFrom sf st_transform st_make_valid
#' @importFrom tibble tibble
#' @importFrom readr write_rds
process_ndvi_data <- function(
  ndvi,
  iso3,
  admin_level,
  admin_name,
  write_raster = TRUE,
  write_stats = TRUE,
  proc_dir = "data/proc",
  datatype = "FLT4S",
  verbose = TRUE
) {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug

  boundary_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_adm.Rds"))
  if (!file.exists(boundary_path)) {
    stop(
      "Boundary file not found at ", boundary_path,
      ". Generate it first with the spatial grid tooling.",
      call. = FALSE
    )
  }
  if (isTRUE(verbose)) message("Loading boundary from ", boundary_path)
  boundary <- readRDS(boundary_path)

  if (!inherits(boundary, "sf")) {
    stop("`boundary` must be an sf object. Did you call get_gadm_data()?", call. = FALSE)
  }

  boundary <- sf::st_make_valid(boundary)


  ndvi_raster <- if (inherits(ndvi, "SpatRaster")) {
    ndvi
  } else if (is.character(ndvi) && length(ndvi) == 1) {
    if (!file.exists(ndvi)) {
      stop("NDVI file does not exist: ", ndvi)
    }
    terra::rast(ndvi)
  } else {
    stop("`ndvi` must be a terra::SpatRaster or a path to a raster file.")
  }

  if (!inherits(ndvi_raster, "SpatRaster")) {
    stop("Unable to create a SpatRaster from `ndvi`.")
  }

  if (isTRUE(verbose)) message("Aligning CRS between NDVI raster and boundary ...")
  boundary_aligned <- sf::st_transform(boundary, terra::crs(ndvi_raster))

  if (isTRUE(verbose)) message("Cropping NDVI raster to boundary extent ...")
  ndvi_cropped <- terra::crop(ndvi_raster, boundary_aligned)

  if (isTRUE(verbose)) message("Masking NDVI raster to boundary ...")
  ndvi_masked <- terra::mask(ndvi_cropped, boundary_aligned)

  if (terra::nlyr(ndvi_masked) > 1L) {
    warning("NDVI raster has multiple bands; processing all bands together.")
  }

  summarise_vals <- function(vals) {
    vals <- vals[!is.na(vals)]
    if (!length(vals)) {
      return(c(mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_, pixels = 0))
    }
    c(
      mean = mean(vals),
      sd = stats::sd(vals),
      min = min(vals),
      max = max(vals),
      pixels = length(vals)
    )
  }

  stats_raw <- terra::global(ndvi_masked, fun = summarise_vals)
  stats_tbl <- as.data.frame(stats_raw)
  stats_tbl <- tibble::tibble(
    mean = stats_tbl$mean,
    sd = stats_tbl$sd,
    min = stats_tbl$min,
    max = stats_tbl$max,
    pixels = stats_tbl$pixels
  )

  if (isTRUE(write_raster) || isTRUE(write_stats)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (isTRUE(write_raster)) {
    output_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_ndvi.tif"))
    if (is.null(datatype)) {
      terra::writeRaster(ndvi_masked, output_path, overwrite = TRUE)
    } else {
      terra::writeRaster(ndvi_masked, output_path, datatype = datatype, overwrite = TRUE)
    }
    if (isTRUE(verbose)) message("Saved masked NDVI raster to ", output_path)
  }

  if (isTRUE(write_stats)) {
    stats_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_ndvi_stats.rds"))
    readr::write_rds(stats_tbl, stats_path)
    if (isTRUE(verbose)) message("Saved NDVI summary statistics to ", stats_path)
  }

  list(raster = ndvi_masked, stats = stats_tbl)
}
