#' Process population raster by clipping to a GADM boundary and computing density
#'
#' Loads a population raster (e.g., WorldPop ppp), crops it to a stored boundary,
#' masks outside pixels, converts to population density (people / km^2), and
#' optionally writes a GeoTIFF to disk.
#'
#' @param population A `terra::SpatRaster` or path to a population GeoTIFF.
#'   If this is WorldPop ppp (people per pixel), density is computed as ppp / cell_area_km2.
#' @param iso3 Three-letter ISO3 country code used to locate the boundary file.
#' @param admin_level Administrative level associated with the boundary.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param write_raster Logical. If `TRUE`, write the masked density raster using
#'   [terra::writeRaster()].
#' @param proc_dir Directory used when `write_raster = TRUE`. Defaults to `"data/proc"`.
#' @param datatype GDAL datatype passed to [terra::writeRaster()]. Defaults to `"FLT4S"`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A `terra::SpatRaster` of population density (people / km^2) clipped to boundary.
#' @export
#' @importFrom terra rast crop mask writeRaster cellSize ifel
#' @importFrom sf st_transform st_make_valid
process_popdensity_data <- function(
    population,
    iso3,
    admin_level,
    admin_name,
    write_raster = TRUE,
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
  
  pop <- if (inherits(population, "SpatRaster")) {
    population
  } else if (is.character(population) && length(population) == 1) {
    if (!file.exists(population)) stop("Population file does not exist: ", population, call. = FALSE)
    terra::rast(population)
  } else {
    stop("`population` must be a terra::SpatRaster or a file path to a raster.")
  }
  
  if (isTRUE(verbose)) message("Aligning CRS between raster and boundary ...")
  boundary_aligned <- sf::st_transform(boundary, terra::crs(pop))
  
  if (isTRUE(verbose)) message("Cropping raster to boundary extent ...")
  pop_crop <- terra::crop(pop, boundary_aligned)
  
  if (isTRUE(verbose)) message("Masking raster to boundary ...")
  pop_masked <- terra::mask(pop_crop, boundary_aligned)
  
  # Some rasters can carry NaN outside the mask; convert NaN -> NA
  # (terra usually treats NaN as NA in many ops, but this makes it explicit)
  pop_masked <- terra::ifel(is.nan(pop_masked), NA, pop_masked)
  
  if (isTRUE(verbose)) message("Computing population density (people / km^2) ...")
  area_km2 <- terra::cellSize(pop_masked, unit = "km")
  pop_density <- pop_masked / area_km2
  names(pop_density) <- "popdensity"
  
  if (isTRUE(write_raster)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
    output_path <- file.path(proc_dir, paste0("spatial_", location_slug, "_popdensity.tif"))
    if (is.null(datatype)) {
      terra::writeRaster(pop_density, output_path, overwrite = TRUE)
    } else {
      terra::writeRaster(pop_density, output_path, datatype = datatype, overwrite = TRUE)
    }
    if (isTRUE(verbose)) message("Saved masked population density raster to ", output_path)
  }
  
  pop_density
}