#' Get ~30 m SRTM DEM for a GADM admin unit or full country
#'
#' Downloads a ~30 m (1 arc-second) DEM via \pkg{elevatr} for a selected
#' administrative unit (or entire country) from \pkg{geodata} and returns a
#' \pkg{terra} SpatRaster.
#'
#' @param country Country name or ISO3 code accepted by \code{geodata::gadm()}.
#' @param level Integer GADM level (0 = country, 1 = region, 2 = district/province, etc.).
#'               Default: 2.
#' @param name_value Character. Name of the unit to select (matched against the relevant
#'                   NAME column). If \code{NULL}, the full level is used.
#' @param z Integer zoom for \code{elevatr::get_elev_raster()} (11 ≈ ~30 m). Default: 11.
#' @param path Directory to cache GADM files. Default: "data/".
#' @param write_tif Logical. If TRUE (default), write the DEM to
#'   `data/proc/spatial_<iso3>_<level>[_name]_elevation.tif` as a GeoTIFF.
#' @param proc_dir Directory to store the DEM when `write_tif = TRUE`. Default: "data/proc".
#'
#' @return A \code{terra::SpatRaster} DEM clipped to the selected admin unit or country.
#' @importFrom geodata gadm
#' @importFrom sf st_as_sf st_crs st_transform
#' @importFrom elevatr get_elev_raster
#' @importFrom terra rast writeRaster
#' @export
#' @examples
#' \dontrun{
#' Dhaka District (level 2 → NAME_2)
#'
#' dem <- get_elevation_data("BGD", level = 2, name_value = "Dhaka") 
#' terra::plot(dem) 
#' }
get_elevation_data <- function(country,
                               level = 2,
                               name_value = NULL,
                               z = 11,
                               path = "data/gadm",
                               write_tif = TRUE,
                               proc_dir = "data/proc") {
  # deps
  if (!requireNamespace("geodata", quietly = TRUE)) stop("Install 'geodata'.")
  if (!requireNamespace("sf",      quietly = TRUE)) stop("Install 'sf'.")
  if (!requireNamespace("elevatr", quietly = TRUE)) stop("Install 'elevatr'.")
  if (!requireNamespace("terra",   quietly = TRUE)) stop("Install 'terra'.")

  # 1) Get GADM
  gadm <- geodata::gadm(country = country, level = level, path = path)

  if (!is.null(name_value)) {
    df <- as.data.frame(gadm)

    # Build column name
    name_col <- paste0("NAME_", level)
    if (!name_col %in% names(df)) {
      stop("Column '", name_col, "' not found. Available: ", paste(names(df), collapse = ", "))
    }

    # Subset
    idx <- df[[name_col]] == name_value
    if (!any(idx, na.rm = TRUE)) {
      stop("No match for ", name_col, " == '", name_value, "'.")
    }
    gadm <- gadm[idx, ]
  }

  # 2) Convert to sf, ensure EPSG:4326
  sel_sf <- sf::st_as_sf(gadm)
  if (is.na(sf::st_crs(sel_sf)) || sf::st_crs(sel_sf)$epsg != 4326) {
    sel_sf <- sf::st_transform(sel_sf, 4326)
  }

  # 3) Fetch DEM
  dem_r <- elevatr::get_elev_raster(loc = sel_sf, z = z, clip = "locations")
  dem <- terra::rast(dem_r)

  if (isTRUE(write_tif)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

    name_suffix <- ""
    if (!is.null(name_value) && length(name_value)) {
      clean_names <- sanitize_slug(name_value)
      if (length(clean_names)) {
        name_suffix <- paste0("_", paste(clean_names, collapse = "-"))
      }
    }

    file_stem <- paste0(
      "spatial_",
      tolower(country),
      "_",
      level,
      name_suffix,
      "_elevation.tif"
    )
    output_path <- file.path(proc_dir, file_stem)

    terra::writeRaster(dem, output_path, overwrite = TRUE)
    message("Saved DEM GeoTIFF to ", output_path)
  }

  dem
}
