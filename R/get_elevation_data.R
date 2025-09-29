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
#'
#' @return A \code{terra::SpatRaster} DEM clipped to the selected admin unit or country.
#' @importFrom geodata gadm
#' @importFrom sf st_as_sf st_crs st_transform
#' @importFrom elevatr get_elev_raster
#' @importFrom terra rast
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
                               path = "data/") {
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
  terra::rast(dem_r)
}
