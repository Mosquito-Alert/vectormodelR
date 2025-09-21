#' Get ~30 m SRTM DEM for a GADM admin unit
#'
#' Downloads a ~30 m (1 arc-second) DEM via \pkg{elevatr} for a selected
#' administrative unit from \pkg{geodata} and returns either a
#' \pkg{raster} \code{RasterLayer} or a \pkg{terra} \code{SpatRaster}.
#'
#' @param country Country name or ISO3 code accepted by \code{geodata::gadm()} (e.g., "BGD", "Spain").
#' @param level Integer GADM level (0 country, 1 region, 2 district/province, etc.). Default: 2.
#' @param name_col Character. GADM attribute to match (e.g., "NAME_2"). Default: "NAME_2".
#' @param name_value Character. Value to select in \code{name_col} (e.g., "Dhaka"). Required.
#' @param z Integer zoom for \code{elevatr::get_elev_raster()} (11 ≈ ~30 m). Default: 11.
#' @param path Directory to cache GADM files. Default: "data/".
#' @param return_class Either \code{"terra"} (default; returns \code{SpatRaster}) or
#'   \code{"raster"} (returns \code{RasterLayer} directly from \pkg{elevatr}).
#'
#' @return A DEM as either a \code{terra::SpatRaster} or a \code{raster::RasterLayer},
#'   depending on \code{return_class}.
#' @export
get_elevation_data <- function(country,
                           level = 2,
                           name_col = "NAME_2",
                           name_value,
                           z = 11,
                           path = "data/",
                           return_class = c("terra", "raster")) {
  return_class <- match.arg(return_class)

  # deps (kept minimal; elevatr brings raster along)
  if (!requireNamespace("geodata", quietly = TRUE)) stop("Install 'geodata'.")
  if (!requireNamespace("sf",      quietly = TRUE)) stop("Install 'sf'.")
  if (!requireNamespace("elevatr", quietly = TRUE)) stop("Install 'elevatr'.")
  # terra only needed if returning SpatRaster
  if (return_class == "terra" && !requireNamespace("terra", quietly = TRUE)) {
    stop("Install 'terra' to return a SpatRaster.")
  }

  # 1) Get GADM and subset by name
  gadm <- geodata::gadm(country = country, level = level, path = path)
  df   <- as.data.frame(gadm)

  if (!name_col %in% names(df)) {
    stop("Column '", name_col, "' not found. Available: ", paste(names(df), collapse = ", "))
  }
  idx <- df[[name_col]] == name_value
  if (!any(idx, na.rm = TRUE)) stop("No match for ", name_col, " == '", name_value, "'.")

  sel <- gadm[idx, ]

  # 2) Convert to sf and ensure EPSG:4326 for elevatr
  sel_sf <- sf::st_as_sf(sel)
  if (is.na(sf::st_crs(sel_sf)) || sf::st_crs(sel_sf)$epsg != 4326) {
    sel_sf <- sf::st_transform(sel_sf, 4326)
  }

  # 3) Fetch DEM from elevatr (returns raster::RasterLayer)
  dem_r <- elevatr::get_elev_raster(loc = sel_sf, z = z, clip = "locations")

  # 4) Return in requested class
  if (return_class == "raster") {
    return(dem_r)                               # plot(dem_r) works
  } else {
    return(terra::rast(dem_r))                  # terra::plot() & terra ops
  }
}
