#' Transform a geometry to its local UTM zone
#'
#' Computes the centroid of the supplied geometry (after ensuring it is
#' expressed in WGS84), determines the corresponding UTM zone, and reprojects
#' the geometry to that metric CRS. Works for geometries in either hemisphere.
#'
#' @param x An `sf` or `sfc` object with a known CRS (usually EPSG:4326).
#' @return The input geometry transformed to the appropriate UTM CRS.
#' @examples
#' \dontrun{
#' library(sf)
#' poly <- st_as_sfc(st_bbox(c(xmin = 2.05, xmax = 2.25,
#'                              ymin = 41.3, ymax = 41.5)), crs = 4326)
#' convert_to_utm(poly)
#' }
#' @export
convert_to_utm <- function(x) {
  x_ll <- sf::st_transform(x, 4326)
  ctr <- sf::st_coordinates(sf::st_centroid(sf::st_union(x_ll)))
  lon <- ctr[1]
  lat <- ctr[2]
  
  zone <- floor((lon + 180) / 6) + 1
  epsg <- if (lat >= 0) 32600 + zone else 32700 + zone

  sf::st_transform(x_ll, epsg)
}