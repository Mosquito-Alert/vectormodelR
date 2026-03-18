#' Grid utility functions
#'
#' Internal/helper functions for snapping coordinates to a regular grid
#' and creating standard sampling cell IDs.
#'
#' @keywords internal
NULL

#' Round a given number x downward to the nearest n.
#'
#' @param x The number to be rounded.
#' @param n The number to be rounded.
#' @returns The numeric result of the downward rounding.
#' @export
#' @examples
#' round_down(4.569, 0.05)
round_down = function(x, n) round( floor( (x*1000)/ (n*1000))*n, decimal_places(n))


#' Creates standard sampling cell IDs by masking a set of longitude and latitude values.
#'
#' @param lon A vector of longitudes to be masked
#' @param lat A vector of latitudes to be masked
#' @param mask The masking value.
#' @returns A character vector of sampling cell IDs.
#' @export
#' @examples
#' make_samplingcell_ids(lon=c(2.1686, 2.1032), lat=c(41.3874, 41.2098), 0.05)
make_samplingcell_ids = function(lon, lat, mask=0.05){
  masked_lon = round_down(as.numeric(lon), mask)
  masked_lat = round_down(as.numeric(lat), mask)
  return(paste(masked_lon, masked_lat, sep="_"))
}
