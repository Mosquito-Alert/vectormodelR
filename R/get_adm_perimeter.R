#' Compute ADM polygon perimeters using local UTM projections
#'
#' For each feature in an `sf` object, this helper estimates the most
#' appropriate UTM zone from the feature's centroid, reprojects the geometry,
#' and calculates the perimeter length in metres. Optionally, the resulting
#' layer can be written to disk as an `.rds` file under `data/proc`.
#'
#' @param sf_obj An `sf` object containing polygon geometries with a defined CRS.
#' @param output_filename Optional file name to use when persisting the result
#'   (when `rds = TRUE`). If `NULL`, defaults to `"perim_adm.rds"`.
#' @param rds Logical. If `TRUE`, save the computed layer as an `.rds` file under
#'   `proc_dir`. Defaults to `TRUE`.
#' @param proc_dir Directory where the `.rds` file should be written when
#'   `rds = TRUE`. Default is `"data/proc"`.
#' @param verbose Logical. Print messages when saving the `.rds` file. Defaults to TRUE.
#'
#' @return An `sf` object with two columns: `perimeter_m` (numeric) and the
#'   original `geometry` transformed back to EPSG:4326.
#'
#' @examples
#' \dontrun{
#' gadm_sf <- get_gadm_data("ESP", 2, name = "Barcelona", rds = FALSE)
#' perim_sf <- get_adm_perimeter(gadm_sf, output_filename = "perim_barcelona.rds")
#' }
#'
#' @export
#' @importFrom sf st_geometry st_crs st_centroid st_coordinates st_transform st_sfc st_length st_sf
#' @importFrom readr write_rds
get_adm_perimeter <- function(
  sf_obj,
  output_filename = NULL,
  rds     = TRUE,
  proc_dir = "data/proc",
  verbose = TRUE
) {
  if (!inherits(sf_obj, "sf")) {
    stop("`sf_obj` must be an sf object.")
  }

  geom <- sf::st_geometry(sf_obj)
  crs <- sf::st_crs(geom)
  if (is.null(crs)) {
    stop("`sf_obj` must have a defined coordinate reference system.")
  }
  if (!identical(crs$epsg, 4326L)) {
    geom <- sf::st_transform(geom, 4326)
  }

  centroids <- sf::st_centroid(geom)
  coords <- sf::st_coordinates(centroids)
  lon <- coords[, "X"]
  lat <- coords[, "Y"]

  utm_zone <- floor((lon + 180) / 6) + 1
  epsg <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)

  perimeter <- vapply(seq_along(geom), function(i) {
    projected <- sf::st_transform(sf::st_sfc(geom[i], crs = sf::st_crs(geom)), epsg[i])
    as.numeric(sf::st_length(projected))
  }, numeric(1))

  result <- sf::st_sf(perimeter_m = perimeter, geometry = geom)

  if (isTRUE(rds)) {
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

    file_name <- if (is.null(output_filename)) "perim_adm.rds" else output_filename
    if (!is.character(file_name) || length(file_name) != 1 || !nzchar(file_name)) {
      stop("`output_filename` must be a non-empty character string when provided.")
    }

    output_path <- file.path(proc_dir, file_name)

    readr::write_rds(result, output_path)
    if (isTRUE(verbose)) {
      message("Saved perimeter RDS to ", output_path)
    }
  }

  result
}
