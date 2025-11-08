#' Build a regular grid for a supplied polygon
#'
#' Creates a tessellation (hexagonal or square) at a given cell size (meters),
#' optionally clips the grid to the polygon footprint, and returns the grid in the desired
#' CRS. Uses `convert_to_utm()` to pick an appropriate metric projection.
#'
#' @param map An `sf` object describing the polygon(s) to cover.
#' @param cellsize_m Numeric. Grid cell size in meters (default 400).
#' @param clip Logical. If TRUE (default), intersect hexes with the polygon’s
#'   outline.
#' @param return_crs Coordinate reference system for the output (default 4326).
#' @param shape Character. "hex" (default) for hexagons or "square" for squares.
#' @param out_rds Optional path to write the grid as an RDS file. If NULL,
#'   nothing is written.
#'
#' @return An `sf` object with polygons and a `grid_id` column.
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Example polygon (Barcelona bbox)
#' bcn_poly <- st_as_sfc(st_bbox(c(
#'   xmin = 2.05, xmax = 2.25,
#'   ymin = 41.30, ymax = 41.50
#' )), crs = 4326)
#'
#' # Hexagonal grid at 400 m cells, returning sf object
#' hex_grid <- build_spatial_grid(bcn_poly, cellsize_m = 400)
#'
#' # Square grid at 1 km cells, saving to disk
#' sq_grid <- build_spatial_grid(
#'   map = bcn_poly,
#'   cellsize_m = 1000,
#'   shape = "square",
#'   out_rds = "data/proc/bcn_square_grid.rds"
#' )
#' }
#' @export
build_spatial_grid <- function(
  map,
  cellsize_m = 400,
  clip = TRUE,
  return_crs = 4326,
  shape = c("hex", "square"),
  out_rds = NULL
) {
  stopifnot(!missing(map))
  shape <- match.arg(shape)
  map <- sf::st_as_sf(map)
  map <- sf::st_make_valid(map)
  map <- map[!sf::st_is_empty(map), ]
  if (nrow(map) == 0) stop("Input map is empty.")

  map_utm <- convert_to_utm(map)
  perimeter <- sf::st_union(map_utm)

  grid <- sf::st_make_grid(
    perimeter,
    cellsize = c(cellsize_m, cellsize_m),
    what = "polygons",
    square = identical(shape, "square")
  ) |>
    sf::st_sf() |>
    tibble::rowid_to_column("grid_id")

  if (isTRUE(clip)) {
    grid <- suppressWarnings(sf::st_intersection(grid, map_utm))
    grid <- grid[!sf::st_is_empty(grid), ]
  }

  if (!is.null(return_crs)) {
    grid <- sf::st_transform(grid, return_crs)
  }

  sf_col <- attr(grid, "sf_column")
  if (!is.null(sf_col) && sf_col != "geometry") {
    names(grid)[names(grid) == sf_col] <- "geometry"
    attr(grid, "sf_column") <- "geometry"
  }

  if (!is.null(out_rds)) {
    dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)
    readr::write_rds(grid, out_rds)
  }

  grid
}