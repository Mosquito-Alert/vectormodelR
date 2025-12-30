#' Build a regular grid for a supplied polygon
#'
#' Creates a tessellation (hexagonal or square) at a given cell size (meters),
#' optionally clips the grid to the polygon footprint, and returns the grid in the desired
#' CRS. Uses `convert_to_utm()` to pick an appropriate metric projection.
#'
#' @param map Optional `sf` object describing the polygon(s) to cover.
#' @param cellsize_m Numeric. Grid cell size in meters (default 400).
#' @param clip Logical. If TRUE (default), intersect hexes with the polygon’s
#'   outline.
#' @param return_crs Coordinate reference system for the output (default 4326).
#' @param shape Character. "hex" (default) for hexagons or "square" for squares.
#' @param write Logical. If TRUE, saves the generated grid to `data_dir`
#'   using the slug-based filename.
#' @param iso3 Optional three-letter ISO3 code used with `admin_level` and
#'   `admin_name` to locate boundary files when `map` is not supplied.
#' @param admin_level Optional administrative level corresponding to the
#'   boundary file when `map` is not supplied.
#' @param admin_name Optional administrative unit name corresponding to the
#'   boundary file when `map` is not supplied.
#' @param data_dir Directory where boundary inputs and grid outputs are stored.
#'   Defaults to `"data/proc"`.
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
#'   write = FALSE
#' )
#'
#' # Using stored administrative boundaries (requires matching RDS file)
#' hex_grid <- build_spatial_grid(
#'   iso3 = "ESP",
#'   admin_level = 2,
#'   admin_name = "Barcelona",
#'   write = TRUE
#' )
#' }
#' @export
build_spatial_grid <- function(
  map = NULL,
  cellsize_m = 400,
  clip = TRUE,
  return_crs = 4326,
  shape = c("hex", "square"),
  write = TRUE,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  data_dir = "data/proc"
) {
  shape <- match.arg(shape)
  if (!is.null(data_dir) && !dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ids <- NULL
  if (!is.null(map)) {
    map <- sf::st_as_sf(map)
  } else {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop("Supply either `map` or the trio `iso3`, `admin_level`, and `admin_name`.",
        call. = FALSE)
    }
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    map_path <- file.path(data_dir, sprintf("spatial_%s_adm.Rds", ids$slug))
    if (!file.exists(map_path)) {
      stop("Boundary file not found at ", map_path, call. = FALSE)
    }
    map <- readr::read_rds(map_path)
    map <- sf::st_as_sf(map)
  }

  if (is.null(ids) && !is.null(iso3) && !is.null(admin_level) && !is.null(admin_name)) {
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
  }

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

  if (isTRUE(write)) {
    if (is.null(ids)) {
      stop("Automatic output naming requires `iso3`, `admin_level`, and `admin_name`.",
        call. = FALSE)
    }
    grid_suffix <- if (identical(shape, "hex")) "hex" else "square"
    out_path <- file.path(data_dir, sprintf("spatial_%s_%s_grid.Rds", ids$slug, grid_suffix))
    readr::write_rds(grid, out_path)
  }

  grid
}