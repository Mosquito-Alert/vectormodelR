#' Build a TIGA-like grid over a GADM admin unit
#'
#' @param iso3        ISO3 country code, e.g. "ESP".
#' @param gadm_level  GADM level (0=country, 1=region, 2=province, ...).
#' @param admin_name  Optional. Exact NAME_<level> to keep (e.g. "Barcelona"). If NULL, uses the union of all units at that level.
#' @param cellsize_deg Grid cell size in degrees (lon/lat). Default 0.025.
#' @param path        Directory to cache GADM downloads.
#' @param clip        If TRUE (default) intersect cells with the polygon; if FALSE, keep full bbox grid.
#' @param id_precision Decimal places for the corner coords in TigacellID. Default 3.
#' @param as_sp       If TRUE, return a SpatialPolygonsDataFrame (sp). Otherwise sf (default).
#' @param rds         Logical. If TRUE (default), write the grid to
#'   `data/proc/spatial_iso3_level[_admin]_grid.rds`.
#' @param quiet       Suppress messages.
#'
#' @return An sf (or sp) polygon grid with column `TigacellID`.
#' @export
#' @importFrom readr write_rds
build_tigacell_grid <- function(
  iso3,
  gadm_level = 0,
  admin_name = NULL,
  cellsize_deg = 0.025,
  path = "data/gadm",
  clip = TRUE,
  id_precision = 3,
  rds = TRUE,
  as_sp = FALSE,
  quiet = FALSE
) {
  msg <- function(...) if (!quiet) message(sprintf(...))

  stopifnot(is.character(iso3), length(iso3) == 1)
  stopifnot(is.numeric(gadm_level), length(gadm_level) == 1, gadm_level >= 0)
  stopifnot(is.numeric(cellsize_deg), length(cellsize_deg) == 1, is.finite(cellsize_deg), cellsize_deg > 0)
  stopifnot(is.numeric(id_precision), length(id_precision) == 1, id_precision >= 0)

  # 1) Get & validate GADM geometry (WGS84)
  msg("Fetching GADM for %s level %d ...", iso3, gadm_level)
  g <- geodata::gadm(country = iso3, level = gadm_level, path = path) |>
    sf::st_as_sf() |>
    sf::st_make_valid() |>
    sf::st_transform(4326)

  if (!is.null(admin_name)) {
    nm <- paste0("NAME_", gadm_level)
    if (!nm %in% names(g)) stop("Column ", nm, " not found in GADM table.")
    g <- g[g[[nm]] == admin_name, , drop = FALSE]
    if (nrow(g) == 0) stop("Admin name '", admin_name, "' not found at level ", gadm_level, " for ", iso3)
  }

  g <- g[!sf::st_is_empty(g), ]
  if (nrow(g) == 0) stop("Geometry is empty after filtering.")
  g_u <- sf::st_union(g)
  bb  <- sf::st_bbox(g_u)
  if (any(is.na(bb))) stop("GADM bbox contains NA—cannot grid.")

  # 2) Make regular grid over bbox
  msg("Building %.3f° grid over bbox ...", cellsize_deg)
  grid0 <- sf::st_make_grid(sf::st_as_sfc(bb), cellsize = cellsize_deg, square = TRUE)
  grid  <- sf::st_sf(geom = grid0)

  # 3) Optional clip to polygon
  if (isTRUE(clip)) {
    msg("Clipping grid to admin polygon ...")
    # Use st_intersection; for speed on large grids you could use st_filter(grid, g_u, .predicate = st_intersects)
    grid <- suppressWarnings(sf::st_intersection(grid, g_u))
    grid <- grid[!sf::st_is_empty(grid), ]
  }

  # 4) Build TigacellID from lower-left corner of each cell
  ll_corner <- function(geom) {
    b <- sf::st_bbox(geom)
    c(b["xmin"], b["ymin"])
  }
  ll <- do.call(rbind, lapply(sf::st_geometry(grid), ll_corner))

  fmt <- paste0("%.", id_precision, "f")
  grid$TigacellID <- paste0(sprintf(fmt, ll[, 1]), "_", sprintf(fmt, ll[, 2]))

  # 5) Return / save as requested
  # Keep only the ID in attributes (geometry is in sfc)
  grid <- grid[, "TigacellID"]

  if (isTRUE(rds)) {
    proc_dir <- file.path("data", "proc")
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

    sanitize <- function(x) {
      x <- gsub("[^A-Za-z0-9]+", "-", tolower(x))
      x[nzchar(x)]
    }

    name_suffix <- ""
    if (!is.null(admin_name) && length(admin_name)) {
      clean_names <- sanitize(admin_name)
      if (length(clean_names)) {
        name_suffix <- paste0("_", paste(clean_names, collapse = "-"))
      }
    }

    base_filename <- paste0(
      "spatial_",
      tolower(iso3),
      "_",
      gadm_level,
      name_suffix
    )

    grid_filename <- paste0(base_filename, "_grid.rds")
    grid_path <- file.path(proc_dir, grid_filename)
    readr::write_rds(grid, grid_path)
    msg("Saved grid RDS to %s", grid_path)

    cells_filename <- paste0(base_filename, "_cells.rds")
    cells_path <- file.path(proc_dir, cells_filename)
    readr::write_rds(grid$TigacellID, cells_path)
    msg("Saved TigacellID vector to %s", cells_path)
  }

  if (as_sp) {
    grid_sp <- methods::as(grid, "Spatial")
    return(grid_sp)
  }
  grid
}