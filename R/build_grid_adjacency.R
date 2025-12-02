#' Build adjacency matrix for a grid
#'
#' Uses slug-based file naming to read the hex grid (`spatial_*_hex_grid.Rds`)
#' and the prepared model dataset (`model_prep_*_wx_lc_ndvi.Rds`), keeps only
#' grid cells present in the model data, and returns a binary adjacency matrix
#' using queen contiguity.
#'
#' @param iso3 Three-letter ISO3 country code used in the slug.
#' @param admin_level Administrative level tied to the boundary/grid slug.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param data_dir Directory holding processed outputs. Defaults to `"data/proc"`.
#' @return A binary adjacency matrix suitable for `brms::car()`.
#' @export
#' @importFrom sf st_relate
build_grid_adjacency <- function(
  iso3,
  admin_level,
  admin_name,
  data_dir = "data/proc"
) {
  location <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- location$slug

  grid_path <- file.path(data_dir, paste0("spatial_", location_slug, "_hex_grid.Rds"))
  if (!file.exists(grid_path)) {
    stop("Hex grid not found at ", grid_path, call. = FALSE)
  }
  grid <- readRDS(grid_path)

  if (!inherits(grid, "sf")) {
    stop("Hex grid must be an sf object.", call. = FALSE)
  }

  # *** FIX: Reproject to planar CRS ***
  grid <- sf::st_transform(grid, 3035)

  model_path <- file.path(data_dir, paste0("model_prep_", location_slug, "_wx_lc_ndvi_elev.Rds"))
  if (!file.exists(model_path)) {
    stop("Model dataset not found at ", model_path, call. = FALSE)
  }
  model_data <- readRDS(model_path)

  model_ids <- unique(model_data$grid_id)
  grid_sub <- grid[grid$grid_id %in% model_ids, ]
  grid_sub <- grid_sub[order(grid_sub$grid_id), ]

  st_queen <- function(a, b = a) sf::st_relate(a, b, pattern = "F***T****", sparse = FALSE)

  W <- 1 * st_queen(grid_sub)
  rownames(W) <- grid_sub$grid_id
  colnames(W) <- grid_sub$grid_id

  diag(W) <- 1

  W
}