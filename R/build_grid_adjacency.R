#' Build adjacency matrix for a grid (global-safe)
#'
#' Reads a polygon grid (`spatial_*_hex_grid_<cellsize>.Rds`) and prepared model
#' data (`model_prep_*_wx_lc_ndvi_elev.Rds`), keeps only cells present in model
#' data, reprojects (only if needed) to a locally appropriate projected CRS, and
#' returns a binary queen-contiguity adjacency matrix suitable for
#' `brms::car()`. The function first searches for size-specific grid filenames
#' and falls back to legacy names without the `<cellsize>` token.
#'
#' @param iso3 Three-letter ISO3 country code used in the slug.
#' @param admin_level Administrative level tied to the boundary/grid slug.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param data_dir Directory holding processed outputs. Defaults to `"data/proc"`.
#' @param cellsize_m Numeric cell size (meters) for the hex grid file name.
#'   Defaults to 400. Use values matching the `build_spatial_grid()` output
#'   (for example, setting `cellsize_m = 800` targets
#'   `spatial_*_hex_grid_800.Rds` and the corresponding `grid_id_800` column).
#' @param target_crs Optional. EPSG code or `sf::st_crs()` object. If `NULL`,
#'   a suitable CRS is chosen automatically (UTM if input is lon/lat).
#' @param sparse Logical. If TRUE (default), return a sparse Matrix.
#' @param model Either the in-memory model dataset or a path to a prepared
#'   `model_prep_*` RDS file. This must be supplied; the function no longer
#'   derives the model path automatically.
#' @return An adjacency matrix (dense base matrix if `sparse = FALSE`,
#'   otherwise a `Matrix::dgCMatrix`) with row/col names equal to the
#'   size-specific grid identifier (for example `grid_id_400`).
#' @export
build_grid_adjacency <- function(
  iso3,
  admin_level,
  admin_name,
  data_dir = "data/proc",
  cellsize_m = 400,
  target_crs = NULL,
  sparse = TRUE,
  model
) {
  stopifnot(requireNamespace("sf", quietly = TRUE))
  stopifnot(requireNamespace("Matrix", quietly = TRUE))

  location <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- location$slug

  cellsize_token <- gsub("\\.", "_", format(cellsize_m, trim = TRUE, scientific = FALSE))
  candidate_paths <- c(
    file.path(data_dir, sprintf("spatial_%s_hex_grid_%s.Rds", location_slug, cellsize_token)),
    file.path(data_dir, sprintf("spatial_%s_hex_grid_%s.rds", location_slug, cellsize_token)),
    file.path(data_dir, sprintf("spatial_%s_hex_grid.Rds", location_slug)),
    file.path(data_dir, sprintf("spatial_%s_hex_grid.rds", location_slug))
  )
  existing <- candidate_paths[file.exists(candidate_paths)]
  if (!length(existing)) {
    stop(
      "Hex grid not found. Looked for: ",
      paste(candidate_paths, collapse = "; "),
      call. = FALSE
    )
  }
  grid_path <- existing[[1]]
  grid <- readRDS(grid_path)
  if (!inherits(grid, "sf")) stop("Hex grid must be an sf object.", call. = FALSE)

  grid_id_col <- paste0("grid_id_", cellsize_token)
  if (grid_id_col %in% names(grid)) {
    grid_col_in_grid <- grid_id_col
  } else if ("grid_id" %in% names(grid)) {
    grid_col_in_grid <- "grid_id"
    warning(
      "Grid file at ", grid_path,
      " does not contain '", grid_id_col,
      "'; falling back to 'grid_id'.",
      call. = FALSE
    )
  } else {
    stop(
      "Grid sf object must contain either '", grid_id_col,
      "' or 'grid_id'."
    )
  }

  if (missing(model)) {
    stop("`model` must be provided as an object or file path.", call. = FALSE)
  }

  if (is.character(model) && length(model) == 1L) {
    if (!file.exists(model)) stop("Model dataset not found at ", model, call. = FALSE)
    model_data <- readRDS(model)
  } else {
    model_data <- model
  }

  model_col_candidates <- c(grid_id_col, "grid_id")
  model_col_in_model <- model_col_candidates[model_col_candidates %in% names(model_data)][1]
  if (is.na(model_col_in_model)) {
    stop(
      "Model data must contain a size-specific grid identifier column ('",
      grid_id_col,
      "') or a legacy 'grid_id'."
    )
  }

  grid_ids <- as.character(grid[[grid_col_in_grid]])
  model_ids <- unique(as.character(model_data[[model_col_in_model]]))

  if (!length(model_ids)) {
    stop("Model grid identifier column contains no values.", call. = FALSE)
  }

  if (!all(model_ids %in% grid_ids)) {
    missing <- setdiff(model_ids, grid_ids)
    warning("Some model grid_id not found in grid (showing up to 20): ",
            paste(head(missing, 20), collapse = ", "), call. = FALSE)
  }

  keep_idx <- grid_ids %in% model_ids
  grid_sub <- grid[keep_idx, , drop = FALSE]
  if (nrow(grid_sub) < 2) stop("Not enough grid cells after subsetting to build adjacency.", call. = FALSE)
  sub_ids <- as.character(grid_sub[[grid_col_in_grid]])
  if (anyDuplicated(sub_ids)) stop("Duplicate grid identifiers found in grid subset.", call. = FALSE)

  ord <- order(sub_ids)
  grid_sub <- grid_sub[ord, , drop = FALSE]
  sub_ids <- sub_ids[ord]

  # ---- CRS handling (global-safe) ----
  is_longlat <- sf::st_is_longlat(grid_sub)

  if (!is.null(target_crs)) {
    grid_sub <- sf::st_transform(grid_sub, target_crs)
  } else if (is_longlat) {
    # choose UTM zone from centroid (works globally)
    cxy <- sf::st_coordinates(sf::st_centroid(sf::st_union(sf::st_geometry(grid_sub))))
    lon <- cxy[1]; lat <- cxy[2]
    zone <- floor((lon + 180) / 6) + 1
    epsg <- if (lat >= 0) 32600 + zone else 32700 + zone
    grid_sub <- sf::st_transform(grid_sub, epsg)
  }
  # If already projected and target_crs is NULL, leave as-is.

  # ---- Queen contiguity adjacency ----
  if (sparse) {
    rel <- sf::st_relate(grid_sub, grid_sub, pattern = "F***T****", sparse = TRUE)
    n <- nrow(grid_sub)
    i <- rep(seq_len(n), lengths(rel))
    j <- unlist(rel, use.names = FALSE)

    W <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(n, n))
    dimnames(W) <- list(sub_ids, sub_ids)

    # ensure symmetry + diagonal
    W <- (W + Matrix::t(W)) > 0
    W <- Matrix::Matrix(W * 1, sparse = TRUE)
    Matrix::diag(W) <- 1
  } else {
    W <- 1 * sf::st_relate(grid_sub, grid_sub, pattern = "F***T****", sparse = FALSE)
    rownames(W) <- sub_ids
    colnames(W) <- sub_ids
    diag(W) <- 1
  }

  # sanity: isolated nodes check
  rs <- Matrix::rowSums(W)
  if (any(rs <= 1)) {
    iso <- names(rs)[rs <= 1]
    warning("Some grid cells appear isolated (no neighbors besides self). ",
            "This can be okay at borders, but check if unexpected. ",
            "Example ids: ", paste(head(iso, 10), collapse = ", "),
            call. = FALSE)
  }

  W
}