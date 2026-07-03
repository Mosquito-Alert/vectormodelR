# -----------------------------------------------------------------------------
# Internal shared helpers for BYM2 spatial data preparation.
#
# These helpers are used by both `prepare_bym2_data()` (brms) and
# `prepare_inla_bym2_data()` (INLA) so the two pipelines resolve and validate
# the grid identifier column in exactly the same way.
# -----------------------------------------------------------------------------

# Resolve the grid identifier column for a BYM2 preparation step.
#
# The column is taken from `prepared_object$grid_col`, then
# `prepared_meta$grid_col`, and finally inferred from a single `grid_id_*`
# column when a bare data frame is supplied.
.resolve_bym2_grid_col <- function(
    prepared_object = NULL,
    prepared_meta = NULL,
    model_data
) {
  grid_col <- NULL

  if (!is.null(prepared_object$grid_col) &&
      is.character(prepared_object$grid_col) &&
      length(prepared_object$grid_col) == 1L &&
      nzchar(prepared_object$grid_col)) {
    grid_col <- prepared_object$grid_col
  }

  if (is.null(grid_col) &&
      !is.null(prepared_meta$grid_col) &&
      is.character(prepared_meta$grid_col) &&
      length(prepared_meta$grid_col) == 1L &&
      nzchar(prepared_meta$grid_col)) {
    grid_col <- prepared_meta$grid_col
  }

  if (is.null(grid_col)) {
    candidate_grid_cols <- grep(
      "^grid_id_",
      names(model_data),
      value = TRUE
    )

    if (length(candidate_grid_cols) == 1L) {
      grid_col <- candidate_grid_cols
    } else if (!length(candidate_grid_cols)) {
      stop(
        "Could not resolve the grid identifier column. Pass a prepared ",
        "object from `prepare_model_data()` that carries `grid_col`, or a ",
        "data frame containing a single `grid_id_*` column.",
        call. = FALSE
      )
    } else {
      stop(
        "Multiple possible grid identifier columns were found: ",
        paste(candidate_grid_cols, collapse = ", "),
        ". Provide a prepared object whose `grid_col` disambiguates them.",
        call. = FALSE
      )
    }
  }

  if (!grid_col %in% names(model_data)) {
    stop(
      "Grid identifier column `",
      grid_col,
      "` is missing from the model data.",
      call. = FALSE
    )
  }

  grid_col
}

# Coerce a grid identifier column to character and validate it contains no
# missing or empty values. Returns the coerced character vector.
.coerce_bym2_grid_ids <- function(x, grid_col, context = "model_data") {
  ids <- as.character(x)

  if (anyNA(ids) || any(!nzchar(ids))) {
    stop(
      "`",
      context,
      "` contains missing or empty grid identifiers in `",
      grid_col,
      "`.",
      call. = FALSE
    )
  }

  ids
}

# Locate a saved polygon grid file for a given location and cell size, mirroring
# the file-lookup logic used by `build_grid_adjacency()`. Returns a list with
# the resolved `path` and the loaded `sf` `grid` object.
.locate_bym2_grid_file <- function(
    location_slug,
    cellsize_token,
    data_dir = "data/proc",
    verbose = TRUE
) {
  if (is.null(location_slug) ||
      !is.character(location_slug) ||
      length(location_slug) != 1L ||
      is.na(location_slug) ||
      !nzchar(location_slug)) {
    stop(
      "Cannot locate a polygon grid automatically without a location slug. ",
      "Ensure the prepared object carries a location slug in `meta$slug`.",
      call. = FALSE
    )
  }

  candidate_paths <- c(
    file.path(
      data_dir,
      sprintf("spatial_%s_hex_grid_%s.Rds", location_slug, cellsize_token)
    ),
    file.path(
      data_dir,
      sprintf("spatial_%s_hex_grid_%s.rds", location_slug, cellsize_token)
    ),
    file.path(
      data_dir,
      sprintf("spatial_%s_hex_grid.Rds", location_slug)
    ),
    file.path(
      data_dir,
      sprintf("spatial_%s_hex_grid.rds", location_slug)
    )
  )

  existing <- candidate_paths[file.exists(candidate_paths)]

  if (!length(existing)) {
    stop(
      "Could not locate a polygon grid for slug '", location_slug,
      "'. Looked for: ",
      paste(candidate_paths, collapse = "; "),
      "\nRun `build_spatial_grid()` for this location, or place the grid file in `data_dir`.",
      call. = FALSE
    )
  }

  grid_path <- existing[[1]]
  grid <- readRDS(grid_path)

  if (!inherits(grid, "sf")) {
    stop(
      "Located grid file is not an `sf` object: ",
      grid_path,
      call. = FALSE
    )
  }

  list(
    path = grid_path,
    grid = grid
  )
}
