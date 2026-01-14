#' Attach hex-grid identifiers to Mosquito Alert model inputs
#'
#' Reads the precomputed hex grid for a given location, assigns each report to
#' its containing cell, and writes a new dataset with size-specific grid
#' identifiers. When the input is provided as an in-memory object it must carry
#' an `output_path` attribute so the new filename can be derived.
#'
#' @param dataset Either the in-memory model-preparation dataset or a path to
#'   the corresponding RDS file.
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when generating the hex grid.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param grid_dir Directory containing the precomputed hex grids. Defaults to
#'   "data/proc". The function searches for files following the
#'   `spatial_<slug>_hex_grid_<cellsize>.rds` naming convention, with fallbacks
#'   to legacy filenames when necessary.
#' @param cellsize_m Numeric cell size (meters) used when the grid was
#'   generated. Defaults to 400 to match the package's standard grid.
#' @param verbose Logical; if `TRUE`, prints status updates. Defaults to `TRUE`.
#' @param write_output Logical flag; when `TRUE` (default) the enriched dataset
#'   is written to disk with `_hex<cellsize>.Rds` appended to the filename stem
#'   (for example `_hex400.Rds`).
#'
#' @return A tibble/data frame containing the augmented dataset. Attributes from
#'   the input object are preserved and supplemented with `hex_grid_source`,
#'   `location_slug`, and an updated `output_path`. Each call adds a
#'   size-specific `grid_id_<cellsize>` column, and the
#'   `hex_grid_id_column` attribute records the available grid identifiers.
#' @export
#' @importFrom sf st_as_sf st_join st_within st_drop_geometry
add_hex_grid <- function(
  dataset,
  iso3,
  admin_level,
  admin_name,
  grid_dir = "data/proc",
  cellsize_m = 400,
  verbose = TRUE,
  write_output = TRUE
) {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug

  cellsize_token <- gsub("\\.", "_", format(cellsize_m, trim = TRUE, scientific = FALSE))
  candidate_paths <- c(
    file.path(grid_dir, sprintf("spatial_%s_hex_grid_%s.Rds", location_slug, cellsize_token)),
    file.path(grid_dir, sprintf("spatial_%s_hex_grid_%s.rds", location_slug, cellsize_token)),
    file.path(grid_dir, sprintf("spatial_%s_hex_grid.Rds", location_slug)),
    file.path(grid_dir, sprintf("spatial_%s_hex_grid.rds", location_slug))
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
  if (isTRUE(verbose)) {
    message("Loading hex grid from ", grid_path)
  }
  hex_grid <- readr::read_rds(grid_path)
  if (!inherits(hex_grid, "sf") || !"grid_id" %in% names(hex_grid)) {
    stop("Hex grid must be an sf object with a `grid_id` column.", call. = FALSE)
  }

  dataset_is_path <- is.character(dataset) && length(dataset) == 1L && nzchar(dataset)
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Dataset not found at ", dataset_path, call. = FALSE)
    }
    if (isTRUE(verbose)) {
      message("Reading dataset from ", dataset_path)
    }
    enriched <- readRDS(dataset_path)
  } else {
    enriched <- dataset
    dataset_path <- attr(enriched, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop("When supplying an in-memory dataset, it must carry an `output_path` attribute.",
           call. = FALSE)
    }
    if (isTRUE(verbose)) {
      message("Using in-memory dataset referenced by ", dataset_path)
    }
  }

  if (!all(c("lon", "lat") %in% names(enriched))) {
    stop("Input dataset must contain `lon` and `lat` columns.", call. = FALSE)
  }

  base_attrs <- attributes(enriched)
  if ("grid_id" %in% names(enriched)) {
    enriched$grid_id <- NULL
  }

  enriched_sf <- sf::st_as_sf(enriched, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  enriched_sf <- sf::st_join(enriched_sf, hex_grid["grid_id"], join = sf::st_within)
  enriched_sf <- enriched_sf[!is.na(enriched_sf$grid_id), , drop = FALSE]
  if (nrow(enriched_sf) == 0) {
    stop("No reports fell inside the hex grid.", call. = FALSE)
  }
  enriched_out <- sf::st_drop_geometry(enriched_sf)

  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_filename <- paste0(stem, "_hex", cellsize_token, ".Rds")
  output_path <- file.path(dirname(dataset_path), output_filename)

  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched_out, nm) <- preserve[[nm]]
  }

  grid_id_suffix <- paste0("grid_id_", cellsize_token)
  if (!"grid_id" %in% names(enriched_out)) {
    stop("Hex grid join did not produce a `grid_id` column.", call. = FALSE)
  }
  names(enriched_out)[names(enriched_out) == "grid_id"] <- grid_id_suffix

  attr(enriched_out, "hex_grid_source") <- grid_path
  attr(enriched_out, "location_slug") <- location_slug
  existing_hex_cols <- attr(enriched_out, "hex_grid_id_column", exact = TRUE)
  attr(enriched_out, "hex_grid_id_column") <- unique(c(existing_hex_cols, grid_id_suffix))
  attr(enriched_out, "output_path") <- output_path

  if (isTRUE(write_output)) {
    if (isTRUE(verbose)) {
      message("Saving hex-enriched dataset to ", output_path)
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(enriched_out, output_path)
  } else if (isTRUE(verbose)) {
    message("Hex grid added (not written to disk).")
  }

  enriched_out
}
