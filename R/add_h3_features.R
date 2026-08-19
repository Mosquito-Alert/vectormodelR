#' Attach H3 identifiers to model inputs
#'
#' Loads an existing H3 grid, or builds it when it does not exist, and assigns
#' each observation to an H3 cell using its longitude and latitude.
#'
#' Multiple resolutions can be added to the same dataset. Each resolution is
#' stored in a separate column, such as `h3_id_6` or `h3_id_7`.
#'
#' @param dataset Either an in-memory model-preparation dataset or a path to
#'   the corresponding RDS file.
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when generating the H3 grid.
#' @param admin_name Administrative unit name.
#' @param grid_dir Directory containing H3 grids.
#' @param resolution Integer H3 resolution from 0 to 15.
#' @param verbose Logical. Print progress messages.
#' @param write_output Logical. Write the enriched dataset to disk.
#'
#' @return The dataset with an `h3_id_<resolution>` column.
#' @export
add_h3_grid <- function(
  dataset,
  iso3,
  admin_level,
  admin_name,
  grid_dir = "data/proc",
  resolution = 9L,
  verbose = TRUE,
  write_output = TRUE
) {
  if (
    length(resolution) != 1L ||
      is.na(resolution) ||
      resolution != as.integer(resolution) ||
      !resolution %in% 0:15
  ) {
    stop(
      "`resolution` must be an integer from 0 to 15.",
      call. = FALSE
    )
  }

  resolution <- as.integer(resolution)

  ids <- build_location_identifiers(
    iso3,
    admin_level,
    admin_name
  )
  location_slug <- ids$slug

  grid_path <- file.path(
    grid_dir,
    sprintf(
      "spatial_%s_h3_grid_%s.Rds",
      location_slug,
      resolution
    )
  )

  if (file.exists(grid_path)) {
    if (isTRUE(verbose)) {
      message("Loading H3 grid from: ", grid_path)
    }

    h3_grid <- readRDS(grid_path)
  } else {
    if (isTRUE(verbose)) {
      message(
        "H3 grid not found. Building resolution ",
        resolution,
        " grid."
      )
    }

    h3_grid <- build_h3_grid(
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      resolution = resolution,
      data_dir = grid_dir,
      write = TRUE,
      verbose = verbose
    )
  }

  if (
    !inherits(h3_grid, "sf") ||
      !"h3_id" %in% names(h3_grid)
  ) {
    stop(
      "H3 grid must be an `sf` object with an `h3_id` column.",
      call. = FALSE
    )
  }

  dataset_is_path <- (
    is.character(dataset) &&
      length(dataset) == 1L &&
      nzchar(dataset)
  )

  if (dataset_is_path) {
    dataset_path <- dataset

    if (!file.exists(dataset_path)) {
      stop(
        "Dataset not found at ",
        dataset_path,
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Reading dataset from: ", dataset_path)
    }

    enriched <- readRDS(dataset_path)
  } else {
    enriched <- dataset

    dataset_path <- attr(
      enriched,
      "output_path",
      exact = TRUE
    )

    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop(
        "When supplying an in-memory dataset, it must have an ",
        "`output_path` attribute.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Using in-memory dataset referenced by: ",
        dataset_path
      )
    }
  }

  if (!all(c("lon", "lat") %in% names(enriched))) {
    stop(
      "Input dataset must contain `lon` and `lat` columns.",
      call. = FALSE
    )
  }

  points <- sf::st_as_sf(
    enriched,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )

  h3_ids <- h3jsr::point_to_cell(
    sf::st_geometry(points),
    res = resolution,
    simple = TRUE
  )

  h3_ids <- as.character(
    unlist(
      h3_ids,
      use.names = FALSE
    )
  )

  if (length(h3_ids) != nrow(enriched)) {
    stop(
      "Could not assign one H3 identifier to each observation.",
      call. = FALSE
    )
  }

  resolution_column <- paste0(
    "h3_id_",
    resolution
  )

  enriched[[resolution_column]] <- h3_ids

  stem <- tools::file_path_sans_ext(
    basename(dataset_path)
  )

  output_filename <- paste0(
    stem,
    "_h3_",
    resolution,
    ".Rds"
  )

  output_path <- file.path(
    dirname(dataset_path),
    output_filename
  )

  existing_columns <- attr(
    enriched,
    "h3_grid_id_columns",
    exact = TRUE
  )

  existing_resolutions <- attr(
    enriched,
    "h3_resolutions",
    exact = TRUE
  )

  existing_sources <- attr(
    enriched,
    "h3_grid_sources",
    exact = TRUE
  )

  attr(enriched, "h3_grid_id_columns") <- unique(
    c(existing_columns, resolution_column)
  )

  attr(enriched, "h3_resolutions") <- unique(
    c(existing_resolutions, resolution)
  )

  attr(enriched, "h3_grid_sources") <- unique(
    c(existing_sources, grid_path)
  )

  attr(enriched, "location_slug") <- location_slug
  attr(enriched, "output_path") <- output_path

  if (isTRUE(write_output)) {
    if (isTRUE(verbose)) {
      message(
        "Saving H3-enriched dataset to: ",
        output_path
      )
    }

    dir.create(
      dirname(output_path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    saveRDS(
      enriched,
      output_path
    )
  } else if (isTRUE(verbose)) {
    message(
      "H3 resolution ",
      resolution,
      " added without writing the dataset."
    )
  }

  enriched
}