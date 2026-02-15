#' Add elevation to Mosquito Alert model inputs
#'
#' Takes a model-preparation dataset, extracts elevation values from a
#' corresponding raster, and writes a new RDS file with an `elevation_m` column
#' and `_el` appended to the original filename when output is enabled.
#'
#' The elevation raster is expected at
#' `file.path(data_dir, paste0("spatial_", slug, "_elevation.tif"))`.
#'
#' @param dataset Either the in-memory model-preparation dataset or a path to
#'   the corresponding RDS file. When a data object is provided it must carry an
#'   `output_path` attribute naming the most recently saved file; the
#'   elevation-enriched dataset is written to `data_dir` with `_el.Rds`
#'   appended to that stem when `write_output` is `TRUE`.
#' @param data_dir Directory that houses the processed rasters and where the
#'   elevation-enriched dataset will be written. Defaults to `"data/proc"`.
#' @param verbose Logical; if `TRUE`, prints status messages while processing.
#' @param write_output Logical flag; when `TRUE` (default) the enriched dataset
#'   is written to disk. Set to `FALSE` to skip writing while still returning the
#'   augmented object and updating its metadata.
#'
#' @return A tibble/data frame containing the augmented dataset. Attributes
#'   from the input object are preserved, and two additional attributes are set:
#'   `elevation_source` (the raster path) and `output_path` (the saved file).
#' @export
#' @importFrom terra rast crs extract vect
#' @importFrom sf st_as_sf st_transform st_make_valid
add_elevation_features <- function(
  dataset,
  data_dir = "data/proc",
  verbose  = TRUE,
  write_output = TRUE
) {
  infer_slug <- function(path) {
    fname <- basename(path)
    matches <- regexec("^model_prep_(.+?)_base", fname)
    parts <- regmatches(fname, matches)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }

  # ---- Input dataset ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L && nzchar(dataset)
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Input dataset not found at ", dataset_path, call. = FALSE)
    }
    if (isTRUE(verbose)) message("Reading input dataset from ", dataset_path)
    enriched <- readRDS(dataset_path)
  } else {
    enriched <- dataset
    dataset_path <- attr(enriched, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop(
        "When supplying an in-memory dataset, it must carry an `output_path` attribute.",
        call. = FALSE
      )
    }
    if (isTRUE(verbose)) message("Using in-memory dataset referenced by ", dataset_path)
  }
  base_attrs <- attributes(enriched)

  location_slug <- attr(enriched, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- infer_slug(dataset_path)
  }
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine location slug from dataset; ensure it carries a `location_slug` attribute.", call. = FALSE)
  }
  attr(enriched, "location_slug") <- location_slug

  if (!all(c("lon", "lat") %in% names(enriched))) {
    stop("Input dataset must contain `lon` and `lat` columns.", call. = FALSE)
  }

  valid_points <- stats::complete.cases(enriched[c("lon", "lat")])
  if (!any(valid_points)) {
    stop("No valid lon/lat coordinates available to extract elevation.", call. = FALSE)
  }

  # ---- Elevation raster ----
  elev_filename <- paste0("spatial_", location_slug, "_elevation.tif")
  elev_path     <- file.path(data_dir, elev_filename)
  if (!file.exists(elev_path)) {
    stop("Elevation raster not found at ", elev_path, call. = FALSE)
  }

  if (isTRUE(verbose)) message("Loading elevation raster from ", elev_path)
  elev_raster <- terra::rast(elev_path)

  # ---- Prepare points in raster CRS ----
  points_sf <- sf::st_as_sf(
    enriched[valid_points, , drop = FALSE],
    coords = c("lon", "lat"),
    crs    = 4326,
    remove = FALSE
  )
  points_sf <- sf::st_make_valid(points_sf)
  points_sf <- sf::st_transform(points_sf, terra::crs(elev_raster))

  # ---- Extract elevation ----
  elev_vals <- terra::extract(
    elev_raster,
    terra::vect(points_sf),
    ID = FALSE
  )

  if (ncol(elev_vals) == 0L) {
    stop("No elevation values were extracted; check coordinate reference systems.",
         call. = FALSE)
  }

  elev_full <- rep(NA_real_, nrow(enriched))
  elev_full[valid_points] <- as.numeric(elev_vals[[1]])

  # ---- Attach and save ----
  enriched$elevation_m <- elev_full

  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_filename <- paste0(stem, "_el.Rds")
  output_path     <- file.path(data_dir, output_filename)

  # restore non-structural attributes
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(enriched, nm) <- preserve[[nm]]
  }

  attr(enriched, "elevation_source") <- elev_path
  attr(enriched, "output_path")      <- output_path
  attr(enriched, "location_slug")     <- location_slug

  if (isTRUE(write_output)) {
    if (isTRUE(verbose)) {
      message("Saving elevation-enriched dataset to ", output_path)
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(enriched, output_path)
  } else if (isTRUE(verbose)) {
    message("Elevation features added (not written to disk).")
  }

  enriched
}