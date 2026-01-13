#' Sequentially enrich Mosquito Alert model-preparation datasets
#'
#' Convenience wrapper that locates the base `model_prep_*_base.Rds` file for a
#' given location and applies one or more enrichment helpers (hex-grid IDs,
#' weather, landcover, NDVI, elevation, population density, pseudoabsences) in
#' the order supplied.
#' Helpers run sequentially, with the last step persisting the enriched
#' dataset alongside the base inputs. Intermediate helpers update metadata
#' (notably the `output_path` attribute) so suffixes accumulate as expected.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when preparing the inputs.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param features Character vector (or comma-separated string) indicating which
#'   feature steps to run. Accepted values: `"hex"`,
#'   `"hex_<cellsize>"` (for example `hex_800`), `"wx"`/`"weather"`,
#'   `"lc"`/`"landcover"`, `"ndvi"`, `"el"`/`"elevation"`,
#'   `"pd"`/`"popdensity"`, `"se"`/`"pseudoabsence"`.
#' @param data_dir Directory containing the model-preparation datasets and
#'   derived artefacts. Defaults to `"data/proc"`.
#' @param grid_cellsize_m Numeric cell size (meters) corresponding to the stored
#'   hex grid. Defaults to 400.
#' @param verbose Logical; if `TRUE`, prints progress messages while processing.
#'
#' @return The enriched dataset returned by the final helper that ran. The
#'   object retains the `output_path` attribute referencing the file written by
#'   that helper.
#' @export
add_features <- function(
  iso3,
  admin_level,
  admin_name,
  features,
  data_dir = "data/proc",
  verbose  = TRUE
) {
  ids <- build_location_identifiers(iso3, admin_level, admin_name)
  location_slug <- ids$slug

  if (missing(features) || length(features) == 0L) {
    stop("`features` must supply at least one feature code (e.g. 'wx,lc,ndvi').", call. = FALSE)
  }

  if (length(features) == 1L) {
    features <- unlist(strsplit(features, "[,\\s]+", perl = TRUE))
  }
  features <- tolower(trimws(features))
  features <- features[nzchar(features)]
  if (length(features) == 0L) {
    stop("`features` did not contain any recognised feature codes.", call. = FALSE)
  }

  feature_aliases <- c(
    hex = "hex",
    hexgrid = "hex",
    grid = "hex",
    grid_id = "hex",
    wx = "wx",
    weather = "wx",
    lc = "lc",
    landcover = "lc",
    land_cover = "lc",
    ndvi = "ndvi",
    el = "el",
    elevation = "el",
    pd = "pd",
    popdensity = "pd",
    pop_density = "pd",
    populationdensity = "pd",
    se = "se",
    pseudoabsence = "se",
    pseudoabsences = "se",
    pseudo_absence = "se",
    pseudoabsense = "se",
    pseudo_absense = "se"
  )

  default_hex_cellsize <- 400

  parse_feature <- function(raw_code) {
    raw_code <- tolower(trimws(raw_code))
    hex_match <- grepl("^hex(?:_[0-9]+(?:\\.[0-9]+)?)?$", raw_code)
    if (hex_match) {
      size_part <- sub("^hex_?", "", raw_code)
      if (!nzchar(size_part)) {
        size_val <- default_hex_cellsize
      } else {
        size_val <- suppressWarnings(as.numeric(size_part))
        if (is.na(size_val)) {
          stop("Could not parse hex grid cell size from '", raw_code, "'.", call. = FALSE)
        }
      }
      return(list(code = "hex", raw = raw_code, cellsize = size_val))
    }

    alias <- feature_aliases[[raw_code]]
    if (is.na(alias)) {
      stop(
        "Unsupported feature code: ", raw_code,
        ". Allowed values include hex, wx, lc, ndvi, el, pd, se (hex can optionally include a cell size, e.g. hex_800).",
        call. = FALSE
      )
    }
    list(code = alias, raw = raw_code, cellsize = NA_real_)
  }

  feature_specs <- lapply(features, parse_feature)

  # Remove duplicates while preserving order (including cellsize for hex)
  spec_keys <- vapply(feature_specs, function(x) paste0(x$code, ":", ifelse(is.na(x$cellsize), "", x$cellsize)), character(1))
  unique_idx <- match(unique(spec_keys), spec_keys)
  feature_specs <- feature_specs[unique_idx]

  dataset_filename <- paste0("model_prep_", location_slug, "_base.Rds")
  dataset_path <- file.path(data_dir, dataset_filename)
  if (!file.exists(dataset_path)) {
    stop("Base dataset not found at ", dataset_path, call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message("Reading base dataset from ", dataset_path)
  }
  current <- readRDS(dataset_path)
  if (is.null(attr(current, "output_path", exact = TRUE))) {
    attr(current, "output_path") <- dataset_path
  }
  attr(current, "location_slug") <- location_slug

  feature_labels <- list(
    hex = "hex-grid",
    wx = "ERA5 weather",
    lc = "land-cover",
    ndvi = "NDVI proximity",
    el = "elevation",
    pd = "population density",
    se = "sampling-effort pseudoabsences"
  )

  last_idx <- length(feature_specs)

  for (idx in seq_along(feature_specs)) {
    spec <- feature_specs[[idx]]
    code <- spec$code
    write_current <- idx == last_idx

    if (isTRUE(verbose)) {
      label <- feature_labels[[code]]
      if (code == "hex") {
        label <- sprintf("%s (%.0f m)", label, spec$cellsize)
      }
      message("→ Adding ", label, " features (", spec$raw, ")")
    }

    current <- switch(
      code,
      wx = add_weather_features(
        dataset = current,
        data_dir = data_dir,
        write_output = write_current,
        verbose = verbose
      ),
      hex = add_hex_grid(
        dataset = current,
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        grid_dir = data_dir,
        cellsize_m = if (is.na(spec$cellsize)) default_hex_cellsize else spec$cellsize,
        verbose = verbose,
        write_output = write_current
      ),
      lc = add_landcover_features(
        dataset = current,
        data_dir = data_dir,
        verbose = verbose,
        write_output = write_current
      ),
      ndvi = add_ndvi_features(
        dataset = current,
        data_dir = data_dir,
        verbose = verbose,
        write_output = write_current
      ),
      el = add_elevation_features(
        dataset = current,
        data_dir = data_dir,
        verbose = verbose,
        write_output = write_current
      ),
      pd = add_popdensity_features(
        dataset = current,
        data_dir = data_dir,
        verbose = verbose,
        write_output = write_current
      ),
      se = add_pseudoabsences_se(
        dataset = current,
        data_dir = data_dir,
        write_output = write_current
      ),
      stop("Internal error: unhandled feature code '", code, "'.", call. = FALSE)
    )
  }

  if (!"sea_days" %in% names(current) || anyNA(current$sea_days)) {
    if (!"date" %in% names(current)) {
      warning("Final dataset lacks `date`; cannot recompute `sea_days`.", call. = FALSE)
    } else {
      if (!inherits(current$date, "Date")) {
        suppressWarnings(current$date <- as.Date(current$date))
      }
      recomputed <- lubridate::yday(current$date)
      if (!"sea_days" %in% names(current)) {
        current$sea_days <- recomputed
      } else {
        fill_idx <- is.na(current$sea_days)
        if (any(fill_idx)) {
          current$sea_days[fill_idx] <- recomputed[fill_idx]
        }
      }
      if (isTRUE(verbose)) {
        message("`sea_days` values recomputed from `date` before return where missing.")
      }
    }
  }

  final_path <- attr(current, "output_path", exact = TRUE)
  if (!is.null(final_path) && nzchar(final_path)) {
    dir.create(dirname(final_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(current, final_path)
    if (isTRUE(verbose)) {
      message("Finished feature enrichment. Saved dataset to ", final_path)
    }
  } else if (isTRUE(verbose)) {
    message("Finished feature enrichment (no output path available to save).")
  }

  current
}
