#' Sequentially enrich Mosquito Alert model-preparation datasets
#'
#' Convenience wrapper that locates the base `model_prep_*_base.Rds` file for a
#' given location and applies one or more enrichment helpers (hex-grid IDs,
#' weather, landcover, NDVI, elevation, population density, pseudoabsences) in
#' the order supplied.
#' Only the final helper in the sequence writes to disk; earlier steps run
#' in-memory while still updating the dataset metadata (notably the
#' `output_path` attribute) so suffixes accumulate as expected.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when preparing the inputs.
#' @param admin_name Administrative unit name used in the file naming scheme.
#' @param features Character vector (or comma-separated string) indicating which
#'   feature steps to run. Accepted values: `"hex"`, `"wx"`/`"weather"`,
#'   `"lc"`/`"landcover"`, `"ndvi"`, `"el"`/`"elevation"`,
#'   `"pd"`/`"popdensity"`, `"se"`/`"pseudoabsence"`.
#' @param data_dir Directory containing the model-preparation datasets and
#'   derived artefacts. Defaults to `"data/proc"`.
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

  canonical <- feature_aliases[features]
  if (any(is.na(canonical))) {
    unknown <- unique(features[is.na(canonical)])
    stop(
      "Unsupported feature code(s): ", paste(unknown, collapse = ", "),
      ". Allowed values include hex, wx, lc, ndvi, el, pd, se.",
      call. = FALSE
    )
  }

  # Preserve user-specified order but drop duplicates to avoid redundant work.
  canonical <- canonical[!duplicated(canonical)]

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

  feature_labels <- c(
    hex = "hex-grid",
    wx = "ERA5 weather",
    lc = "land-cover",
    ndvi = "NDVI proximity",
    el = "elevation",
    pd = "population density",
    se = "sampling-effort pseudoabsences"
  )

  last_code <- tail(canonical, 1L)

  for (code in canonical) {
    if (isTRUE(verbose)) {
      message("→ Adding ", feature_labels[[code]], " features (", code, ")")
    }

    write_current <- identical(code, last_code)

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

  if (isTRUE(verbose)) {
    final_path <- attr(current, "output_path", exact = TRUE)
    if (!is.null(final_path) && nzchar(final_path)) {
      exists_flag <- file.exists(final_path)
      message("Finished feature enrichment. Latest dataset ",
              if (exists_flag) "written to " else "located at ", final_path)
    } else {
      message("Finished feature enrichment.")
    }
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

  current
}
