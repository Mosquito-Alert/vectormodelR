#' Sequentially enrich model-preparation datasets
#'
#' Locates a prepared model dataset and adds requested features in the supplied
#' order. Weather resolution is controlled by `temporal_resolution`. When `se`
#' is requested, the most recent preceding grid feature determines the shared
#' cell-ID column: `h3_<resolution>` maps to `h3_id_<resolution>`, and
#' `hex_<cellsize>` maps to `hex_id_<cellsize>`.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when preparing the dataset.
#' @param admin_name Administrative unit name.
#' @param features Character vector or comma-separated feature codes. Available
#'   codes are `"hex"`, `"hex_<cellsize>"`, `"h3_<resolution>"`, `"wx_land"`,
#'   `"wx_single"`, `"lc"`, `"ndvi"`, `"el"`, `"pd"`, and `"se"`.
#' @param temporal_resolution Either `"daily"` or `"hourly"`. This controls
#'   weather enrichment and pseudoabsence generation.
#' @param vector_sources Vector data sources used to prepare the base dataset.
#'   Accepted values are `"malert"` and `"gbif"`.
#' @param data_dir Directory containing processed data.
#' @param verbose Logical. Print progress messages.
#'
#' @return The enriched dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' daily_data <- add_features(
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona",
#'   temporal_resolution = "daily",
#'   features = "h3_9,se,el,pd,wx_land,ndvi,lc"
#' )
#'
#' hourly_data <- add_features(
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona",
#'   temporal_resolution = "hourly",
#'   features = "hex_1200,se,el,pd,wx_land,ndvi,lc"
#' )
#' }
add_features <- function(
    iso3,
    admin_level,
    admin_name,
    features,
    temporal_resolution = c("daily", "hourly"),
    vector_sources = c("malert", "gbif"),
    data_dir = "data/proc",
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)

  ids <- build_location_identifiers(
    iso3,
    admin_level,
    admin_name
  )
  location_slug <- ids$slug

  if (length(vector_sources) == 1L) {
    vector_sources <- unlist(
      strsplit(vector_sources, "[,\\s]+", perl = TRUE)
    )
  }

  vector_sources <- tolower(trimws(vector_sources))
  vector_sources <- vector_sources[nzchar(vector_sources)]

  allowed_sources <- c("malert", "gbif")
  invalid_sources <- setdiff(vector_sources, allowed_sources)

  if (length(invalid_sources)) {
    stop(
      "Unsupported vector source(s): ",
      paste(invalid_sources, collapse = ", "),
      call. = FALSE
    )
  }

  vector_sources <- allowed_sources[
    allowed_sources %in% vector_sources
  ]

  if (!length(vector_sources)) {
    stop(
      "`vector_sources` must contain `malert`, `gbif`, or both.",
      call. = FALSE
    )
  }

  vector_suffix <- paste(vector_sources, collapse = "_")

  if (missing(features) || !length(features)) {
    stop(
      "`features` must contain at least one feature code.",
      call. = FALSE
    )
  }

  if (length(features) == 1L) {
    features <- unlist(
      strsplit(features, "[,\\s]+", perl = TRUE)
    )
  }

  features <- tolower(trimws(features))
  features <- unique(features[nzchar(features)])

  feature_aliases <- c(
    lc = "lc",
    landcover = "lc",
    ndvi = "ndvi",
    el = "el",
    elevation = "el",
    pd = "pd",
    popdensity = "pd",
    se = "se",
    pseudoabsence = "se",
    wx_land = "wx_land",
    weather_land = "wx_land",
    wx_single = "wx_single",
    weather_single = "wx_single"
  )

  parse_feature <- function(feature) {
    if (grepl("^hex(?:_[0-9]+(?:\\.[0-9]+)?)?$", feature)) {
      cellsize <- sub("^hex_?", "", feature)

      if (!nzchar(cellsize)) {
        cellsize <- 400
      } else {
        cellsize <- suppressWarnings(as.numeric(cellsize))
      }

      if (is.na(cellsize) || !is.finite(cellsize) || cellsize <= 0) {
        stop(
          "Invalid hexagonal-grid cell size in feature: ",
          feature,
          call. = FALSE
        )
      }

      token <- gsub(
        "\\.",
        "_",
        format(cellsize, trim = TRUE, scientific = FALSE)
      )

      return(list(
        code = "hex",
        value = cellsize,
        raw = feature,
        cell_id_col = paste0("hex_id_", token)
      ))
    }

    if (grepl("^h3_[0-9]+$", feature)) {
      resolution <- as.integer(sub("^h3_", "", feature))

      if (!resolution %in% 0:15) {
        stop(
          "H3 resolution must be between 0 and 15.",
          call. = FALSE
        )
      }

      return(list(
        code = "h3",
        value = resolution,
        raw = feature,
        cell_id_col = paste0("h3_id_", resolution)
      ))
    }

    code <- unname(feature_aliases[feature])

    if (is.na(code)) {
      stop(
        "Unsupported feature code: ",
        feature,
        call. = FALSE
      )
    }

    list(
      code = code,
      value = NA,
      raw = feature,
      cell_id_col = NA_character_
    )
  }

  feature_specs <- lapply(features, parse_feature)
  feature_codes <- vapply(
    feature_specs,
    function(x) x$code,
    character(1L)
  )

  if ("se" %in% feature_codes) {
    se_index <- match("se", feature_codes)
    preceding_grid <- which(
      seq_along(feature_codes) < se_index &
        feature_codes %in% c("hex", "h3")
    )

    if (!length(preceding_grid)) {
      stop(
        "The `se` feature requires a grid feature before it. ",
        "For example, use `h3_9,se,...` or `hex_1200,se,...`.",
        call. = FALSE
      )
    }
  }

  dataset_path <- file.path(
    data_dir,
    paste0(
      "model_prep_",
      location_slug,
      "_",
      vector_suffix,
      ".Rds"
    )
  )

  if (!file.exists(dataset_path)) {
    stop(
      "Base dataset not found at ",
      dataset_path,
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message("Reading base dataset from: ", dataset_path)
  }

  current <- readRDS(dataset_path)

  if (is.null(attr(current, "output_path", exact = TRUE))) {
    attr(current, "output_path") <- dataset_path
  }

  attr(current, "location_slug") <- location_slug

  feature_labels <- c(
    hex = "hexagonal grid",
    h3 = "H3 grid",
    wx_land = paste(temporal_resolution, "ERA5-Land weather"),
    wx_single = paste(temporal_resolution, "ERA5 Single Levels weather"),
    lc = "land cover",
    ndvi = "NDVI",
    el = "elevation",
    pd = "population density",
    se = "sampling-effort pseudoabsences"
  )

  # The most recently added grid is used by a subsequent `se` step.
  active_grid_spec <- NULL

  for (index in seq_along(feature_specs)) {
    spec <- feature_specs[[index]]
    code <- spec$code
    write_current <- index == length(feature_specs)

    if (isTRUE(verbose)) {
      message("Adding ", feature_labels[[code]], " features.")
    }

    current <- switch(
      code,

      hex = {
        enriched <- add_hex_grid(
          dataset = current,
          iso3 = iso3,
          admin_level = admin_level,
          admin_name = admin_name,
          grid_dir = data_dir,
          cellsize_m = spec$value,
          verbose = verbose,
          write_output = write_current
        )

        active_grid_spec <- spec
        enriched
      },

      h3 = {
        enriched <- add_h3_grid(
          dataset = current,
          iso3 = iso3,
          admin_level = admin_level,
          admin_name = admin_name,
          grid_dir = data_dir,
          resolution = spec$value,
          verbose = verbose,
          write_output = write_current
        )

        active_grid_spec <- spec
        enriched
      },

      wx_land = {
        if (temporal_resolution == "daily") {
          add_daily_weather_features(
            dataset = current,
            dataset_type = "reanalysis-era5-land",
            data_dir = data_dir,
            write_output = write_current,
            verbose = verbose
          )
        } else {
          add_hourly_weather_features(
            dataset = current,
            dataset_type = "reanalysis-era5-land",
            data_dir = data_dir,
            write_output = write_current,
            verbose = verbose
          )
        }
      },

      wx_single = {
        if (temporal_resolution == "daily") {
          add_daily_weather_features(
            dataset = current,
            dataset_type = "reanalysis-era5-single-levels",
            data_dir = data_dir,
            write_output = write_current,
            verbose = verbose
          )
        } else {
          add_hourly_weather_features(
            dataset = current,
            dataset_type = "reanalysis-era5-single-levels",
            data_dir = data_dir,
            write_output = write_current,
            verbose = verbose
          )
        }
      },

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

      se = {
        if (is.null(active_grid_spec) ||
            !active_grid_spec$cell_id_col %in% names(current)) {
          stop(
            "Pseudoabsence generation requires the preceding grid column in the current dataset.",
            call. = FALSE
          )
        }

        if (isTRUE(verbose)) {
          message(
            "Using `",
            active_grid_spec$cell_id_col,
            "` as the pseudoabsence cell ID."
          )
        }

        add_pseudoabsences_se(
          dataset = current,
          iso3 = iso3,
          admin_level = admin_level,
          admin_name = admin_name,
          data_dir = data_dir,
          temporal_resolution = temporal_resolution,
          cell_id_col = active_grid_spec$cell_id_col,
          write_output = write_current
        )
      },

      stop(
        "Unsupported feature code: ",
        code,
        call. = FALSE
      )
    )
  }

  if (!"sea_days" %in% names(current) || anyNA(current$sea_days)) {
    if ("date" %in% names(current)) {
      current$date <- as.Date(current$date)
      sea_days <- lubridate::yday(current$date)

      if (!"sea_days" %in% names(current)) {
        current$sea_days <- sea_days
      } else {
        missing_sea_days <- is.na(current$sea_days)
        current$sea_days[missing_sea_days] <- sea_days[missing_sea_days]
      }
    }
  }

  final_path <- attr(current, "output_path", exact = TRUE)

  if (!is.null(final_path)) {
    saveRDS(current, final_path)

    if (isTRUE(verbose)) {
      message("Finished feature enrichment.")
      message("Final dataset written to: ", final_path)
    }
  }

  current
}
