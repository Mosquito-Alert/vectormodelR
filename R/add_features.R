#' Sequentially enrich model-preparation datasets
#'
#' Locates a prepared model dataset and adds the requested features in the
#' order supplied.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level used when preparing the dataset.
#' @param admin_name Administrative unit name.
#' @param features Character vector or comma-separated feature codes. Available
#'   codes are `"hex"`, `"hex_<cellsize>"`, `"h3_<resolution>"`,
#'   `"wx_daily_land"`, `"wx_hourly_land"`, `"wx_daily_single"`,
#'   `"wx_hourly_single"`, `"lc"`, `"ndvi"`, `"el"`, `"pd"`, and `"se"`.
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
#'   features = "se,el,pd,wx_daily_land,ndvi,lc,h3_6"
#' )
#'
#' hourly_data <- add_features(
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona",
#'   features = "se,el,pd,wx_hourly_land,ndvi,lc,h3_6"
#' )
#' }
add_features <- function(
  iso3,
  admin_level,
  admin_name,
  features,
  vector_sources = c("malert", "gbif"),
  data_dir = "data/proc",
  verbose = TRUE
) {
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
  invalid_sources <- setdiff(
    vector_sources,
    allowed_sources
  )

  if (length(invalid_sources) > 0L) {
    stop(
      "Unsupported vector source(s): ",
      paste(invalid_sources, collapse = ", "),
      call. = FALSE
    )
  }

  vector_sources <- allowed_sources[
    allowed_sources %in% vector_sources
  ]

  if (length(vector_sources) == 0L) {
    stop(
      "`vector_sources` must contain `malert`, `gbif`, or both.",
      call. = FALSE
    )
  }

  vector_suffix <- paste(
    vector_sources,
    collapse = "_"
  )

  if (missing(features) || length(features) == 0L) {
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
    wx_daily_land = "wx_daily_land",
    wx_hourly_land = "wx_hourly_land",
    wx_daily_single = "wx_daily_single",
    wx_hourly_single = "wx_hourly_single"
  )

  parse_feature <- function(feature) {
    if (grepl("^hex(?:_[0-9]+(?:\\.[0-9]+)?)?$", feature)) {
      cellsize <- sub("^hex_?", "", feature)

      if (!nzchar(cellsize)) {
        cellsize <- 400
      } else {
        cellsize <- as.numeric(cellsize)
      }

      return(list(
        code = "hex",
        value = cellsize,
        raw = feature
      ))
    }

    if (grepl("^h3_[0-9]+$", feature)) {
      resolution <- as.integer(
        sub("^h3_", "", feature)
      )

      if (!resolution %in% 0:15) {
        stop(
          "H3 resolution must be between 0 and 15.",
          call. = FALSE
        )
      }

      return(list(
        code = "h3",
        value = resolution,
        raw = feature
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
      raw = feature
    )
  }

  feature_specs <- lapply(
    features,
    parse_feature
  )

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
    message(
      "Reading base dataset from: ",
      dataset_path
    )
  }

  current <- readRDS(dataset_path)

  if (is.null(attr(current, "output_path", exact = TRUE))) {
    attr(current, "output_path") <- dataset_path
  }

  attr(current, "location_slug") <- location_slug

  feature_labels <- c(
    hex = "hexagonal grid",
    h3 = "H3 grid",
    wx_daily_land = "daily ERA5-Land weather",
    wx_hourly_land = "hourly ERA5-Land weather",
    wx_daily_single = "daily ERA5 Single Levels weather",
    wx_hourly_single = "hourly ERA5 Single Levels weather",
    lc = "land cover",
    ndvi = "NDVI",
    el = "elevation",
    pd = "population density",
    se = "sampling-effort pseudoabsences"
  )

  for (index in seq_along(feature_specs)) {
    spec <- feature_specs[[index]]
    code <- spec$code
    write_current <- index == length(feature_specs)

    if (isTRUE(verbose)) {
      message(
        "Adding ",
        feature_labels[[code]],
        " features."
      )
    }

    current <- switch(
      code,

      hex = add_hex_grid(
        dataset = current,
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        grid_dir = data_dir,
        cellsize_m = spec$value,
        verbose = verbose,
        write_output = write_current
      ),

      h3 = add_h3_grid(
        dataset = current,
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        grid_dir = data_dir,
        resolution = spec$value,
        verbose = verbose,
        write_output = write_current
      ),

      wx_daily_land = add_daily_weather_features(
        dataset = current,
        dataset_type = "reanalysis-era5-land",
        data_dir = data_dir,
        write_output = write_current,
        verbose = verbose
      ),

      wx_hourly_land = add_hourly_weather_features(
        dataset = current,
        dataset_type = "reanalysis-era5-land",
        data_dir = data_dir,
        write_output = write_current,
        verbose = verbose
      ),

      wx_daily_single = add_daily_weather_features(
        dataset = current,
        dataset_type = "reanalysis-era5-single-levels",
        data_dir = data_dir,
        write_output = write_current,
        verbose = verbose
      ),

      wx_hourly_single = add_hourly_weather_features(
        dataset = current,
        dataset_type = "reanalysis-era5-single-levels",
        data_dir = data_dir,
        write_output = write_current,
        verbose = verbose
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

      stop(
        "Unsupported feature code: ",
        code,
        call. = FALSE
      )
    )
  }

  if (!"sea_days" %in% names(current) ||
      anyNA(current$sea_days)) {
    if ("date" %in% names(current)) {
      current$date <- as.Date(current$date)
      sea_days <- lubridate::yday(current$date)

      if (!"sea_days" %in% names(current)) {
        current$sea_days <- sea_days
      } else {
        missing_sea_days <- is.na(current$sea_days)
        current$sea_days[missing_sea_days] <-
          sea_days[missing_sea_days]
      }
    }
  }

  final_path <- attr(
    current,
    "output_path",
    exact = TRUE
  )

  if (!is.null(final_path)) {
    saveRDS(current, final_path)

    if (isTRUE(verbose)) {
      message("Finished feature enrichment.")
      message(
        "Final dataset written to: ",
        final_path
      )
    }
  }

  current
}