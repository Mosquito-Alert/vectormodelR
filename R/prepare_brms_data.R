#' Prepare data for brms modelling
#'
#' Cleans the dataset, optionally filters rows with missing values in user-supplied
#' variables, scales predictors (storing means/SDs for future use), and returns an
#' object ready to be passed to [run_brms_model()]. Optionally saves the prepared
#' object to disk.
#'
#' @inheritParams run_brms_model
#' @param cellsize_m Numeric cell size (meters). Defaults to 800.
#' @param temporal_resolution Character. Either `"daily"` (default) or `"hourly"`.
#'   When `"hourly"`, filters to records with valid hourly weather values and
#'   scales the current-hour and short-window hourly predictors. When `"daily"`,
#'   uses the daily weather summaries and precipitation lag predictors.
#' @param vars_to_check Optional character vector of column names to check for
#'   missing values before scaling. If `NULL`, no explicit missing-value filtering
#'   is applied beyond required grid/hour checks.
#' @param output_dir Directory where the prepared dataset is written when `write = TRUE`.
#'   Defaults to `"data/proc"`.
#' @param write Logical. If `TRUE` the prepared object is written to disk. Defaults to `FALSE`.
#' @param verbose Logical. Emit informative messages when `TRUE`.
#'
#' @return A list of class `brms_data_prep` containing:
#'   \item{model_data}{The filtered, scaled data.frame ready for brms.}
#'   \item{grid_col}{The name of the grid identifier column used.}
#'   \item{scaling}{A list of mean/sd values used for scaling predictors.}
#'   \item{meta}{Metadata (iso3, admin_level, etc.) for file naming.}
#' @export
prepare_brms_data <- function(
    dataset,
    cellsize_m = 800,
    temporal_resolution = c("daily", "hourly"),
    vars_to_check = NULL,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)

  # ---- load dataset ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L && nzchar(dataset)
  dataset_path <- if (dataset_is_path) dataset else NULL

  if (dataset_is_path) {
    if (!file.exists(dataset_path)) {
      stop("Dataset not found at ", dataset_path, call. = FALSE)
    }
    dataset <- readRDS(dataset_path)
  }

  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or a path to an RDS file.", call. = FALSE)
  }

  # ---- metadata & slug ----
  explicit_slug <- NULL

  if (!all(vapply(list(iso3, admin_level, admin_name), is.null, logical(1)))) {
    if (any(vapply(list(iso3, admin_level, admin_name), is.null, logical(1)))) {
      stop("`iso3`, `admin_level`, and `admin_name` must be supplied together.", call. = FALSE)
    }

    ids <- tryCatch(
      build_location_identifiers(iso3, admin_level, admin_name),
      error = function(e) list(slug = "custom")
    )

    explicit_slug <- ids$slug
  }

  location_slug <- attr(dataset, "location_slug", exact = TRUE)

  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- explicit_slug
  }

  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- "custom"
  }

  # ---- validation ----
  if (!is.numeric(cellsize_m) || length(cellsize_m) != 1L || is.na(cellsize_m) || cellsize_m <= 0) {
    stop("`cellsize_m` must be a positive numeric scalar.", call. = FALSE)
  }

  cellsize_token <- gsub(
    "\\.",
    "_",
    format(cellsize_m, trim = TRUE, scientific = FALSE)
  )

  grid_col <- paste0("grid_id_", cellsize_token)

  if (isTRUE(verbose)) {
    message("Using grid column: ", grid_col)
  }

  base_required_cols <- c(
    "date",
    "sea_days",
    "presence",
    "landcover_code",
    "ndvi_ddf_proximity",
    "elevation_m",
    "popdensity_km2",
    "source",
    grid_col
  )

  weather_required_cols <- if (identical(temporal_resolution, "hourly")) {
    c(
      "t2m_C_hour",
      "RH_hour",
      "ws10_hour",
      "ppt_mm_hour",
      "ppt_mm_prev_6h",
      "ppt_mm_prev_24h",
      "t2m_C_mean_prev_6h",
      "RH_mean_prev_6h"
    )
  } else {
    c(
      "maxTM",
      "meanPPT24H",
      "PPT_3d",
      "PPT_7d",
      "PPT_14d",
      "PPT_21d",
      "PPT_30d",
      "PPT_3d_lag7",
      "PPT_7d_lag7",
      "PPT_14d_lag7",
      "PPT_21d_lag7",
      "PPT_30d_lag7"
    )
  }

  required_cols <- c(base_required_cols, weather_required_cols)

  missing <- setdiff(required_cols, names(dataset))

  if (length(missing)) {
    stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (identical(temporal_resolution, "hourly") &&
      !"hour" %in% names(dataset) &&
      !"datetime" %in% names(dataset)) {
    stop("Hourly resolution requires an `hour` column or a `datetime` column.", call. = FALSE)
  }

  if (!is.null(vars_to_check) && !is.character(vars_to_check)) {
    stop("`vars_to_check` must be NULL or a character vector.", call. = FALSE)
  }

  # ---- filtering & formatting ----
  dataset[[grid_col]] <- as.character(dataset[[grid_col]])

  df <- dataset |>
    dplyr::filter(!is.na(.data[[grid_col]]))

  if (!nrow(df)) {
    stop("No observations remain after filtering for valid grid identifiers.", call. = FALSE)
  }

  df <- df |>
    dplyr::mutate(date = as.Date(.data$date))

  if (!"year" %in% names(df)) {
    df$year <- NA_integer_
  }

  df$year <- ifelse(
    is.na(df$year),
    as.integer(format(df$date, "%Y")),
    df$year
  )

  if (identical(temporal_resolution, "hourly") && "datetime" %in% names(df)) {
    parsed_datetime <- if (inherits(df[["datetime"]], "POSIXt")) {
      as.POSIXct(df[["datetime"]], tz = "UTC")
    } else {
      suppressWarnings(as.POSIXct(
        df[["datetime"]],
        tz = "UTC",
        tryFormats = c(
          "%Y-%m-%d %H:%M:%OS",
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%d %H:%M",
          "%Y-%m-%d"
        )
      ))
    }

    if (!"hour" %in% names(df)) {
      df$hour <- NA_integer_
    }

    df$hour <- ifelse(
      is.na(df$hour),
      as.integer(format(parsed_datetime, "%H")),
      df$hour
    )
  }

  # ---- hourly-specific filtering ----
  if (identical(temporal_resolution, "hourly")) {
    if (!"hour" %in% names(df)) {
      stop("Hourly resolution requested but `hour` column not found in dataset.", call. = FALSE)
    }

    n_before_hour <- nrow(df)

    df <- df |>
      dplyr::filter(!is.na(.data$hour))

    n_after_hour <- nrow(df)
    n_dropped_hour <- n_before_hour - n_after_hour

    if (n_dropped_hour > 0 && isTRUE(verbose)) {
      message(sprintf(
        "Hourly mode: Dropped %d records (%.1f%%) with missing hour values.",
        n_dropped_hour,
        100 * n_dropped_hour / n_before_hour
      ))
    }

    if (!nrow(df)) {
      stop("No observations remain after filtering for valid hours.", call. = FALSE)
    }
  }

  # ---- user-controlled NA filtering ----
  if (!is.null(vars_to_check)) {
    vars_to_check <- unique(vars_to_check)

    missing_check_cols <- setdiff(vars_to_check, names(df))

    if (length(missing_check_cols)) {
      stop(
        "Variables in `vars_to_check` are missing from the dataset: ",
        paste(missing_check_cols, collapse = ", "),
        call. = FALSE
      )
    }

    n_before_na <- nrow(df)

    df <- df |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(vars_to_check), ~ !is.na(.)))

    n_after_na <- nrow(df)
    n_dropped_na <- n_before_na - n_after_na

    if (isTRUE(verbose)) {
      message(
        "NA check used variables: ",
        paste(vars_to_check, collapse = ", ")
      )

      message(sprintf(
        "Dropped %d records (%.1f%%) with missing values in checked variables.",
        n_dropped_na,
        100 * n_dropped_na / n_before_na
      ))
    }

    if (!nrow(df)) {
      stop("No observations remain after dropping NAs.", call. = FALSE)
    }
  } else if (isTRUE(verbose)) {
    message("No `vars_to_check` supplied; skipping explicit NA filtering.")
  }

  # ---- scaling ----
  scale_params <- list()

  do_scale <- function(x, name) {
    mu <- mean(x, na.rm = TRUE)
    sigma <- stats::sd(x, na.rm = TRUE)

    scale_params[[name]] <<- list(mean = mu, sd = sigma)

    if (is.na(sigma) || sigma == 0) {
      warning(
        "Variable `", name, "` has zero or undefined standard deviation; scaled values set to 0.",
        call. = FALSE
      )
      return(rep(0, length(x)))
    }

    (x - mu) / sigma
  }

  df <- df |>
    dplyr::mutate(
      year = factor(.data$year),
      landcover_code = factor(.data$landcover_code),
      source = factor(.data$source),
      ndvi_z = do_scale(.data$ndvi_ddf_proximity, "ndvi"),
      elev_z = do_scale(.data$elevation_m, "elevation"),
      pop_z = do_scale(log1p(.data$popdensity_km2), "log1p_pop")
    )

  if (identical(temporal_resolution, "hourly")) {
    df <- df |>
      dplyr::mutate(
        t2m_C_hour_z = do_scale(.data$t2m_C_hour, "t2m_C_hour"),
        rh_hour_z = do_scale(.data$RH_hour, "RH_hour"),
        ws10_hour_z = do_scale(.data$ws10_hour, "ws10_hour"),
        ppt_mm_hour_z = do_scale(log1p(.data$ppt_mm_hour), "log1p_ppt_hour"),
        ppt_mm_prev_6h_z = do_scale(log1p(.data$ppt_mm_prev_6h), "log1p_ppt_6h"),
        ppt_mm_prev_24h_z = do_scale(log1p(.data$ppt_mm_prev_24h), "log1p_ppt_24h"),
        t2m_C_mean_prev_6h_z = do_scale(.data$t2m_C_mean_prev_6h, "t2m_C_mean_prev_6h"),
        RH_mean_prev_6h_z = do_scale(.data$RH_mean_prev_6h, "RH_mean_prev_6h")
      )
  } else {
    df <- df |>
      dplyr::mutate(
        maxTM_z = do_scale(.data$maxTM, "maxTM"),
        ppt_z = do_scale(log1p(.data$meanPPT24H), "log1p_ppt"),
        ppt_3d_z = do_scale(log1p(.data$PPT_3d), "log1p_PPT_3d"),
        ppt_7d_z = do_scale(log1p(.data$PPT_7d), "log1p_PPT_7d"),
        ppt_14d_z = do_scale(log1p(.data$PPT_14d), "log1p_PPT_14d"),
        ppt_21d_z = do_scale(log1p(.data$PPT_21d), "log1p_PPT_21d"),
        ppt_30d_z = do_scale(log1p(.data$PPT_30d), "log1p_PPT_30d"),
        ppt_3d_lag7_z = do_scale(log1p(.data$PPT_3d_lag7), "log1p_PPT_3d_lag7"),
        ppt_7d_lag7_z = do_scale(log1p(.data$PPT_7d_lag7), "log1p_PPT_7d_lag7"),
        ppt_14d_lag7_z = do_scale(log1p(.data$PPT_14d_lag7), "log1p_PPT_14d_lag7"),
        ppt_21d_lag7_z = do_scale(log1p(.data$PPT_21d_lag7), "log1p_PPT_21d_lag7"),
        ppt_30d_lag7_z = do_scale(log1p(.data$PPT_30d_lag7), "log1p_PPT_30d_lag7")
      )
  }

  # ---- collapse duplicate modelling units ----
  df <- df |>
    dplyr::arrange(.data$source, .data[[grid_col]], .data$date)

  n_before_distinct <- nrow(df)

  if (identical(temporal_resolution, "hourly")) {
    df <- df |>
      dplyr::distinct(
        .data$source,
        .data[[grid_col]],
        .data$date,
        .data$hour,
        .data$presence,
        .keep_all = TRUE
      )
  } else {
    df <- df |>
      dplyr::distinct(
        .data$source,
        .data[[grid_col]],
        .data$date,
        .data$presence,
        .keep_all = TRUE
      )
  }

  n_after_distinct <- nrow(df)
  n_dropped_distinct <- n_before_distinct - n_after_distinct

  if (isTRUE(verbose) && n_dropped_distinct > 0) {
    message(sprintf(
      "Collapsed %d duplicate modelling-unit records using `%s`.",
      n_dropped_distinct,
      grid_col
    ))
  }

  # ---- return object ----
  obj <- structure(
    list(
      model_data = df,
      grid_col = grid_col,
      scaling = scale_params,
      meta = list(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        slug = location_slug,
        source_path = dataset_path,
        temporal_resolution = temporal_resolution,
        vars_to_check = vars_to_check
      )
    ),
    class = "brms_data_prep"
  )

  if (isTRUE(write)) {
    if (is.null(output_dir) || !nzchar(output_dir)) {
      stop("`write = TRUE` requires a valid `output_dir`.", call. = FALSE)
    }

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    resolution_suffix <- if (identical(temporal_resolution, "hourly")) {
      "_hourly"
    } else {
      "_daily"
    }

    output_path <- file.path(
      output_dir,
      sprintf("model_prep_%s%s_data.rds", location_slug, resolution_suffix)
    )

    saveRDS(obj, output_path)

    if (isTRUE(verbose)) {
      message("Prepared brms data object (", temporal_resolution, ") written to ", output_path)
    }
  }

  obj
}