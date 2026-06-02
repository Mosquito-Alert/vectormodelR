#' Prepare data for brms modelling
#'
#' Cleans the dataset, optionally filters rows with missing values in user-supplied
#' variables, scales predictors using user-supplied scaling specifications, and
#' returns an object ready to be passed to [run_brms_model()]. Optionally saves
#' the prepared object to disk.
#'
#' @inheritParams run_brms_model
#' @param cellsize_m Numeric cell size (meters). Defaults to 800.
#' @param temporal_resolution Character. Either `"daily"` (default) or `"hourly"`.
#'   When `"hourly"`, prepares records for hourly models. When `"daily"`,
#'   prepares records for daily models.
#' @param base_required_cols Optional character vector of column names that must
#'   exist for the preparation machinery to run. The active grid column is always
#'   added automatically. If `NULL`, defaults to `c("date", "presence", "source")`.
#' @param vars_to_check Optional character vector of column names to check for
#'   missing values before scaling. If `NULL`, no explicit missing-value filtering
#'   is applied beyond required grid/hour checks.
#' @param scale_specs Optional named list of scaling specifications. If `NULL`,
#'   no predictor scaling is applied. Use [scale_spec()] to define how raw
#'   columns should be transformed and scaled into model-ready columns.
#' @param output_dir Directory where the prepared dataset is written when
#'   `write = TRUE`. Defaults to `"data/proc"`.
#' @param write Logical. If `TRUE` the prepared object is written to disk.
#'   Defaults to `FALSE`.
#' @param verbose Logical. Emit informative messages when `TRUE`.
#'
#' @return A list of class `brms_data_prep` containing:
#'   \item{model_data}{The filtered, scaled data.frame ready for brms.}
#'   \item{grid_col}{The name of the grid identifier column used.}
#'   \item{scaling}{A list of mean/sd values used for scaling predictors.}
#'   \item{scale_specs}{The scaling specifications supplied to the function.}
#'   \item{meta}{Metadata (iso3, admin_level, etc.) for file naming.}
#'
#' @export
prepare_brms_data <- function(
    dataset,
    cellsize_m = 800,
    temporal_resolution = c("daily", "hourly"),
    base_required_cols = NULL,
    vars_to_check = NULL,
    scale_specs = NULL,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)

  # ---------------------------------------------------------------------------
  # 1. Load dataset
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # 2. Metadata and slug
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # 3. Argument validation
  # ---------------------------------------------------------------------------

  if (!is.numeric(cellsize_m) ||
      length(cellsize_m) != 1L ||
      is.na(cellsize_m) ||
      cellsize_m <= 0) {
    stop("`cellsize_m` must be a positive numeric scalar.", call. = FALSE)
  }

  if (!is.null(base_required_cols) && !is.character(base_required_cols)) {
    stop("`base_required_cols` must be NULL or a character vector.", call. = FALSE)
  }

  if (!is.null(vars_to_check) && !is.character(vars_to_check)) {
    stop("`vars_to_check` must be NULL or a character vector.", call. = FALSE)
  }

  if (!is.null(scale_specs) && !is.list(scale_specs)) {
    stop("`scale_specs` must be NULL or a named list.", call. = FALSE)
  }

  if (!is.null(scale_specs)) {
    validate_scale_specs(scale_specs)
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

  # ---------------------------------------------------------------------------
  # 4. Required columns
  # ---------------------------------------------------------------------------

  if (is.null(base_required_cols)) {
    base_required_cols <- c(
      "date",
      "presence",
      "source"
    )
  }

  base_required_cols <- unique(c(
    base_required_cols,
    grid_col
  ))

  scale_input_cols <- character(0)

  if (!is.null(scale_specs)) {
    scale_input_cols <- vapply(
      scale_specs,
      function(spec) spec$input,
      character(1)
    )
  }

  required_cols <- unique(c(
    base_required_cols,
    scale_input_cols
  ))

  missing <- setdiff(required_cols, names(dataset))

  if (length(missing)) {
    stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  if (identical(temporal_resolution, "hourly") &&
      !"hour" %in% names(dataset) &&
      !"datetime" %in% names(dataset)) {
    stop("Hourly resolution requires an `hour` column or a `datetime` column.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 5. Basic filtering and formatting
  # ---------------------------------------------------------------------------

  dataset[[grid_col]] <- as.character(dataset[[grid_col]])

  df <- dataset |>
    dplyr::filter(!is.na(.data[[grid_col]]))

  if (!nrow(df)) {
    stop("No observations remain after filtering for valid grid identifiers.", call. = FALSE)
  }

  if ("date" %in% names(df)) {
    df <- df |>
      dplyr::mutate(date = as.Date(.data$date))
  }

  if (!"year" %in% names(df)) {
    df$year <- NA_integer_
  }

  if ("date" %in% names(df)) {
    df$year <- ifelse(
      is.na(df$year),
      as.integer(format(df$date, "%Y")),
      df$year
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Hour handling
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # 7. User-controlled NA filtering
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # 8. Convert categorical/grouping fields if present
  # ---------------------------------------------------------------------------

  if ("year" %in% names(df)) {
    df$year <- factor(df$year)
  }

  if ("landcover_code" %in% names(df)) {
    df$landcover_code <- factor(df$landcover_code)
  }

  if ("source" %in% names(df)) {
    df$source <- factor(df$source)
  }

  # ---------------------------------------------------------------------------
  # 9. Apply scaling specs
  # ---------------------------------------------------------------------------

  scaled <- apply_scale_specs(
    df = df,
    scale_specs = scale_specs
  )

  df <- scaled$data
  scale_params <- scaled$scaling

  if (isTRUE(verbose)) {
    if (is.null(scale_specs)) {
      message("No `scale_specs` supplied; skipping predictor scaling.")
    } else {
      scaled_outputs <- vapply(
        scale_specs,
        function(spec) spec$output,
        character(1)
      )

      message(
        "Scaled variables created: ",
        paste(scaled_outputs, collapse = ", ")
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 10. Collapse duplicate modelling units
  # ---------------------------------------------------------------------------

  n_before_distinct <- nrow(df)

  if (identical(temporal_resolution, "hourly")) {
    collapse_cols <- c("source", grid_col, "date", "hour", "presence")
  } else {
    collapse_cols <- c("source", grid_col, "date", "presence")
  }

  missing_collapse_cols <- setdiff(collapse_cols, names(df))

  if (length(missing_collapse_cols)) {
    stop(
      "Cannot collapse duplicate modelling units because these columns are missing: ",
      paste(missing_collapse_cols, collapse = ", "),
      call. = FALSE
    )
  }

  df <- df |>
    dplyr::arrange(
      .data$source,
      .data[[grid_col]],
      .data$date
    ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(collapse_cols)),
      .keep_all = TRUE
    )

  n_after_distinct <- nrow(df)
  n_dropped_distinct <- n_before_distinct - n_after_distinct

  if (isTRUE(verbose) && n_dropped_distinct > 0) {
    message(sprintf(
      "Collapsed %d duplicate modelling-unit records using `%s`.",
      n_dropped_distinct,
      grid_col
    ))
  }

  # ---------------------------------------------------------------------------
  # 11. Return object
  # ---------------------------------------------------------------------------

  obj <- structure(
    list(
      model_data = df,
      grid_col = grid_col,
      scaling = scale_params,
      scale_specs = scale_specs,
      meta = list(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        slug = location_slug,
        source_path = dataset_path,
        temporal_resolution = temporal_resolution,
        base_required_cols = base_required_cols,
        vars_to_check = vars_to_check,
        scale_spec_names = if (is.null(scale_specs)) NULL else names(scale_specs)
      )
    ),
    class = "brms_data_prep"
  )

  # ---------------------------------------------------------------------------
  # 12. Optional write
  # ---------------------------------------------------------------------------

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