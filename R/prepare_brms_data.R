#' Prepare data for brms modelling
#'
#' Cleans the dataset, optionally filters rows with missing values in user-supplied
#' variables, aggregates repeated records to modelling units, optionally converts
#' selected columns to factors, scales predictors using user-supplied scaling
#' specifications, and returns an object ready to be passed to [run_brms_model()].
#' Optionally saves the prepared object to disk.
#'
#' @inheritParams run_brms_model
#' @param cellsize_m Numeric cell size in meters. Defaults to 800.
#' @param temporal_resolution Character. Either `"daily"` or `"hourly"`.
#'   When `"daily"`, records are aggregated to source-grid-date units.
#'   When `"hourly"`, records are aggregated to source-grid-date-hour units.
#' @param base_required_cols Optional character vector of column names that must
#'   exist for the preparation machinery to run. The active grid column is always
#'   added automatically. If `NULL`, defaults to `c("date", "presence", "source")`.
#' @param vars_to_check Optional character vector of column names to check for
#'   missing values before aggregation and scaling. If `NULL`, no explicit
#'   missing-value filtering is applied beyond required grid/hour checks.
#' @param scale_specs Optional named list of scaling specifications. If `NULL`,
#'   no predictor scaling is applied. Specs may be plain named lists with fields
#'   `input`, `output`, optional `transform`, and optional `scale_name`; these are
#'   normalized internally by `normalize_scale_specs()`.
#' @param aggregation_specs Optional named list or named character vector defining
#'   how columns should be aggregated when multiple rows occur in the same
#'   modelling unit. Names are column names and values are aggregation methods.
#'   Supported methods are `"any"`, `"all"`, `"mean"`, `"median"`, `"sum"`,
#'   `"min"`, `"max"`, `"first"`, `"last"`, `"mode"`, and `"paste_unique"`.
#'   If `NULL`, all non-grouping columns are aggregated using `"first"`, except
#'   `presence`, which defaults to `"any"`.
#' @param factor_cols Optional character vector of columns to convert to factors
#'   after aggregation and before scaling/modelling. If `NULL`, no columns are
#'   converted. For typical models, use columns such as `"year"`,
#'   `"landcover_code"`, and `"source"` when they should be treated as categorical
#'   predictors or grouping variables.
#' @param output_dir Directory where the prepared dataset is written when
#'   `write = TRUE`. Defaults to `"data/proc"`.
#' @param write Logical. If `TRUE`, the prepared object is written to disk.
#'   Defaults to `FALSE`.
#' @param verbose Logical. Emit informative messages when `TRUE`.
#'
#' @return A list of class `brms_data_prep` containing:
#'   \item{model_data}{The filtered, aggregated, scaled data.frame ready for brms.}
#'   \item{grid_col}{The name of the grid identifier column used.}
#'   \item{scaling}{A list of mean/sd values used for scaling predictors.}
#'   \item{scale_specs}{The normalized scaling specifications supplied to the function.}
#'   \item{aggregation_specs}{The aggregation specifications used.}
#'   \item{factor_cols}{The factor columns used.}
#'   \item{meta}{Metadata for file naming and reproducibility.}
#'
#' @export
prepare_brms_data <- function(
    dataset,
    cellsize_m = 800,
    temporal_resolution = c("daily", "hourly"),
    base_required_cols = NULL,
    vars_to_check = NULL,
    scale_specs = NULL,
    aggregation_specs = NULL,
    factor_cols = NULL,
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
      stop(
        "`iso3`, `admin_level`, and `admin_name` must be supplied together.",
        call. = FALSE
      )
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

  if (!is.null(aggregation_specs) &&
      !is.list(aggregation_specs) &&
      !is.character(aggregation_specs)) {
    stop(
      "`aggregation_specs` must be NULL, a named character vector, or a named list.",
      call. = FALSE
    )
  }

  if (!is.null(factor_cols) && !is.character(factor_cols)) {
    stop("`factor_cols` must be NULL or a character vector.", call. = FALSE)
  }

  scale_specs <- normalize_scale_specs(scale_specs)
  aggregation_specs <- normalize_aggregation_specs(aggregation_specs)

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
    stop(
      "No observations remain after filtering for valid grid identifiers.",
      call. = FALSE
    )
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
      stop(
        "Hourly resolution requested but `hour` column not found in dataset.",
        call. = FALSE
      )
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
  # 8. Aggregate duplicate modelling units
  # ---------------------------------------------------------------------------

  n_before_aggregation <- nrow(df)

  aggregation_cols <- if (identical(temporal_resolution, "hourly")) {
    c("source", grid_col, "date", "hour")
  } else {
    c("source", grid_col, "date")
  }

  missing_aggregation_cols <- setdiff(
    c(aggregation_cols, "presence"),
    names(df)
  )

  if (length(missing_aggregation_cols)) {
    stop(
      "Cannot aggregate modelling units because these columns are missing: ",
      paste(missing_aggregation_cols, collapse = ", "),
      call. = FALSE
    )
  }

  aggregatable_cols <- setdiff(names(df), aggregation_cols)

  default_aggregation_specs <- rep("first", length(aggregatable_cols))
  names(default_aggregation_specs) <- aggregatable_cols

  if ("presence" %in% aggregatable_cols) {
    default_aggregation_specs[["presence"]] <- "any"
  }

  if (!is.null(aggregation_specs)) {
    unknown_aggregation_cols <- setdiff(names(aggregation_specs), names(df))

    if (length(unknown_aggregation_cols)) {
      stop(
        "Columns in `aggregation_specs` are missing from the dataset: ",
        paste(unknown_aggregation_cols, collapse = ", "),
        call. = FALSE
      )
    }

    grouping_spec_cols <- intersect(names(aggregation_specs), aggregation_cols)

    if (length(grouping_spec_cols)) {
      stop(
        "Columns in `aggregation_specs` cannot also be aggregation grouping columns: ",
        paste(grouping_spec_cols, collapse = ", "),
        call. = FALSE
      )
    }

    default_aggregation_specs[names(aggregation_specs)] <- aggregation_specs
  }

  df <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(aggregation_cols))
    ) |>
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::all_of(names(default_aggregation_specs)),
        .fns = ~ apply_aggregation_method(
          .x,
          default_aggregation_specs[[dplyr::cur_column()]]
        )
      ),
      n_records = dplyr::n(),
      n_presence_records = sum(as.logical(.data$presence), na.rm = TRUE),
      .groups = "drop"
    )

  n_after_aggregation <- nrow(df)
  n_aggregated <- n_before_aggregation - n_after_aggregation

  if (isTRUE(verbose) && n_aggregated > 0) {
    message(sprintf(
      "Aggregated %d repeated records into %d modelling-unit rows using `%s`.",
      n_aggregated,
      n_after_aggregation,
      grid_col
    ))
  }

  # ---------------------------------------------------------------------------
  # 9. Convert user-specified columns to factors after aggregation
  # ---------------------------------------------------------------------------

  if (!is.null(factor_cols)) {
    factor_cols <- unique(factor_cols)

    missing_factor_cols <- setdiff(factor_cols, names(df))

    if (length(missing_factor_cols)) {
      stop(
        "Columns in `factor_cols` are missing from the prepared data: ",
        paste(missing_factor_cols, collapse = ", "),
        call. = FALSE
      )
    }

    df <- df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(factor_cols),
          factor
        )
      )

    if (isTRUE(verbose)) {
      message(
        "Converted columns to factors: ",
        paste(factor_cols, collapse = ", ")
      )
    }
  } else if (isTRUE(verbose)) {
    message("No `factor_cols` supplied; skipping factor conversion.")
  }

  # ---------------------------------------------------------------------------
  # 10. Apply scaling specs after aggregation and factor conversion
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
  # 11. Return object
  # ---------------------------------------------------------------------------

  obj <- structure(
    list(
      model_data = df,
      grid_col = grid_col,
      scaling = scale_params,
      scale_specs = scale_specs,
      aggregation_specs = default_aggregation_specs,
      factor_cols = factor_cols,
      meta = list(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        slug = location_slug,
        source_path = dataset_path,
        temporal_resolution = temporal_resolution,
        base_required_cols = base_required_cols,
        vars_to_check = vars_to_check,
        scale_spec_names = if (is.null(scale_specs)) NULL else names(scale_specs),
        aggregation_specs = default_aggregation_specs,
        aggregation_cols = aggregation_cols,
        factor_cols = factor_cols,
        n_before_aggregation = n_before_aggregation,
        n_after_aggregation = n_after_aggregation
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
      message(
        "Prepared brms data object (",
        temporal_resolution,
        ") written to ",
        output_path
      )
    }
  }

  obj
}