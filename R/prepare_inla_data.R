#' Prepare an existing model-data object for INLA
#'
#' Takes the output from [prepare_model_data()], converts the binary response
#' to numeric 0/1, and adds indices commonly required by INLA latent models.
#'
#' The prepared data may be supplied directly as:
#' \itemize{
#'   \item a `model_data_prep` object;
#'   \item a data frame;
#'   \item a path to an RDS file created by [prepare_model_data()]; or
#'   \item `NULL`, in which case the function attempts to locate the saved
#'   preparation file using `iso3`, `admin_level`, `admin_name`, and
#'   `temporal_resolution`.
#' }
#'
#' This function does not repeat filtering, aggregation, factor conversion,
#' or predictor scaling. Those operations must first be performed by
#' [prepare_model_data()].
#'
#' @param dataset A `model_data_prep` object, a data frame, a path to an RDS
#'   file created by [prepare_model_data()], or `NULL`.
#' @param temporal_resolution Character. Either `"daily"` or `"hourly"`.
#'   Used when locating a saved preparation file and constructing output names.
#' @param iso3 Optional ISO3 country code. Required with `admin_level` and
#'   `admin_name` when `dataset = NULL`.
#' @param admin_level Optional administrative level. Required with `iso3` and
#'   `admin_name` when `dataset = NULL`.
#' @param admin_name Optional administrative name. Required with `iso3` and
#'   `admin_level` when `dataset = NULL`.
#' @param input_dir Directory containing files created by
#'   [prepare_model_data()]. Defaults to `"data/proc"`.
#' @param response_col Name of the binary response column. Defaults to
#'   `"presence"`.
#' @param create_year_index Logical. Create an integer INLA year index.
#'   Defaults to `TRUE`.
#' @param year_col Name of the original year column. Defaults to `"year"`.
#' @param year_index_col Name of the generated year index. Defaults to
#'   `"year_id"`.
#' @param create_seasonal_index Logical. Create a consistent 365-position
#'   seasonal index from the date. Defaults to `TRUE`.
#' @param date_col Name of the date column. Defaults to `"date"`.
#' @param seasonal_index_col Name of the generated seasonal index. Defaults to
#'   `"sea_day_id"`.
#' @param leap_day_method How February 29 is mapped in the 365-day seasonal
#'   index. `"feb28"` maps it to position 59; `"mar01"` maps it to position 60.
#' @param group_specs Optional named list describing continuous variables to
#'   group using [INLA::inla.group()]. Each specification must contain
#'   `input`, `output`, and `n`; `method` is optional and defaults to
#'   `"quantile"`.
#' @param drop_incomplete Logical. Drop rows missing the response or generated
#'   INLA indices and grouped predictors. Defaults to `FALSE`.
#' @param output_dir Directory used when `write = TRUE`. Defaults to
#'   `"data/proc"`.
#' @param write Logical. Write the resulting INLA preparation object to disk.
#'   Defaults to `FALSE`.
#' @param verbose Logical. Emit informative messages. Defaults to `TRUE`.
#'
#' @return An object of class `inla_data_prep` and `model_data_prep` containing:
#'   \item{model_data}{The INLA-ready data frame.}
#'   \item{grid_col}{The grid identifier inherited from the source object.}
#'   \item{scaling}{Scaling parameters inherited from the source object.}
#'   \item{scale_specs}{Scaling specifications inherited from the source object.}
#'   \item{aggregation_specs}{Aggregation specifications inherited from the source object.}
#'   \item{factor_cols}{Factor columns inherited from the source object.}
#'   \item{index_lookup}{Lookup tables for generated indices.}
#'   \item{inla_group_specs}{Normalized continuous grouping specifications.}
#'   \item{meta}{Shared and INLA-specific metadata.}
#'
#' @export
prepare_inla_data <- function(
    dataset = NULL,
    temporal_resolution = c("daily", "hourly"),
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    input_dir = "data/proc",
    response_col = "presence",
    create_year_index = TRUE,
    year_col = "year",
    year_index_col = "year_id",
    create_seasonal_index = TRUE,
    date_col = "date",
    seasonal_index_col = "sea_day_id",
    leap_day_method = c("feb28", "mar01"),
    group_specs = NULL,
    drop_incomplete = FALSE,
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)
  leap_day_method <- match.arg(leap_day_method)

  # ---------------------------------------------------------------------------
  # 1. Validate arguments
  # ---------------------------------------------------------------------------

  logical_args <- list(
    create_year_index = create_year_index,
    create_seasonal_index = create_seasonal_index,
    drop_incomplete = drop_incomplete,
    write = write,
    verbose = verbose
  )

  for (arg_name in names(logical_args)) {
    value <- logical_args[[arg_name]]

    if (!is.logical(value) ||
        length(value) != 1L ||
        is.na(value)) {
      stop(
        "`", arg_name, "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }

  character_args <- list(
    response_col = response_col,
    year_col = year_col,
    year_index_col = year_index_col,
    date_col = date_col,
    seasonal_index_col = seasonal_index_col
  )

  for (arg_name in names(character_args)) {
    value <- character_args[[arg_name]]

    if (!is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !nzchar(value)) {
      stop(
        "`", arg_name, "` must be a non-empty character scalar.",
        call. = FALSE
      )
    }
  }

  if (!is.null(group_specs) && !is.list(group_specs)) {
    stop(
      "`group_specs` must be NULL or a named list.",
      call. = FALSE
    )
  }

  if (!is.null(group_specs) &&
      (
        is.null(names(group_specs)) ||
        any(is.na(names(group_specs))) ||
        any(!nzchar(names(group_specs)))
      )) {
    stop(
      "`group_specs` must be a named list.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Resolve the input object or file
  # ---------------------------------------------------------------------------

  input_path <- NULL

  if (is.null(dataset)) {
    location_values <- list(
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name
    )

    if (any(vapply(location_values, is.null, logical(1)))) {
      stop(
        "When `dataset = NULL`, supply `iso3`, `admin_level`, and ",
        "`admin_name` so that the prepared model-data file can be located.\n\n",
        "Alternatively, pass the object returned by `prepare_model_data()` ",
        "or provide the path to its RDS file.",
        call. = FALSE
      )
    }

    ids <- tryCatch(
      build_location_identifiers(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name
      ),
      error = function(e) {
        stop(
          "Could not construct the location identifier: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    resolution_suffix <- if (
      identical(temporal_resolution, "hourly")
    ) {
      "_hourly"
    } else {
      "_daily"
    }

    input_path <- file.path(
      input_dir,
      sprintf(
        "model_prep_%s%s_data.rds",
        ids$slug,
        resolution_suffix
      )
    )

    if (!file.exists(input_path)) {
      stop(
        "Prepared model-data file not found:\n",
        input_path,
        "\n\nRun `prepare_model_data(..., write = TRUE)` first, ",
        "or pass its returned object directly to `prepare_inla_data()`.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Loading prepared model data from: ",
        input_path
      )
    }

    dataset <- readRDS(input_path)

  } else if (
    is.character(dataset) &&
      length(dataset) == 1L &&
      !is.na(dataset) &&
      nzchar(dataset)
  ) {
    input_path <- dataset

    if (!file.exists(input_path)) {
      stop(
        "Prepared model-data file not found:\n",
        input_path,
        "\n\nRun `prepare_model_data(..., write = TRUE)` first, ",
        "or pass its returned object directly to `prepare_inla_data()`.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Loading prepared model data from: ",
        input_path
      )
    }

    dataset <- readRDS(input_path)
  }

  # ---------------------------------------------------------------------------
  # 3. Normalize input into a preparation object
  # ---------------------------------------------------------------------------

  if (is.data.frame(dataset)) {
    obj <- structure(
      list(
        model_data = dataset,
        grid_col = NULL,
        scaling = NULL,
        scale_specs = NULL,
        aggregation_specs = NULL,
        factor_cols = NULL,
        meta = list(
          slug = "custom",
          temporal_resolution = temporal_resolution,
          source_path = input_path
        )
      ),
      class = "model_data_prep"
    )

  } else if (
    is.list(dataset) &&
      !is.null(dataset$model_data) &&
      is.data.frame(dataset$model_data)
  ) {
    obj <- dataset

  } else {
    stop(
      "`dataset` must be a data frame, a model-data preparation object, ",
      "a path to a saved preparation object, or NULL.",
      call. = FALSE
    )
  }

  df <- obj$model_data

  if (!is.list(obj$meta)) {
    obj$meta <- list()
  }

  # ---------------------------------------------------------------------------
  # 4. Convert the binary response to numeric 0/1
  # ---------------------------------------------------------------------------

  if (!response_col %in% names(df)) {
    stop(
      "Response column `",
      response_col,
      "` is missing from the prepared data.",
      call. = FALSE
    )
  }

  response <- df[[response_col]]

  if (is.logical(response)) {
    response <- as.integer(response)

  } else if (is.factor(response)) {
    response_values <- trimws(
      tolower(as.character(response))
    )

    response_map <- c(
      "0" = 0L,
      "1" = 1L,
      "false" = 0L,
      "true" = 1L,
      "no" = 0L,
      "yes" = 1L,
      "absence" = 0L,
      "presence" = 1L,
      "absent" = 0L,
      "present" = 1L
    )

    invalid_values <- setdiff(
      unique(response_values[!is.na(response_values)]),
      names(response_map)
    )

    if (length(invalid_values)) {
      stop(
        "Response column `",
        response_col,
        "` contains unsupported factor levels: ",
        paste(invalid_values, collapse = ", "),
        call. = FALSE
      )
    }

    response <- as.integer(
      unname(response_map[response_values])
    )

  } else if (is.character(response)) {
    response_values <- trimws(
      tolower(response)
    )

    response_map <- c(
      "0" = 0L,
      "1" = 1L,
      "false" = 0L,
      "true" = 1L,
      "no" = 0L,
      "yes" = 1L,
      "absence" = 0L,
      "presence" = 1L,
      "absent" = 0L,
      "present" = 1L
    )

    invalid_values <- setdiff(
      unique(response_values[!is.na(response_values)]),
      names(response_map)
    )

    if (length(invalid_values)) {
      stop(
        "Response column `",
        response_col,
        "` contains unsupported values: ",
        paste(invalid_values, collapse = ", "),
        call. = FALSE
      )
    }

    response <- as.integer(
      unname(response_map[response_values])
    )

  } else if (is.numeric(response) || is.integer(response)) {
    response <- as.integer(response)

  } else {
    stop(
      "Response column `",
      response_col,
      "` must be logical, numeric, integer, factor, or character.",
      call. = FALSE
    )
  }

  if (!all(response %in% c(0L, 1L, NA_integer_))) {
    stop(
      "Response column `",
      response_col,
      "` must contain only 0, 1, or NA.",
      call. = FALSE
    )
  }

  df[[response_col]] <- response

  if (isTRUE(verbose)) {
    message(
      "Converted `",
      response_col,
      "` to integer 0/1."
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Create the year index
  # ---------------------------------------------------------------------------

  index_lookup <- list()

  if (isTRUE(create_year_index)) {
    if (!year_col %in% names(df)) {
      stop(
        "`create_year_index = TRUE` requires column `",
        year_col,
        "`.",
        call. = FALSE
      )
    }

    year_values <- df[[year_col]]

    observed_years <- sort(
      unique(year_values[!is.na(year_values)])
    )

    if (!length(observed_years)) {
      stop(
        "Year column `",
        year_col,
        "` contains no non-missing values.",
        call. = FALSE
      )
    }

    df[[year_index_col]] <- as.integer(
      factor(
        year_values,
        levels = observed_years
      )
    )

    year_lookup <- data.frame(
      index = seq_along(observed_years),
      year = observed_years,
      stringsAsFactors = FALSE
    )

    names(year_lookup) <- c(
      year_index_col,
      year_col
    )

    index_lookup[[year_index_col]] <- year_lookup

    if (isTRUE(verbose)) {
      message(
        "Created `",
        year_index_col,
        "` with ",
        length(observed_years),
        " year levels."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Create the 365-day seasonal index
  # ---------------------------------------------------------------------------

  if (isTRUE(create_seasonal_index)) {
    if (!date_col %in% names(df)) {
      stop(
        "`create_seasonal_index = TRUE` requires column `",
        date_col,
        "`.",
        call. = FALSE
      )
    }

    original_dates <- df[[date_col]]
    date_values <- as.Date(original_dates)

    invalid_dates <- is.na(date_values) & !is.na(original_dates)

    if (any(invalid_dates)) {
      stop(
        "Column `",
        date_col,
        "` contains values that could not be converted to Date.",
        call. = FALSE
      )
    }

    day_of_year <- as.integer(
      format(date_values, "%j")
    )

    month_day <- format(
      date_values,
      "%m-%d"
    )

    year_number <- as.integer(
      format(date_values, "%Y")
    )

    leap_year <- (
      year_number %% 4L == 0L &
        year_number %% 100L != 0L
    ) |
      year_number %% 400L == 0L

    seasonal_index <- day_of_year

    after_february_29 <- (
      leap_year &
        !is.na(month_day) &
        month_day > "02-29"
    )

    seasonal_index[after_february_29] <-
      seasonal_index[after_february_29] - 1L

    february_29 <- (
      !is.na(month_day) &
        month_day == "02-29"
    )

    if (identical(leap_day_method, "feb28")) {
      seasonal_index[february_29] <- 59L
    } else {
      seasonal_index[february_29] <- 60L
    }

    invalid_seasonal_index <- (
      !is.na(seasonal_index) &
        (
          seasonal_index < 1L |
            seasonal_index > 365L
        )
    )

    if (any(invalid_seasonal_index)) {
      stop(
        "Failed to create a valid seasonal index ranging from 1 to 365.",
        call. = FALSE
      )
    }

    df[[seasonal_index_col]] <- as.integer(
      seasonal_index
    )

    seasonal_lookup <- data.frame(
      seasonal_index = 1:365
    )

    names(seasonal_lookup) <- seasonal_index_col

    index_lookup[[seasonal_index_col]] <- seasonal_lookup

    if (isTRUE(verbose)) {
      n_observed_positions <- length(
        unique(
          df[[seasonal_index_col]][
            !is.na(df[[seasonal_index_col]])
          ]
        )
      )

      message(
        "Created `",
        seasonal_index_col,
        "` as a 365-position seasonal index; ",
        n_observed_positions,
        " positions are represented."
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Create grouped continuous variables
  # ---------------------------------------------------------------------------

  normalized_group_specs <- NULL

  if (!is.null(group_specs)) {
    if (!requireNamespace("INLA", quietly = TRUE)) {
      stop(
        "Package `INLA` is required when `group_specs` is supplied.",
        call. = FALSE
      )
    }

    normalized_group_specs <- list()

    for (spec_name in names(group_specs)) {
      spec <- group_specs[[spec_name]]

      if (!is.list(spec)) {
        stop(
          "Grouping specification `",
          spec_name,
          "` must be a list.",
          call. = FALSE
        )
      }

      required_fields <- c(
        "input",
        "output",
        "n"
      )

      missing_fields <- setdiff(
        required_fields,
        names(spec)
      )

      if (length(missing_fields)) {
        stop(
          "Grouping specification `",
          spec_name,
          "` is missing: ",
          paste(missing_fields, collapse = ", "),
          call. = FALSE
        )
      }

      input_col <- spec$input
      output_col <- spec$output
      requested_groups <- spec$n
      method <- if (is.null(spec$method)) {
        "quantile"
      } else {
        spec$method
      }

      if (!is.character(input_col) ||
          length(input_col) != 1L ||
          is.na(input_col) ||
          !nzchar(input_col)) {
        stop(
          "`input` in grouping specification `",
          spec_name,
          "` must be a non-empty column name.",
          call. = FALSE
        )
      }

      if (!is.character(output_col) ||
          length(output_col) != 1L ||
          is.na(output_col) ||
          !nzchar(output_col)) {
        stop(
          "`output` in grouping specification `",
          spec_name,
          "` must be a non-empty column name.",
          call. = FALSE
        )
      }

      if (!is.numeric(requested_groups) ||
          length(requested_groups) != 1L ||
          is.na(requested_groups) ||
          requested_groups < 3) {
        stop(
          "`n` in grouping specification `",
          spec_name,
          "` must be a numeric scalar of at least 3.",
          call. = FALSE
        )
      }

      if (!is.character(method) ||
          length(method) != 1L ||
          is.na(method) ||
          !nzchar(method)) {
        stop(
          "`method` in grouping specification `",
          spec_name,
          "` must be a non-empty character scalar.",
          call. = FALSE
        )
      }

      if (!input_col %in% names(df)) {
        stop(
          "Grouping input column `",
          input_col,
          "` was not found.",
          call. = FALSE
        )
      }

      if (!is.numeric(df[[input_col]])) {
        stop(
          "Grouping input column `",
          input_col,
          "` must be numeric.",
          call. = FALSE
        )
      }

      n_unique <- length(
        unique(
          df[[input_col]][!is.na(df[[input_col]])]
        )
      )

      if (n_unique < 3L) {
        stop(
          "Grouping input column `",
          input_col,
          "` has fewer than three unique non-missing values.",
          call. = FALSE
        )
      }

      n_groups <- min(
        as.integer(requested_groups),
        n_unique
      )

      df[[output_col]] <- INLA::inla.group(
        df[[input_col]],
        n = n_groups,
        method = method
      )

      group_lookup <- df |>
        dplyr::filter(
          !is.na(.data[[input_col]]),
          !is.na(.data[[output_col]])
        ) |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(output_col)
          )
        ) |>
        dplyr::summarise(
          input_min = min(
            .data[[input_col]],
            na.rm = TRUE
          ),
          input_mean = mean(
            .data[[input_col]],
            na.rm = TRUE
          ),
          input_median = stats::median(
            .data[[input_col]],
            na.rm = TRUE
          ),
          input_max = max(
            .data[[input_col]],
            na.rm = TRUE
          ),
          n = dplyr::n(),
          .groups = "drop"
        )

      index_lookup[[output_col]] <- group_lookup

      normalized_group_specs[[spec_name]] <- list(
        input = input_col,
        output = output_col,
        n_requested = as.integer(requested_groups),
        n_used = n_groups,
        method = method
      )

      if (isTRUE(verbose)) {
        message(
          "Created `",
          output_col,
          "` from `",
          input_col,
          "` using ",
          n_groups,
          " ",
          method,
          " groups."
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # 8. Optionally remove incomplete INLA rows
  # ---------------------------------------------------------------------------

  required_inla_cols <- response_col

  if (isTRUE(create_year_index)) {
    required_inla_cols <- c(
      required_inla_cols,
      year_index_col
    )
  }

  if (isTRUE(create_seasonal_index)) {
    required_inla_cols <- c(
      required_inla_cols,
      seasonal_index_col
    )
  }

  if (!is.null(normalized_group_specs)) {
    grouped_output_cols <- vapply(
      normalized_group_specs,
      function(spec) spec$output,
      character(1)
    )

    required_inla_cols <- c(
      required_inla_cols,
      grouped_output_cols
    )
  }

  required_inla_cols <- unique(required_inla_cols)

  n_before_inla_filter <- nrow(df)

  if (isTRUE(drop_incomplete)) {
    df <- df |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(required_inla_cols),
          ~ !is.na(.x)
        )
      )
  }

  n_after_inla_filter <- nrow(df)
  n_dropped_inla <- n_before_inla_filter - n_after_inla_filter

  if (!n_after_inla_filter) {
    stop(
      "No observations remain after INLA-specific preparation.",
      call. = FALSE
    )
  }

  if (isTRUE(verbose) && isTRUE(drop_incomplete)) {
    message(
      "Dropped ",
      n_dropped_inla,
      " incomplete INLA row",
      if (n_dropped_inla == 1L) "." else "s."
    )
  }

  # ---------------------------------------------------------------------------
  # 9. Construct result
  # ---------------------------------------------------------------------------

  obj$model_data <- df
  obj$index_lookup <- index_lookup
  obj$inla_group_specs <- normalized_group_specs

  obj$meta$inla <- list(
    enabled = TRUE,
    source_path = input_path,
    response_col = response_col,
    year_col = year_col,
    year_index_col = if (isTRUE(create_year_index)) {
      year_index_col
    } else {
      NULL
    },
    create_year_index = create_year_index,
    date_col = date_col,
    seasonal_index_col = if (isTRUE(create_seasonal_index)) {
      seasonal_index_col
    } else {
      NULL
    },
    create_seasonal_index = create_seasonal_index,
    seasonal_cycle_length = if (isTRUE(create_seasonal_index)) {
      365L
    } else {
      NULL
    },
    leap_day_method = if (isTRUE(create_seasonal_index)) {
      leap_day_method
    } else {
      NULL
    },
    group_specs = normalized_group_specs,
    drop_incomplete = drop_incomplete,
    n_before_inla_filter = n_before_inla_filter,
    n_after_inla_filter = n_after_inla_filter
  )

  class(obj) <- unique(c(
    "inla_data_prep",
    "model_data_prep",
    class(obj)
  ))

  # ---------------------------------------------------------------------------
  # 10. Optional write
  # ---------------------------------------------------------------------------

  if (isTRUE(write)) {
    if (!is.character(output_dir) ||
        length(output_dir) != 1L ||
        is.na(output_dir) ||
        !nzchar(output_dir)) {
      stop(
        "`write = TRUE` requires a valid `output_dir`.",
        call. = FALSE
      )
    }

    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }

    location_slug <- obj$meta$slug

    if (is.null(location_slug) ||
        !is.character(location_slug) ||
        length(location_slug) != 1L ||
        is.na(location_slug) ||
        !nzchar(location_slug)) {
      location_slug <- "custom"
    }

    resolution_suffix <- if (
      identical(temporal_resolution, "hourly")
    ) {
      "_hourly"
    } else {
      "_daily"
    }

    output_path <- file.path(
      output_dir,
      sprintf(
        "model_prep_%s%s_inla_data.rds",
        location_slug,
        resolution_suffix
      )
    )

    obj$meta$inla$output_path <- output_path

    saveRDS(
      obj,
      output_path
    )

    if (isTRUE(verbose)) {
      message(
        "Prepared INLA data object written to ",
        output_path
      )
    }
  }

  obj
}