#' Prepare base model data for INLA
#'
#' Takes the object returned by [prepare_model_data()] and adds indices required
#' by INLA. Hourly datasets also receive a cyclic 1-to-24 hour index.
#' Continuous predictors can optionally be grouped for INLA smooths.
#'
#' @param dataset Object returned by [prepare_model_data()].
#' @param landcover_reference Reference level for `landcover_class`.
#' @param source_reference Optional reference level for `source`.
#' @param group_specs Optional named list of continuous predictors to group.
#'   Each specification must contain `input`, `output`, and `n`. The optional
#'   `method` defaults to `"quantile"`.
#' @param output_dir Directory used when `write = TRUE`.
#' @param write Whether to save the prepared object.
#' @param verbose Whether to emit progress messages.
#'
#' @return An object of class `inla_data_prep`.
#'
#' @export
prepare_inla_data <- function(
    dataset,
    landcover_reference = "Built-up",
    source_reference = NULL,
    group_specs = NULL,
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  if (!inherits(dataset, "brms_data_prep")) {
    stop(
      "`dataset` must be returned by `prepare_model_data()`.",
      call. = FALSE
    )
  }

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "Package `INLA` is required.",
      call. = FALSE
    )
  }

  if (!is.null(group_specs) && !is.list(group_specs)) {
    stop(
      "`group_specs` must be `NULL` or a named list.",
      call. = FALSE
    )
  }

  df <- dataset$model_data

  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop(
      "`dataset$model_data` must be a non-empty data frame.",
      call. = FALSE
    )
  }

  temporal_resolution <- dataset$meta$temporal_resolution

  if (
    is.null(temporal_resolution) ||
      !temporal_resolution %in% c("daily", "hourly")
  ) {
    temporal_resolution <- "daily"
  }

  required_cols <- c(
    "presence",
    "landcover_class",
    "source",
    "year",
    "date"
  )

  if (identical(temporal_resolution, "hourly")) {
    required_cols <- c(
      required_cols,
      "hour"
    )
  }

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Binary response.
  if (
    is.logical(df$presence) ||
      is.numeric(df$presence) ||
      is.integer(df$presence)
  ) {
    df$presence <- as.integer(
      df$presence
    )
  } else {
    stop(
      "`presence` must be logical or numeric.",
      call. = FALSE
    )
  }

  if (!all(
    df$presence %in% c(
      0L,
      1L,
      NA_integer_
    )
  )) {
    stop(
      "`presence` must contain only 0, 1, or NA.",
      call. = FALSE
    )
  }

  # Factors.
  df$landcover_class <- factor(
    df$landcover_class
  )

  if (!landcover_reference %in%
      levels(df$landcover_class)) {
    stop(
      "Land-cover reference level `",
      landcover_reference,
      "` was not found.",
      call. = FALSE
    )
  }

  df$landcover_class <- stats::relevel(
    df$landcover_class,
    ref = landcover_reference
  )

  df$source <- factor(
    df$source
  )

  if (!is.null(source_reference)) {
    if (!source_reference %in% levels(df$source)) {
      stop(
        "Source reference level `",
        source_reference,
        "` was not found.",
        call. = FALSE
      )
    }

    df$source <- stats::relevel(
      df$source,
      ref = source_reference
    )
  }

  # Consecutive year index.
  df$year_id <- match(
    df$year,
    sort(unique(df$year))
  )

  # Consistent 365-day seasonal index.
  df$date <- as.Date(
    df$date
  )

  if (anyNA(df$date)) {
    stop(
      "`date` contains missing or invalid dates.",
      call. = FALSE
    )
  }

  day_of_year <- as.integer(
    format(df$date, "%j")
  )

  month_day <- format(
    df$date,
    "%m-%d"
  )

  year_number <- as.integer(
    format(df$date, "%Y")
  )

  is_leap_year <- (
    year_number %% 4L == 0L &
      year_number %% 100L != 0L
  ) |
    year_number %% 400L == 0L

  df$sea_day_id <- day_of_year -
    as.integer(
      is_leap_year &
        month_day > "02-29"
    )

  df$sea_day_id[
    month_day == "02-29"
  ] <- 59L

  if (!all(
    df$sea_day_id >= 1L &
      df$sea_day_id <= 365L
  )) {
    stop(
      "Failed to create a seasonal index between 1 and 365.",
      call. = FALSE
    )
  }

  # Create a 1-to-24 cyclic index for hourly data.
  hour_index <- NULL

  if (identical(temporal_resolution, "hourly")) {
    hour_value <- suppressWarnings(
      as.integer(
        as.character(df$hour)
      )
    )

    if (
      anyNA(hour_value) ||
        !all(hour_value %in% 0:23)
    ) {
      stop(
        "For hourly data, `hour` must contain integers from 0 to 23.",
        call. = FALSE
      )
    }

    df$hour_id <- hour_value + 1L
    hour_index <- "hour_id"
  }

  # Optionally group continuous predictors for INLA smooths.
  grouped_predictors <- list()

  if (!is.null(group_specs)) {
    if (
      is.null(names(group_specs)) ||
        any(!nzchar(names(group_specs)))
    ) {
      stop(
        "`group_specs` must be a named list.",
        call. = FALSE
      )
    }

    for (spec_name in names(group_specs)) {
      spec <- group_specs[[spec_name]]

      if (!is.list(spec)) {
        stop(
          "Each entry in `group_specs` must be a list.",
          call. = FALSE
        )
      }

      required_spec_fields <- c(
        "input",
        "output",
        "n"
      )

      missing_fields <- setdiff(
        required_spec_fields,
        names(spec)
      )

      if (length(missing_fields) > 0L) {
        stop(
          "Group specification `",
          spec_name,
          "` is missing: ",
          paste(missing_fields, collapse = ", "),
          call. = FALSE
        )
      }

      input <- spec$input
      output <- spec$output
      n_groups <- as.integer(spec$n)

      method <- if (is.null(spec$method)) {
        "quantile"
      } else {
        spec$method
      }

      if (!input %in% names(df)) {
        stop(
          "Grouping input `",
          input,
          "` was not found.",
          call. = FALSE
        )
      }

      if (!is.numeric(df[[input]])) {
        stop(
          "Grouping input `",
          input,
          "` must be numeric.",
          call. = FALSE
        )
      }

      if (
        length(n_groups) != 1L ||
          is.na(n_groups) ||
          n_groups < 3L
      ) {
        stop(
          "The number of groups for `",
          spec_name,
          "` must be at least 3.",
          call. = FALSE
        )
      }

      df[[output]] <- INLA::inla.group(
        df[[input]],
        n = n_groups,
        method = method
      )

      grouped_predictors[[spec_name]] <- list(
        input = input,
        output = output,
        n = n_groups,
        method = method
      )
    }
  }

  obj <- dataset
  obj$model_data <- df

  if (is.null(obj$meta)) {
    obj$meta <- list()
  }

  obj$meta$inla <- list(
    temporal_resolution = temporal_resolution,
    landcover_reference = landcover_reference,
    source_reference = source_reference,
    seasonal_index = "sea_day_id",
    seasonal_cycle_length = 365L,
    hour_index = hour_index,
    hour_cycle_length = if (
      identical(temporal_resolution, "hourly")
    ) {
      24L
    } else {
      NULL
    },
    year_index = "year_id",
    grouped_predictors = grouped_predictors
  )

  class(obj) <- unique(c(
    "inla_data_prep",
    class(dataset)
  ))

  if (isTRUE(write)) {
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }

    slug <- obj$meta$slug

    if (is.null(slug) || !nzchar(slug)) {
      slug <- "custom"
    }

    output_path <- file.path(
      output_dir,
      sprintf(
        "model_prep_%s_%s_inla_data.rds",
        slug,
        temporal_resolution
      )
    )

    obj$meta$inla$output_path <- output_path

    saveRDS(
      obj,
      output_path
    )

    if (isTRUE(verbose)) {
      message(
        "Prepared INLA data written to ",
        output_path
      )
    }
  }

  if (isTRUE(verbose)) {
    message(
      "Prepared ",
      nrow(df),
      " observations for INLA."
    )
  }

  obj
}