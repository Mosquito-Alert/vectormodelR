#' Prepare base model data for INLA
#'
#' Takes the object returned by [prepare_model_data()] and adds the variables
#' required by the standard INLA occupancy model:
#'
#' - integer binary response;
#' - factor reference levels;
#' - integer year index;
#' - consistent 365-day seasonal index;
#' - grouped maximum temperature.
#'
#' Filtering, aggregation, and scaling are not repeated.
#'
#' @param dataset Object returned by [prepare_model_data()].
#' @param landcover_reference Reference level for `landcover_class`.
#' @param source_reference Optional reference level for `source`.
#' @param temperature_groups Number of groups used for `maxTM_z`.
#' @param output_dir Directory used when `write = TRUE`.
#' @param write Whether to save the prepared object.
#' @param verbose Whether to emit progress messages.
#'
#' @return An object of class `inla_data_prep` containing the original
#'   preparation information and INLA-ready `model_data`.
#'
#' @export
prepare_inla_data <- function(
    dataset,
    landcover_reference = "Built-up",
    source_reference = NULL,
    temperature_groups = 30,
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  # ---------------------------------------------------------------------------
  # 1. Check input
  # ---------------------------------------------------------------------------

  if (!inherits(dataset, "brms_data_prep")) {
    stop(
      "`dataset` must be an object returned by `prepare_model_data()`.",
      call. = FALSE
    )
  }

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "Package `INLA` is required.",
      call. = FALSE
    )
  }

  if (!is.numeric(temperature_groups) ||
      length(temperature_groups) != 1L ||
      is.na(temperature_groups) ||
      temperature_groups < 3) {
    stop(
      "`temperature_groups` must be an integer of at least 3.",
      call. = FALSE
    )
  }

  temperature_groups <- as.integer(
    temperature_groups
  )

  df <- dataset$model_data

  if (!is.data.frame(df) || !nrow(df)) {
    stop(
      "`dataset$model_data` must be a non-empty data frame.",
      call. = FALSE
    )
  }

  required_cols <- c(
    "presence",
    "landcover_class",
    "source",
    "year",
    "date",
    "maxTM_z"
  )

  missing_cols <- setdiff(
    required_cols,
    names(df)
  )

  if (length(missing_cols)) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Binary response
  # ---------------------------------------------------------------------------

  if (is.logical(df$presence)) {
    df$presence <- as.integer(
      df$presence
    )
  } else if (is.numeric(df$presence) ||
             is.integer(df$presence)) {
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
    df$presence %in% c(0L, 1L, NA_integer_)
  )) {
    stop(
      "`presence` must contain only 0, 1, or NA.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Factors
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # 4. Year index
  # ---------------------------------------------------------------------------

  df$year_id <- as.integer(
    factor(df$year)
  )

  # ---------------------------------------------------------------------------
  # 5. Seasonal index
  # ---------------------------------------------------------------------------

  df$date <- as.Date(
    df$date
  )

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
      df$sea_day_id <= 365L,
    na.rm = TRUE
  )) {
    stop(
      "Failed to create a seasonal index between 1 and 365.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Group maximum temperature
  # ---------------------------------------------------------------------------

  if (!is.numeric(df$maxTM_z)) {
    stop(
      "`maxTM_z` must be numeric.",
      call. = FALSE
    )
  }

  df$maxTM_group <- INLA::inla.group(
    df$maxTM_z,
    n = temperature_groups,
    method = "quantile"
  )

  # ---------------------------------------------------------------------------
  # 7. Return object
  # ---------------------------------------------------------------------------

  obj <- dataset
  obj$model_data <- df

  if (is.null(obj$meta)) {
    obj$meta <- list()
  }

  obj$meta$inla <- list(
    landcover_reference = landcover_reference,
    source_reference = source_reference,
    seasonal_index = "sea_day_id",
    seasonal_cycle_length = 365L,
    year_index = "year_id",
    temperature_input = "maxTM_z",
    temperature_group = "maxTM_group",
    temperature_groups = temperature_groups
  )

  class(obj) <- unique(c(
    "inla_data_prep",
    class(dataset)
  ))

  # ---------------------------------------------------------------------------
  # 8. Optionally write output
  # ---------------------------------------------------------------------------

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

    temporal_resolution <- obj$meta$temporal_resolution

    if (is.null(temporal_resolution) ||
        !nzchar(temporal_resolution)) {
      temporal_resolution <- "daily"
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