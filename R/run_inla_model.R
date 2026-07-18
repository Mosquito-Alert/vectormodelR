#' Fit a general model with INLA
#'
#' Fits a user-supplied INLA formula to data prepared by
#' [prepare_inla_data()]. The runner handles input resolution, basic formula and
#' response validation, common Bernoulli defaults, optional output, and forwards
#' additional arguments to [INLA::inla()]. Model-specific latent effects remain
#' explicit in `formula`.
#'
#' @param dataset An `inla_data_prep` object, a model-data preparation object, a
#'   data frame, a path to a prepared RDS file, or `NULL`. When `NULL`, the saved
#'   INLA preparation file is located from the location arguments.
#' @param formula A formula or single character string accepted by
#'   [INLA::inla()].
#' @param family Character vector passed to [INLA::inla()]. Defaults to
#'   `"binomial"`.
#' @param Ntrials Optional binomial trial counts. For a binomial model with a
#'   binary response, `NULL` creates one trial per observation.
#' @param temporal_resolution Character. Either `"daily"` or `"hourly"`.
#' @param iso3,admin_level,admin_name Optional location identifiers used when
#'   `dataset = NULL`.
#' @param input_dir Directory containing prepared INLA data.
#' @param control.family,control.compute,control.predictor Control lists passed
#'   to [INLA::inla()]. Defaults request a logit link, DIC, WAIC, CPO, posterior
#'   configuration, and fitted-value computation.
#' @param inla_args Optional named list of additional arguments passed to
#'   [INLA::inla()], such as `control.inla` or `verbose`.
#' @param write_output Logical. Save the fitted model when `TRUE`.
#' @param output_path Output directory or RDS filename. Defaults to
#'   `"data/proc"`.
#' @param verbose Logical. Emit informative messages.
#'
#' @return The fitted `inla` object. The source data path, location slug, formula,
#'   and output path are attached as attributes when available.
#'
#' @examples
#' \dontrun{
#' inla_data <- prepare_inla_data(
#'   brms_dataset_daily,
#'   group_specs = list(
#'     max_temperature = list(
#'       input = "maxTM_z",
#'       output = "maxTM_group",
#'       n = 30,
#'       method = "quantile"
#'     )
#'   )
#' )
#'
#' occupancy_formula <- presence ~
#'   f(sea_day_id, model = "rw2", cyclic = TRUE, values = 1:365,
#'     constr = TRUE, scale.model = TRUE) +
#'   f(maxTM_group, model = "rw2", constr = TRUE, scale.model = TRUE) +
#'   ppt_3d_lag7_z + ndvi_z + elev_z + pop_z +
#'   landcover_class + source +
#'   f(year_id, model = "iid", constr = TRUE)
#'
#' fit <- run_inla_model(inla_data, occupancy_formula)
#' }
#'
#' @export
run_inla_model <- function(
    dataset = NULL,
    formula,
    family = "binomial",
    Ntrials = NULL,
    temporal_resolution = c("daily", "hourly"),
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    input_dir = "data/proc",
    control.family = list(link = "logit"),
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      config = TRUE
    ),
    control.predictor = list(
      compute = TRUE,
      link = 1
    ),
    inla_args = list(),
    write_output = TRUE,
    output_path = "data/proc",
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package `INLA` must be installed to fit an INLA model.", call. = FALSE)
  }

  if (missing(formula)) {
    stop("`formula` is required.", call. = FALSE)
  }

  if (is.character(formula)) {
    if (length(formula) != 1L || is.na(formula) || !nzchar(formula)) {
      stop("`formula` must be a formula or one non-empty string.", call. = FALSE)
    }
    formula <- stats::as.formula(formula, env = parent.frame())
  } else if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula or one non-empty string.", call. = FALSE)
  }

  if (!is.character(family) || !length(family) || anyNA(family) ||
      any(!nzchar(family))) {
    stop("`family` must contain one or more non-empty family names.", call. = FALSE)
  }

  logical_args <- list(
    write_output = write_output,
    verbose = verbose
  )
  for (arg_name in names(logical_args)) {
    value <- logical_args[[arg_name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", arg_name, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  control_args <- list(
    control.family = control.family,
    control.compute = control.compute,
    control.predictor = control.predictor,
    inla_args = inla_args
  )
  invalid_controls <- names(control_args)[
    !vapply(control_args, is.list, logical(1))
  ]
  if (length(invalid_controls)) {
    stop(
      "`", invalid_controls[[1L]], "` must be a list.",
      call. = FALSE
    )
  }
  if (length(inla_args) &&
      (is.null(names(inla_args)) || any(!nzchar(names(inla_args))))) {
    stop("`inla_args` must be a named list.", call. = FALSE)
  }

  input_path <- NULL
  if (is.null(dataset)) {
    location_values <- list(iso3, admin_level, admin_name)
    if (any(vapply(location_values, is.null, logical(1)))) {
      stop(
        "When `dataset = NULL`, supply `iso3`, `admin_level`, and ",
        "`admin_name`, or pass prepared data directly.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    input_path <- file.path(
      input_dir,
      sprintf(
        "model_prep_%s_%s_inla_data.rds",
        ids$slug,
        temporal_resolution
      )
    )
    if (!file.exists(input_path)) {
      stop(
        "Prepared INLA data not found at: ", input_path,
        "\nRun `prepare_inla_data(..., write = TRUE)` first.",
        call. = FALSE
      )
    }
    dataset <- readRDS(input_path)
  } else if (is.character(dataset) && length(dataset) == 1L &&
             !is.na(dataset) && nzchar(dataset)) {
    input_path <- dataset
    if (!file.exists(input_path)) {
      stop("Prepared data file not found: ", input_path, call. = FALSE)
    }
    dataset <- readRDS(input_path)
  }

  prep_obj <- NULL
  if (is.data.frame(dataset)) {
    model_data <- dataset
  } else if (is.list(dataset) && is.data.frame(dataset$model_data)) {
    prep_obj <- dataset
    model_data <- dataset$model_data
  } else {
    stop(
      "`dataset` must be a data frame, a preparation object, an RDS path, or NULL.",
      call. = FALSE
    )
  }

  if (!nrow(model_data)) {
    stop("No observations are available in the model data.", call. = FALSE)
  }

  formula_variables <- all.vars(formula)
  missing_variables <- setdiff(formula_variables, names(model_data))
  if (length(missing_variables)) {
    stop(
      "Variables used in `formula` are missing from the model data: ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  response_name <- all.vars(formula[[2L]])
  if (length(response_name) != 1L) {
    stop("`formula` must have one response column on its left-hand side.", call. = FALSE)
  }
  response <- model_data[[response_name]]

  if (identical(family, "binomial")) {
    if (!is.numeric(response) && !is.integer(response)) {
      stop(
        "Binomial response `", response_name,
        "` must be numeric. Run `prepare_inla_data()` first.",
        call. = FALSE
      )
    }

    invalid_response <- !is.na(response) &
      (response < 0 | response != floor(response))
    if (any(invalid_response)) {
      stop(
        "Binomial response `", response_name,
        "` must contain non-negative integer values or NA.",
        call. = FALSE
      )
    }

    if (is.null(Ntrials)) {
      if (any(response > 1, na.rm = TRUE)) {
        stop(
          "Supply `Ntrials` when a binomial response contains counts above 1.",
          call. = FALSE
        )
      }
      Ntrials <- rep.int(1L, nrow(model_data))
    }
  }

  if (!is.null(Ntrials)) {
    if (!is.numeric(Ntrials) || anyNA(Ntrials) ||
        any(Ntrials < 1) || any(Ntrials != floor(Ntrials)) ||
        !length(Ntrials) %in% c(1L, nrow(model_data))) {
      stop(
        "`Ntrials` must be a positive integer scalar or have one value per row.",
        call. = FALSE
      )
    }
    Ntrials <- as.integer(Ntrials)
  }

  reserved_args <- c(
    "formula", "family", "data", "Ntrials", "control.family",
    "control.compute", "control.predictor"
  )
  duplicate_args <- intersect(names(inla_args), reserved_args)
  if (length(duplicate_args)) {
    stop(
      "Pass `", duplicate_args[[1L]], "` through its dedicated argument, ",
      "not through `inla_args`.",
      call. = FALSE
    )
  }

  fit_args <- c(
    list(
      formula = formula,
      family = family,
      data = model_data,
      control.family = control.family,
      control.compute = control.compute,
      control.predictor = control.predictor
    ),
    if (!is.null(Ntrials)) list(Ntrials = Ntrials),
    inla_args
  )

  if (isTRUE(verbose)) {
    message(
      "Fitting INLA ", paste(family, collapse = ", "),
      " model using ", nrow(model_data), " observations."
    )
  }

  model_fit <- do.call(INLA::inla, fit_args)

  location_slug <- NULL
  if (!is.null(prep_obj) && is.list(prep_obj$meta)) {
    location_slug <- prep_obj$meta$slug
  }
  if ((is.null(location_slug) || !nzchar(location_slug)) && !is.null(iso3)) {
    location_slug <- build_location_identifiers(
      iso3, admin_level, admin_name
    )$slug
  }

  attr(model_fit, "model_formula") <- formula
  if (!is.null(input_path)) {
    attr(model_fit, "source_dataset") <- input_path
  }
  if (!is.null(location_slug) && length(location_slug) == 1L &&
      !is.na(location_slug) && nzchar(location_slug)) {
    attr(model_fit, "location_slug") <- location_slug
  }

  if (isTRUE(write_output)) {
    if (!is.character(output_path) || length(output_path) != 1L ||
        is.na(output_path) || !nzchar(output_path)) {
      stop("`write_output = TRUE` requires a valid `output_path`.", call. = FALSE)
    }

    is_rds_file <- grepl("\\.[Rr][Dd][Ss]$", output_path)
    if (is_rds_file) {
      final_output_path <- output_path
    } else {
      slug <- if (!is.null(location_slug) && nzchar(location_slug)) {
        location_slug
      } else {
        "custom"
      }
      final_output_path <- file.path(
        output_path,
        sprintf("model_%s_inla_%s.rds", slug, temporal_resolution)
      )
    }

    attr(model_fit, "output_path") <- final_output_path
    dir.create(dirname(final_output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(model_fit, final_output_path)

    if (isTRUE(verbose)) {
      message("INLA model saved to ", final_output_path)
    }
  }

  model_fit
}
