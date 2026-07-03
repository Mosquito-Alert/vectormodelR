#' Fit a standard (non-spatial) Mosquito Alert occupancy model with INLA
#'
#' Fits an occupancy model on the prepared Mosquito Alert modelling data using
#' R-INLA. This is the INLA counterpart to [run_brms_model()]: it consumes the
#' same prepared data produced by [prepare_model_data()] and does NOT include a
#' spatial BYM2 term. For the spatial version, use [run_inla_bym2_model()].
#'
#' @param dataset A prepared modelling object (e.g. a `brms_data_prep` created
#'   by [prepare_model_data()]), a data frame, a path to a prepared RDS file, or
#'   `NULL`. If `NULL`, `iso3`, `admin_level`, and `admin_name` are used to
#'   locate a prepared object in `input_dir`.
#' @param formula A formula or character string giving the fixed and random
#'   effects structure. Required.
#' @param cellsize_m Numeric cell size in metres, used only for prepared-file
#'   lookup. Defaults to `800`.
#' @param temporal_resolution Character. Either `"daily"` or `"hourly"`; used
#'   for prepared-file lookup when `dataset = NULL`.
#' @param family Character INLA likelihood. Defaults to `"binomial"` (the INLA
#'   equivalent of a Bernoulli occupancy likelihood).
#' @param Ntrials Number of binomial trials, passed to `INLA::inla()` when
#'   `family = "binomial"`. Defaults to `1`.
#' @param control_compute List passed to `INLA::inla(control.compute = ...)`.
#' @param control_predictor List passed to `INLA::inla(control.predictor = ...)`.
#' @param control_inla Optional list passed to `INLA::inla(control.inla = ...)`.
#' @param iso3,admin_level,admin_name Optional location identifiers used for
#'   prepared-file lookup when `dataset = NULL`.
#' @param write_output Logical. Whether to save the fitted model to disk.
#' @param output_path Directory or file path for saved model output.
#' @param input_dir Directory used when automatically locating prepared data.
#' @param verbose Logical. Emit messages when `TRUE`.
#'
#' @return The fitted `inla` object, with attributes for the model data,
#'   temporal resolution, formula text, source dataset, location slug, and
#'   output path.
#'
#' @export
run_inla_model <- function(
    dataset = NULL,
    formula,
    cellsize_m = 800,
    temporal_resolution = c("daily", "hourly"),
    family = "binomial",
    Ntrials = 1,
    control_compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
    control_predictor = list(compute = TRUE),
    control_inla = NULL,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    write_output = TRUE,
    output_path = "data/proc",
    input_dir = "data/proc",
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)

  # ---------------------------------------------------------------------------
  # 1. Dependencies and argument checks
  # ---------------------------------------------------------------------------

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "Package 'INLA' must be installed. See ",
      "https://www.r-inla.org/download-install.",
      call. = FALSE
    )
  }

  if (missing(formula)) {
    stop("`formula` is required and must be supplied.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 2. Load prepared modelling data
  # ---------------------------------------------------------------------------

  loaded <- .inla_load_model_data(
    dataset = dataset,
    temporal_resolution = temporal_resolution,
    iso3 = iso3,
    admin_level = admin_level,
    admin_name = admin_name,
    input_dir = input_dir,
    verbose = verbose
  )

  model_data <- loaded$model_data
  location_slug <- loaded$location_slug
  dataset_path <- loaded$dataset_path

  if (!nrow(model_data)) {
    stop("No observations available in the model data.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 3. Build formula
  # ---------------------------------------------------------------------------

  formula_text <- .inla_formula_text(formula)
  model_formula <- stats::as.formula(formula_text)
  environment(model_formula) <- new.env(parent = parent.frame())

  response_var <- all.vars(model_formula)[1L]

  if (response_var %in% names(model_data) &&
      is.logical(model_data[[response_var]])) {
    model_data[[response_var]] <- as.integer(model_data[[response_var]])
  }

  missing_vars <- setdiff(all.vars(model_formula), names(model_data))

  if (length(missing_vars)) {
    stop(
      "The formula references variables not found in `model_data`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Fit model
  # ---------------------------------------------------------------------------

  if (isTRUE(verbose)) {
    message("Fitting standard INLA model.")
    message("Temporal resolution: ", temporal_resolution)
    message("Observations used: ", nrow(model_data))
    message("Formula: ", formula_text)
  }

  inla_args <- list(
    formula = model_formula,
    family = family,
    data = model_data,
    control.compute = control_compute,
    control.predictor = control_predictor,
    verbose = isTRUE(verbose)
  )

  if (identical(family, "binomial")) {
    inla_args$Ntrials <- Ntrials
  }

  if (!is.null(control_inla)) {
    inla_args$control.inla <- control_inla
  }

  model_fit <- do.call(INLA::inla, inla_args)

  # ---------------------------------------------------------------------------
  # 5. Attach attributes
  # ---------------------------------------------------------------------------

  attr(model_fit, "model_data") <- model_data
  attr(model_fit, "temporal_resolution") <- temporal_resolution
  attr(model_fit, "formula_text") <- formula_text

  if (!is.null(dataset_path)) {
    attr(model_fit, "source_dataset") <- dataset_path
  }

  if (!is.null(location_slug) && nzchar(location_slug)) {
    attr(model_fit, "location_slug") <- location_slug
  }

  # ---------------------------------------------------------------------------
  # 6. Optional write output
  # ---------------------------------------------------------------------------

  if (isTRUE(write_output)) {
    output_file <- .inla_write_output(
      fit = model_fit,
      output_path = output_path,
      dataset_path = dataset_path,
      location_slug = location_slug,
      temporal_resolution = temporal_resolution,
      model_kind = "inla",
      verbose = verbose
    )

    attr(model_fit, "output_path") <- output_file
  }

  model_fit
}
