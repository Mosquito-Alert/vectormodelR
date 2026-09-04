#' Fit a BYM2 model with INLA
#'
#' Fits a complete user-supplied INLA formula using data prepared by
#' [prepare_inla_bym2_data()]. The formula must include the BYM2 spatial term.
#'
#' The prepared spatial graph is available inside the formula as
#' `spatial_graph`. When the formula contains `space_time_id`, the prepared
#' Knorr-Held Type IV objects are made available as `R_int`, `A_kh`, and `e_kh`.
#'
#' Objects defined in the environment where the formula was created, including
#' hyperprior specifications, are also available when fitting the model.
#'
#' @param dataset An `inla_bym2_data_prep` object, a path to a saved preparation
#'   object, or `NULL`.
#' @param formula Complete INLA formula, including the BYM2 spatial term.
#' @param family INLA likelihood family. Defaults to `"binomial"`.
#' @param Ntrials Optional binomial trial counts.
#' @param temporal_resolution Either `"daily"` or `"hourly"`.
#' @param iso3,admin_level,admin_name Location identifiers used when
#'   `dataset = NULL`.
#' @param input_dir Directory containing prepared INLA BYM2 data.
#' @param control.family List passed to `INLA::inla(control.family = ...)`.
#' @param control.compute List passed to `INLA::inla(control.compute = ...)`.
#' @param control.predictor List passed to
#'   `INLA::inla(control.predictor = ...)`.
#' @param inla_args Additional named arguments passed to [INLA::inla()].
#' @param write_output Whether to save the fitted model.
#' @param output_path Output directory or RDS filename.
#' @param verbose Whether to emit progress messages.
#'
#' @return A fitted `inla` object.
#'
#' @export
run_inla_bym2_model <- function(
    dataset = NULL,
    formula,
    family = "binomial",
    Ntrials = NULL,
    temporal_resolution = c("daily", "hourly"),
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    input_dir = "data/proc",
    control.family = list(
      link = "logit"
    ),
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
  temporal_resolution <- match.arg(
    temporal_resolution
  )

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "Package `INLA` must be installed.",
      call. = FALSE
    )
  }

  if (missing(formula)) {
    stop(
      "`formula` is required.",
      call. = FALSE
    )
  }

  if (inherits(formula, "formula")) {
    formula_text <- paste(
      deparse(formula),
      collapse = " "
    )

    formula_parent_env <- environment(formula)

    if (is.null(formula_parent_env)) {
      formula_parent_env <- parent.frame()
    }
  } else if (
    is.character(formula) &&
      length(formula) == 1L &&
      !is.na(formula) &&
      nzchar(formula)
  ) {
    formula_text <- formula
    formula_parent_env <- parent.frame()
  } else {
    stop(
      "`formula` must be a formula or one character string.",
      call. = FALSE
    )
  }

  if (!is.list(inla_args)) {
    stop(
      "`inla_args` must be a named list.",
      call. = FALSE
    )
  }

  if (
    length(inla_args) > 0L &&
      (
        is.null(names(inla_args)) ||
          any(!nzchar(names(inla_args)))
      )
  ) {
    stop(
      "`inla_args` must be a named list.",
      call. = FALSE
    )
  }

  # Load prepared data.
  input_path <- NULL

  if (is.null(dataset)) {
    if (
      is.null(iso3) ||
        is.null(admin_level) ||
        is.null(admin_name)
    ) {
      stop(
        "When `dataset = NULL`, supply `iso3`, `admin_level`, and ",
        "`admin_name`.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(
      iso3,
      admin_level,
      admin_name
    )

    input_path <- file.path(
      input_dir,
      sprintf(
        "model_prep_%s_%s_inla_bym2_data.rds",
        ids$slug,
        temporal_resolution
      )
    )

    if (!file.exists(input_path)) {
      stop(
        "Prepared INLA BYM2 data not found at: ",
        input_path,
        "\nRun `prepare_inla_bym2_data(..., write = TRUE)` first.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Loading prepared INLA BYM2 data from: ",
        input_path
      )
    }

    dataset <- readRDS(
      input_path
    )
  } else if (
    is.character(dataset) &&
      length(dataset) == 1L
  ) {
    input_path <- dataset

    if (!file.exists(input_path)) {
      stop(
        "Prepared INLA BYM2 data not found at: ",
        input_path,
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Loading prepared INLA BYM2 data from: ",
        input_path
      )
    }

    dataset <- readRDS(
      input_path
    )
  }

  if (!inherits(dataset, "inla_bym2_data_prep")) {
    stop(
      "`dataset` must be returned by `prepare_inla_bym2_data()` ",
      "or be a path to one.",
      call. = FALSE
    )
  }

  model_data <- dataset$model_data
  spatial_graph <- dataset$spatial_graph

  if (
    !is.data.frame(model_data) ||
      nrow(model_data) == 0L
  ) {
    stop(
      "The prepared object does not contain valid model data.",
      call. = FALSE
    )
  }

  if (!"spatial_id" %in% names(model_data)) {
    stop(
      "The prepared data do not contain `spatial_id`.",
      call. = FALSE
    )
  }

  if (is.null(spatial_graph)) {
    stop(
      "The prepared object does not contain an INLA spatial graph.",
      call. = FALSE
    )
  }

  prepared_resolution <- dataset$meta$temporal_resolution

  if (!is.null(prepared_resolution)) {
    temporal_resolution <- prepared_resolution
  }

  # Require the complete BYM2 term in the supplied formula.
  has_bym2_term <- grepl(
    "model\\s*=\\s*[\"']bym2[\"']",
    formula_text
  )

  if (!has_bym2_term) {
    stop(
      "`formula` must include a BYM2 term, for example: ",
      "`f(spatial_id, model = \"bym2\", graph = spatial_graph, ...)`.",
      call. = FALSE
    )
  }

  uses_space_time <- grepl(
    "\\bspace_time_id\\b",
    formula_text
  )

  if (uses_space_time) {
    if (!"space_time_id" %in% names(model_data)) {
      stop(
        "The formula uses `space_time_id`, but it is missing from ",
        "`model_data`.",
        call. = FALSE
      )
    }

    if (
      is.null(dataset$space_time_precision) ||
        is.null(dataset$space_time_constraints) ||
        is.null(dataset$space_time_constraint_values)
    ) {
      stop(
        "The formula uses `space_time_id`, but the prepared Type IV ",
        "objects are missing. Run `prepare_inla_bym2_data()` again.",
        call. = FALSE
      )
    }
  }

  # Make prepared and externally defined objects available inside the formula.
  formula_env <- new.env(
    parent = formula_parent_env
  )

  formula_env$model.frame <- stats::model.frame
  formula_env$f <- INLA::f
  formula_env$spatial_graph <- spatial_graph

  if (uses_space_time) {
    formula_env$R_int <-
      dataset$space_time_precision
    formula_env$A_kh <-
      dataset$space_time_constraints
    formula_env$e_kh <-
      dataset$space_time_constraint_values
  }

  model_formula <- stats::as.formula(
    formula_text,
    env = formula_env
  )

  # Validate variables against the model data and formula environment.
  formula_variables <- all.vars(
    model_formula
  )

  available_externally <- vapply(
    formula_variables,
    exists,
    logical(1L),
    envir = formula_env,
    inherits = TRUE
  )

  missing_variables <- formula_variables[
    !formula_variables %in% names(model_data) &
      !available_externally
  ]

  if (length(missing_variables) > 0L) {
    stop(
      "Formula variables missing from model data or formula environment: ",
      paste(
        missing_variables,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  response_name <- all.vars(
    model_formula[[2L]]
  )

  if (length(response_name) != 1L) {
    stop(
      "`formula` must have one response variable.",
      call. = FALSE
    )
  }

  response <- model_data[[response_name]]

  if (identical(family, "binomial")) {
    if (
      !is.numeric(response) &&
        !is.integer(response)
    ) {
      stop(
        "Binomial response `",
        response_name,
        "` must be numeric.",
        call. = FALSE
      )
    }

    if (is.null(Ntrials)) {
      if (!all(
        response %in% c(
          0L,
          1L,
          NA_integer_
        )
      )) {
        stop(
          "With `Ntrials = NULL`, the response must contain only ",
          "0, 1, or NA.",
          call. = FALSE
        )
      }

      Ntrials <- rep.int(
        1L,
        nrow(model_data)
      )
    }
  }

  fit_args <- list(
    formula = model_formula,
    family = family,
    data = model_data,
    control.family = control.family,
    control.compute = control.compute,
    control.predictor = control.predictor
  )

  if (identical(family, "binomial")) {
    fit_args$Ntrials <- Ntrials
  }

  fit_args <- c(
    fit_args,
    inla_args
  )

  if (isTRUE(verbose)) {
    if (uses_space_time) {
      message(
        "Using prepared Knorr-Held Type IV space-time interaction."
      )
    }

    message(
      "Fitting INLA BYM2 ",
      family,
      " model using ",
      nrow(model_data),
      " observations and ",
      dataset$meta$n_spatial_cells,
      " spatial cells."
    )
  }

  model_fit <- do.call(
    INLA::inla,
    fit_args
  )

  location_slug <- dataset$meta$slug

  attr(model_fit, "formula_text") <- formula_text
  attr(model_fit, "temporal_resolution") <-
    temporal_resolution
  attr(model_fit, "spatial_model") <- if (uses_space_time) {
    "BYM2 + Type IV"
  } else {
    "BYM2"
  }

  if (!is.null(input_path)) {
    attr(model_fit, "source_dataset") <-
      input_path
  }

  if (
    !is.null(location_slug) &&
      nzchar(location_slug)
  ) {
    attr(model_fit, "location_slug") <-
      location_slug
  }

  if (isTRUE(write_output)) {
    path_is_file <- grepl(
      "\\.[Rr][Dd][Ss]$",
      output_path
    )

    if (path_is_file) {
      final_output_path <- output_path
    } else {
      slug <- if (
        !is.null(location_slug) &&
          nzchar(location_slug)
      ) {
        location_slug
      } else {
        "custom"
      }

      model_name <- if (uses_space_time) {
        "inla_bym2_type4"
      } else {
        "inla_bym2"
      }

      final_output_path <- file.path(
        output_path,
        sprintf(
          "model_%s_%s_%s.rds",
          slug,
          model_name,
          temporal_resolution
        )
      )
    }

    dir.create(
      dirname(final_output_path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    attr(model_fit, "output_path") <-
      final_output_path

    saveRDS(
      model_fit,
      final_output_path
    )

    if (isTRUE(verbose)) {
      message(
        "INLA BYM2 model saved to ",
        final_output_path
      )
    }
  }

  model_fit
}