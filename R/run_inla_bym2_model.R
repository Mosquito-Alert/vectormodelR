#' Fit a BYM2 model with INLA
#'
#' Fits an INLA model using data prepared by [prepare_inla_bym2_data()].
#' A BYM2 spatial term is added automatically when the supplied formula does
#' not already contain one.
#'
#' @param dataset An `inla_bym2_data_prep` object, a path to a saved preparation
#'   object, or `NULL`.
#' @param formula Formula or character string specifying the fixed and
#'   non-spatial random effects.
#' @param family INLA likelihood family. Defaults to `"binomial"`.
#' @param Ntrials Optional binomial trial counts. When `NULL`, one trial per
#'   observation is used.
#' @param bym2_hyper Optional BYM2 hyperprior specification.
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
    bym2_hyper = NULL,
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

  # ---------------------------------------------------------------------------
  # 1. Dependencies and formula
  # ---------------------------------------------------------------------------

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
  } else if (is.character(formula) &&
             length(formula) == 1L &&
             !is.na(formula) &&
             nzchar(formula)) {
    formula_text <- formula
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

  # ---------------------------------------------------------------------------
  # 2. Load prepared INLA BYM2 data
  # ---------------------------------------------------------------------------

  input_path <- NULL

  if (is.null(dataset)) {
    if (is.null(iso3) ||
        is.null(admin_level) ||
        is.null(admin_name)) {
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
  } else if (is.character(dataset) &&
             length(dataset) == 1L) {
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
      "`dataset` must be an object returned by ",
      "`prepare_inla_bym2_data()` or a path to one.",
      call. = FALSE
    )
  }

  model_data <- dataset$model_data
  spatial_graph <- dataset$spatial_graph

  if (!is.data.frame(model_data) ||
      !nrow(model_data)) {
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

  # ---------------------------------------------------------------------------
  # 3. Add the BYM2 term
  # ---------------------------------------------------------------------------

  if (is.null(bym2_hyper)) {
    bym2_hyper <- list(
      prec = list(
        prior = "pc.prec",
        param = c(1, 0.01)
      ),
      phi = list(
        prior = "pc",
        param = c(0.5, 2 / 3)
      )
    )
  }

  has_bym2_term <- grepl(
    "model\\s*=\\s*[\"']bym2[\"']",
    formula_text
  )

  if (!has_bym2_term) {
    bym2_term <- paste(
      "f(",
      "spatial_id,",
      "model = \"bym2\",",
      "graph = .bym2_graph,",
      "constr = TRUE,",
      "scale.model = TRUE,",
      "adjust.for.con.comp = TRUE,",
      "hyper = .bym2_hyper",
      ")"
    )

    formula_text <- paste(
      formula_text,
      "+",
      bym2_term
    )

    if (isTRUE(verbose)) {
      message("Appending INLA BYM2 spatial term.")
    }
  }

  formula_env <- new.env(
    parent = baseenv()
  )

  formula_env$model.frame <- stats::model.frame
  formula_env$f <- INLA::f
  formula_env$.bym2_graph <- spatial_graph
  formula_env$.bym2_hyper <- bym2_hyper

  model_formula <- stats::as.formula(
    formula_text,
    env = formula_env
  )

  # ---------------------------------------------------------------------------
  # 4. Validate formula variables
  # ---------------------------------------------------------------------------

  formula_variables <- setdiff(
    all.vars(model_formula),
    c(
      ".bym2_graph",
      ".bym2_hyper"
    )
  )

  missing_variables <- setdiff(
    formula_variables,
    names(model_data)
  )

  if (length(missing_variables)) {
    stop(
      "Formula variables missing from model data: ",
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

  # ---------------------------------------------------------------------------
  # 5. Binomial settings
  # ---------------------------------------------------------------------------

  if (identical(family, "binomial")) {
    if (!is.numeric(response) &&
        !is.integer(response)) {
      stop(
        "Binomial response `",
        response_name,
        "` must be numeric.",
        call. = FALSE
      )
    }

    if (is.null(Ntrials)) {
      if (!all(
        response %in% c(0L, 1L, NA_integer_)
      )) {
        stop(
          "With `Ntrials = NULL`, the response must contain only 0, 1, or NA.",
          call. = FALSE
        )
      }

      Ntrials <- rep.int(
        1L,
        nrow(model_data)
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Fit model
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # 7. Attach metadata
  # ---------------------------------------------------------------------------

  location_slug <- dataset$meta$slug

  attr(model_fit, "formula_text") <- formula_text
  attr(model_fit, "temporal_resolution") <- temporal_resolution
  attr(model_fit, "spatial_model") <- "BYM2"

  if (!is.null(input_path)) {
    attr(model_fit, "source_dataset") <- input_path
  }

  if (!is.null(location_slug) &&
      nzchar(location_slug)) {
    attr(model_fit, "location_slug") <- location_slug
  }

  # ---------------------------------------------------------------------------
  # 8. Optionally save model
  # ---------------------------------------------------------------------------

  if (isTRUE(write_output)) {
    path_is_file <- grepl(
      "\\.[Rr][Dd][Ss]$",
      output_path
    )

    if (path_is_file) {
      final_output_path <- output_path
    } else {
      slug <- if (!is.null(location_slug) &&
                  nzchar(location_slug)) {
        location_slug
      } else {
        "custom"
      }

      final_output_path <- file.path(
        output_path,
        sprintf(
          "model_%s_inla_bym2_%s.rds",
          slug,
          temporal_resolution
        )
      )
    }

    dir.create(
      dirname(final_output_path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    attr(model_fit, "output_path") <- final_output_path

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