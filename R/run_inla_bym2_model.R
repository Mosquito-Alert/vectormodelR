#' Fit a BYM2 Mosquito Alert occupancy model with INLA
#'
#' Fits an occupancy model using R-INLA with a spatial BYM2 random effect based
#' on a polygon-contiguity graph. This is the INLA counterpart to
#' [run_brms_bym2_model()]. It expects an `inla_bym2_data_prep` object created
#' by [prepare_inla_bym2_data()], a path to such an object, or a prepared
#' modelling object / data frame, which is passed through
#' [prepare_inla_bym2_data()] internally (the polygon grid is located
#' automatically from `input_dir`).
#'
#' If the supplied `formula` does not already contain a BYM2 `f(...)` term, the
#' spatial term
#' `+ f(grid_index, model = "bym2", graph = <graph>, scale.model = TRUE, constr = TRUE)`
#' is appended automatically (mirroring how [run_brms_bym2_model()] appends a
#' `car()` term).
#'
#' @param dataset An `inla_bym2_data_prep` object, a prepared modelling object,
#'   a data frame, a path to a prepared RDS file, or `NULL`. If `NULL`, `iso3`,
#'   `admin_level`, and `admin_name` are used to locate a prepared INLA BYM2
#'   object in `input_dir`.
#' @param formula A formula or character string giving the fixed and
#'   non-spatial random effects structure. Required. The BYM2 spatial term is
#'   appended automatically unless the formula already contains one.
#' @param cellsize_m Numeric cell size in metres, used for prepared-file
#'   lookup. Defaults to `800`.
#' @param temporal_resolution Character. Either `"daily"` or `"hourly"`; used
#'   for prepared-file lookup when `dataset = NULL`.
#' @param family Character INLA likelihood. Defaults to `"binomial"`.
#' @param Ntrials Number of binomial trials, passed to `INLA::inla()` when
#'   `family = "binomial"`. Defaults to `1`.
#' @param bym2_hyper Optional list of BYM2 hyperparameter priors passed to the
#'   `hyper` argument of the spatial `f()` term. If `NULL`, INLA defaults are
#'   used.
#' @param scale_model Logical. Value of `scale.model` in the BYM2 term.
#'   Defaults to `TRUE`.
#' @param constr Logical. Value of `constr` in the BYM2 term. Defaults to
#'   `TRUE`.
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
#' @return The fitted `inla` object, with attributes for the model data, INLA
#'   graph, grid column, grid index column, temporal resolution, formula text,
#'   source dataset, location slug, and output path.
#'
#' @export
run_inla_bym2_model <- function(
    dataset = NULL,
    formula,
    cellsize_m = 800,
    temporal_resolution = c("daily", "hourly"),
    family = "binomial",
    Ntrials = 1,
    bym2_hyper = NULL,
    scale_model = TRUE,
    constr = TRUE,
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
  # 2. Obtain the prepared INLA BYM2 spatial object
  # ---------------------------------------------------------------------------

  dataset_path <- NULL

  if (is.null(dataset)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop(
        "If `dataset` is NULL, you must provide `iso3`, `admin_level`, and ",
        "`admin_name` to locate the prepared INLA BYM2 object.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(iso3, admin_level, admin_name)

    cellsize_token <- gsub(
      "\\.",
      "_",
      format(cellsize_m, trim = TRUE, scientific = FALSE)
    )

    grid_token <- paste0("grid_id_", cellsize_token)

    target_file <- file.path(
      input_dir,
      sprintf("model_prep_%s_inla_bym2_%s.rds", ids$slug, grid_token)
    )

    if (!file.exists(target_file)) {
      stop(
        "Prepared INLA BYM2 object not found at: ", target_file,
        "\nRun `prepare_inla_bym2_data(..., write = TRUE)` first, or supply ",
        "`dataset` directly.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Loading prepared INLA BYM2 object from: ", target_file)
    }

    dataset <- readRDS(target_file)
    dataset_path <- target_file
  } else if (is.character(dataset) && length(dataset) == 1L) {
    if (!file.exists(dataset)) {
      stop("Dataset file not found: ", dataset, call. = FALSE)
    }

    if (isTRUE(verbose)) {
      message("Loading dataset from: ", dataset)
    }

    dataset_path <- dataset
    dataset <- readRDS(dataset)
  }

  if (inherits(dataset, "inla_bym2_data_prep")) {
    prep <- dataset
  } else if (is.data.frame(dataset) ||
             inherits(
               dataset,
               c("brms_data_prep", "bym2_data_prep", "model_data_prep")
             )) {
    if (isTRUE(verbose)) {
      message(
        "Building INLA BYM2 spatial structure via `prepare_inla_bym2_data()`."
      )
    }

    prep <- prepare_inla_bym2_data(
      dataset = dataset,
      data_dir = input_dir,
      output_dir = input_dir,
      write = FALSE,
      verbose = verbose
    )
  } else {
    stop(
      "`dataset` must be an `inla_bym2_data_prep` object, a prepared ",
      "model-data object, a data frame, a path, or NULL.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Extract prepared spatial data
  # ---------------------------------------------------------------------------

  model_data <- prep$model_data
  graph <- prep$graph
  grid_col <- prep$grid_col

  grid_index_col <- prep$grid_index_col

  if (is.null(grid_index_col) || !nzchar(grid_index_col)) {
    grid_index_col <- "grid_index"
  }

  location_slug <- prep$meta$slug

  prep_resolution <- prep$meta$temporal_resolution

  if (!is.null(prep_resolution) && nzchar(prep_resolution)) {
    temporal_resolution <- prep_resolution
  }

  if (!is.data.frame(model_data)) {
    stop(
      "Prepared INLA BYM2 object does not contain a valid `model_data` ",
      "data.frame.",
      call. = FALSE
    )
  }

  if (!nrow(model_data)) {
    stop("No observations available in the model data.", call. = FALSE)
  }

  if (!grid_index_col %in% names(model_data)) {
    stop(
      "Prepared model data is missing the spatial index column `",
      grid_index_col, "`.",
      call. = FALSE
    )
  }

  if (is.null(graph)) {
    stop(
      "Prepared INLA BYM2 object does not contain an INLA `graph`.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Build formula with the BYM2 spatial term
  # ---------------------------------------------------------------------------

  formula_text <- .inla_formula_text(formula)

  hyper_text <- if (is.null(bym2_hyper)) {
    ""
  } else {
    ", hyper = inla_bym2_hyper"
  }

  spatial_term <- sprintf(
    "f(%s, model = \"bym2\", graph = inla_bym2_graph, scale.model = %s, constr = %s%s)",
    grid_index_col,
    if (isTRUE(scale_model)) "TRUE" else "FALSE",
    if (isTRUE(constr)) "TRUE" else "FALSE",
    hyper_text
  )

  already_spatial <- grepl("model\\s*=\\s*[\"']bym2[\"']", formula_text) ||
    grepl(paste0("f\\(\\s*", grid_index_col, "\\b"), formula_text)

  if (!already_spatial) {
    formula_text <- paste(formula_text, "+", spatial_term)

    if (isTRUE(verbose)) {
      message("Appending INLA BYM2 spatial term: ", spatial_term)
    }
  } else if (isTRUE(verbose)) {
    message(
      "Supplied formula already contains a spatial term; not appending ",
      "another one."
    )
  }

  model_formula <- stats::as.formula(formula_text)

  formula_env <- new.env(parent = parent.frame())
  formula_env$inla_bym2_graph <- graph

  if (!is.null(bym2_hyper)) {
    formula_env$inla_bym2_hyper <- bym2_hyper
  }

  environment(model_formula) <- formula_env

  # ---------------------------------------------------------------------------
  # 5. Response coercion and formula variable check
  # ---------------------------------------------------------------------------

  response_var <- all.vars(model_formula)[1L]

  if (response_var %in% names(model_data) &&
      is.logical(model_data[[response_var]])) {
    model_data[[response_var]] <- as.integer(model_data[[response_var]])
  }

  formula_vars <- setdiff(
    all.vars(model_formula),
    c("inla_bym2_graph", "inla_bym2_hyper")
  )

  missing_vars <- setdiff(formula_vars, names(model_data))

  if (length(missing_vars)) {
    stop(
      "The formula references variables not found in `model_data`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Fit model
  # ---------------------------------------------------------------------------

  if (isTRUE(verbose)) {
    message("Fitting INLA BYM2 model.")
    message("Temporal resolution: ", temporal_resolution)
    message("Observations used: ", nrow(model_data))
    message("Spatial nodes: ", graph$n)
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
  # 7. Attach attributes
  # ---------------------------------------------------------------------------

  attr(model_fit, "model_data") <- model_data
  attr(model_fit, "graph") <- graph
  attr(model_fit, "grid_column") <- grid_col
  attr(model_fit, "grid_index_col") <- grid_index_col
  attr(model_fit, "temporal_resolution") <- temporal_resolution
  attr(model_fit, "formula_text") <- formula_text

  if (!is.null(dataset_path)) {
    attr(model_fit, "source_dataset") <- dataset_path
  }

  if (!is.null(location_slug) && nzchar(location_slug)) {
    attr(model_fit, "location_slug") <- location_slug
  }

  # ---------------------------------------------------------------------------
  # 8. Optional write output
  # ---------------------------------------------------------------------------

  if (isTRUE(write_output)) {
    output_file <- .inla_write_output(
      fit = model_fit,
      output_path = output_path,
      dataset_path = dataset_path,
      location_slug = location_slug,
      temporal_resolution = temporal_resolution,
      model_kind = "inla_bym2",
      verbose = verbose
    )

    attr(model_fit, "output_path") <- output_file
  }

  model_fit
}
