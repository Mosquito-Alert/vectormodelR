#' Fit a BYM2 Mosquito Alert occupancy model with brms
#'
#' Fits a Bayesian occupancy model using `brms` with a BYM2 spatial
#' random effect based on a grid adjacency matrix.
#'
#' @param dataset A `brms_bym2_data_prep` object, data frame, prepared RDS
#'   path, or `NULL`.
#' @param formula Formula or character string. The BYM2 term is added
#'   automatically when the formula does not contain `car()`.
#' @param cellsize Numeric hex-grid cell size in metres, or an H3 specification
#'   such as `"h3_9"`.
#' @param temporal_resolution Either `"daily"` or `"hourly"`.
#' @param adjacency Optional adjacency matrix when preparing raw data.
#' @param adjacency_args Additional adjacency-builder arguments.
#' @param priors Optional brms prior specification.
#' @param nchains Number of MCMC chains.
#' @param threads_per_chain Number of threads per chain.
#' @param adapt_delta Target acceptance probability.
#' @param max_treedepth Maximum NUTS tree depth.
#' @param backend Either `"cmdstanr"` or `"rstan"`.
#' @param iso3,admin_level,admin_name Location identifiers.
#' @param write_output Whether to save the fitted model.
#' @param output_path Output directory or RDS filename.
#' @param input_dir Directory containing prepared data.
#' @param save_pars Whether to save latent parameters.
#' @param verbose Whether to emit progress messages.
#'
#' @return A fitted `brmsfit` object.
#' @export
run_brms_bym2_model <- function(
  dataset = NULL,
  formula,
  cellsize = 800,
  temporal_resolution = c("daily", "hourly"),
  adjacency = NULL,
  adjacency_args = list(),
  priors = NULL,
  nchains = 4,
  threads_per_chain = 1,
  adapt_delta = 0.99,
  max_treedepth = 15,
  backend = c("cmdstanr", "rstan"),
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  write_output = TRUE,
  output_path = "data/proc",
  input_dir = "data/proc",
  save_pars = TRUE,
  verbose = TRUE
) {
  backend <- match.arg(backend)
  temporal_resolution <- match.arg(temporal_resolution)

  for (pkg in c("brms", "Matrix")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package `", pkg, "` must be installed.",
        call. = FALSE
      )
    }
  }

  if (
    identical(backend, "cmdstanr") &&
      !requireNamespace("cmdstanr", quietly = TRUE)
  ) {
    stop(
      "Backend `cmdstanr` was selected but package `cmdstanr` is not installed.",
      call. = FALSE
    )
  }

  if (missing(formula)) {
    stop("`formula` is required.", call. = FALSE)
  }

  resolve_grid_col(cellsize)

  if (
    !is.numeric(nchains) ||
      length(nchains) != 1L ||
      is.na(nchains) ||
      nchains < 1
  ) {
    stop(
      "`nchains` must be a positive numeric scalar.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(threads_per_chain) ||
      length(threads_per_chain) != 1L ||
      is.na(threads_per_chain) ||
      threads_per_chain < 1
  ) {
    stop(
      "`threads_per_chain` must be a positive numeric scalar.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(adapt_delta) ||
      length(adapt_delta) != 1L ||
      is.na(adapt_delta) ||
      adapt_delta <= 0 ||
      adapt_delta >= 1
  ) {
    stop(
      "`adapt_delta` must be between 0 and 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(max_treedepth) ||
      length(max_treedepth) != 1L ||
      is.na(max_treedepth) ||
      max_treedepth < 1
  ) {
    stop(
      "`max_treedepth` must be a positive numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.list(adjacency_args)) {
    stop(
      "`adjacency_args` must be a list.",
      call. = FALSE
    )
  }

  nchains <- as.integer(nchains)
  threads_per_chain <- as.integer(threads_per_chain)
  max_treedepth <- as.integer(max_treedepth)

  dataset_path <- NULL

  if (is.null(dataset)) {
    if (
      is.null(iso3) ||
        is.null(admin_level) ||
        is.null(admin_name)
    ) {
      stop(
        "When `dataset = NULL`, supply `iso3`, `admin_level`, and `admin_name`.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(
      iso3,
      admin_level,
      admin_name
    )

    target_file <- file.path(
      input_dir,
      sprintf(
        "model_prep_%s_%s_brms_bym2_data.rds",
        ids$slug,
        temporal_resolution
      )
    )

    if (!file.exists(target_file)) {
      stop(
        "Prepared brms BYM2 data not found at: ",
        target_file,
        "\nRun `prepare_brms_bym2_data(..., write = TRUE)` first.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Loading prepared brms BYM2 data from: ",
        target_file
      )
    }

    dataset <- readRDS(target_file)
    dataset_path <- target_file
  } else if (
    is.character(dataset) &&
      length(dataset) == 1L
  ) {
    if (!file.exists(dataset)) {
      stop(
        "Dataset file not found: ",
        dataset,
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Loading prepared data from: ", dataset)
    }

    dataset_path <- dataset
    dataset <- readRDS(dataset)
  }

  if (inherits(dataset, "brms_bym2_data_prep")) {
    prep_obj <- dataset

    prepared_resolution <- prep_obj$meta$temporal_resolution

    if (
      !is.null(prepared_resolution) &&
        !identical(prepared_resolution, temporal_resolution)
    ) {
      if (isTRUE(verbose)) {
        message(
          "Using temporal resolution from prepared data: ",
          prepared_resolution
        )
      }

      temporal_resolution <- prepared_resolution
    }

    if (!is.null(adjacency)) {
      prep_obj$adjacency <- adjacency
    }
  } else if (inherits(dataset, "brms_data_prep")) {
    stop(
      "A non-spatial `brms_data_prep` object was supplied. ",
      "Run `prepare_brms_bym2_data()` first and pass its result.",
      call. = FALSE
    )
  } else if (is.data.frame(dataset)) {
    if (isTRUE(verbose)) {
      message(
        "Raw data frame supplied; calling `prepare_brms_bym2_data()`."
      )
    }

    prep_obj <- prepare_brms_bym2_data(
      dataset = dataset,
      cellsize = cellsize,
      temporal_resolution = temporal_resolution,
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      adjacency = adjacency,
      adjacency_args = adjacency_args,
      output_dir = input_dir,
      write = FALSE,
      verbose = verbose
    )
  } else {
    stop(
      "`dataset` must be NULL, a path, a raw data frame, or a ",
      "`brms_bym2_data_prep` object.",
      call. = FALSE
    )
  }

  model_data <- prep_obj$model_data
  adjacency_aligned <- prep_obj$adjacency
  grid_col <- prep_obj$grid_col
  location_slug <- prep_obj$meta$slug

  if (!is.data.frame(model_data) || !nrow(model_data)) {
    stop(
      "The prepared object does not contain valid model data.",
      call. = FALSE
    )
  }

  if (
    is.null(grid_col) ||
      !nzchar(grid_col) ||
      !grid_col %in% names(model_data)
  ) {
    stop(
      "The prepared object does not contain a valid grid column.",
      call. = FALSE
    )
  }

  if (is.null(adjacency_aligned)) {
    stop(
      "The prepared object does not contain an adjacency matrix.",
      call. = FALSE
    )
  }

  if (!inherits(adjacency_aligned, "Matrix")) {
    adjacency_aligned <- Matrix::Matrix(
      adjacency_aligned,
      sparse = TRUE
    )
  }

  if (
    is.null(rownames(adjacency_aligned)) ||
      is.null(colnames(adjacency_aligned))
  ) {
    stop(
      "Adjacency matrix must have row and column names.",
      call. = FALSE
    )
  }

  model_data[[grid_col]] <- as.character(
    model_data[[grid_col]]
  )

  grid_ids <- sort(
    unique(model_data[[grid_col]])
  )

  missing_grid_ids <- setdiff(
    grid_ids,
    intersect(
      rownames(adjacency_aligned),
      colnames(adjacency_aligned)
    )
  )

  if (length(missing_grid_ids)) {
    stop(
      "Adjacency matrix is missing grid identifiers: ",
      paste(
        utils::head(missing_grid_ids, 10L),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  adjacency_aligned <- adjacency_aligned[
    grid_ids,
    grid_ids,
    drop = FALSE
  ]

  adjacency_aligned <- 1L * (
    (
      adjacency_aligned +
        Matrix::t(adjacency_aligned)
    ) > 0
  )

  diag(adjacency_aligned) <- 0
  adjacency_aligned <- Matrix::drop0(adjacency_aligned)

  dimnames(adjacency_aligned) <- list(
    grid_ids,
    grid_ids
  )

  model_data[[grid_col]] <- factor(
    model_data[[grid_col]],
    levels = grid_ids
  )

  if (identical(temporal_resolution, "hourly")) {
    if (
      !"hour" %in% names(model_data) ||
        all(is.na(model_data$hour))
    ) {
      stop(
        "Hourly model data require a non-missing `hour` column.",
        call. = FALSE
      )
    }
  }

  if (is.null(priors)) {
    priors <- c(
      brms::set_prior(
        "normal(0, 1)",
        class = "b"
      ),
      brms::set_prior(
        "student_t(3, 0, 2.5)",
        class = "Intercept"
      ),
      brms::set_prior(
        "student_t(3, 0, 2.5)",
        class = "sds"
      ),
      brms::set_prior(
        "student_t(3, 0, 2.5)",
        class = "sd"
      ),
      brms::set_prior(
        "student_t(3, 0, 2.5)",
        class = "sdcar"
      ),
      brms::set_prior(
        "beta(1, 1)",
        class = "rhocar"
      )
    )

    if (isTRUE(verbose)) {
      message("Using default BYM2 priors.")
    }
  }

  if (inherits(formula, "formula")) {
    formula_text <- paste(
      deparse(formula),
      collapse = " "
    )
  } else if (
    is.character(formula) &&
      length(formula) >= 1L &&
      !anyNA(formula) &&
      all(nzchar(formula))
  ) {
    formula_text <- paste(
      formula,
      collapse = " + "
    )
  } else {
    stop(
      "`formula` must be a formula or a non-empty character vector.",
      call. = FALSE
    )
  }

  car_term <- paste0(
    "car(W, gr = ",
    grid_col,
    ", type = \"bym2\")"
  )

  if (!grepl("car\\s*\\(", formula_text)) {
    formula_text <- paste(
      formula_text,
      "+",
      car_term
    )

    if (isTRUE(verbose)) {
      message(
        "Appending BYM2 term: ",
        car_term
      )
    }
  }

  model_formula <- stats::as.formula(
    formula_text
  )

  formula_env <- new.env(
    parent = parent.frame()
  )

  formula_env$s <- brms::s
  formula_env$car <- brms::car

  environment(model_formula) <- formula_env

  formula_vars <- setdiff(
    all.vars(model_formula),
    "W"
  )

  missing_formula_vars <- setdiff(
    formula_vars,
    names(model_data)
  )

  if (length(missing_formula_vars)) {
    stop(
      "Formula variables missing from model data: ",
      paste(
        missing_formula_vars,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  response_var <- all.vars(
    model_formula[[2L]]
  )

  if (
    length(response_var) == 1L &&
      is.logical(model_data[[response_var]])
  ) {
    model_data[[response_var]] <- as.integer(
      model_data[[response_var]]
    )
  }

  thread_arg <- if (threads_per_chain > 1L) {
    brms::threading(threads_per_chain)
  } else {
    NULL
  }

  save_pars_arg <- if (isTRUE(save_pars)) {
    brms::save_pars(latent = TRUE)
  } else {
    NULL
  }

  if (isTRUE(verbose)) {
    message(
      "Fitting BYM2 brms model with ",
      nchains,
      " chains using backend `",
      backend,
      "`."
    )

    message("Observations: ", nrow(model_data))
    message("Spatial grid cells: ", length(grid_ids))
    message("Formula: ", formula_text)
  }

  model_fit <- brms::brm(
    formula = model_formula,
    data = model_data,
    data2 = list(
      W = adjacency_aligned
    ),
    family = brms::bernoulli(),
    backend = backend,
    chains = nchains,
    cores = nchains,
    threads = thread_arg,
    prior = priors,
    control = list(
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    ),
    save_pars = save_pars_arg
  )

  attr(model_fit, "model_data") <- model_data
  attr(model_fit, "adjacency") <- adjacency_aligned
  attr(model_fit, "adjacency_ids") <- grid_ids
  attr(model_fit, "grid_column") <- grid_col
  attr(model_fit, "temporal_resolution") <- temporal_resolution
  attr(model_fit, "formula_text") <- formula_text

  if (!is.null(dataset_path)) {
    attr(model_fit, "source_dataset") <- dataset_path
  }

  if (
    !is.null(location_slug) &&
      nzchar(location_slug)
  ) {
    attr(model_fit, "location_slug") <- location_slug
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

      final_output_path <- file.path(
        output_path,
        sprintf(
          "model_%s_brms_bym2_%s.rds",
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
        "BYM2 brms model saved to ",
        final_output_path
      )
    }
  }

  model_fit
}