#' Fit a BYM2 Mosquito Alert occupancy model with brms
#'
#' Fits a Bayesian occupancy model using `brms` with a spatial BYM2/CAR
#' random effect based on a grid adjacency matrix.
#'
#' This function is the spatial counterpart to [run_brms_model()]. It expects
#' either a `bym2_data_prep` object created by [prepare_bym2_data()], a path to
#' a prepared BYM2 RDS file, a `brms_data_prep` object plus enough information
#' to build/provide adjacency, or a raw modelling data frame that can be passed
#' through [prepare_bym2_data()].
#'
#' @param dataset An in-memory modelling dataset, a `brms_data_prep` object,
#'   a `bym2_data_prep` object, a data.frame, a path to a prepared RDS file,
#'   or `NULL`. If `NULL`, `iso3`, `admin_level`, and `admin_name` are used
#'   to locate a prepared BYM2 object in `input_dir`.
#' @param formula Character string or formula object specifying the fixed and
#'   non-spatial random effects structure. This is required. If the formula does
#'   not contain a `car()` term, the BYM2 term
#'   `+ car(W, gr = <grid_col>, type = "bym2")` is appended automatically.
#'   If the formula already contains `car(...)`, it is used as supplied.
#' @param cellsize_m Numeric cell size in meters. Defaults to `800`, expecting
#'   a grid column such as `grid_id_800`.
#' @param temporal_resolution Character. Either `"daily"` or `"hourly"`.
#'   Determines which prepared-file name is used when `dataset = NULL`.
#' @param adjacency Optional pre-computed adjacency matrix whose row/column
#'   names match the grid identifier column.
#' @param adjacency_args Named list of additional arguments forwarded to
#'   [build_grid_adjacency()] when adjacency needs to be built.
#' @param priors Optional `brms::set_prior` object. If `NULL`, default BYM2
#'   priors are used.
#' @param nchains Integer. Number of MCMC chains.
#' @param threads_per_chain Integer. Number of threads per chain.
#' @param adapt_delta Numeric. Target average proposal acceptance probability.
#' @param max_treedepth Integer. Maximum tree depth for NUTS.
#' @param backend Character. `"cmdstanr"` or `"rstan"`.
#' @param iso3,admin_level,admin_name Optional location identifiers used for
#'   prepared-file lookup and adjacency construction.
#' @param write_output Logical. Whether to save the fitted model to disk.
#' @param output_path Directory or file path for saved model output.
#' @param input_dir Directory used when automatically locating prepared data.
#' @param save_pars Logical. Forwarded to `brms::brm()`.
#' @param verbose Logical. Emit messages when `TRUE`.
#'
#' @return A fitted `brmsfit` object with attributes for model data, adjacency
#'   IDs, grid column, source dataset, location slug, formula text, and output path.
#'
#' @export
run_brms_bym2_model <- function(
    dataset = NULL,
    formula,
    cellsize_m = 800,
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

  # ---------------------------------------------------------------------------
  # 1. Dependencies
  # ---------------------------------------------------------------------------

  for (pkg in c("brms", "Matrix", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' must be installed.", call. = FALSE)
    }
  }

  if (identical(backend, "cmdstanr") &&
      !requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      "Backend 'cmdstanr' selected but package 'cmdstanr' is not installed.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Argument checks
  # ---------------------------------------------------------------------------

  if (missing(formula)) {
    stop("`formula` is required and must be supplied.", call. = FALSE)
  }

  if (!is.numeric(cellsize_m) ||
      length(cellsize_m) != 1L ||
      is.na(cellsize_m) ||
      cellsize_m <= 0) {
    stop("`cellsize_m` must be a positive numeric scalar.", call. = FALSE)
  }

  validate_count <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1) {
      stop(sprintf("`%s` must be a positive numeric scalar.", name), call. = FALSE)
    }
    as.integer(x)
  }

  nchains <- validate_count(nchains, "nchains")
  threads_per_chain <- validate_count(threads_per_chain, "threads_per_chain")

  if (!is.numeric(adapt_delta) ||
      length(adapt_delta) != 1L ||
      is.na(adapt_delta) ||
      adapt_delta <= 0 ||
      adapt_delta >= 1) {
    stop("`adapt_delta` must be a numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(max_treedepth) ||
      length(max_treedepth) != 1L ||
      is.na(max_treedepth) ||
      max_treedepth < 1) {
    stop("`max_treedepth` must be a positive numeric scalar.", call. = FALSE)
  }

  if (!is.list(adjacency_args)) {
    stop("`adjacency_args` must be a named list.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 3. Load dataset if needed
  # ---------------------------------------------------------------------------

  dataset_path <- NULL

  if (is.null(dataset)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop(
        "If `dataset` is NULL, you must provide `iso3`, `admin_level`, ",
        "and `admin_name` to locate the prepared BYM2 data.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    slug <- ids$slug

    resolution_suffix <- if (identical(temporal_resolution, "hourly")) {
      "_hourly"
    } else {
      "_daily"
    }

    target_file <- file.path(
      input_dir,
      sprintf("model_prep_%s%s_bym2_data.rds", slug, resolution_suffix)
    )

    if (!file.exists(target_file)) {
      legacy_candidates <- c(
        file.path(input_dir, sprintf("model_prep_%s_bym2_data.rds", slug)),
        file.path(input_dir, sprintf("model_prep_%s_data.rds", slug)),
        file.path(input_dir, sprintf("model_prep_%s%s_data.rds", slug, resolution_suffix))
      )

      existing_legacy <- legacy_candidates[file.exists(legacy_candidates)]

      if (length(existing_legacy)) {
        stop(
          "Prepared BYM2 dataset not found at: ", target_file,
          "\nHowever, a possible legacy/non-BYM2 prepared file exists at: ",
          existing_legacy[1],
          "\nRe-run `prepare_bym2_data(..., temporal_resolution = \"",
          temporal_resolution,
          "\", write = TRUE)` to generate the expected BYM2 file.",
          call. = FALSE
        )
      }

      stop(
        "Prepared BYM2 dataset not found at: ", target_file,
        "\nPlease run `prepare_bym2_data(..., temporal_resolution = \"",
        temporal_resolution,
        "\", write = TRUE)` first.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Loading prepared BYM2 dataset from: ", target_file)
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

  # ---------------------------------------------------------------------------
  # 4. Convert input into a BYM2 prepared object
  # ---------------------------------------------------------------------------

  if (inherits(dataset, "bym2_data_prep")) {
    prep_obj <- dataset

    if (!is.null(prep_obj$meta$temporal_resolution) &&
        !identical(prep_obj$meta$temporal_resolution, temporal_resolution)) {
      if (isTRUE(verbose)) {
        message(
          "Prepared BYM2 data temporal resolution is '",
          prep_obj$meta$temporal_resolution,
          "' but `temporal_resolution` was set to '",
          temporal_resolution,
          "'. Using the prepared data setting."
        )
      }

      temporal_resolution <- prep_obj$meta$temporal_resolution
    }

  } else if (inherits(dataset, "brms_data_prep")) {
    if (isTRUE(verbose)) {
      message("`brms_data_prep` object supplied; adding/building BYM2 adjacency.")
    }

    model_data <- dataset$model_data
    grid_col <- dataset$grid_col

    if (is.null(grid_col) || !nzchar(grid_col) || !grid_col %in% names(model_data)) {
      stop(
        "`brms_data_prep` object must contain a valid `grid_col` present in `model_data`.",
        call. = FALSE
      )
    }

    if (!is.null(dataset$meta$temporal_resolution) &&
        !identical(dataset$meta$temporal_resolution, temporal_resolution)) {
      if (isTRUE(verbose)) {
        message(
          "Prepared brms data temporal resolution is '",
          dataset$meta$temporal_resolution,
          "' but `temporal_resolution` was set to '",
          temporal_resolution,
          "'. Using the prepared data setting."
        )
      }

      temporal_resolution <- dataset$meta$temporal_resolution
    }

    if (is.null(adjacency)) {
      if (is.null(iso3)) iso3 <- dataset$meta$iso3
      if (is.null(admin_level)) admin_level <- dataset$meta$admin_level
      if (is.null(admin_name)) admin_name <- dataset$meta$admin_name

      if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
        stop(
          "A `brms_data_prep` object was supplied without adjacency. ",
          "Provide either `adjacency`, or provide/ensure `iso3`, `admin_level`, ",
          "and `admin_name` so adjacency can be built.",
          call. = FALSE
        )
      }
    }

    prep_obj <- prepare_bym2_data(
      dataset = model_data,
      cellsize_m = cellsize_m,
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

    if (!is.null(dataset$meta)) {
      prep_obj$meta <- utils::modifyList(dataset$meta, prep_obj$meta)
      prep_obj$meta$temporal_resolution <- temporal_resolution
    }

  } else if (is.data.frame(dataset)) {
    if (isTRUE(verbose)) {
      message("Raw data.frame supplied; calling `prepare_bym2_data()` internally.")
    }

    prep_obj <- prepare_bym2_data(
      dataset = dataset,
      cellsize_m = cellsize_m,
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
      "`dataset` must be NULL, a path, a data.frame, a `brms_data_prep` object, ",
      "or a `bym2_data_prep` object.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 5. Extract and validate prepared BYM2 data
  # ---------------------------------------------------------------------------

  model_data <- prep_obj$model_data
  adjacency_aligned <- prep_obj$adjacency
  grid_col <- prep_obj$grid_col
  location_slug <- prep_obj$meta$slug

  if (!is.data.frame(model_data)) {
    stop("Prepared BYM2 object does not contain a valid `model_data` data.frame.", call. = FALSE)
  }

  if (!nrow(model_data)) {
    stop("No observations available in the model data.", call. = FALSE)
  }

  if (is.null(grid_col) || !nzchar(grid_col)) {
    stop("Prepared BYM2 object does not contain a valid `grid_col`.", call. = FALSE)
  }

  if (!grid_col %in% names(model_data)) {
    stop("Grid column `", grid_col, "` was not found in model data.", call. = FALSE)
  }

  model_data[[grid_col]] <- as.character(model_data[[grid_col]])

  if (is.null(adjacency_aligned)) {
    stop("Prepared BYM2 object does not contain an adjacency matrix.", call. = FALSE)
  }

  if (!inherits(adjacency_aligned, "Matrix")) {
    adjacency_aligned <- Matrix::Matrix(adjacency_aligned, sparse = TRUE)
  }

  if (is.null(rownames(adjacency_aligned)) ||
      is.null(colnames(adjacency_aligned))) {
    stop("Adjacency matrix must have rownames and colnames.", call. = FALSE)
  }

  grid_ids <- sort(unique(model_data[[grid_col]]))

  missing_from_adj <- setdiff(grid_ids, rownames(adjacency_aligned))

  if (length(missing_from_adj)) {
    stop(
      "Adjacency matrix is missing ",
      length(missing_from_adj),
      " grid IDs found in model data. Examples: ",
      paste(utils::head(missing_from_adj, 10), collapse = ", "),
      call. = FALSE
    )
  }

  adjacency_aligned <- adjacency_aligned[grid_ids, grid_ids, drop = FALSE]

  if (!all(rownames(adjacency_aligned) == grid_ids)) {
    stop("Adjacency row alignment failed.", call. = FALSE)
  }

  if (!all(colnames(adjacency_aligned) == grid_ids)) {
    stop("Adjacency column alignment failed.", call. = FALSE)
  }

  if (!Matrix::isSymmetric(adjacency_aligned)) {
    if (isTRUE(verbose)) {
      message("Adjacency matrix is not symmetric; symmetrising it.")
    }

    adjacency_aligned <- (adjacency_aligned + Matrix::t(adjacency_aligned)) / 2
  }

  adjacency_aligned <- Matrix::drop0(adjacency_aligned)

  if (identical(temporal_resolution, "hourly")) {
    if (!"hour" %in% names(model_data)) {
      stop(
        "Hourly resolution requested but `hour` is missing from model data.",
        call. = FALSE
      )
    }

    if (all(is.na(model_data$hour))) {
      stop("Hourly resolution requested but all `hour` values are NA.", call. = FALSE)
    }
  }

  # ---------------------------------------------------------------------------
  # 6. Priors
  # ---------------------------------------------------------------------------

  default_priors <- c(
    brms::set_prior("normal(0, 1)", class = "b"),
    brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
    brms::set_prior("student_t(3, 0, 2.5)", class = "sds"),
    brms::set_prior("student_t(3, 0, 2.5)", class = "sd"),
    brms::set_prior("student_t(3, 0, 2.5)", class = "sdcar"),
    brms::set_prior("beta(1, 1)", class = "rhocar")
  )

  if (is.null(priors)) {
    priors <- default_priors

    if (isTRUE(verbose)) {
      message("No priors supplied; using default BYM2 priors.")
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Build formula from supplied formula only
  # ---------------------------------------------------------------------------

  if (is.character(formula) && length(formula) > 1L) {
    formula_text <- paste(formula, collapse = " + ")
  } else if (inherits(formula, "formula")) {
    formula_text <- paste(deparse(formula), collapse = " ")
  } else if (is.character(formula) && length(formula) == 1L) {
    formula_text <- formula
  } else {
    stop("`formula` must be a string or formula object.", call. = FALSE)
  }

  car_term <- paste0("car(W, gr = ", grid_col, ", type = \"bym2\")")

  if (!grepl("car\\s*\\(", formula_text)) {
    formula_text <- paste(formula_text, "+", car_term)

    if (isTRUE(verbose)) {
      message("Appending BYM2 spatial term to supplied formula: ", car_term)
    }
  } else if (isTRUE(verbose)) {
    message("Supplied formula already contains a `car()` term; not appending another one.")
  }

  model_formula <- stats::as.formula(formula_text)

  formula_env <- new.env(parent = parent.frame())
  formula_env$s <- brms::s
  formula_env$car <- brms::car
  environment(model_formula) <- formula_env

  # ---------------------------------------------------------------------------
  # 8. Basic formula variable check
  # ---------------------------------------------------------------------------

  formula_vars <- all.vars(model_formula)

  formula_vars <- setdiff(
    formula_vars,
    c("W", "bym2")
  )

  missing_formula_vars <- setdiff(formula_vars, names(model_data))

  if (length(missing_formula_vars)) {
    stop(
      "The formula references variables not found in `model_data`: ",
      paste(missing_formula_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 9. Fit model
  # ---------------------------------------------------------------------------

  thread_arg <- NULL

  if (threads_per_chain > 1) {
    thread_arg <- brms::threading(threads_per_chain)
  }

  if (isTRUE(verbose)) {
    message("Fitting BYM2 brms model with ", nchains, " chain(s) using backend '", backend, "'.")
    message("Temporal resolution: ", temporal_resolution)
    message("Observations used: ", nrow(model_data))
    message("Grid IDs used: ", length(grid_ids))
    message("Formula: ", formula_text)
  }

  save_pars_arg <- if (isTRUE(save_pars)) {
    brms::save_pars(latent = TRUE)
  } else {
    NULL
  }

  model_fit <- brms::brm(
    formula = model_formula,
    data = model_data,
    data2 = list(W = adjacency_aligned),
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

  # ---------------------------------------------------------------------------
  # 10. Attach useful attributes
  # ---------------------------------------------------------------------------

  attr(model_fit, "model_data") <- model_data
  attr(model_fit, "adjacency") <- adjacency_aligned
  attr(model_fit, "adjacency_ids") <- grid_ids
  attr(model_fit, "grid_column") <- grid_col
  attr(model_fit, "temporal_resolution") <- temporal_resolution
  attr(model_fit, "formula_text") <- formula_text

  if (!is.null(dataset_path)) {
    attr(model_fit, "source_dataset") <- dataset_path
  }

  if (!is.null(location_slug) && nzchar(location_slug)) {
    attr(model_fit, "location_slug") <- location_slug
  }

  # ---------------------------------------------------------------------------
  # 11. Optional write output
  # ---------------------------------------------------------------------------

  if (isTRUE(write_output)) {
    final_output_path <- output_path

    if (is.null(final_output_path) || !nzchar(final_output_path)) {
      if (!is.null(dataset_path)) {
        final_output_path <- dirname(dataset_path)
      } else {
        stop(
          "`write_output = TRUE` requires `dataset` be a path or `output_path` be supplied.",
          call. = FALSE
        )
      }
    }

    path_ext <- tools::file_ext(final_output_path)

    is_dir_target <- dir.exists(final_output_path) ||
      identical(path_ext, "") ||
      grepl("[\\/]+$", final_output_path)

    if (!is_dir_target) {
      parent_dir <- dirname(final_output_path)

      if (!dir.exists(parent_dir)) {
        dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
      }
    } else if (!dir.exists(final_output_path)) {
      dir.create(final_output_path, recursive = TRUE, showWarnings = FALSE)
    }

    stem_base <- if (!is.null(location_slug) && nzchar(location_slug)) {
      paste0("model_", location_slug, "_brms_bym2_", temporal_resolution)
    } else if (!is.null(dataset_path)) {
      paste0(
        tools::file_path_sans_ext(basename(dataset_path)),
        "_brms_bym2_",
        temporal_resolution
      )
    } else {
      paste0(
        "brms_bym2_model_",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "_",
        temporal_resolution
      )
    }

    if (is_dir_target) {
      final_output_path <- file.path(final_output_path, paste0(stem_base, ".Rds"))
    } else if (!grepl("\\.rds$", final_output_path, ignore.case = TRUE)) {
      final_output_path <- paste0(final_output_path, ".Rds")
    }

    dir.create(dirname(final_output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(model_fit, final_output_path)

    attr(model_fit, "output_path") <- final_output_path

    if (isTRUE(verbose)) {
      message("BYM2 brms model saved to ", final_output_path)
    }

  } else if (!is.null(output_path)) {
    warning(
      "`output_path` was supplied but `write_output` is FALSE; nothing was written.",
      call. = FALSE
    )
  }

  model_fit
}