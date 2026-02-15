#' Fit a BYM2 Mosquito Alert occupancy model with brms
#'
#' Extends [run_brms_model()] with a spatial BYM2 random effect using an
#' adjacency matrix derived from the hex grid. The helper accepts either an
#' in-memory modelling dataset or a path to the enriched RDS file and will
#' construct the required adjacency matrix when only the location identifiers
#' are supplied. Predictors are scaled to match the baseline specification and
#' duplicate observations per grid-date-source-presence combination are dropped
#' prior to model fitting.
#'
#' @inheritParams run_brms_model
#' @param cellsize_m Numeric cell size (meters) of the hex grid whose adjacency
#'   matrix will be used. Defaults to 800, expecting a `grid_id_800` column in
#'   the dataset.
#' @param adjacency Optional pre-computed adjacency matrix whose row/column
#'   names match the grid identifier column. When `NULL`, the helper calls
#'   [build_grid_adjacency()].
#' @param adjacency_args Named list of additional arguments forwarded to
#'   [build_grid_adjacency()] when `adjacency` is `NULL`. These values override
#'   the defaults assembled by the helper.
#' @param save_pars Logical; forwarded to `brms::brm()` as the `save_pars`
#'   argument. Defaults to `TRUE` to retain latent BYM2 parameters.
#'
#' @return The fitted `brmsfit` object with attributes for the modelling data,
#'   aligned adjacency matrix, and saved path when written to disk.
#' @examples
#' \dontrun{
#' bym2_fit <- run_brms_bym2_model(
#'   dataset = "data/proc/model_prep_esp_4_barcelona_malert_gbif_se_el_ndvi_pd_wx_lc_hex400_hex800.Rds",
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona",
#'   cellsize_m = 800,
#'   nchains = 4,
#'   threads_per_chain = 2
#' )
#' }
#' @export
run_brms_bym2_model <- function(
  dataset = NULL,
  cellsize_m = 800,
  adjacency = NULL,
  adjacency_args = list(),
  priors = NULL,
  nchains = 4,
  threads_per_chain = 1,
  adapt_delta = 0.995,
  max_treedepth = 20,
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

  # ---- deps ----
  for (pkg in c("brms", "Matrix", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' must be installed.", call. = FALSE)
    }
  }
  if (identical(backend, "cmdstanr") && !requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Backend 'cmdstanr' selected but package 'cmdstanr' is not installed.", call. = FALSE)
  }

  # ---- arg checks ----
  if (!is.numeric(cellsize_m) || length(cellsize_m) != 1L || is.na(cellsize_m) || cellsize_m <= 0) {
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

  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L || is.na(adapt_delta) ||
      adapt_delta <= 0 || adapt_delta >= 1) {
    stop("`adapt_delta` must be a numeric value between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(max_treedepth) || length(max_treedepth) != 1L || is.na(max_treedepth) || max_treedepth < 1) {
    stop("`max_treedepth` must be a positive numeric scalar.", call. = FALSE)
  }

  # ---- load dataset ----
  # 1. Automatic lookup if dataset is NULL
  if (is.null(dataset)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop("If `dataset` is NULL, you must provide `iso3`, `admin_level`, and `admin_name` to locate the prepared data.", call. = FALSE)
    }
    
    # Reconstruct the expected slug and filename
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    slug <- ids$slug
    
    target_file <- file.path(input_dir, sprintf("model_prep_%s_data.rds", slug))
    if (!file.exists(target_file)) {
      stop(
        "Prepared dataset not found at: ", target_file, 
        "\nPlease ensure you have run `prepare_bym2_data(..., write = TRUE)` first.", 
        call. = FALSE
      )
    }
    
    if (isTRUE(verbose)) message("Loading prepared dataset from: ", target_file)
    dataset <- readRDS(target_file)
    dataset_path <- target_file
  } else {
    # 2. Handle passed string path
    if (is.character(dataset) && length(dataset) == 1L) {
      if (!file.exists(dataset)) stop("Dataset file not found: ", dataset, call. = FALSE)
      if (isTRUE(verbose)) message("Loading dataset from: ", dataset)
      dataset_path <- dataset
      dataset <- readRDS(dataset)
    } else {
      dataset_path <- NULL
    }
  }
  
  # ---- process dataset object ----
  if (inherits(dataset, "bym2_data_prep")) {
    # Use the pre-baked object
    model_data        <- dataset$model_data
    adjacency_aligned <- dataset$adjacency
    grid_col          <- dataset$grid_col
    location_slug     <- dataset$meta$slug
    
    # If explicit ISO/Admin provided, warn if mismatch? (skipped for now, trust user)
    
  } else if (is.data.frame(dataset)) {
    if (isTRUE(verbose)) message("Raw dataframe provided; calling `prepare_bym2_data()` internally...")
    
    # Delegate to the preparation function
    prep_obj <- prepare_bym2_data(
      dataset = dataset,
      cellsize_m = cellsize_m,
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      adjacency = adjacency,
      adjacency_args = adjacency_args,
      write = FALSE, # internal use only
      verbose = verbose
    )
    
    model_data        <- prep_obj$model_data
    adjacency_aligned <- prep_obj$adjacency
    grid_col          <- prep_obj$grid_col
    location_slug     <- prep_obj$meta$slug
    
  } else {
    stop("`dataset` must be a path, a data frame, or a `bym2_data_prep` object.", call. = FALSE)
  }

  # Ensure everything is ready
  grid_ids <- rownames(adjacency_aligned)
  
  if (!nrow(model_data)) stop("No observations available in the model data.", call. = FALSE)



  default_priors <- c(
    # fixed effects (env + source)
    brms::set_prior("normal(0, 1)", class = "b"),

    # intercept
    brms::set_prior("student_t(3, 0, 2.5)", class = "Intercept"),

    # smooth terms
    brms::set_prior("student_t(3, 0, 2.5)", class = "sds"),

    # random effects
    brms::set_prior("student_t(3, 0, 2.5)", class = "sd"),

    # BYM2 spatial SD
    brms::set_prior("student_t(3, 0, 2.5)", class = "sdcar"),

    # BYM2 mixing parameter
    brms::set_prior("beta(1, 1)", class = "rhocar")
  )

  if (is.null(priors)) {
    priors <- default_priors
    if (isTRUE(verbose)) {
      message("No priors supplied; using default BYM2 priors.")
    }
  }

  # ---- Determine if 'source' should be included ----
  include_source <- FALSE

  if ("source" %in% names(model_data) && length(unique(model_data$source)) > 1) {
    
    # Standardize presence to 0/1 for checking (handles logical TRUE/FALSE or numeric 0/1)
    # If logical: FALSE->0, TRUE->1
    check_presence <- as.integer(as.logical(model_data$presence))
    
    # Get counts of Presences (1) and Absences (0) for each source
    source_counts <- table(model_data$source, check_presence)
    
    # Calculate total size of the smallest source group
    min_group_size <- min(rowSums(source_counts))
    min_group_prop <- min_group_size / nrow(model_data)
    
    # Check for minimum events: Does every source have at least 10 presences AND 10 absences?
    has_0 <- "0" %in% colnames(source_counts) && all(source_counts[, "0"] >= 10)
    has_1 <- "1" %in% colnames(source_counts) && all(source_counts[, "1"] >= 10)
    has_enough_events <- has_0 && has_1
    
    # The Dynamic Rule: Needs enough events AND must be at least 1% of the dataset
    if (has_enough_events && min_group_prop >= 0.01) {
      include_source <- TRUE
      if (isTRUE(verbose)) message("Validating sources: Multiple robust data streams detected. Adding 'source'.")
    } else {
      if (isTRUE(verbose)) message("Validating sources: Minority source too small or lacks variance. Dropping 'source'.")
    }
  } else if (isTRUE(verbose)) {
    message("Validating sources: Only one source present. Dropping 'source'.")
  }

  # ---- build formula with dynamic grid column ----
  car_term <- paste0("car(W, gr = ", grid_col, ", type = \"bym2\")")
  
  # Base formula
  formula_parts <- c(
    "presence ~ s(sea_days, bs = \"cc\", k = 12)",
    "s(maxTM_z, k = 6)",
    "ppt_z + ndvi_z + elev_z + s(pop_z, k = 5)",
    "(1 | year)",
    "(1 | landcover_code)",
    car_term
  )
  
  # Add source if validated
  if (include_source) {
    formula_parts <- append(formula_parts, "source", after = 3)
  }
  
  formula_text <- paste(formula_parts, collapse = " + ")
  model_formula <- stats::as.formula(formula_text)

  # ensure brms functions are found when formula is evaluated
  formula_env <- new.env(parent = parent.frame())
  formula_env$s <- brms::s
  formula_env$car <- brms::car
  environment(model_formula) <- formula_env

  # threading
  thread_arg <- NULL
  if (threads_per_chain > 1) thread_arg <- brms::threading(threads_per_chain)

  if (isTRUE(verbose)) {
    message("Fitting BYM2 brms model with ", nchains, " chain(s) using backend '", backend, "'.")
    message("Observations used: ", nrow(model_data), " | Grid IDs used: ", length(grid_ids))
  }

  save_pars_arg <- if (isTRUE(save_pars)) brms::save_pars(latent = TRUE) else NULL

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
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    save_pars = save_pars_arg
  )

  # ---- attrs ----
  attr(model_fit, "model_data") <- model_data
  attr(model_fit, "adjacency_ids") <- grid_ids
  attr(model_fit, "grid_column") <- grid_col
  if (!is.null(dataset_path)) attr(model_fit, "source_dataset") <- dataset_path
  if (!is.null(location_slug) && nzchar(location_slug)) attr(model_fit, "location_slug") <- location_slug

  # ---- write output ----
  if (isTRUE(write_output)) {
    final_output_path <- output_path
    if (is.null(final_output_path) || !nzchar(final_output_path)) {
      if (!is.null(dataset_path)) {
        final_output_path <- dirname(dataset_path)
      } else {
        stop("`write_output = TRUE` requires `dataset` be a path or `output_path` be supplied.", call. = FALSE)
      }
    }

    path_ext <- tools::file_ext(final_output_path)
    is_dir_target <- dir.exists(final_output_path) ||
      identical(path_ext, "") ||
      grepl("[\\/]+$", final_output_path)

    if (!is_dir_target) {
      parent_dir <- dirname(final_output_path)
      if (!dir.exists(parent_dir)) dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
    } else if (!dir.exists(final_output_path)) {
      dir.create(final_output_path, recursive = TRUE, showWarnings = FALSE)
    }

    stem_base <- if (!is.null(location_slug) && nzchar(location_slug)) {
      paste0("model_", location_slug, "_brms_bym2")
    } else if (!is.null(dataset_path)) {
      paste0(tools::file_path_sans_ext(basename(dataset_path)), "_brms_bym2")
    } else {
      paste0("brms_bym2_model_", format(Sys.time(), "%Y%m%d%H%M%S"))
    }

    if (is_dir_target) {
      final_output_path <- file.path(final_output_path, paste0(stem_base, ".Rds"))
    } else if (!grepl("\\.rds$", final_output_path, ignore.case = TRUE)) {
      final_output_path <- paste0(final_output_path, ".Rds")
    }

    dir.create(dirname(final_output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(model_fit, final_output_path)
    attr(model_fit, "output_path") <- final_output_path

    if (isTRUE(verbose)) message("BYM2 brms model saved to ", final_output_path)
  } else if (!is.null(output_path)) {
    warning("`output_path` was supplied but `write_output` is FALSE; nothing was written.", call. = FALSE)
  }

  model_fit
}