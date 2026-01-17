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
  dataset,
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
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L && nzchar(dataset)
  dataset_path <- NULL
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) stop("Dataset not found at ", dataset_path, call. = FALSE)
    dataset <- readRDS(dataset_path)
  }

  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or a path to an RDS file containing one.", call. = FALSE)
  }

  # ---- slug ----
  explicit_slug <- NULL
  if (!all(vapply(list(iso3, admin_level, admin_name), is.null, logical(1)))) {
    if (any(vapply(list(iso3, admin_level, admin_name), is.null, logical(1)))) {
      stop("`iso3`, `admin_level`, and `admin_name` must be supplied together.", call. = FALSE)
    }
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    explicit_slug <- ids$slug
  }

  location_slug <- attr(dataset, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) location_slug <- explicit_slug
  if (!is.null(location_slug) && nzchar(location_slug)) attr(dataset, "location_slug") <- location_slug

  # ---- grid col ----
  cellsize_token <- gsub("\\.", "_", format(cellsize_m, trim = TRUE, scientific = FALSE))
  grid_col <- paste0("grid_id_", cellsize_token)

  # ---- required cols ----
  required_cols <- c(
    "date", "sea_days", "presence", "year", "landcover_code",
    "maxTM", "meanPPT24H", "ndvi_ddf_proximity", "elevation_m",
    "popdensity_km2", "source", grid_col
  )

  missing_cols <- setdiff(required_cols, names(dataset))
  if (length(missing_cols)) {
    stop("Dataset is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  allowed_sources <- c("malert", "gbif")
  src_vals <- unique(as.character(dataset$source))
  bad_sources <- setdiff(src_vals, allowed_sources)

  if (length(bad_sources)) {
    stop(
      "Invalid values in `source`: ",
      paste(bad_sources, collapse = ", "),
      ". Expected only: ", paste(allowed_sources, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- model_data prep ----
  model_data <- dataset
  model_data[[grid_col]] <- as.character(model_data[[grid_col]])

  model_data <- model_data |>
    dplyr::filter(!is.na(.data[[grid_col]])) |>
    dplyr::mutate(
      year = factor(.data$year),
      landcover_code = factor(.data$landcover_code),
      source = factor(.data$source),
      maxTM_z = as.numeric(scale(.data$maxTM)),
      ppt_z   = as.numeric(scale(log1p(.data$meanPPT24H))),
      ndvi_z  = as.numeric(scale(.data$ndvi_ddf_proximity)),
      elev_z  = as.numeric(scale(.data$elevation_m)),
      pop_z   = as.numeric(scale(log1p(.data$popdensity_km2)))
    ) |>
    dplyr::arrange(.data$source, .data[[grid_col]], .data$date) |>
    dplyr::distinct(.data$source, .data[[grid_col]], .data$date, .data$presence, .keep_all = TRUE)

  if (!nrow(model_data)) {
    stop("No observations remain after filtering for valid grid identifiers.", call. = FALSE)
  }

  # ---- explicit NA filtering for variables used in formula ----
  vars_used <- c(
    "presence", "sea_days", "maxTM_z", "ppt_z", "ndvi_z", "elev_z", "pop_z",
    "source", "year", "landcover_code", grid_col
  )
  model_data <- model_data |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(vars_used), ~ !is.na(.)))

  if (!nrow(model_data)) {
    stop("No observations remain after dropping rows with missing model variables.", call. = FALSE)
  }

  # ---- adjacency ----
  grid_ids <- sort(unique(model_data[[grid_col]]))
  if (anyNA(grid_ids)) stop("Grid identifier column contains missing values after filtering.", call. = FALSE)

  # ---- adjacency ----
  grid_ids <- sort(unique(model_data[[grid_col]]))

  if (is.null(adjacency)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop("Building the adjacency matrix requires `iso3`, `admin_level`, and `admin_name`.", call. = FALSE)
    }

    adjacency_matrix <- build_grid_adjacency(
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      cellsize_m = cellsize_m,
      model = dataset,
      sparse = TRUE
    )
  } else {
    adjacency_matrix <- adjacency
  }

  if (!inherits(adjacency_matrix, "Matrix")) {
    adjacency_matrix <- Matrix::Matrix(adjacency_matrix)
  }

  if (is.null(rownames(adjacency_matrix)) || is.null(colnames(adjacency_matrix))) {
    stop("Adjacency matrix must have row and column names matching the grid identifiers.", call. = FALSE)
  }

  rownames(adjacency_matrix) <- as.character(rownames(adjacency_matrix))
  colnames(adjacency_matrix) <- as.character(colnames(adjacency_matrix))

  missing_ids <- setdiff(grid_ids, rownames(adjacency_matrix))
  if (length(missing_ids)) {
    stop("Adjacency matrix is missing grid identifiers: ", paste(head(missing_ids, 10), collapse = ", "), call. = FALSE)
  }

  adjacency_aligned <- adjacency_matrix[grid_ids, grid_ids, drop = FALSE]
  if (!Matrix::isSymmetric(adjacency_aligned)) {
    adjacency_aligned <- (adjacency_aligned + Matrix::t(adjacency_aligned)) / 2
  }

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

  # ---- build formula with dynamic grid column ----
  car_term <- paste0("car(W, gr = ", grid_col, ", type = \"bym2\")")
  formula_text <- paste(
    "presence ~ s(sea_days, bs = \"cc\", k = 12) +",
    "s(maxTM_z, k = 6) +",
    "ppt_z + ndvi_z + elev_z + pop_z + source +",
    "(1 | year) +",
    "(1 | landcover_code) +",
    car_term
  )
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