#' Fit a baseline Mosquito Alert MaxEnt model (maxnet)
#'
#' Reads a Mosquito Alert modelling dataset (from disk or memory), prepares
#' predictors, and fits a MaxEnt-style model using \pkg{maxnet}. MaxEnt requires
#' presence/background data (1/0). If the dataset contains only presences,
#' background points can be sampled from unique grid cells.
#'
#' @param dataset A data frame or a path to an .Rds containing one.
#' @param predictors Character vector of predictor column names to use.
#'   Defaults to a baseline set (no sea_days).
#' @param presence_col Name of presence column. Defaults to "presence".
#'   Expected to be logical TRUE/FALSE, or 0/1.
#' @param grid_id_col Optional column used to sample background by unique grid
#'   cells (defaults to "grid_id"). If missing, background is sampled by rows.
#' @param feature_classes Maxnet feature classes (e.g. c("l","q","h")).
#' @param regmult Regularization multiplier passed to maxnet.
#' @param n_background Number of background samples if presences-only.
#' @param seed Optional seed for reproducibility.
#' @param iso3,admin_level,admin_name Optional naming inputs used to derive a slug
#'   via build_location_identifiers(). Supply all three together when used.
#' @param write_output Save fitted model to disk? Defaults TRUE.
#' @param output_path Directory or full file path for saving. Defaults "data/proc".
#' @param verbose Print progress messages? Defaults TRUE.
#'
#' @return A fitted maxnet model object. Attributes:
#'   - "model_data" (data used to fit)
#'   - "output_path" (if saved)
#'   - "source_dataset" (if dataset was a path)
#'   - "location_slug" (if available)
#' @export
run_maxent_model <- function(
    dataset,
    predictors = c(
      "maxTM",
      "meanPPT24H",
      "ndvi_ddf_proximity",
      "elevation_m",
      "popdensity_km2",
      "landcover_code"
    ),
    presence_col = "presence",
    grid_id_col = "grid_id",
    feature_classes = c("l", "q", "h"),
    regmult = 1,
    n_background = 10000,
    seed = 1234,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    write_output = TRUE,
    output_path = "data/proc",
    verbose = TRUE
) {
  if (!requireNamespace("maxnet", quietly = TRUE)) {
    stop("Package 'maxnet' must be installed to fit the MaxEnt model.", call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package 'stats' is required.", call. = FALSE)
  }
  
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L && nzchar(dataset)
  dataset_path <- NULL
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Dataset not found at ", dataset_path, call. = FALSE)
    }
    dataset <- readRDS(dataset_path)
  }
  
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or a path to an RDS file containing one.", call. = FALSE)
  }
  
  # slug (optional)
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
  
  # checks
  need_cols <- unique(c(presence_col, predictors))
  missing_cols <- setdiff(need_cols, names(dataset))
  if (length(missing_cols) > 0) {
    stop("Dataset is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # build model frame
  model_data <- dataset[, need_cols, drop = FALSE]
  
  # coerce presence -> 0/1
  pa_raw <- model_data[[presence_col]]
  if (is.logical(pa_raw)) {
    pa <- as.integer(pa_raw)
  } else {
    pa <- as.integer(pa_raw)
  }
  if (!all(pa %in% c(0L, 1L))) {
    stop("`", presence_col, "` must be logical (TRUE/FALSE) or 0/1.", call. = FALSE)
  }
  model_data$pa <- pa
  
  # ---- background handling ----
  n_pres <- sum(model_data$pa == 1L, na.rm = TRUE)
  n_bg   <- sum(model_data$pa == 0L, na.rm = TRUE)
  
  if (n_pres == 0) stop("No presence rows found in `", presence_col, "`.", call. = FALSE)
  
  if (n_bg == 0) {
    # Presence-only dataset: sample background
    if (!is.null(seed)) set.seed(seed)
    
    if (!is.null(grid_id_col) && grid_id_col %in% names(dataset)) {
      # sample from unique grid cells (recommended if available)
      grid_df <- dataset[, unique(c(grid_id_col, predictors)), drop = FALSE]
      grid_df <- unique(grid_df)
      
      # remove any rows with NAs in predictors (maxnet can't handle NAs)
      cc <- stats::complete.cases(grid_df[, predictors, drop = FALSE])
      grid_df <- grid_df[cc, , drop = FALSE]
      
      # drop any grid ids that are used as presence points if possible
      pres_grid_ids <- unique(dataset[[grid_id_col]][dataset[[presence_col]] %in% c(TRUE, 1)])
      grid_df <- grid_df[!(grid_df[[grid_id_col]] %in% pres_grid_ids), , drop = FALSE]
      
      if (nrow(grid_df) == 0) stop("No candidate background grid cells available after filtering.", call. = FALSE)
      
      idx <- sample(seq_len(nrow(grid_df)), size = min(n_background, nrow(grid_df)), replace = FALSE)
      bg <- grid_df[idx, predictors, drop = FALSE]
    } else {
      # fallback: sample rows
      cc <- stats::complete.cases(dataset[, predictors, drop = FALSE])
      pool <- dataset[cc, predictors, drop = FALSE]
      if (nrow(pool) == 0) stop("No candidate background rows available after filtering.", call. = FALSE)
      
      idx <- sample(seq_len(nrow(pool)), size = min(n_background, nrow(pool)), replace = FALSE)
      bg <- pool[idx, , drop = FALSE]
    }
    
    pres <- dataset[dataset[[presence_col]] %in% c(TRUE, 1), predictors, drop = FALSE]
    pres <- pres[stats::complete.cases(pres), , drop = FALSE]
    
    if (nrow(pres) == 0) stop("All presence rows have missing predictor values.", call. = FALSE)
    
    model_data <- rbind(
      data.frame(pa = 1L, pres, check.names = FALSE),
      data.frame(pa = 0L, bg,   check.names = FALSE)
    )
  } else {
    # presence + background already present; just keep complete cases
    keep <- stats::complete.cases(model_data[, c("pa", predictors), drop = FALSE])
    model_data <- model_data[keep, , drop = FALSE]
  }
  
  # ---- predictor transforms (simple, robust defaults) ----
  # log-transform rainfall and popdensity if present (common in your pipeline)
  if ("meanPPT24H" %in% predictors) model_data$meanPPT24H <- log1p(model_data$meanPPT24H)
  if ("popdensity_km2" %in% predictors) model_data$popdensity_km2 <- log1p(model_data$popdensity_km2)
  
  # ensure factors stay factors
  if ("landcover_code" %in% predictors) model_data$landcover_code <- factor(model_data$landcover_code)
  
  if (isTRUE(verbose)) {
    message("MaxEnt (maxnet) fitting data: ", sum(model_data$pa == 1L), " presence / ",
            sum(model_data$pa == 0L), " background; predictors = ",
            paste(predictors, collapse = ", "))
  }
  
  # ---- fit maxnet ----
  f <- maxnet::maxnet.formula(p = model_data$pa, data = model_data[, predictors, drop = FALSE],
                              classes = feature_classes)
  
  fit <- maxnet::maxnet(
    p = model_data$pa,
    data = model_data[, predictors, drop = FALSE],
    f = f,
    regmult = regmult
  )
  
  attr(fit, "model_data") <- model_data
  if (!is.null(dataset_path)) attr(fit, "source_dataset") <- dataset_path
  if (!is.null(location_slug) && nzchar(location_slug)) attr(fit, "location_slug") <- location_slug
  
  # ---- save ----
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
    is_dir_target <- dir.exists(final_output_path) || identical(path_ext, "") || grepl("[\\/]+$", final_output_path)
    
    if (is_dir_target && !dir.exists(final_output_path)) dir.create(final_output_path, recursive = TRUE, showWarnings = FALSE)
    if (!is_dir_target && !dir.exists(dirname(final_output_path))) dir.create(dirname(final_output_path), recursive = TRUE, showWarnings = FALSE)
    
    stem_base <- if (!is.null(location_slug) && nzchar(location_slug)) {
      paste0("model_", location_slug, "_maxent_maxnet")
    } else if (!is.null(dataset_path)) {
      paste0(tools::file_path_sans_ext(basename(dataset_path)), "_maxent_maxnet")
    } else {
      paste0("maxent_model_", format(Sys.time(), "%Y%m%d%H%M%S"))
    }
    
    if (is_dir_target) {
      final_output_path <- file.path(final_output_path, paste0(stem_base, ".Rds"))
    } else if (!grepl("\\.rds$", final_output_path, ignore.case = TRUE)) {
      final_output_path <- paste0(final_output_path, ".Rds")
    }
    
    saveRDS(fit, final_output_path)
    attr(fit, "output_path") <- final_output_path
    if (isTRUE(verbose)) message("MaxEnt model saved to ", final_output_path)
  }
  
  fit
}
