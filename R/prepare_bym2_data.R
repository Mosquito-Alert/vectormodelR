#' Prepare data and adjacency matrix for BYM2 modelling
#'
#' Cleans the dataset, scales predictors (storing means/SDs for future use),
#' and aligns the spatial adjacency matrix to the filtered data. Optionally
#' saves the prepared object to disk.
#'
#' @inheritParams run_brms_model
#' @param cellsize_m Numeric cell size (meters). Defaults to 800.
#' @param adjacency Optional pre-computed adjacency matrix. If NULL, one is built.
#' @param adjacency_args List of arguments passed to [build_grid_adjacency()].
#' @param output_dir Directory where the prepared dataset is written when `write = TRUE`. Defaults to `"data/proc"`.
#' @param write Logical. If `TRUE` the prepared object is written to disk. Defaults to `FALSE`.
#' @param verbose Logical. Emit informative messages when `TRUE`.
#'
#' @return A list of class `bym2_data_list` containing:
#'   \item{model_data}{The filtered, scaled data.frame ready for brms.}
#'   \item{adjacency}{The aligned sparse matrix corresponding to `model_data`.}
#'   \item{grid_col}{The name of the grid identifier column used.}
#'   \item{scaling}{A list of mean/sd values used for scaling predictors.}
#'   \item{meta}{Metadata (iso3, admin_level, etc.) for file naming.}
#' @export
prepare_bym2_data <- function(
    dataset,
    cellsize_m = 800,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    adjacency = NULL,
    adjacency_args = list(),
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  # ---- load dataset ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L && nzchar(dataset)
  dataset_path <- if (dataset_is_path) dataset else NULL
  
  if (dataset_is_path) {
    if (!file.exists(dataset_path)) stop("Dataset not found at ", dataset_path, call. = FALSE)
    dataset <- readRDS(dataset_path)
  }
  
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or a path to an RDS file.", call. = FALSE)
  }
  
  # ---- metadata & slug ----
  explicit_slug <- NULL
  if (!all(vapply(list(iso3, admin_level, admin_name), is.null, logical(1)))) {
    if (any(vapply(list(iso3, admin_level, admin_name), is.null, logical(1)))) {
      stop("`iso3`, `admin_level`, and `admin_name` must be supplied together.", call. = FALSE)
    }
    # Assuming build_location_identifiers is available in your package
    ids <- tryCatch(build_location_identifiers(iso3, admin_level, admin_name), error = function(e) list(slug = "custom"))
    explicit_slug <- ids$slug
  }
  
  location_slug <- attr(dataset, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) location_slug <- explicit_slug
  
  # ---- validation ----
  if (!is.numeric(cellsize_m) || cellsize_m <= 0) stop("`cellsize_m` must be positive.", call. = FALSE)
  
  cellsize_token <- gsub("\\.", "_", format(cellsize_m, trim = TRUE, scientific = FALSE))
  grid_col <- paste0("grid_id_", cellsize_token)
  
  required_cols <- c(
    "date", "sea_days", "presence", "year", "landcover_code",
    "maxTM", "meanPPT24H", "ndvi_ddf_proximity", "elevation_m",
    "popdensity_km2", "source", grid_col
  )
  
  missing <- setdiff(required_cols, names(dataset))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  
  # ---- filtering & formatting ----
  # Convert grid ID to character to ensure matching with matrix rownames
  dataset[[grid_col]] <- as.character(dataset[[grid_col]])
  
  # 1. Filter valid grid IDs
  df <- dataset |>
    dplyr::filter(!is.na(.data[[grid_col]]))
  
  if (!nrow(df)) stop("No observations remain after filtering for valid grid identifiers.", call. = FALSE)
  
  # 2. explicit NA filtering for ALL model variables
  # We do this BEFORE scaling to ensure we don't scale based on rows we drop later
  vars_to_check <- c("presence", "sea_days", "maxTM", "meanPPT24H", 
                     "ndvi_ddf_proximity", "elevation_m", "popdensity_km2",
                     "source", "year", "landcover_code")
  
  df <- df |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(vars_to_check), ~ !is.na(.)))
  
  if (!nrow(df)) stop("No observations remain after dropping NAs.", call. = FALSE)
  
  # ---- scaling (manual to capture parameters) ----
  # We define a helper to scale and return the params
  scale_params <- list()
  
  do_scale <- function(x, name) {
    mu <- mean(x, na.rm = TRUE)
    sigma <- sd(x, na.rm = TRUE)
    scale_params[[name]] <<- list(mean = mu, sd = sigma)
    (x - mu) / sigma
  }
  
  df <- df |>
    dplyr::mutate(
      year           = factor(.data$year),
      landcover_code = factor(.data$landcover_code),
      source         = factor(.data$source),
      # Transformations + Scaling
      maxTM_z = do_scale(.data$maxTM, "maxTM"),
      ppt_z   = do_scale(log1p(.data$meanPPT24H), "log1p_ppt"),
      ndvi_z  = do_scale(.data$ndvi_ddf_proximity, "ndvi"),
      elev_z  = do_scale(.data$elevation_m, "elevation"),
      pop_z   = do_scale(log1p(.data$popdensity_km2), "log1p_pop")
    ) |>
    dplyr::arrange(.data$source, .data[[grid_col]], .data$date) |>
    dplyr::distinct(.data$source, .data[[grid_col]], .data$date, .data$presence, .keep_all = TRUE)
  
  # ---- adjacency ----
  grid_ids <- sort(unique(df[[grid_col]]))
  
  if (is.null(adjacency)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop("`iso3`, `admin_level`, `admin_name` required to build adjacency.", call. = FALSE)
    }
    
    # Forward extra args if provided
    adj_args <- c(
      list(iso3 = iso3, admin_level = admin_level, admin_name = admin_name, 
           cellsize_m = cellsize_m, model = dataset, sparse = TRUE),
      adjacency_args
    )
    # Assuming build_grid_adjacency handles duplicate args by taking the last one
    # or you might need more robust list merging here
    adjacency_matrix <- do.call(build_grid_adjacency, adj_args)
    
  } else {
    adjacency_matrix <- adjacency
  }
  
  if (!inherits(adjacency_matrix, "Matrix")) adjacency_matrix <- Matrix::Matrix(adjacency_matrix)
  
  # Ensure strict alignment
  if (is.null(rownames(adjacency_matrix))) stop("Adjacency matrix must have rownames.", call. = FALSE)
  
  missing_ids <- setdiff(grid_ids, rownames(adjacency_matrix))
  if (length(missing_ids)) {
    stop("Adjacency matrix is missing ", length(missing_ids), " grid identifiers found in data.", call. = FALSE)
  }
  
  # Subset and sort matrix to match data exactly
  adjacency_aligned <- adjacency_matrix[grid_ids, grid_ids, drop = FALSE]
  
  # Ensure symmetry (numerical precision issues can sometimes break this)
  if (!Matrix::isSymmetric(adjacency_aligned)) {
    adjacency_aligned <- (adjacency_aligned + Matrix::t(adjacency_aligned)) / 2
  }
  
  # ---- return object ----
  obj <- structure(
    list(
      model_data = df,
      adjacency = adjacency_aligned,
      grid_col = grid_col,
      scaling = scale_params,
      meta = list(
        iso3 = iso3, 
        admin_level = admin_level, 
        admin_name = admin_name, 
        slug = location_slug,
        source_path = dataset_path
      )
    ),
    class = "bym2_data_prep"
  )

  if (isTRUE(write)) {
    if (is.null(output_dir) || !nzchar(output_dir)) {
      stop("`write = TRUE` requires a valid `output_dir`.", call. = FALSE)
    }
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    # We save the entire object to preserve scaling params and adjacency
    output_path <- file.path(output_dir, sprintf("model_prep_%s_data.rds", location_slug))
    saveRDS(obj, output_path)
    
    if (isTRUE(verbose)) message("Prepared brms/BYM2 object written to ", output_path)
  }

  obj
}