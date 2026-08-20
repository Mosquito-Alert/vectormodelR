#' Prepare data and adjacency matrix for BYM2 brms modelling
#'
#' Prepares model data using [prepare_model_data()], then builds or aligns a
#' spatial adjacency matrix for BYM2 modelling.
#'
#' This function is a spatial wrapper around [prepare_model_data()]. Use
#' [prepare_model_data()] for non-spatial models and
#' [prepare_brms_bym2_data()] when the model requires a BYM2/CAR spatial
#' component.
#'
#' @inheritParams prepare_model_data
#' @param adjacency Optional precomputed adjacency matrix. If `NULL`, one is
#'   built using [build_grid_adjacency()] or [build_h3_adjacency()].
#' @param adjacency_args Named list of additional arguments passed to the
#'   selected adjacency builder when `adjacency = NULL`.
#'
#' @return A list of class `bym2_data_prep` containing:
#'   \item{model_data}{The prepared data frame ready for brms.}
#'   \item{adjacency}{The aligned sparse adjacency matrix.}
#'   \item{grid_col}{The active grid identifier column.}
#'   \item{scaling}{Scaling parameters from [prepare_model_data()].}
#'   \item{scale_specs}{The normalized scaling specifications.}
#'   \item{aggregation_specs}{The aggregation specifications used.}
#'   \item{factor_cols}{The columns converted to factors.}
#'   \item{meta}{Preparation and adjacency metadata.}
#'
#' @export
prepare_brms_bym2_data <- function(
    dataset,
    cellsize = 800,
    temporal_resolution = c("daily", "hourly"),
    base_required_cols = NULL,
    vars_to_check = NULL,
    scale_specs = NULL,
    aggregation_specs = NULL,
    factor_cols = NULL,
    remove_unused_cols = FALSE,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    adjacency = NULL,
    adjacency_args = list(),
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  temporal_resolution <- match.arg(temporal_resolution)
  
  # ---------------------------------------------------------------------------
  # 1. Validate arguments
  # ---------------------------------------------------------------------------
  
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop(
      "Package `Matrix` is required for BYM2 preparation.",
      call. = FALSE
    )
  }
  
  if (!is.null(base_required_cols) &&
      !is.character(base_required_cols)) {
    stop(
      "`base_required_cols` must be NULL or a character vector.",
      call. = FALSE
    )
  }
  
  if (!is.null(vars_to_check) &&
      !is.character(vars_to_check)) {
    stop(
      "`vars_to_check` must be NULL or a character vector.",
      call. = FALSE
    )
  }
  
  if (!is.null(scale_specs) &&
      !is.list(scale_specs)) {
    stop(
      "`scale_specs` must be NULL or a named list.",
      call. = FALSE
    )
  }
  
  if (!is.null(aggregation_specs) &&
      !is.list(aggregation_specs) &&
      !is.character(aggregation_specs)) {
    stop(
      "`aggregation_specs` must be NULL, a named character vector, ",
      "or a named list.",
      call. = FALSE
    )
  }
  
  if (!is.null(factor_cols) &&
      !is.character(factor_cols)) {
    stop(
      "`factor_cols` must be NULL or a character vector.",
      call. = FALSE
    )
  }
  
  if (!is.logical(remove_unused_cols) ||
      length(remove_unused_cols) != 1L ||
      is.na(remove_unused_cols)) {
    stop(
      "`remove_unused_cols` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  if (!is.list(adjacency_args)) {
    stop(
      "`adjacency_args` must be a list.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 2. Prepare model data
  # ---------------------------------------------------------------------------
  
  prepared <- prepare_model_data(
    dataset = dataset,
    cellsize = cellsize,
    temporal_resolution = temporal_resolution,
    base_required_cols = base_required_cols,
    vars_to_check = vars_to_check,
    scale_specs = scale_specs,
    aggregation_specs = aggregation_specs,
    factor_cols = factor_cols,
    remove_unused_cols = remove_unused_cols,
    iso3 = iso3,
    admin_level = admin_level,
    admin_name = admin_name,
    output_dir = output_dir,
    write = FALSE,
    verbose = verbose
  )
  
  df <- prepared$model_data
  grid_col <- prepared$grid_col
  is_h3 <- startsWith(grid_col, "h3_id_")
  
  if (!is.data.frame(df)) {
    stop(
      "`prepare_model_data()` did not return valid model data.",
      call. = FALSE
    )
  }
  
  if (is.null(grid_col) || !nzchar(grid_col)) {
    stop(
      "`prepare_model_data()` did not return a valid `grid_col`.",
      call. = FALSE
    )
  }
  
  if (!grid_col %in% names(df)) {
    stop(
      "Grid column `", grid_col,
      "` was not found in prepared model data.",
      call. = FALSE
    )
  }
  
  if (!nrow(df)) {
    stop(
      "Prepared model data have zero rows.",
      call. = FALSE
    )
  }
  
  df[[grid_col]] <- as.character(df[[grid_col]])
  
  grid_ids <- sort(
    unique(df[[grid_col]])
  )
  
  if (!length(grid_ids)) {
    stop(
      "No grid identifiers were found in prepared model data.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 3. Build or use the supplied adjacency matrix
  # ---------------------------------------------------------------------------
  
  if (is.null(adjacency)) {
    if (!is_h3 &&
        (is.null(iso3) ||
        is.null(admin_level) ||
        is.null(admin_name))) {
      stop(
        "`iso3`, `admin_level`, and `admin_name` are required when ",
        "`adjacency = NULL`.",
        call. = FALSE
      )
    }
    
    if (isTRUE(verbose)) {
      message(
        "Building BYM2 adjacency matrix for ",
        length(grid_ids),
        " grid cells."
      )
    }
    
    if (is_h3) {
      adjacency_builder <- build_h3_adjacency
      
      default_adjacency_args <- list(
        model = df,
        cellsize = cellsize,
        sparse = TRUE
      )
    } else {
      adjacency_builder <- build_grid_adjacency
      
      default_adjacency_args <- list(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        cellsize_m = cellsize,
        model = df,
        sparse = TRUE
      )
    }
    
    adjacency_args <- utils::modifyList(
      default_adjacency_args,
      adjacency_args
    )
    
    adjacency_matrix <- do.call(
      adjacency_builder,
      adjacency_args
    )
  } else {
    if (isTRUE(verbose)) {
      message("Using supplied adjacency matrix.")
    }
    
    adjacency_matrix <- adjacency
  }
  
  # ---------------------------------------------------------------------------
  # 4. Validate adjacency
  # ---------------------------------------------------------------------------
  
  if (!inherits(adjacency_matrix, "Matrix")) {
    adjacency_matrix <- Matrix::Matrix(
      adjacency_matrix,
      sparse = TRUE
    )
  }
  
  if (nrow(adjacency_matrix) != ncol(adjacency_matrix)) {
    stop(
      "Adjacency matrix must be square.",
      call. = FALSE
    )
  }
  
  if (is.null(rownames(adjacency_matrix)) ||
      is.null(colnames(adjacency_matrix))) {
    stop(
      "Adjacency matrix must have row and column names.",
      call. = FALSE
    )
  }
  
  missing_row_ids <- setdiff(
    grid_ids,
    rownames(adjacency_matrix)
  )
  
  missing_col_ids <- setdiff(
    grid_ids,
    colnames(adjacency_matrix)
  )
  
  if (length(missing_row_ids)) {
    stop(
      "Adjacency matrix rows are missing ",
      length(missing_row_ids),
      " grid identifiers. Examples: ",
      paste(
        utils::head(missing_row_ids, 10L),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  if (length(missing_col_ids)) {
    stop(
      "Adjacency matrix columns are missing ",
      length(missing_col_ids),
      " grid identifiers. Examples: ",
      paste(
        utils::head(missing_col_ids, 10L),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 5. Align and normalize adjacency
  # ---------------------------------------------------------------------------
  
  adjacency_aligned <- adjacency_matrix[
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
  
  adjacency_aligned <- Matrix::drop0(
    adjacency_aligned
  )
  
  dimnames(adjacency_aligned) <- list(
    grid_ids,
    grid_ids
  )
  
  isolated_grid_ids <- names(
    which(Matrix::rowSums(adjacency_aligned) == 0)
  )
  
  if (length(isolated_grid_ids)) {
    warning(
      length(isolated_grid_ids),
      " grid cells have no neighbours. Examples: ",
      paste(
        utils::head(isolated_grid_ids, 10L),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # 6. Add BYM2 information
  # ---------------------------------------------------------------------------
  
  obj <- prepared
  
  obj$model_data <- df
  obj$adjacency <- adjacency_aligned
  
  if (is.null(obj$meta)) {
    obj$meta <- list()
  }
  
  obj$meta$spatial_model <- "BYM2"
  obj$meta$adjacency_nrow <- nrow(adjacency_aligned)
  obj$meta$adjacency_ncol <- ncol(adjacency_aligned)
  obj$meta$adjacency_nonzero <- Matrix::nnzero(adjacency_aligned)
  obj$meta$isolated_grid_ids <- isolated_grid_ids
  obj$meta$remove_unused_cols <- remove_unused_cols
  
  class(obj) <- unique(c(
    "brms_bym2_data_prep",
    class(prepared)
  ))
  
  # ---------------------------------------------------------------------------
  # 7. Optionally write output
  # ---------------------------------------------------------------------------
  
  if (isTRUE(write)) {
    if (is.null(output_dir) || !nzchar(output_dir)) {
      stop(
        "`write = TRUE` requires a valid `output_dir`.",
        call. = FALSE
      )
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    slug <- obj$meta$slug
    
    if (is.null(slug) || !nzchar(slug)) {
      slug <- "custom"
    }
    
    resolution_suffix <- paste0(
      "_",
      temporal_resolution
    )
    
    output_path <- file.path(
      output_dir,
      sprintf(
        "model_prep_%s%s_brms_bym2_data.rds",
        slug,
        resolution_suffix
      )
    )
    
    saveRDS(
      obj,
      output_path
    )
    
    if (isTRUE(verbose)) {
      message(
        "Prepared brms BYM2 object written to ",
        output_path
      )
    }
  }
  
  obj
}