#' Prepare data and adjacency matrix for BYM2 brms modelling
#'
#' Cleans and scales model data using [prepare_brms_data()], then builds or
#' aligns a spatial adjacency matrix for BYM2 modelling.
#'
#' This function is a spatial wrapper around [prepare_brms_data()]. Use
#' [prepare_brms_data()] for non-spatial brms models, and use
#' `prepare_bym2_data()` when the model requires a BYM2/CAR spatial component.
#'
#' @inheritParams prepare_brms_data
#' @param adjacency Optional pre-computed adjacency matrix. If `NULL`, one is
#'   built using [build_grid_adjacency()].
#' @param adjacency_args List of additional arguments passed to
#'   [build_grid_adjacency()] when `adjacency = NULL`.
#'
#' @return A list of class `bym2_data_prep` containing:
#'   \item{model_data}{The filtered, scaled data.frame ready for brms.}
#'   \item{adjacency}{The aligned sparse adjacency matrix corresponding to `model_data`.}
#'   \item{grid_col}{The name of the grid identifier column used.}
#'   \item{scaling}{A list of mean/sd values used for scaling predictors.}
#'   \item{meta}{Metadata including iso3, admin_level, admin_name, slug, source path, and temporal resolution.}
#'
#' @export
prepare_bym2_data <- function(
    dataset,
    cellsize_m = 800,
    temporal_resolution = c("daily", "hourly"),
    vars_to_check = NULL,
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
  # 1. Prepare the regular brms modelling data
  # ---------------------------------------------------------------------------

  prepared <- prepare_brms_data(
    dataset = dataset,
    cellsize_m = cellsize_m,
    temporal_resolution = temporal_resolution,
    vars_to_check = vars_to_check,
    iso3 = iso3,
    admin_level = admin_level,
    admin_name = admin_name,
    output_dir = output_dir,
    write = FALSE,
    verbose = verbose
  )

  df <- prepared$model_data
  grid_col <- prepared$grid_col

  if (!is.data.frame(df)) {
    stop("`prepare_brms_data()` did not return a valid `model_data` data.frame.", call. = FALSE)
  }

  if (is.null(grid_col) || !nzchar(grid_col)) {
    stop("`prepare_brms_data()` did not return a valid `grid_col`.", call. = FALSE)
  }

  if (!grid_col %in% names(df)) {
    stop("Grid column `", grid_col, "` was not found in prepared model data.", call. = FALSE)
  }

  if (!nrow(df)) {
    stop("Prepared model data has zero rows.", call. = FALSE)
  }

  df[[grid_col]] <- as.character(df[[grid_col]])

  grid_ids <- sort(unique(df[[grid_col]]))

  if (!length(grid_ids)) {
    stop("No grid identifiers found in prepared model data.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # 2. Build or use supplied adjacency matrix
  # ---------------------------------------------------------------------------

  if (is.null(adjacency)) {
    if (is.null(iso3) || is.null(admin_level) || is.null(admin_name)) {
      stop(
        "`iso3`, `admin_level`, and `admin_name` are required to build adjacency ",
        "when `adjacency = NULL`.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Building BYM2 adjacency matrix for ", length(grid_ids), " grid cells.")
    }

    default_adjacency_args <- list(
      iso3 = iso3,
      admin_level = admin_level,
      admin_name = admin_name,
      cellsize_m = cellsize_m,
      model = df,
      sparse = TRUE
    )

    adjacency_args <- utils::modifyList(
      default_adjacency_args,
      adjacency_args
    )

    adjacency_matrix <- do.call(build_grid_adjacency, adjacency_args)

  } else {
    if (isTRUE(verbose)) {
      message("Using supplied adjacency matrix.")
    }

    adjacency_matrix <- adjacency
  }

  # ---------------------------------------------------------------------------
  # 3. Validate and coerce adjacency matrix
  # ---------------------------------------------------------------------------

  if (!inherits(adjacency_matrix, "Matrix")) {
    adjacency_matrix <- Matrix::Matrix(adjacency_matrix, sparse = TRUE)
  }

  if (is.null(rownames(adjacency_matrix))) {
    stop("Adjacency matrix must have rownames matching grid identifiers.", call. = FALSE)
  }

  if (is.null(colnames(adjacency_matrix))) {
    stop("Adjacency matrix must have colnames matching grid identifiers.", call. = FALSE)
  }

  row_ids <- rownames(adjacency_matrix)
  col_ids <- colnames(adjacency_matrix)

  missing_row_ids <- setdiff(grid_ids, row_ids)
  missing_col_ids <- setdiff(grid_ids, col_ids)

  if (length(missing_row_ids)) {
    stop(
      "Adjacency matrix rownames are missing ",
      length(missing_row_ids),
      " grid identifiers found in model data. Examples: ",
      paste(utils::head(missing_row_ids, 10), collapse = ", "),
      call. = FALSE
    )
  }

  if (length(missing_col_ids)) {
    stop(
      "Adjacency matrix colnames are missing ",
      length(missing_col_ids),
      " grid identifiers found in model data. Examples: ",
      paste(utils::head(missing_col_ids, 10), collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Align adjacency matrix to model data grid IDs
  # ---------------------------------------------------------------------------

  adjacency_aligned <- adjacency_matrix[grid_ids, grid_ids, drop = FALSE]

  if (!all(rownames(adjacency_aligned) == grid_ids)) {
    stop("Adjacency matrix row alignment failed.", call. = FALSE)
  }

  if (!all(colnames(adjacency_aligned) == grid_ids)) {
    stop("Adjacency matrix column alignment failed.", call. = FALSE)
  }

  # brms CAR/BYM2 structures expect symmetric adjacency.
  if (!Matrix::isSymmetric(adjacency_aligned)) {
    if (isTRUE(verbose)) {
      message("Adjacency matrix is not symmetric; symmetrising it.")
    }

    adjacency_aligned <- (adjacency_aligned + Matrix::t(adjacency_aligned)) / 2
  }

  adjacency_aligned <- Matrix::drop0(adjacency_aligned)

  # ---------------------------------------------------------------------------
  # 5. Add BYM2 fields to prepared object
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

  class(obj) <- unique(c("bym2_data_prep", class(prepared)))

  # ---------------------------------------------------------------------------
  # 6. Optional write to disk
  # ---------------------------------------------------------------------------

  if (isTRUE(write)) {
    if (is.null(output_dir) || !nzchar(output_dir)) {
      stop("`write = TRUE` requires a valid `output_dir`.", call. = FALSE)
    }

    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    slug <- obj$meta$slug

    if (is.null(slug) || !nzchar(slug)) {
      slug <- "custom"
    }

    resolution_suffix <- if (identical(temporal_resolution, "hourly")) {
      "_hourly"
    } else {
      "_daily"
    }

    output_path <- file.path(
      output_dir,
      sprintf("model_prep_%s%s_bym2_data.rds", slug, resolution_suffix)
    )

    saveRDS(obj, output_path)

    if (isTRUE(verbose)) {
      message("Prepared brms/BYM2 object written to ", output_path)
    }
  }

  obj
}