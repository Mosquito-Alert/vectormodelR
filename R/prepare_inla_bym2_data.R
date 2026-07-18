#' Add BYM2 spatial data to prepared INLA data
#'
#' Takes the output from [prepare_inla_data()], builds or accepts an adjacency
#' matrix, aligns it with the model grid cells, and creates the integer spatial
#' index and graph required by INLA's BYM2 model.
#'
#' @param dataset Object returned by [prepare_inla_data()].
#' @param cellsize_m Grid-cell size in metres.
#' @param adjacency Optional precomputed adjacency matrix. When `NULL`, it is
#'   created with [build_grid_adjacency()].
#' @param adjacency_args Additional arguments passed to
#'   [build_grid_adjacency()], such as `data_dir`.
#' @param iso3,admin_level,admin_name Optional location identifiers. Values are
#'   taken from the prepared object when available.
#' @param output_dir Directory used when `write = TRUE`.
#' @param write Whether to save the prepared object.
#' @param verbose Whether to emit progress messages.
#'
#' @return An `inla_bym2_data_prep` object containing the INLA-ready model data,
#'   adjacency matrix, spatial graph, and spatial index.
#'
#' @export
prepare_inla_bym2_data <- function(
    dataset,
    cellsize_m = 800,
    adjacency = NULL,
    adjacency_args = list(),
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    output_dir = "data/proc",
    write = FALSE,
    verbose = TRUE
) {
  # ---------------------------------------------------------------------------
  # 1. Check input
  # ---------------------------------------------------------------------------

  if (!inherits(dataset, "inla_data_prep")) {
    stop(
      "`dataset` must be an object returned by `prepare_inla_data()`.",
      call. = FALSE
    )
  }

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "Package `INLA` is required.",
      call. = FALSE
    )
  }

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop(
      "Package `Matrix` is required.",
      call. = FALSE
    )
  }

  if (!is.list(adjacency_args)) {
    stop(
      "`adjacency_args` must be a list.",
      call. = FALSE
    )
  }

  df <- dataset$model_data
  grid_col <- dataset$grid_col

  if (!is.data.frame(df) || !nrow(df)) {
    stop(
      "The prepared object does not contain valid model data.",
      call. = FALSE
    )
  }

  if (is.null(grid_col) ||
      !nzchar(grid_col) ||
      !grid_col %in% names(df)) {
    stop(
      "The prepared object does not contain a valid grid column.",
      call. = FALSE
    )
  }

  df[[grid_col]] <- as.character(
    df[[grid_col]]
  )

  grid_ids <- sort(
    unique(df[[grid_col]])
  )

  # ---------------------------------------------------------------------------
  # 2. Get location information
  # ---------------------------------------------------------------------------

  if (is.null(iso3)) {
    iso3 <- dataset$meta$iso3
  }

  if (is.null(admin_level)) {
    admin_level <- dataset$meta$admin_level
  }

  if (is.null(admin_name)) {
    admin_name <- dataset$meta$admin_name
  }

  # ---------------------------------------------------------------------------
  # 3. Build or use adjacency
  # ---------------------------------------------------------------------------

  if (is.null(adjacency)) {
    if (is.null(iso3) ||
        is.null(admin_level) ||
        is.null(admin_name)) {
      stop(
        "`iso3`, `admin_level`, and `admin_name` are required when ",
        "`adjacency = NULL`.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Building adjacency for ",
        length(grid_ids),
        " grid cells."
      )
    }

    build_args <- utils::modifyList(
      list(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        cellsize_m = cellsize_m,
        model = df,
        sparse = TRUE
      ),
      adjacency_args
    )

    adjacency <- do.call(
      build_grid_adjacency,
      build_args
    )
  } else if (isTRUE(verbose)) {
    message("Using supplied adjacency matrix.")
  }

  # ---------------------------------------------------------------------------
  # 4. Align adjacency
  # ---------------------------------------------------------------------------

  if (!inherits(adjacency, "Matrix")) {
    adjacency <- Matrix::Matrix(
      adjacency,
      sparse = TRUE
    )
  }

  if (is.null(rownames(adjacency)) ||
      is.null(colnames(adjacency))) {
    stop(
      "Adjacency matrix must have row and column names.",
      call. = FALSE
    )
  }

  missing_grid_ids <- setdiff(
    grid_ids,
    intersect(
      rownames(adjacency),
      colnames(adjacency)
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

  adjacency <- adjacency[
    grid_ids,
    grid_ids,
    drop = FALSE
  ]

  adjacency <- 1L * (
    (
      adjacency +
        Matrix::t(adjacency)
    ) > 0
  )

  diag(adjacency) <- 0
  adjacency <- Matrix::drop0(adjacency)

  dimnames(adjacency) <- list(
    grid_ids,
    grid_ids
  )

  isolated_grid_ids <- names(
    which(Matrix::rowSums(adjacency) == 0)
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
  # 5. Create the INLA spatial index and graph
  # ---------------------------------------------------------------------------

  spatial_lookup <- stats::setNames(
    seq_along(grid_ids),
    grid_ids
  )

  df$spatial_id <- unname(
    spatial_lookup[df[[grid_col]]]
  )

  if (anyNA(df$spatial_id)) {
    stop(
      "Failed to match observations to the spatial graph.",
      call. = FALSE
    )
  }

  spatial_graph <- INLA::inla.read.graph(
    adjacency
  )

  # ---------------------------------------------------------------------------
  # 6. Return object
  # ---------------------------------------------------------------------------

  obj <- dataset
  obj$model_data <- df
  obj$adjacency <- adjacency
  obj$spatial_graph <- spatial_graph

  if (is.null(obj$meta)) {
    obj$meta <- list()
  }

  obj$meta$spatial_model <- "BYM2"
  obj$meta$spatial_index <- "spatial_id"
  obj$meta$cellsize_m <- cellsize_m
  obj$meta$n_spatial_cells <- length(grid_ids)
  obj$meta$isolated_grid_ids <- isolated_grid_ids

  class(obj) <- unique(c(
    "inla_bym2_data_prep",
    class(dataset)
  ))

  # ---------------------------------------------------------------------------
  # 7. Optionally write output
  # ---------------------------------------------------------------------------

  if (isTRUE(write)) {
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

    temporal_resolution <- obj$meta$temporal_resolution

    if (is.null(temporal_resolution) ||
        !nzchar(temporal_resolution)) {
      temporal_resolution <- "daily"
    }

    output_path <- file.path(
      output_dir,
      sprintf(
        "model_prep_%s_%s_inla_bym2_data.rds",
        slug,
        temporal_resolution
      )
    )

    obj$meta$inla_bym2_output_path <- output_path

    saveRDS(
      obj,
      output_path
    )

    if (isTRUE(verbose)) {
      message(
        "Prepared INLA BYM2 data written to ",
        output_path
      )
    }
  }

  obj
}