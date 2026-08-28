#' Add BYM2 spatial data to prepared INLA data
#'
#' Takes the output from [prepare_inla_data()], builds or accepts an adjacency
#' matrix, and creates the spatial and space-time objects required by INLA.
#'
#' @param dataset Object returned by [prepare_inla_data()].
#' @param cellsize Numeric hex-grid cell size in metres, or an H3 specification
#'   such as `"h3_9"`.
#' @param adjacency Optional precomputed adjacency matrix. When `NULL`, it is
#'   created with [build_grid_adjacency()] or [build_h3_adjacency()].
#' @param adjacency_args Additional arguments passed to the selected adjacency
#'   builder.
#' @param iso3,admin_level,admin_name Optional location identifiers.
#' @param output_dir Directory used when `write = TRUE`.
#' @param write Whether to save the prepared object.
#' @param verbose Whether to emit progress messages.
#'
#' @return An `inla_bym2_data_prep` object containing the model data,
#'   adjacency matrix, spatial graph, and space-time interaction objects.
#'
#' @export
prepare_inla_bym2_data <- function(
  dataset,
  cellsize = 800,
  adjacency = NULL,
  adjacency_args = list(),
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  output_dir = "data/proc",
  write = FALSE,
  verbose = TRUE
) {
  if (!inherits(dataset, "inla_data_prep")) {
    stop(
      "`dataset` must be returned by `prepare_inla_data()`.",
      call. = FALSE
    )
  }

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package `INLA` is required.", call. = FALSE)
  }

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package `Matrix` is required.", call. = FALSE)
  }

  if (!is.list(adjacency_args)) {
    stop("`adjacency_args` must be a list.", call. = FALSE)
  }

  df <- dataset$model_data
  grid_col <- dataset$grid_col
  is_h3 <- startsWith(grid_col, "h3_id_")

  if (!is.data.frame(df) || nrow(df) == 0L) {
    stop(
      "The prepared object does not contain model data.",
      call. = FALSE
    )
  }

  if (is.null(grid_col) || !grid_col %in% names(df)) {
    stop(
      "The prepared object does not contain a valid grid column.",
      call. = FALSE
    )
  }

  if (!"year_id" %in% names(df)) {
    stop(
      "`model_data` must contain `year_id`.",
      call. = FALSE
    )
  }

  df[[grid_col]] <- as.character(df[[grid_col]])
  grid_ids <- sort(unique(df[[grid_col]]))

  if (is.null(iso3)) {
    iso3 <- dataset$meta$iso3
  }

  if (is.null(admin_level)) {
    admin_level <- dataset$meta$admin_level
  }

  if (is.null(admin_name)) {
    admin_name <- dataset$meta$admin_name
  }

  if (is.null(adjacency)) {
    if (
      !is_h3 &&
        (
          is.null(iso3) ||
            is.null(admin_level) ||
            is.null(admin_name)
        )
    ) {
      stop(
        "`iso3`, `admin_level`, and `admin_name` are required for hex adjacency.",
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

    if (is_h3) {
      adjacency_builder <- build_h3_adjacency

      default_args <- list(
        model = df,
        cellsize = cellsize,
        sparse = TRUE
      )
    } else {
      adjacency_builder <- build_grid_adjacency

      default_args <- list(
        iso3 = iso3,
        admin_level = admin_level,
        admin_name = admin_name,
        cellsize_m = cellsize,
        model = df,
        sparse = TRUE
      )
    }

    build_args <- utils::modifyList(
      default_args,
      adjacency_args
    )

    adjacency <- do.call(
      adjacency_builder,
      build_args
    )
  } else if (isTRUE(verbose)) {
    message("Using supplied adjacency matrix.")
  }

  if (!inherits(adjacency, "Matrix")) {
    adjacency <- Matrix::Matrix(
      adjacency,
      sparse = TRUE
    )
  }

  if (
    is.null(rownames(adjacency)) ||
      is.null(colnames(adjacency))
  ) {
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

  if (length(missing_grid_ids) > 0L) {
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

  isolated_grid_ids <- grid_ids[
    Matrix::rowSums(adjacency) == 0
  ]

  if (length(isolated_grid_ids) > 0L) {
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

  # Create the spatial index and INLA graph.
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

  # Build the grid-by-year Type IV interaction.
  space_time <- build_inla_type4(
    adjacency = adjacency,
    spatial_graph = spatial_graph,
    spatial_id = df$spatial_id,
    year_id = df$year_id
  )

  df$year_id <- space_time$year_id
  df$component_id <- space_time$component_id
  df$space_time_id <- space_time$space_time_id

  obj <- dataset
  obj$model_data <- df
  obj$adjacency <- adjacency
  obj$spatial_graph <- spatial_graph

  obj$space_time_precision <- space_time$precision
  obj$space_time_constraints <- space_time$constraints
  obj$space_time_constraint_values <-
    space_time$constraint_values
  obj$space_time_rankdef <- space_time$rankdef
  obj$space_time_spatial_ids <-
    grid_ids[space_time$spatial_cells]

  if (is.null(obj$meta)) {
    obj$meta <- list()
  }

  obj$meta$spatial_model <- "BYM2"
  obj$meta$spatial_index <- "spatial_id"
  obj$meta$space_time_model <- "Type IV"
  obj$meta$space_time_index <- "space_time_id"
  obj$meta$cellsize <- cellsize
  obj$meta$n_spatial_cells <- length(grid_ids)
  obj$meta$n_space_time_cells <-
    length(space_time$spatial_cells)
  obj$meta$n_spatial_components <-
    spatial_graph$cc$n
  obj$meta$n_space_time_components <-
    space_time$n_components
  obj$meta$n_years <- length(unique(df$year_id))
  obj$meta$isolated_grid_ids <- isolated_grid_ids

  class(obj) <- unique(c(
    "inla_bym2_data_prep",
    class(dataset)
  ))

  if (isTRUE(verbose)) {
    message(
      "Prepared BYM2 data with ",
      obj$meta$n_spatial_cells,
      " spatial cells."
    )

    if (!is.null(space_time$precision)) {
      message(
        "Prepared Type IV interaction for ",
        obj$meta$n_space_time_cells,
        " cells across ",
        obj$meta$n_years,
        " years."
      )
    } else {
      message(
        "Type IV interaction was not created because at least two years ",
        "and one connected group of cells are required."
      )
    }
  }

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

    if (
      is.null(temporal_resolution) ||
        !nzchar(temporal_resolution)
    ) {
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