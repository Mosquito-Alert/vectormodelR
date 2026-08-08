#' Build a Knorr-Held Type IV space-time interaction
#'
#' Creates a spatial-by-year interaction using a Besag spatial structure and
#' an RW1 temporal structure. Disconnected spatial components are handled
#' separately. Isolated cells are excluded from the structured interaction.
#'
#' @param adjacency Aligned spatial adjacency matrix.
#' @param spatial_graph INLA graph created from `adjacency`.
#' @param spatial_id Integer spatial index for each observation.
#' @param year_id Year index for each observation.
#'
#' @return A list containing the space-time index, precision matrix,
#'   constraints, and supporting metadata.
#'
#' @keywords internal
build_inla_type4 <- function(
    adjacency,
    spatial_graph,
    spatial_id,
    year_id
) {
  if (!inherits(adjacency, "Matrix")) {
    adjacency <- Matrix::Matrix(adjacency, sparse = TRUE)
  }

  if (length(spatial_id) != length(year_id)) {
    stop(
      "`spatial_id` and `year_id` must have the same length.",
      call. = FALSE
    )
  }

  if (anyNA(spatial_id) || anyNA(year_id)) {
    stop(
      "`spatial_id` and `year_id` cannot contain missing values.",
      call. = FALSE
    )
  }

  if (any(spatial_id < 1L | spatial_id > nrow(adjacency))) {
    stop(
      "`spatial_id` does not match the adjacency matrix.",
      call. = FALSE
    )
  }

  # Ensure that years are indexed consecutively from 1.
  year_levels <- sort(unique(year_id))
  year_id <- match(year_id, year_levels)
  n_years <- length(year_levels)

  component_id <- spatial_graph$cc$id[spatial_id]
  component_sizes <- lengths(spatial_graph$cc$nodes)

  # Isolated cells keep their ordinary BYM2 effect but do not receive a
  # structured space-time interaction.
  spatial_cells <- sort(
    unlist(
      spatial_graph$cc$nodes[component_sizes > 1L],
      use.names = FALSE
    )
  )

  space_time_id <- rep(NA_integer_, length(spatial_id))

  if (n_years < 2L || length(spatial_cells) == 0L) {
    return(list(
      year_id = year_id,
      component_id = component_id,
      space_time_id = space_time_id,
      precision = NULL,
      constraints = NULL,
      constraint_values = NULL,
      spatial_cells = spatial_cells,
      n_components = 0L,
      rankdef = 0L
    ))
  }

  n_space <- length(spatial_cells)

  interaction_spatial_id <- match(
    spatial_id,
    spatial_cells
  )

  has_interaction <- !is.na(interaction_spatial_id)

  space_time_id[has_interaction] <- as.integer(
    (year_id[has_interaction] - 1L) * n_space +
      interaction_spatial_id[has_interaction]
  )

  interaction_adjacency <- adjacency[
    spatial_cells,
    spatial_cells,
    drop = FALSE
  ]

  interaction_component <- spatial_graph$cc$id[spatial_cells]
  interaction_component <- match(
    interaction_component,
    unique(interaction_component)
  )

  n_components <- max(interaction_component)

  # Spatial Besag precision.
  spatial_precision <- Matrix::Diagonal(
    x = Matrix::rowSums(interaction_adjacency)
  ) - interaction_adjacency

  component_matrix <- Matrix::sparseMatrix(
    i = interaction_component,
    j = seq_len(n_space),
    x = 1,
    dims = c(n_components, n_space)
  )

  spatial_precision <- INLA::inla.scale.model(
    methods::as(
      spatial_precision,
      "CsparseMatrix"
    ),
    constr = list(
      A = component_matrix,
      e = rep(0, n_components)
    )
  )

  # Temporal RW1 precision.
  temporal_precision <- matrix(
    0,
    nrow = n_years,
    ncol = n_years
  )

  diag(temporal_precision) <- c(
    1,
    rep(2, max(0L, n_years - 2L)),
    1
  )

  for (i in seq_len(n_years - 1L)) {
    temporal_precision[i, i + 1L] <- -1
    temporal_precision[i + 1L, i] <- -1
  }

  temporal_precision <- INLA::inla.scale.model(
    methods::as(
      Matrix::Matrix(
        temporal_precision,
        sparse = TRUE
      ),
      "CsparseMatrix"
    ),
    constr = list(
      A = matrix(1, 1, n_years),
      e = 0
    )
  )

  # Type IV precision: spatial Besag multiplied by temporal RW1.
  precision <- methods::as(
    kronecker(
      temporal_precision,
      spatial_precision
    ),
    "CsparseMatrix"
  )

  time_index <- rep(
    seq_len(n_years),
    each = n_space
  )

  space_index <- rep(
    seq_len(n_space),
    times = n_years
  )

  interaction_column <- (
    time_index - 1L
  ) * n_space + space_index

  # Sum spatial interactions to zero within each component and year.
  constraints_by_year <- Matrix::sparseMatrix(
    i = (time_index - 1L) * n_components +
      interaction_component[space_index],
    j = interaction_column,
    x = 1,
    dims = c(
      n_years * n_components,
      n_years * n_space
    )
  )

  # Sum each cell's interaction to zero across years.
  constraints_by_cell <- Matrix::sparseMatrix(
    i = space_index,
    j = interaction_column,
    x = 1,
    dims = c(
      n_space,
      n_years * n_space
    )
  )

  # One cell constraint per component is redundant.
  redundant_constraints <- vapply(
    seq_len(n_components),
    function(component) {
      which(interaction_component == component)[1L]
    },
    integer(1)
  )

  constraints_by_cell <- constraints_by_cell[
    -redundant_constraints,
    ,
    drop = FALSE
  ]

  constraints <- rbind(
    constraints_by_year,
    constraints_by_cell
  )

  constraint_values <- rep(
    0,
    nrow(constraints)
  )

  list(
    year_id = year_id,
    component_id = component_id,
    space_time_id = space_time_id,
    precision = precision,
    constraints = constraints,
    constraint_values = constraint_values,
    spatial_cells = spatial_cells,
    n_components = n_components,
    rankdef = nrow(constraints)
  )
}