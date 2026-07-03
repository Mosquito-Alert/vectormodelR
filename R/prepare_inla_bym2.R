#' Prepare spatial data for an INLA BYM2 model
#'
#' Aligns prepared modelling data with a polygon grid, creates a stable integer
#' spatial index, constructs polygon-contiguity neighbours, writes an INLA graph,
#' and returns an object ready for fitting a BYM2 spatial model with R-INLA.
#'
#' This function is intended to be called after [prepare_model_data()].
#' It does not repeat filtering, temporal aggregation, factor conversion, or
#' predictor scaling.
#'
#' @param dataset A `model_data_prep` object, a legacy `brms_data_prep`
#'   object, a data frame, or a path to an RDS file containing one of these
#'   objects.
#' @param queen Logical. If `TRUE`, polygons sharing an edge or corner are
#'   neighbours. If `FALSE`, only polygons sharing an edge are neighbours.
#' @param snap Optional numeric snapping tolerance passed to
#'   `spdep::poly2nb()`. If `NULL`, the package default is used.
#' @param make_valid Logical. If `TRUE`, invalid polygon geometries are repaired
#'   using [sf::st_make_valid()].
#' @param drop_unused_grid Logical. If `TRUE`, polygons without model
#'   observations are removed before constructing the graph.
#' @param allow_islands Logical. Whether polygons with no neighbours are allowed.
#'   Defaults to `FALSE`.
#' @param allow_disconnected Logical. Whether more than one connected spatial
#'   component is allowed. Defaults to `TRUE`; a warning is issued when the graph
#'   contains multiple components.
#' @param data_dir Directory searched for the polygon grid file, located
#'   automatically from the prepared object's location slug and cell size.
#'   Defaults to `"data/proc"`.
#' @param output_dir Directory used when `write = TRUE`.
#' @param graph_filename Optional graph filename. If `NULL`, a filename is
#'   generated from the location slug and grid column.
#' @param write Logical. If `TRUE`, save the prepared object and retain the graph
#'   file in `output_dir`.
#' @param verbose Logical. Emit informative messages when `TRUE`.
#'
#' @return An object of class `inla_bym2_data_prep` containing:
#'   \item{model_data}{Prepared modelling data with `grid_index` and
#'     `component_id` columns.}
#'   \item{grid}{Ordered `sf` polygons with matching `grid_index` and
#'     `component_id` values.}
#'   \item{grid_col}{The grid identifier column.}
#'   \item{grid_index_col}{The name `"grid_index"`.}
#'   \item{node_map}{Mapping between original grid identifiers, INLA node
#'     indices, and connected components.}
#'   \item{neighbours}{An `nb` neighbourhood object.}
#'   \item{graph}{An INLA graph object.}
#'   \item{graph_path}{Persistent path to the adjacency graph when
#'     `write = TRUE`; otherwise `NULL`.}
#'   \item{meta}{Spatial preparation metadata.}
#'
#' @export
prepare_inla_bym2_data <- function(
    dataset,
    queen = TRUE,
    snap = NULL,
    make_valid = TRUE,
    drop_unused_grid = TRUE,
    allow_islands = FALSE,
    allow_disconnected = TRUE,
    data_dir = "data/proc",
    output_dir = "data/proc",
    graph_filename = NULL,
    write = FALSE,
    verbose = TRUE
) {
  # ---------------------------------------------------------------------------
  # 1. Dependencies
  # ---------------------------------------------------------------------------

  required_packages <- c(
    "sf",
    "spdep",
    "INLA"
  )

  missing_packages <- required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

  if (length(missing_packages)) {
    stop(
      "The following packages must be installed: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 2. Argument validation
  # ---------------------------------------------------------------------------

  validate_flag <- function(x, name) {
    if (!is.logical(x) ||
        length(x) != 1L ||
        is.na(x)) {
      stop(
        "`", name, "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }

  validate_flag(queen, "queen")
  validate_flag(make_valid, "make_valid")
  validate_flag(drop_unused_grid, "drop_unused_grid")
  validate_flag(allow_islands, "allow_islands")
  validate_flag(allow_disconnected, "allow_disconnected")
  validate_flag(write, "write")
  validate_flag(verbose, "verbose")

  if (!is.null(snap) &&
      (!is.numeric(snap) ||
       length(snap) != 1L ||
       is.na(snap) ||
       snap < 0)) {
    stop(
      "`snap` must be NULL or a non-negative numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.null(graph_filename) &&
      (!is.character(graph_filename) ||
       length(graph_filename) != 1L ||
       is.na(graph_filename) ||
       !nzchar(graph_filename))) {
    stop(
      "`graph_filename` must be NULL or a non-empty character scalar.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Load modelling data
  # ---------------------------------------------------------------------------

  dataset_path <- NULL

  if (is.character(dataset) &&
      length(dataset) == 1L &&
      nzchar(dataset)) {
    dataset_path <- dataset

    if (!file.exists(dataset_path)) {
      stop(
        "Dataset not found at: ",
        dataset_path,
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Loading prepared modelling data from: ",
        dataset_path
      )
    }

    dataset <- readRDS(dataset_path)
  }

  prepared_object <- NULL
  prepared_meta <- NULL
  location_slug <- NULL

  if (inherits(
    dataset,
    c(
      "model_data_prep",
      "brms_data_prep",
      "bym2_data_prep"
    )
  )) {
    prepared_object <- dataset
    model_data <- dataset$model_data
    prepared_meta <- dataset$meta

    if (!is.null(dataset$meta$slug)) {
      location_slug <- dataset$meta$slug
    }
  } else if (is.data.frame(dataset)) {
    model_data <- dataset

    location_slug <- attr(
      dataset,
      "location_slug",
      exact = TRUE
    )
  } else {
    stop(
      "`dataset` must be a prepared model-data object, ",
      "a data frame, or a path to an RDS file.",
      call. = FALSE
    )
  }

  if (!is.data.frame(model_data)) {
    stop(
      "The prepared object's `model_data` component must be a data frame.",
      call. = FALSE
    )
  }

  if (!nrow(model_data)) {
    stop(
      "No observations are available in `model_data`.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 4. Determine grid identifier column
  #
  # Consistent with `prepare_bym2_data()`: the grid identifier column is
  # resolved from the prepared object rather than supplied by the caller (see
  # `.resolve_bym2_grid_col()` in utils_bym2.R).
  # ---------------------------------------------------------------------------

  grid_col <- .resolve_bym2_grid_col(
    prepared_object = prepared_object,
    prepared_meta = prepared_meta,
    model_data = model_data
  )

  model_data[[grid_col]] <- .coerce_bym2_grid_ids(
    model_data[[grid_col]],
    grid_col = grid_col,
    context = "model_data"
  )

  observed_grid_ids <- unique(
    model_data[[grid_col]]
  )

  # ---------------------------------------------------------------------------
  # 5. Load polygon grid
  # ---------------------------------------------------------------------------

  cellsize_token <- sub("^grid_id_", "", grid_col)

  located_grid <- .locate_bym2_grid_file(
    location_slug = location_slug,
    cellsize_token = cellsize_token,
    data_dir = data_dir,
    verbose = verbose
  )

  grid_path <- located_grid$path
  grid <- located_grid$grid

  if (isTRUE(verbose)) {
    message("Derived polygon grid from: ", grid_path)
  }

  if (!inherits(grid, "sf")) {
    stop(
      "The located polygon grid is not an `sf` object.",
      call. = FALSE
    )
  }

  if (!nrow(grid)) {
    stop(
      "`grid` contains no polygon features.",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(grid))) {
    stop(
      "`grid` must have a defined coordinate reference system.",
      call. = FALSE
    )
  }

  geometry_types <- unique(
    as.character(sf::st_geometry_type(grid))
  )

  polygon_types <- c(
    "POLYGON",
    "MULTIPOLYGON"
  )

  if (!all(geometry_types %in% polygon_types)) {
    stop(
      "`grid` must contain only POLYGON or MULTIPOLYGON geometries. ",
      "Found: ",
      paste(geometry_types, collapse = ", "),
      call. = FALSE
    )
  }

  if (!grid_col %in% names(grid)) {
    stop(
      "Grid identifier column `",
      grid_col,
      "` is missing from the polygon grid.",
      call. = FALSE
    )
  }

  grid[[grid_col]] <- .coerce_bym2_grid_ids(
    grid[[grid_col]],
    grid_col = grid_col,
    context = "grid"
  )

  duplicated_grid_ids <- unique(
    grid[[grid_col]][duplicated(grid[[grid_col]])]
  )

  if (length(duplicated_grid_ids)) {
    stop(
      "The polygon grid contains duplicated identifiers in `",
      grid_col,
      "`: ",
      paste(
        utils::head(duplicated_grid_ids, 20L),
        collapse = ", "
      ),
      if (length(duplicated_grid_ids) > 20L) " ..." else "",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 6. Validate and repair geometries
  # ---------------------------------------------------------------------------

  empty_geometry <- sf::st_is_empty(grid)

  if (any(empty_geometry)) {
    stop(
      "The polygon grid contains ",
      sum(empty_geometry),
      " empty geometr",
      if (sum(empty_geometry) == 1L) "y." else "ies.",
      call. = FALSE
    )
  }

  validity <- sf::st_is_valid(grid)

  if (any(!validity)) {
    if (!isTRUE(make_valid)) {
      stop(
        "The polygon grid contains ",
        sum(!validity),
        " invalid geometr",
        if (sum(!validity) == 1L) "y." else "ies.",
        " Set `make_valid = TRUE` to repair them.",
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message(
        "Repairing ",
        sum(!validity),
        " invalid polygon geometr",
        if (sum(!validity) == 1L) "y." else "ies."
      )
    }

    grid <- sf::st_make_valid(grid)

    geometry_types_after <- unique(
      as.character(sf::st_geometry_type(grid))
    )

    if (!all(geometry_types_after %in% polygon_types)) {
      grid <- sf::st_collection_extract(
        grid,
        type = "POLYGON"
      )
    }

    if (any(sf::st_is_empty(grid))) {
      stop(
        "One or more geometries became empty after validity repair.",
        call. = FALSE
      )
    }

    geometry_types_after <- unique(
      as.character(sf::st_geometry_type(grid))
    )

    if (!all(geometry_types_after %in% polygon_types)) {
      stop(
        "Geometry repair did not produce exclusively polygon geometries. ",
        "Found: ",
        paste(geometry_types_after, collapse = ", "),
        call. = FALSE
      )
    }

    if (anyDuplicated(grid[[grid_col]])) {
      duplicated_after_repair <- unique(
        grid[[grid_col]][duplicated(grid[[grid_col]])]
      )

      stop(
        "Geometry repair produced duplicated grid identifiers: ",
        paste(
          utils::head(duplicated_after_repair, 20L),
          collapse = ", "
        ),
        if (length(duplicated_after_repair) > 20L) " ..." else "",
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Match model grid IDs to polygons
  # ---------------------------------------------------------------------------

  missing_grid_polygons <- setdiff(
    observed_grid_ids,
    grid[[grid_col]]
  )

  if (length(missing_grid_polygons)) {
    stop(
      length(missing_grid_polygons),
      " grid identifier",
      if (length(missing_grid_polygons) == 1L) "" else "s",
      " in `model_data` do not have matching polygons in `grid`: ",
      paste(
        utils::head(missing_grid_polygons, 20L),
        collapse = ", "
      ),
      if (length(missing_grid_polygons) > 20L) " ..." else "",
      call. = FALSE
    )
  }

  if (isTRUE(drop_unused_grid)) {
    grid <- grid[
      grid[[grid_col]] %in% observed_grid_ids,
      ,
      drop = FALSE
    ]
  } else if (isTRUE(verbose)) {
    n_unobserved_nodes <- sum(
      !grid[[grid_col]] %in% observed_grid_ids
    )

    message(
      "Retaining ",
      n_unobserved_nodes,
      " grid polygon",
      if (n_unobserved_nodes == 1L) "" else "s",
      " without model observations."
    )
  }

  if (!nrow(grid)) {
    stop(
      "No polygons remain after matching the grid to the model data.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 8. Establish stable node ordering
  # ---------------------------------------------------------------------------

  if ("grid_index" %in% names(grid) &&
      isTRUE(verbose)) {
    message(
      "Replacing existing polygon `grid_index` using stable graph ordering."
    )
  }

  ordered_grid_ids <- sort(
    unique(grid[[grid_col]]),
    na.last = NA
  )

  grid <- grid[
    match(
      ordered_grid_ids,
      grid[[grid_col]]
    ),
    ,
    drop = FALSE
  ]

  rownames(grid) <- NULL

  grid$grid_index <- seq_len(nrow(grid))

  node_map <- sf::st_drop_geometry(grid)[
    ,
    c(grid_col, "grid_index"),
    drop = FALSE
  ]

  node_map[[grid_col]] <- as.character(
    node_map[[grid_col]]
  )

  # ---------------------------------------------------------------------------
  # 9. Add graph index to model rows
  # ---------------------------------------------------------------------------

  if ("grid_index" %in% names(model_data) &&
      isTRUE(verbose)) {
    message(
      "Replacing existing model-data `grid_index` using current graph ordering."
    )
  }

  model_data$grid_index <- match(
    model_data[[grid_col]],
    node_map[[grid_col]]
  )

  if (anyNA(model_data$grid_index)) {
    stop(
      "Failed to assign `grid_index` to all model-data rows.",
      call. = FALSE
    )
  }

  model_data$grid_index <- as.integer(
    model_data$grid_index
  )

  # ---------------------------------------------------------------------------
  # 10. Build neighbourhood and INLA graph
  # ---------------------------------------------------------------------------

  spatial_graph <- .build_inla_spatial_graph(
    grid = grid,
    node_map = node_map,
    grid_col = grid_col,
    location_slug = location_slug,
    queen = queen,
    snap = snap,
    allow_islands = allow_islands,
    allow_disconnected = allow_disconnected,
    write = write,
    output_dir = output_dir,
    graph_filename = graph_filename,
    verbose = verbose
  )

  neighbours <- spatial_graph$neighbours
  neighbour_counts <- spatial_graph$neighbour_counts
  island_indices <- spatial_graph$island_indices
  island_grid_ids <- spatial_graph$island_grid_ids
  n_components <- spatial_graph$n_components
  component_membership <- spatial_graph$component_membership
  grid <- spatial_graph$grid
  node_map <- spatial_graph$node_map
  graph <- spatial_graph$graph
  graph_path <- spatial_graph$graph_path
  location_slug <- spatial_graph$location_slug
  safe_grid_token <- spatial_graph$safe_grid_token
  graph_is_temporary <- spatial_graph$graph_is_temporary

  # ---------------------------------------------------------------------------
  # 11. Add connected-component ID to model rows
  # ---------------------------------------------------------------------------

  model_data$component_id <- node_map$component_id[
    model_data$grid_index
  ]

  if (anyNA(model_data$component_id)) {
    stop(
      "Failed to assign spatial component IDs to all model-data rows.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # 12. Create return object
  # ---------------------------------------------------------------------------

  n_observed_grid_nodes <- length(
    unique(model_data[[grid_col]])
  )

  n_unobserved_grid_nodes <- nrow(node_map) -
    n_observed_grid_nodes

  meta <- list(
    slug = location_slug,
    source_dataset_path = dataset_path,
    source_grid_path = grid_path,
    temporal_resolution = if (!is.null(prepared_meta)) {
      prepared_meta$temporal_resolution
    } else {
      NULL
    },
    cellsize_m = if (!is.null(prepared_meta)) {
      prepared_meta$cellsize_m
    } else {
      NULL
    },
    grid_col = grid_col,
    grid_index_col = "grid_index",
    grid_crs = sf::st_crs(grid),
    queen = queen,
    snap = snap,
    make_valid = make_valid,
    drop_unused_grid = drop_unused_grid,
    allow_islands = allow_islands,
    allow_disconnected = allow_disconnected,
    n_model_rows = nrow(model_data),
    n_grid_nodes = nrow(node_map),
    n_observed_grid_nodes = n_observed_grid_nodes,
    n_unobserved_grid_nodes = n_unobserved_grid_nodes,
    n_directed_links = sum(neighbour_counts),
    n_undirected_links = sum(neighbour_counts) / 2,
    mean_neighbours = mean(neighbour_counts),
    min_neighbours = min(neighbour_counts),
    max_neighbours = max(neighbour_counts),
    n_islands = length(island_indices),
    island_grid_ids = island_grid_ids,
    n_components = n_components,
    component_sizes = as.integer(
      table(component_membership)
    ),
    graph_path = graph_path,
    graph_is_temporary = graph_is_temporary,
    created_at = Sys.time(),
    source_prepare_meta = prepared_meta
  )

  obj <- structure(
    list(
      model_data = model_data,
      grid = grid,
      grid_col = grid_col,
      grid_index_col = "grid_index",
      node_map = node_map,
      neighbours = neighbours,
      graph = graph,
      graph_path = graph_path,
      meta = meta
    ),
    class = c(
      "inla_bym2_data_prep",
      "model_data_prep"
    )
  )

  # ---------------------------------------------------------------------------
  # 13. Optionally save prepared spatial object
  # ---------------------------------------------------------------------------

  if (isTRUE(write)) {
    object_path <- file.path(
      output_dir,
      sprintf(
        "model_prep_%s_inla_bym2_%s.rds",
        location_slug,
        safe_grid_token
      )
    )

    obj$meta$output_path <- object_path
    attr(obj, "output_path") <- object_path

    saveRDS(
      obj,
      object_path
    )

    if (isTRUE(verbose)) {
      message(
        "Prepared INLA BYM2 object written to: ",
        object_path
      )

      message(
        "INLA adjacency graph written to: ",
        graph_path
      )
    }
  }

  if (isTRUE(verbose)) {
    message(
      "BYM2 spatial preparation complete: ",
      nrow(model_data),
      " model rows, ",
      nrow(node_map),
      " spatial nodes, ",
      sum(neighbour_counts) / 2,
      " undirected neighbour links, ",
      n_components,
      " connected component",
      if (n_components == 1L) "." else "s."
    )
  }

  obj
}


# -----------------------------------------------------------------------------
# Internal helper: construct neighbourhood and graph
# -----------------------------------------------------------------------------

.build_inla_spatial_graph <- function(
    grid,
    node_map,
    grid_col,
    location_slug,
    queen,
    snap,
    allow_islands,
    allow_disconnected,
    write,
    output_dir,
    graph_filename,
    verbose
) {
  if (isTRUE(verbose)) {
    message(
      "Constructing ",
      if (isTRUE(queen)) "queen" else "rook",
      " contiguity neighbours for ",
      nrow(grid),
      " grid polygons."
    )
  }

  poly2nb_args <- list(
    pl = grid,
    row.names = as.character(grid$grid_index),
    queen = queen
  )

  if (!is.null(snap)) {
    poly2nb_args$snap <- snap
  }

  neighbours <- do.call(
    spdep::poly2nb,
    poly2nb_args
  )

  expected_region_ids <- as.character(
    grid$grid_index
  )

  actual_region_ids <- attr(
    neighbours,
    "region.id"
  )

  if (!identical(actual_region_ids, expected_region_ids)) {
    stop(
      "Neighbour-list ordering does not match `grid_index`.",
      call. = FALSE
    )
  }

  neighbour_counts <- spdep::card(
    neighbours
  )

  island_indices <- which(
    neighbour_counts == 0L
  )

  island_grid_ids <- if (length(island_indices)) {
    node_map[[grid_col]][island_indices]
  } else {
    character(0)
  }

  if (length(island_indices)) {
    island_message <- paste0(
      length(island_indices),
      " grid polygon",
      if (length(island_indices) == 1L) "" else "s",
      " have no neighbours: ",
      paste(
        utils::head(island_grid_ids, 20L),
        collapse = ", "
      ),
      if (length(island_grid_ids) > 20L) " ..." else ""
    )

    if (!isTRUE(allow_islands)) {
      stop(
        island_message,
        "\nReview grid topology, `queen`, or `snap`, or set ",
        "`allow_islands = TRUE` deliberately.",
        call. = FALSE
      )
    }

    warning(
      island_message,
      call. = FALSE
    )
  }

  component_info <- spdep::n.comp.nb(
    neighbours
  )

  n_components <- component_info$nc
  component_membership <- component_info$comp.id

  node_map$component_id <- as.integer(
    component_membership
  )

  grid$component_id <- node_map$component_id

  if (n_components > 1L) {
    component_sizes <- sort(
      table(component_membership),
      decreasing = TRUE
    )

    component_message <- paste0(
      "The spatial graph contains ",
      n_components,
      " disconnected components. Component sizes: ",
      paste(component_sizes, collapse = ", "),
      "."
    )

    if (!isTRUE(allow_disconnected)) {
      stop(
        component_message,
        call. = FALSE
      )
    }

    warning(
      component_message,
      " BYM2 can account for disconnected components, but the topology ",
      "should be reviewed.",
      call. = FALSE
    )
  }

  graph_info <- .resolve_inla_graph_path(
    location_slug = location_slug,
    grid_col = grid_col,
    write = write,
    output_dir = output_dir,
    graph_filename = graph_filename
  )

  spdep::nb2INLA(
    file = graph_info$graph_path,
    nb = neighbours
  )

  if (!file.exists(graph_info$graph_path)) {
    stop(
      "The INLA graph file was not created successfully.",
      call. = FALSE
    )
  }

  graph <- INLA::inla.read.graph(
    graph_info$graph_path
  )

  if (!identical(
    as.integer(graph$n),
    as.integer(nrow(node_map))
  )) {
    stop(
      "INLA graph node count does not match the node mapping.",
      call. = FALSE
    )
  }

  retained_graph_path <- graph_info$graph_path

  if (isTRUE(graph_info$graph_is_temporary)) {
    unlink(
      graph_info$graph_path
    )

    retained_graph_path <- NULL
  }

  list(
    neighbours = neighbours,
    neighbour_counts = neighbour_counts,
    island_indices = island_indices,
    island_grid_ids = island_grid_ids,
    n_components = n_components,
    component_membership = component_membership,
    grid = grid,
    node_map = node_map,
    graph = graph,
    graph_path = retained_graph_path,
    graph_is_temporary = graph_info$graph_is_temporary,
    location_slug = graph_info$location_slug,
    safe_grid_token = graph_info$safe_grid_token
  )
}


# -----------------------------------------------------------------------------
# Internal helper: determine graph path
# -----------------------------------------------------------------------------

.resolve_inla_graph_path <- function(
    location_slug,
    grid_col,
    write,
    output_dir,
    graph_filename
) {
  if (is.null(location_slug) ||
      !is.character(location_slug) ||
      length(location_slug) != 1L ||
      is.na(location_slug) ||
      !nzchar(location_slug)) {
    location_slug <- "custom"
  }

  safe_grid_token <- gsub(
    "[^A-Za-z0-9_-]+",
    "_",
    grid_col
  )

  if (!is.null(graph_filename) &&
      !grepl(
        "\\.adj$",
        graph_filename,
        ignore.case = TRUE
      )) {
    graph_filename <- paste0(
      graph_filename,
      ".adj"
    )
  }

  if (isTRUE(write)) {
    if (is.null(output_dir) ||
        !is.character(output_dir) ||
        length(output_dir) != 1L ||
        is.na(output_dir) ||
        !nzchar(output_dir)) {
      stop(
        "`write = TRUE` requires a valid `output_dir`.",
        call. = FALSE
      )
    }

    dir.create(
      output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (is.null(graph_filename)) {
      graph_filename <- sprintf(
        "inla_graph_%s_%s.adj",
        location_slug,
        safe_grid_token
      )
    }

    graph_path <- file.path(
      output_dir,
      graph_filename
    )
  } else {
    graph_path <- tempfile(
      pattern = paste0(
        "inla_graph_",
        location_slug,
        "_",
        safe_grid_token,
        "_"
      ),
      fileext = ".adj"
    )
  }

  list(
    location_slug = location_slug,
    safe_grid_token = safe_grid_token,
    graph_path = graph_path,
    graph_is_temporary = !isTRUE(write)
  )
}