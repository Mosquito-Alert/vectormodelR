#' Build an adjacency matrix for H3 cells
#'
#' Uses the H3 identifiers in prepared model data to identify neighbouring
#' cells and returns a binary adjacency matrix.
#'
#' @param model Model data or a path to a prepared model RDS file.
#' @param cellsize H3 specification such as `"h3_9"`.
#' @param sparse Logical. Return a sparse matrix when `TRUE`.
#'
#' @return A binary adjacency matrix with H3 identifiers as row and column names.
#' @export
build_h3_adjacency <- function(
  model,
  cellsize = "h3_9",
  sparse = TRUE
) {
  if (!requireNamespace("h3jsr", quietly = TRUE)) {
    stop("Package `h3jsr` is required.", call. = FALSE)
  }

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package `Matrix` is required.", call. = FALSE)
  }

  grid_col <- resolve_grid_col(cellsize)

  if (!startsWith(grid_col, "h3_id_")) {
    stop(
      "`cellsize` must be an H3 specification such as `h3_9`.",
      call. = FALSE
    )
  }

  if (
    is.character(model) &&
      length(model) == 1L
  ) {
    if (!file.exists(model)) {
      stop(
        "Model dataset not found at ",
        model,
        call. = FALSE
      )
    }

    model_data <- readRDS(model)
  } else {
    model_data <- model
  }

  if (!grid_col %in% names(model_data)) {
    stop(
      "Model data must contain `",
      grid_col,
      "`.",
      call. = FALSE
    )
  }

  h3_ids <- sort(
    unique(
      as.character(
        model_data[[grid_col]]
      )
    )
  )

  h3_ids <- h3_ids[
    !is.na(h3_ids) &
      nzchar(h3_ids)
  ]

  if (length(h3_ids) < 2L) {
    stop(
      "At least two H3 cells are required to build adjacency.",
      call. = FALSE
    )
  }

  neighbours <- h3jsr::get_disk(
    h3_address = h3_ids,
    ring_size = 1,
    simple = TRUE
  )

  neighbour_positions <- lapply(
    neighbours,
    function(x) {
      match(
        intersect(x, h3_ids),
        h3_ids
      )
    }
  )

  row_index <- rep(
    seq_along(h3_ids),
    lengths(neighbour_positions)
  )

  col_index <- unlist(
    neighbour_positions,
    use.names = FALSE
  )

  adjacency <- Matrix::sparseMatrix(
    i = row_index,
    j = col_index,
    x = 1,
    dims = c(
      length(h3_ids),
      length(h3_ids)
    )
  )

  adjacency <- (
    adjacency +
      Matrix::t(adjacency)
  ) > 0

  adjacency <- Matrix::Matrix(
    adjacency * 1,
    sparse = TRUE
  )

  Matrix::diag(adjacency) <- 1

  dimnames(adjacency) <- list(
    h3_ids,
    h3_ids
  )

  row_totals <- Matrix::rowSums(adjacency)

  if (any(row_totals <= 1)) {
    isolated_ids <- names(row_totals)[
      row_totals <= 1
    ]

    warning(
      "Some H3 cells have no neighbours besides themselves. Example IDs: ",
      paste(
        utils::head(isolated_ids, 10L),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(sparse)) {
    adjacency <- as.matrix(adjacency)
  }

  adjacency
}