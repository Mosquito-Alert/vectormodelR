#' Build an H3 grid for a supplied polygon
#'
#' Creates an H3 grid at a specified resolution and returns the cells as an
#' `sf` object. Cells are selected when their centres fall inside the supplied
#' polygon.
#'
#' H3 uses integer resolutions from 0 to 15 rather than exact cell sizes in
#' metres. By default, complete H3 geometries are retained so their identifiers,
#' boundaries, and neighbour relationships remain valid.
#'
#' @param map Optional `sf` object describing the polygon(s) to cover.
#' @param resolution Integer H3 resolution from 0 to 15. Defaults to 9.
#' @param clip Logical. If `TRUE`, clips cells to the polygon boundary.
#'   Defaults to `FALSE`.
#' @param return_crs Coordinate reference system for the returned grid.
#'   Defaults to EPSG:4326.
#' @param write Logical. If `TRUE`, writes the grid to `data_dir`.
#' @param iso3 Optional ISO3 country code used with `admin_level` and
#'   `admin_name` when `map` is not supplied.
#' @param admin_level Optional administrative level.
#' @param admin_name Optional administrative unit name.
#' @param data_dir Directory containing boundary inputs and grid outputs.
#' @param verbose Logical. If `TRUE`, reports progress and file paths.
#'
#' @return An `sf` object containing `h3_id`, `grid_id`,
#'   `h3_id_<resolution>`, `h3_resolution`, and polygon geometry.
#'
#' @examples
#' \dontrun{
#' h3_grid <- build_h3_grid(
#'   iso3 = "ESP",
#'   admin_level = 4,
#'   admin_name = "Barcelona",
#'   resolution = 9,
#'   write = TRUE
#' )
#'
#' h3_grid <- build_h3_grid(
#'   map = bcn_poly,
#'   resolution = 9,
#'   clip = FALSE,
#'   write = FALSE
#' )
#' }
#'
#' @export
build_h3_grid <- function(
    map = NULL,
    resolution = 9L,
    clip = FALSE,
    return_crs = 4326,
    write = TRUE,
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL,
    data_dir = "data/proc",
    verbose = TRUE
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package `sf` is required.",
      call. = FALSE
    )
  }

  if (!requireNamespace("h3jsr", quietly = TRUE)) {
    stop(
      "Package `h3jsr` is required.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(resolution) ||
      length(resolution) != 1L ||
      is.na(resolution) ||
      resolution != as.integer(resolution) ||
      resolution < 0L ||
      resolution > 15L
  ) {
    stop(
      "`resolution` must be an integer from 0 to 15.",
      call. = FALSE
    )
  }

  logical_args <- list(
    clip = clip,
    write = write,
    verbose = verbose
  )

  for (argument_name in names(logical_args)) {
    value <- logical_args[[argument_name]]

    if (
      !is.logical(value) ||
        length(value) != 1L ||
        is.na(value)
    ) {
      stop(
        "`",
        argument_name,
        "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }

  resolution <- as.integer(resolution)

  if (!dir.exists(data_dir)) {
    dir.create(
      data_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  ids <- NULL
  map_path <- NULL

  if (!is.null(map)) {
    map <- sf::st_as_sf(map)

    if (isTRUE(verbose)) {
      message("Using the supplied polygon.")
    }
  } else {
    if (
      is.null(iso3) ||
        is.null(admin_level) ||
        is.null(admin_name)
    ) {
      stop(
        "Supply either `map` or `iso3`, `admin_level`, and ",
        "`admin_name`.",
        call. = FALSE
      )
    }

    ids <- build_location_identifiers(
      iso3,
      admin_level,
      admin_name
    )

    map_path <- file.path(
      data_dir,
      sprintf(
        "spatial_%s_adm.Rds",
        ids$slug
      )
    )

    if (!file.exists(map_path)) {
      stop(
        "Boundary file not found at ",
        map_path,
        call. = FALSE
      )
    }

    if (isTRUE(verbose)) {
      message("Loading boundary from: ", map_path)
    }

    map <- readRDS(map_path)
    map <- sf::st_as_sf(map)
  }

  if (
    is.null(ids) &&
      !is.null(iso3) &&
      !is.null(admin_level) &&
      !is.null(admin_name)
  ) {
    ids <- build_location_identifiers(
      iso3,
      admin_level,
      admin_name
    )
  }

  if (is.na(sf::st_crs(map))) {
    stop(
      "`map` must have a coordinate reference system.",
      call. = FALSE
    )
  }

  map <- sf::st_make_valid(map)
  map <- map[
    !sf::st_is_empty(map),
    ,
    drop = FALSE
  ]

  if (nrow(map) == 0L) {
    stop(
      "Input map is empty.",
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message(
      "Building H3 grid at resolution ",
      resolution,
      "."
    )

    if (is.null(return_crs)) {
      message(
        "Grid will remain in the H3 coordinate system: EPSG:4326."
      )
    } else {
      message(
        "Grid will be returned in CRS: ",
        return_crs,
        "."
      )
    }
  }

  # H3 expects longitude and latitude in EPSG:4326.
  map_wgs84 <- sf::st_transform(
    map,
    4326
  )

  perimeter <- sf::st_union(
    sf::st_geometry(map_wgs84)
  )

  perimeter <- sf::st_sf(
    geometry = perimeter
  )

  if (isTRUE(verbose)) {
    message(
      "Finding H3 cells whose centres fall inside the polygon."
    )
  }

  h3_ids <- h3jsr::polygon_to_cells(
    geometry = perimeter,
    res = resolution,
    simple = TRUE
  )

  h3_ids <- sort(
    unique(
      unlist(
        h3_ids,
        use.names = FALSE
      )
    )
  )

  h3_ids <- h3_ids[
    !is.na(h3_ids) &
      nzchar(h3_ids)
  ]

  if (length(h3_ids) == 0L) {
    stop(
      "No H3 cells were found. Try a finer H3 resolution.",
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message(
      "Found ",
      length(h3_ids),
      " H3 cells."
    )

    message(
      "Converting H3 identifiers to polygon geometries."
    )
  }

  cell_geometry <- h3jsr::cell_to_polygon(
    input = h3_ids,
    simple = TRUE
  )

  grid <- sf::st_sf(
    h3_id = h3_ids,
    grid_id = h3_ids,
    geometry = cell_geometry
  )

  resolution_col <- paste0(
    "h3_id_",
    resolution
  )

  grid[[resolution_col]] <- grid$h3_id
  grid$h3_resolution <- resolution

  if (isTRUE(clip)) {
    if (isTRUE(verbose)) {
      message(
        "Clipping boundary cells to the supplied polygon."
      )

      message(
        "Clipped boundary geometries will no longer be complete H3 cells."
      )
    }

    grid <- suppressWarnings(
      sf::st_intersection(
        grid,
        perimeter
      )
    )

    grid <- grid[
      !sf::st_is_empty(grid),
      ,
      drop = FALSE
    ]
  } else if (isTRUE(verbose)) {
    message(
      "Keeping complete H3 cell geometries."
    )
  }

  if (!is.null(return_crs)) {
    if (isTRUE(verbose) && !identical(return_crs, 4326)) {
      message(
        "Transforming grid to CRS: ",
        return_crs,
        "."
      )
    }

    grid <- sf::st_transform(
      grid,
      return_crs
    )
  }

  attr(grid, "h3_resolution") <- resolution
  attr(grid, "grid_type") <- "h3"

  if (isTRUE(write)) {
    if (is.null(ids)) {
      stop(
        "Automatic output naming requires `iso3`, `admin_level`, ",
        "and `admin_name`.",
        call. = FALSE
      )
    }

    output_path <- file.path(
      data_dir,
      sprintf(
        "spatial_%s_h3_grid_%s.Rds",
        ids$slug,
        resolution
      )
    )

    if (isTRUE(verbose)) {
      message(
        "Writing H3 grid to: ",
        output_path
      )
    }

    saveRDS(
      grid,
      output_path
    )

    attr(grid, "output_path") <- output_path

    if (isTRUE(verbose)) {
      message(
        "H3 grid written successfully."
      )
    }
  } else if (isTRUE(verbose)) {
    message(
      "H3 grid was not written because `write = FALSE`."
    )
  }

  if (isTRUE(verbose)) {
    message(
      "Completed H3 grid with ",
      nrow(grid),
      " cells at resolution ",
      resolution,
      "."
    )

    message(
      "Identifier columns: `h3_id`, `grid_id`, and `",
      resolution_col,
      "`."
    )
  }

  grid
}