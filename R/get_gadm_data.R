#' Retrieve GADM polygons using geodata::gadm()
#'
#' Fetches administrative polygons for a given country and level using
#' \pkg{geodata}. Optionally filters by a specific admin name
#' (e.g., "Barcelona" at level 2) and can return either all polygons or a single
#' unioned region. Matching behaviour can be toggled between substring search
#' and exact name matching.
#'
#' @param iso3 Character. ISO3 country code (e.g., "ESP").
#' @param level Integer. GADM administrative level (0 = country, 1 = region, 2 = province, ...).
#' @param name Character or NULL. Optional name or vector of names to filter by.
#'   Matching defaults to case-insensitive substring search but can be changed
#'   via `match_type`.
#' @param path Character. Directory for caching downloaded data.
#'   Default is `"data/gadm"`.
#' @param rds Logical. If TRUE, save the returned object as an `.rds` file in
#'   `data/proc`. Default TRUE.
#' @param perimeter Logical. If TRUE, augment the result with perimeter and area
#'   metrics via [get_adm_perimeter()]. Default FALSE.
#' @param union Logical. If TRUE, union matched polygons into a single
#'   multipolygon (useful for masking rasters). Default FALSE.
#' @param match_type Character. Matching mode for `name`. Use "exact" for
#'   case-insensitive equality or "contains" (default) for substring matches.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return An `sf` polygon layer (EPSG:4326).
#'
#' @examples
#' \dontrun{
#' # 1) Whole country of Spain
#' esp <- get_gadm_data("ESP", level = 0)
#'
#' # 2) Barcelona province (level 2)
#' barca <- get_gadm_data("ESP", level = 2, name = "Barcelona")
#'
#' # 3) Multiple provinces while also computing perimeter metrics
#' cat <- get_gadm_data(
#'   "ESP",
#'   level = 2,
#'   name = c("Barcelona", "Girona"),
#'   rds = FALSE,
#'   perimeter = TRUE
#' )
#' }
#'
#' @importFrom geodata gadm
#' @importFrom sf st_as_sf st_make_valid st_is_empty st_transform st_union
#' @importFrom readr write_rds
#' @export
get_gadm_data <- function(
  iso3,
  level,
  name = NULL,
  path = "data/gadm",
  rds = TRUE,
  perimeter = FALSE,
  union = FALSE,
  match_type = c("exact", "contains"),
  verbose = TRUE
) {
  stopifnot(is.character(iso3), nchar(iso3) == 3)
  stopifnot(is.numeric(level), length(level) == 1, level >= 0)

  match_type <- match.arg(match_type)

  if (!requireNamespace("geodata", quietly = TRUE))
    stop("Package {geodata} required. Please install it.", call. = FALSE)

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(verbose)) message(sprintf("Fetching GADM for %s (level %d)...", iso3, level))

  g <- geodata::gadm(country = iso3, level = level, path = path) |> sf::st_as_sf()

  g <- sf::st_make_valid(g)
  g <- g[!sf::st_is_empty(g), , drop = FALSE]
  g <- sf::st_transform(g, 4326)

  if (!is.null(name)) {
    nm_col <- paste0("NAME_", level)
    if (!nm_col %in% names(g))
      stop("Expected column '", nm_col, "' not found in GADM level ", level, call. = FALSE)

    x <- g[[nm_col]]
    name <- as.character(name)
    name <- name[!is.na(name)]

    if (!length(name)) {
      stop("`name` vector contains no non-missing values.", call. = FALSE)
    }

    if (identical(match_type, "exact")) {
      keep <- tolower(trimws(x)) %in% tolower(trimws(name))
    } else {
      keep <- rep(FALSE, length(x))
      for (term in name) {
        keep <- keep | grepl(term, x, ignore.case = TRUE, fixed = TRUE)
      }
    }

    g <- g[keep, , drop = FALSE]

    if (!nrow(g))
      stop("No matches found for name(s): ", paste(name, collapse = ", "), call. = FALSE)

    if (isTRUE(verbose))
      message(sprintf("Matched %d polygon(s) for %s.", nrow(g), paste(name, collapse = ", ")))
  }

  if (isTRUE(union) && nrow(g) > 1) {
    geom <- sf::st_union(g)
    g <- sf::st_sf(geometry = geom)
    g$union_id <- 1L
  }

  sanitize <- function(x) {
    x <- gsub("[^A-Za-z0-9]+", "-", tolower(x))
    x[nzchar(x)]
  }

  name_suffix <- ""
  if (!is.null(name) && length(name)) {
    clean_names <- sanitize(name)
    if (length(clean_names)) {
      name_suffix <- paste0("_", paste(clean_names, collapse = "-"))
    }
  }

  if (isTRUE(rds)) {
    proc_dir <- file.path("data", "proc")
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

    file_stem <- paste0("spatial_", tolower(iso3), "_", level, name_suffix, "_adm.rds")
    output_path <- file.path(proc_dir, file_stem)

    readr::write_rds(g, output_path)
    if (isTRUE(verbose)) message("Saved RDS to ", output_path)
  }

  if (isTRUE(perimeter)) {
    perim_filename <- paste0("spatial_", tolower(iso3), "_", level, name_suffix, "_perimeter.rds")
    get_adm_perimeter(
      sf_obj = g,
      output_filename = perim_filename,
      rds = TRUE,
      proc_dir = file.path("data", "proc"),
      verbose = verbose
    )
  }

  g
}