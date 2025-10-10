#' Retrieve GADM polygons using geodata::gadm()
#'
#' Fetches administrative polygons for a given country and level using
#' \pkg{geodata}. Optionally filters by a specific admin name
#' (e.g., "Barcelona" at level 2) and can return either all polygons or a single
#' unioned region.
#'
#' @param iso3 Character. ISO3 country code (e.g., "ESP").
#' @param level Integer. GADM administrative level (0 = country, 1 = region, 2 = province, ...).
#' @param name Character or NULL. Optional name or vector of names to filter by.
#'   Matching is case-insensitive and partial (substring).
#' @param path Character. Directory for caching downloaded data.
#'   Default is `"data/gadm"`.
#' @param rds Logical. If TRUE, save the returned object as an `.rds` file in
#'   `data/proc`. Default TRUE.
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
#' # 3) Multiple provinces without writing an RDS
#' cat <- get_gadm_data("ESP", level = 2, name = c("Barcelona", "Girona"), rds = FALSE)
#' }
#'
#' @importFrom geodata gadm
#' @importFrom sf st_as_sf st_make_valid st_is_empty st_transform
#' @importFrom readr write_rds
#' @export
get_gadm_data <- function(
  iso3,
  level,
  name    = NULL,
  path    = "data/gadm",
  rds     = TRUE,
  verbose = TRUE
) {
  stopifnot(is.character(iso3), nchar(iso3) == 3)
  stopifnot(is.numeric(level), length(level) == 1, level >= 0)

  if (!requireNamespace("geodata", quietly = TRUE))
    stop("Package {geodata} required. Please install it.")

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (verbose) message(sprintf("Fetching GADM for %s (level %d)...", iso3, level))

  # Download (if not cached)
  g <- geodata::gadm(country = iso3, level = level, path = path) |> sf::st_as_sf()

  # Fix geometry
  g <- sf::st_make_valid(g)
  g <- g[!sf::st_is_empty(g), , drop = FALSE]
  g <- sf::st_transform(g, 4326)

  # Optionally filter by name(s)
  if (!is.null(name)) {
    nm_col <- paste0("NAME_", level)
    if (!nm_col %in% names(g))
      stop("Expected column '", nm_col, "' not found in GADM level ", level)

    # Case-insensitive, partial match
    matches <- grepl(paste(name, collapse = "|"), g[[nm_col]], ignore.case = TRUE)
    g <- g[matches, , drop = FALSE]

    if (!nrow(g))
      stop("No matches found for name(s): ", paste(name, collapse = ", "))

    if (verbose)
      message(sprintf("Matched %d polygon(s) for %s.", nrow(g), paste(name, collapse = ", ")))
  }

  if (isTRUE(rds)) {
    proc_dir <- file.path("data", "proc")
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

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

    file_stem <- paste0("map_", tolower(iso3), "_", level, name_suffix, "_adm.rds")
    output_path <- file.path(proc_dir, file_stem)

    readr::write_rds(g, output_path)
    if (verbose) message("Saved RDS to ", output_path)
  }

  return(g)
}