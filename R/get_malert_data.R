#' Download Mosquito Alert report data from GitHub or Zenodo
#'
#' @param source String. Source to download from. Options are "github" or "zenodo".
#' @param doi String. Zenodo DOI if downloading from Zenodo.
#'   Default is the DOI that will always point to the most recent version:
#'   10.5281/zenodo.597466.
#' @param iso3 Optional three-letter ISO code used to locate a perimeter file for
#'   spatial filtering.
#' @param admin_level Optional administrative level associated with the
#'   perimeter file.
#' @param admin_name Optional administrative unit name associated with the
#'   perimeter file.
#' @param desired_cols Optional character vector (or list) of column names to
#'   retain after spatial filtering. When `NULL`, all available columns are kept.
#'
#' The function always writes the raw combined download to
#' `data/vector/vector_global_malert.Rds`. When a perimeter is supplied, the
#' filtered output (after column selection) is persisted to
#' `data/proc/vector_<iso3>_<admin_level>_<admin_name>_malert.Rds`.
#' When spatial filtering is applied, records are further restricted to
#' `type == "adult"` when that column is available.
#'
#' @returns A tibble of Mosquito Alert reports (filtered and column-selected if
#'   the optional perimeter inputs are supplied).
#' @export
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tibble as_tibble
#' @importFrom lubridate year today ymd ymd_hms
#' @importFrom stats complete.cases
#' @examples
#' # Download raw data without filtering
#' malert_reports <- get_malert_data(source = "github")
get_malert_data <- function(source = "zenodo",
                            doi = "10.5281/zenodo.597466",
                            iso3 = NULL,
                            admin_level = NULL,
                            admin_name = NULL,
                            desired_cols = NULL) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required. Install it with install.packages('dplyr').", call. = FALSE)
  }

  this_temp_file <- tempfile()

  # --- 1) Download from GitHub or Zenodo -------------------------------------
  if (identical(source, "github")) {
    temp <- this_temp_file
    download.file(
      "https://github.com/MosquitoAlert/Data/raw/master/all_reports.zip",
      destfile = temp
    )

  } else if (identical(source, "zenodo") && !is.na(doi)) {
    dir.create(this_temp_file, showWarnings = FALSE)
    download_zenodo(doi = doi, path = this_temp_file)
    this_temp_file_zip <- file.path(this_temp_file, list.files(this_temp_file))
    outer_file_name <- unzip(this_temp_file_zip, exdir = this_temp_file, list = TRUE)[1, 1]
    unzip(this_temp_file_zip, exdir = this_temp_file)
    temp <- file.path(this_temp_file, outer_file_name, "all_reports.zip")

  } else {
    stop("This function currently supports `source = 'github'` or 'zenodo'.", call. = FALSE)
  }

  # --- 2) Load JSON for all years --------------------------------------------
  reports <- dplyr::bind_rows(lapply(2014:lubridate::year(lubridate::today()), function(this_year) {
    message("Loading year: ", this_year)
    this_file <- paste0("home/webuser/webapps/tigaserver/static/all_reports", this_year, ".json")
    tibble::as_tibble(jsonlite::fromJSON(unz(temp, file = this_file), flatten = TRUE))
  }))

  unlink(this_temp_file, recursive = TRUE, force = TRUE)

  # Persist raw download for reproducibility
  global_path <- file.path("data", "vector", "vector_global_malert.Rds")
  dir.create(dirname(global_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(reports, global_path)
  message("Saved raw Mosquito Alert reports to ", global_path)

  final_data <- reports
  filtered_path <- NULL

  apply_spatial <- !is.null(iso3) && !is.null(admin_level) && !is.null(admin_name)

  if (apply_spatial) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required for spatial filtering. Install it with install.packages('sf')", call. = FALSE)
    }

    ids <- build_location_identifiers(toupper(iso3), admin_level, admin_name)
    perimeter_dir <- "data/proc"
    perimeter_candidates <- c(
      file.path(perimeter_dir, sprintf("spatial_%s_perimeter.Rds", ids$slug)),
      file.path(perimeter_dir, sprintf("spatial_%s_perimeter.rds", ids$slug))
    )
    perimeter_path <- perimeter_candidates[file.exists(perimeter_candidates)][1]
    if (is.na(perimeter_path)) {
      stop(
        "Perimeter file not found. Looked for: ",
        paste(perimeter_candidates, collapse = "; "),
        call. = FALSE
      )
    }
    message("Applying spatial filter using ", perimeter_path)

    perimeter_sf <- readRDS(perimeter_path)
    perimeter_sf <- sf::st_as_sf(perimeter_sf)
    if (!inherits(perimeter_sf, "sf")) {
      stop("Perimeter file must be an sf object.", call. = FALSE)
    }
    perimeter_sf <- sf::st_make_valid(perimeter_sf)
    perimeter_crs <- sf::st_crs(perimeter_sf)
    if (is.na(perimeter_crs)) {
      perimeter_sf <- sf::st_set_crs(perimeter_sf, 4326)
    } else if (!sf::st_is_longlat(perimeter_sf)) {
      perimeter_sf <- sf::st_transform(perimeter_sf, 4326)
    }

    lon_candidates <- c("lon", "longitude", "decimalLongitude", "Lon", "report_lon")
    lat_candidates <- c("lat", "latitude", "decimalLatitude", "Lat", "report_lat")
    lon_col <- lon_candidates[lon_candidates %in% names(final_data)][1]
    lat_col <- lat_candidates[lat_candidates %in% names(final_data)][1]

    if (is.na(lon_col) || is.na(lat_col)) {
      stop("Longitude/latitude columns not found in reports; cannot apply spatial filter.", call. = FALSE)
    }

    coord_complete <- stats::complete.cases(final_data[, c(lon_col, lat_col)])
    if (!any(coord_complete)) {
      warning("No records with complete coordinates; spatial filter skipped.", call. = FALSE)
    } else {
      points_sf <- sf::st_as_sf(
        final_data[coord_complete, , drop = FALSE],
        coords = c(lon_col, lat_col),
        crs = 4326,
        remove = FALSE
      )
      within_idx <- logical(nrow(final_data))
      within_idx[coord_complete] <- lengths(sf::st_within(points_sf, perimeter_sf)) > 0
      final_data <- final_data[within_idx, , drop = FALSE]
    }

    if ("type" %in% names(final_data)) {
      message("Filtering to records with type == 'adult'.")
      final_data <- final_data[final_data$type == "adult", , drop = FALSE]
    } else {
      warning("Column `type` not found; adult-only filter skipped.", call. = FALSE)
    }

    filtered_path <- file.path(
      "data",
      "proc",
      sprintf("vector_%s_%s_%s_malert.Rds", ids$iso3, ids$admin_level, ids$admin_name)
    )
  }

  if (!is.null(desired_cols)) {
    desired <- unique(as.character(unlist(desired_cols, use.names = FALSE)))
    desired <- desired[nzchar(desired)]
    if (length(desired) && ncol(final_data)) {
      existing_names <- names(final_data)
      norm_existing <- tolower(gsub("[^a-z0-9]", "", existing_names))
      matched <- vapply(desired, function(col) {
        norm_col <- tolower(gsub("[^a-z0-9]", "", col))
        idx <- which(norm_existing == norm_col)
        if (length(idx)) idx[1] else NA_integer_
      }, integer(1))
      keep_idx <- matched[!is.na(matched)]
      missing_cols <- desired[is.na(matched)]
      if (length(missing_cols)) {
        warning(
          paste(
            "The following requested columns were not found and will be omitted:",
            paste(missing_cols, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      if (!length(keep_idx)) {
        warning("Requested columns not found; retaining all columns.", call. = FALSE)
      } else {
        final_data <- final_data[, keep_idx, drop = FALSE]
        names(final_data) <- desired[!is.na(matched)]
      }
    }
  }

  final_data <- tibble::as_tibble(final_data)

  if (!is.null(filtered_path)) {
    dir.create(dirname(filtered_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(final_data, filtered_path)
    message("Saved perimeter-filtered Mosquito Alert reports to ", filtered_path)
    attr(final_data, "filtered_path") <- filtered_path
    attr(final_data, "perimeter_source") <- perimeter_path
    attr(final_data, "location_slug") <- ids$slug
  }

  attr(final_data, "global_path") <- global_path

  final_data
}