#' Download Mosquito Alert report data from GitHub or Zenodo
#'
#' @param source String. Source to download from. Options are "github" or "zenodo".
#' @param doi String. Zenodo DOI if downloading from Zenodo.
#'   Default is the DOI that always points to the most recent version:
#'   10.5281/zenodo.597466.
#' @param iso3 Optional three-letter ISO code used for spatial filtering.
#' @param admin_level Optional administrative level used for spatial filtering.
#' @param admin_name Optional administrative unit name. When `NULL`, all
#'   administrative units at `admin_level` are included.
#' @param desired_cols Optional character vector (or list) of column names to
#'   retain after spatial filtering. When `NULL`, all available columns are kept.
#' @param filters Optional named list of values to filter by. Each element name
#'   should correspond to a column in the dataset, and the value describes what
#'   to keep (e.g. `list(type = "adult")`).
#'
#' @returns A tibble of Mosquito Alert reports.
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
                            desired_cols = NULL,
                            filters = NULL) {

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package 'dplyr' is required. Install it with install.packages('dplyr').",
      call. = FALSE
    )
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

    this_temp_file_zip <- file.path(
      this_temp_file,
      list.files(this_temp_file)
    )

    outer_file_name <- unzip(
      this_temp_file_zip,
      exdir = this_temp_file,
      list = TRUE
    )[1, 1]

    unzip(this_temp_file_zip, exdir = this_temp_file)

    temp <- file.path(
      this_temp_file,
      outer_file_name,
      "all_reports.zip"
    )

  } else {
    stop(
      "This function currently supports `source = 'github'` or 'zenodo'.",
      call. = FALSE
    )
  }

  # --- 2) Load JSON for all years --------------------------------------------
  reports <- dplyr::bind_rows(
    lapply(
      2014:lubridate::year(lubridate::today()),
      function(this_year) {
        message("Loading year: ", this_year)

        this_file <- paste0(
          "home/webuser/webapps/tigaserver/static/all_reports",
          this_year,
          ".json"
        )

        tibble::as_tibble(
          jsonlite::fromJSON(
            unz(temp, file = this_file),
            flatten = TRUE
          )
        )
      }
    )
  )

  unlink(this_temp_file, recursive = TRUE, force = TRUE)

  global_path <- file.path(
    "data",
    "vector",
    "vector_global_malert.Rds"
  )

  dir.create(
    dirname(global_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  saveRDS(reports, global_path)
  message("Saved raw Mosquito Alert reports to ", global_path)

  final_data <- reports

  # --- Apply generic filters -------------------------------------------------
  if (!is.null(filters)) {
    for (col in names(filters)) {
      if (col %in% names(final_data)) {
        val <- filters[[col]]

        message("Filtering on ", col, " (keeping matching records)")

        final_data <- final_data[
          final_data[[col]] %in% val,
          ,
          drop = FALSE
        ]
      } else {
        warning(
          "Filter column '",
          col,
          "' not found in dataset. Skipping.",
          call. = FALSE
        )
      }
    }
  }

  filtered_path <- NULL

  # A name is optional; only the country and level are required.
  apply_spatial <- !is.null(iso3) && !is.null(admin_level)

  if (apply_spatial) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop(
        "Package 'sf' is required for spatial filtering. Install it with install.packages('sf')",
        call. = FALSE
      )
    }

    output_name <- if (is.null(admin_name)) "all" else admin_name

    ids <- build_location_identifiers(
      toupper(iso3),
      admin_level,
      output_name
    )

    perimeter_sf <- vectormodelR::get_gadm_data(
      iso3 = ids$iso3,
      name = admin_name,
      level = admin_level,
      perimeter = TRUE,
      rds = FALSE
    )

    perimeter_sf <- sf::st_as_sf(perimeter_sf)
    perimeter_sf <- sf::st_make_valid(perimeter_sf)

    perimeter_crs <- sf::st_crs(perimeter_sf)

    if (is.na(perimeter_crs)) {
      perimeter_sf <- sf::st_set_crs(perimeter_sf, 4326)
    } else if (!sf::st_is_longlat(perimeter_sf)) {
      perimeter_sf <- sf::st_transform(perimeter_sf, 4326)
    }

    lon_candidates <- c(
      "lon",
      "longitude",
      "decimalLongitude",
      "Lon",
      "report_lon"
    )

    lat_candidates <- c(
      "lat",
      "latitude",
      "decimalLatitude",
      "Lat",
      "report_lat"
    )

    lon_col <- lon_candidates[
      lon_candidates %in% names(final_data)
    ][1]

    lat_col <- lat_candidates[
      lat_candidates %in% names(final_data)
    ][1]

    if (is.na(lon_col) || is.na(lat_col)) {
      stop(
        "Longitude/latitude columns not found in reports; cannot apply spatial filter.",
        call. = FALSE
      )
    }

    coord_complete <- stats::complete.cases(
      final_data[, c(lon_col, lat_col)]
    )

    if (!any(coord_complete)) {
      warning(
        "No records with complete coordinates; spatial filter skipped.",
        call. = FALSE
      )
    } else {
      points_sf <- sf::st_as_sf(
        final_data[coord_complete, , drop = FALSE],
        coords = c(lon_col, lat_col),
        crs = 4326,
        remove = FALSE
      )

      within_idx <- logical(nrow(final_data))

      within_idx[coord_complete] <- lengths(
        sf::st_within(points_sf, perimeter_sf)
      ) > 0

      final_data <- final_data[
        within_idx,
        ,
        drop = FALSE
      ]
    }

    filtered_path <- file.path(
      "data",
      "proc",
      sprintf(
        "vector_%s_%s_%s_malert.Rds",
        ids$iso3,
        ids$admin_level,
        ids$admin_name
      )
    )
  }

  # --- Select requested columns ----------------------------------------------
  if (!is.null(desired_cols)) {
    desired <- unique(
      as.character(
        unlist(desired_cols, use.names = FALSE)
      )
    )

    desired <- desired[nzchar(desired)]

    if (length(desired) && ncol(final_data)) {
      existing_names <- names(final_data)

      norm_existing <- tolower(
        gsub("[^a-z0-9]", "", existing_names)
      )

      matched <- vapply(
        desired,
        function(col) {
          norm_col <- tolower(
            gsub("[^a-z0-9]", "", col)
          )

          idx <- which(norm_existing == norm_col)

          if (length(idx)) idx[1] else NA_integer_
        },
        integer(1)
      )

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
        warning(
          "Requested columns not found; retaining all columns.",
          call. = FALSE
        )
      } else {
        final_data <- final_data[, keep_idx, drop = FALSE]
        names(final_data) <- desired[!is.na(matched)]
      }
    }
  }

  final_data <- tibble::as_tibble(final_data)

  if (!is.null(filtered_path)) {
    dir.create(
      dirname(filtered_path),
      recursive = TRUE,
      showWarnings = FALSE
    )

    saveRDS(final_data, filtered_path)
    message("Saved filtered Mosquito Alert reports to ", filtered_path)

    attr(final_data, "filtered_path") <- filtered_path
    attr(final_data, "location_slug") <- ids$slug
  }

  attr(final_data, "global_path") <- global_path

  final_data
}