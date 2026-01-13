#' Download GBIF occurrence data for a taxon and country
#'
#' Submits an authenticated GBIF download request for a given species and
#' country, waits for completion, retrieves the zipped archive, removes the
#' downloaded file after import, clips occurrences to the supplied perimeter,
#' and returns the parsed occurrence records.
#' GBIF credentials can be provided via arguments or
#' pulled from the `GBIF_USER`, `GBIF_PWD`, and `GBIF_EMAIL` environment
#' variables.
#'
#' @param taxon_key Numeric vector of GBIF taxon keys (e.g. `c(1651430,
#'   1651891)` for *Aedes albopictus* and *Aedes aegypti*). Defaults to both
#'   species.
#' @param iso3 Three-letter ISO country code (e.g. `"ESP"`). It is converted to
#'   the GBIF-required ISO2 code using `countrycode`.
#' @param admin_level Administrative level used when the perimeter was
#'   generated.
#' @param admin_name Administrative unit name used for the perimeter.
#' @param year_min Optional minimum occurrence year.
#' @param year_max Optional maximum occurrence year.
#' @param desired_cols Optional character vector (or list) of column names to
#'   retain after perimeter filtering. Matching is case-insensitive and accepts
#'   common typos for `occurrenceStatus`/`occurrenceID`. When `NULL` (default),
#'   all columns are preserved.
#' @param out_dir Directory where both the raw GBIF download and the
#'   perimeter-clipped table should be saved. Defaults to `"data/vector"`. The
#'   unfiltered results are written as `vector_<iso3>_gbif.Rds`; the
#'   perimeter-clipped output becomes
#'   `vector_<iso3>_<admin_level>_<admin_name>_gbif.Rds` (with the
#'   administrative name normalised to lowercase underscores).
#' @param perimeter_dir Directory containing the perimeter RDS artefacts. The
#'   function looks for `spatial_<slug>_perimeter.rds` in this location.
#' @param gbif_user GBIF username. Defaults to `Sys.getenv("GBIF_USER")`.
#' @param gbif_pwd GBIF password. Defaults to `Sys.getenv("GBIF_PWD")`.
#' @param gbif_email GBIF-registered email. Defaults to
#'   `Sys.getenv("GBIF_EMAIL")`.
#' @param verbose Logical; if `TRUE` (default) progress messages are printed.
#'
#' @return Tibble of occurrence records. Metadata attributes include the GBIF
#'   download key (`gbif_key`), the perimeter-clipped table path
#'   (`gbif_table_path`), the raw unfiltered table path (`gbif_raw_table_path`),
#'   the location slug (`location_slug`), and the perimeter source path
#'   (`perimeter_source`).
#' @export
get_gbif_data <- function(
  taxon_key = c(1651430, 1651891),
  iso3,
  admin_level,
  admin_name,
  year_min = NULL,
  year_max = NULL,
  desired_cols = NULL,
  out_dir = "data/vector",
  perimeter_dir = "data/proc",
  gbif_user = Sys.getenv("GBIF_USER", unset = NA_character_),
  gbif_pwd = Sys.getenv("GBIF_PWD", unset = NA_character_),
  gbif_email = Sys.getenv("GBIF_EMAIL", unset = NA_character_),
  verbose = TRUE
) {
  if (!requireNamespace("rgbif", quietly = TRUE)) {
    stop("Package 'rgbif' is required. Install it with install.packages('rgbif').")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required. Install it with install.packages('countrycode').")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required. Install it with install.packages('sf').")
  }

  creds <- list(user = gbif_user, pwd = gbif_pwd, email = gbif_email)
  if (anyNA(unlist(creds))) {
    stop("GBIF credentials (user, password, email) must be supplied either via arguments or environment variables.",
      call. = FALSE
    )
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  original_env <- Sys.getenv(c("GBIF_USER", "GBIF_PWD", "GBIF_EMAIL"), unset = NA_character_)
  on.exit({
    for (nm in names(original_env)) {
      value <- original_env[[nm]]
      if (is.na(value)) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, stats::setNames(list(value), nm))
      }
    }
  }, add = TRUE)

  Sys.setenv(
    GBIF_USER = creds$user,
    GBIF_PWD = creds$pwd,
    GBIF_EMAIL = creds$email
  )

  iso3_code <- toupper(as.character(iso3)[1])
  iso2_code <- countrycode::countrycode(
    iso3_code,
    origin = "iso3c",
    destination = "iso2c",
    warn = FALSE
  )
  if (is.na(iso2_code)) {
    stop("Unable to convert ISO3 code '", iso3_code, "' to the ISO2 code required by GBIF.",
      call. = FALSE
    )
  }

  ids <- build_location_identifiers(iso3_code, admin_level, admin_name)
  if (isTRUE(verbose)) {
    message("Resolved location slug: ", ids$slug)
  }

  if (!dir.exists(perimeter_dir)) {
    stop("Perimeter directory not found at ", perimeter_dir, call. = FALSE)
  }

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
  if (isTRUE(verbose)) {
    message("Using perimeter from ", perimeter_path)
  }
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

  taxon_vals <- unique(as.integer(taxon_key))
  if (length(taxon_vals) == 0L || anyNA(taxon_vals)) {
    stop("`taxon_key` must contain at least one valid numeric GBIF taxon key.", call. = FALSE)
  }

  taxon_pred <- if (length(taxon_vals) == 1L) {
    rgbif::pred("taxonKey", taxon_vals)
  } else {
    rgbif::pred_in("taxonKey", taxon_vals)
  }

  predicates <- list(
    taxon_pred,
    rgbif::pred("country", iso2_code),
    rgbif::pred("hasCoordinate", TRUE)
  )

  if (!is.null(year_min)) {
    predicates <- c(predicates, list(rgbif::pred_gte("year", year_min)))
  }
  if (!is.null(year_max)) {
    predicates <- c(predicates, list(rgbif::pred_lte("year", year_max)))
  }

  if (isTRUE(verbose)) {
    message(
      "Submitting GBIF download request for taxon key(s) ",
      paste(taxon_vals, collapse = ", "),
      " in ", iso3_code, " (ISO2: ", iso2_code, ")"
    )
  }
  download_key <- do.call(rgbif::occ_download, c(predicates, list(format = "SIMPLE_CSV")))

  if (isTRUE(verbose)) {
    message("Waiting for GBIF download ", download_key)
  }
  rgbif::occ_download_wait(download_key)

  if (isTRUE(verbose)) {
    message("Retrieving GBIF archive to ", out_dir)
  }
  zip_path <- rgbif::occ_download_get(download_key, path = out_dir, overwrite = TRUE)

  occ_data <- tibble::as_tibble(rgbif::occ_download_import(zip_path))

  raw_iso_token <- tolower(iso3_code)
  raw_filename <- sprintf("vector_%s_gbif.Rds", raw_iso_token)
  raw_path <- file.path(out_dir, raw_filename)
  if (isTRUE(verbose)) {
    message("Saving unfiltered occurrences to ", raw_path)
  }
  saveRDS(occ_data, raw_path)

  coord_cols <- c("decimalLongitude", "decimalLatitude")
  if (!all(coord_cols %in% names(occ_data))) {
    stop("GBIF download is missing required coordinate columns.", call. = FALSE)
  }
  occ_data <- occ_data[stats::complete.cases(occ_data[coord_cols]), , drop = FALSE]
  if (!nrow(occ_data)) {
    warning("All GBIF records lacked valid coordinates after filtering; returning empty tibble.",
      call. = FALSE
    )
  }

  if (ncol(occ_data)) {
    occ_sf <- sf::st_as_sf(
      occ_data,
      coords = coord_cols,
      crs = 4326,
      remove = FALSE
    )
    within_idx <- lengths(sf::st_within(occ_sf, perimeter_sf)) > 0
    occ_sf <- occ_sf[within_idx, , drop = FALSE]
    if (nrow(occ_sf) == 0) {
      warning("No GBIF records fell within the supplied perimeter; returning empty tibble.",
        call. = FALSE
      )
      occ_data <- occ_data[FALSE, , drop = FALSE]
    } else {
      occ_data <- tibble::as_tibble(sf::st_drop_geometry(occ_sf))
    }
  }

  if (!is.null(desired_cols)) {
    retained <- as.character(unlist(desired_cols, use.names = FALSE))
    retained <- unique(retained[!is.na(retained) & nzchar(retained)])
    if (length(retained) && ncol(occ_data)) {
      alt_map <- list(
        occurrenceStatus = c("occurenceStatus"),
        occurrenceID = c("occurenceID")
      )
      matched <- vapply(retained, function(col) {
        matches <- names(occ_data)[tolower(names(occ_data)) == tolower(col)]
        if (!length(matches) && col %in% names(alt_map)) {
          matches <- names(occ_data)[tolower(names(occ_data)) %in% tolower(alt_map[[col]])]
        }
        if (length(matches)) matches[1] else NA_character_
      }, character(1), USE.NAMES = FALSE)
      available_idx <- !is.na(matched)
      if (!any(available_idx)) {
        warning("Requested GBIF columns not found; returning full dataset.", call. = FALSE)
      } else {
        selected <- occ_data[, matched[available_idx], drop = FALSE]
        names(selected) <- retained[available_idx]
        occ_data <- selected
      }
    }
  }

  occ_data <- tibble::as_tibble(occ_data)

  iso_token <- ids$slug
  table_filename <- sprintf("vector_%s_gbif.Rds", iso_token)
  table_path <- file.path(out_dir, table_filename)
  if (isTRUE(verbose)) {
    message("Saving parsed occurrences to ", table_path)
  }
  dir.create(dirname(table_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(occ_data, table_path)

  if (file.exists(zip_path)) {
    if (isTRUE(verbose)) {
      message("Removing GBIF archive ", zip_path)
    }
    unlink(zip_path)
  }

  attr(occ_data, "gbif_key") <- download_key
  attr(occ_data, "gbif_table_path") <- table_path
  attr(occ_data, "gbif_raw_table_path") <- raw_path
  attr(occ_data, "location_slug") <- ids$slug
  attr(occ_data, "perimeter_source") <- perimeter_path
  occ_data
}