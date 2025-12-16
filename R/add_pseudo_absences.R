#' Generate pseudoabsences using sampling effort only (TRS-based, Option 2)
#'
#' This version treats `trs_daily` as the "true" sampling grid (Tigacell),
#' samples rows proportional to sampling effort, and uses their point
#' geometry directly. It does *not* depend on the hex grid; you can attach
#' hex `grid_id` and other spatial attributes in a later step.
#'
#' @param dataset Either the in-memory NDVI-enriched dataset (output of
#'   [add_ndvi_features()]) or a path to the corresponding RDS file. When a data
#'   object is supplied it must carry an `output_path` attribute naming the most
#'   recently saved file; the pseudoabsence dataset is written to `data_dir`
#'   with `_se.Rds` appended to that stem when `write_output` is `TRUE`.
#' @param data_dir Directory holding processed datasets (for TRS files, etc.)
#'   and where the pseudoabsence dataset will be written. Defaults to "data/proc".
#' @param sampling_factor Number of pseudoabsences per presence (default 10).
#' @param date_col Name of date column in `D` and `trs_daily` (default `"date"`).
#' @param se_col Name of sampling-effort column in `trs_daily`
#'   (e.g. `"SE_expected"` or `"SE"`).
#' @param write_output Logical flag; when `TRUE` (default) the combined presence
#'   and pseudoabsence dataset is written to disk. Set to `FALSE` to skip
#'   writing while still returning the augmented object and updating metadata.
#'
#' @return A tibble/data frame combining presences (`presence = TRUE`) and
#'   pseudoabsences (`presence = FALSE`) with numeric `lon`/`lat` columns. The
#'   dataset is also written to `model_prep_<slug>_wx_lc_ndvi_se.Rds`.
#' @export
add_pseudoabsences_se <- function(
  dataset,
  data_dir        = "data/proc",
  sampling_factor = 10,
  date_col        = "date",
  se_col          = "SE_expected",
  write_output    = TRUE
) {
  infer_slug <- function(path) {
    fname <- basename(path)
    matches <- regexec("^model_prep_(.+?)_base", fname)
    parts <- regmatches(fname, matches)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }

  # ---- 1. Presence dataset (already weather + landcover + NDVI) ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) {
      stop("Model dataset not found at ", dataset_path, call. = FALSE)
    }
    D <- readRDS(dataset_path)
  } else {
    D <- dataset
    dataset_path <- attr(D, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop(
        "Input dataset must carry an `output_path` attribute or be provided as a file path.",
        call. = FALSE
      )
    }
  }

  location_slug <- attr(D, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) {
    location_slug <- infer_slug(dataset_path)
  }
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine location slug from dataset; ensure it carries a `location_slug` attribute.", call. = FALSE)
  }
  attr(D, "location_slug") <- location_slug

  if (!all(c("lon", "lat") %in% names(D))) {
    stop("Model dataset must contain `lon` and `lat` columns.", call. = FALSE)
  }
  if (!date_col %in% names(D)) {
    stop("Model dataset must contain date column: ", date_col, call. = FALSE)
  }

  # Convert to sf (WGS84)
  D_sf <- sf::st_as_sf(D, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  # Make sure date & year exist
  D_sf[[date_col]] <- as.Date(D_sf[[date_col]])
  D_sf$year        <- lubridate::year(D_sf[[date_col]])

  # ---- 2. TRS (sampling effort) dataset ----
  trs_path <- file.path(
    data_dir,
    paste0("model_prep_", location_slug, "_trs_daily.Rds")
  )
  if (!file.exists(trs_path)) {
    stop("Sampling effort dataset not found at ", trs_path, call. = FALSE)
  }
  trs_daily <- readRDS(trs_path)

  if (!inherits(trs_daily, "sf")) {
    stop("`trs_daily` must be an sf object with point geometry.", call. = FALSE)
  }
  if (!date_col %in% names(trs_daily)) {
    stop("trs_daily must contain date column: ", date_col, call. = FALSE)
  }
  if (!se_col %in% names(trs_daily)) {
    stop("trs_daily must contain sampling-effort column: ", se_col, call. = FALSE)
  }

  se_cols <- unique(c(se_col, intersect(c("SE", "SE_expected"), names(trs_daily))))

  # Ensure date & year in trs_daily
  trs_daily[[date_col]] <- as.Date(trs_daily[[date_col]])
  trs_daily$year        <- lubridate::year(trs_daily[[date_col]])

  # Restrict trs_daily to years in D_sf and positive SE
  years_use <- sort(unique(D_sf$year))
  trs_daily <- trs_daily |>
    dplyr::filter(
      .data$year %in% years_use,
      .data[[se_col]] > 0
    )

  if (nrow(trs_daily) == 0) {
    stop("No non-zero sampling effort rows after filtering by years.", call. = FALSE)
  }

  # ---- 3. How many pseudoabsences total & per year? ----
  n_pres <- nrow(D_sf)
  n_abs  <- sampling_factor * n_pres

  # Yearly total sampling effort
  trs_yearly <- trs_daily |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      se_year = sum(.data[[se_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$se_year > 0)

  if (nrow(trs_yearly) == 0) {
    stop("trs_yearly has no rows with positive effort.", call. = FALSE)
  }

  # Sample years proportional to se_year
  sampled_years <- sample(
    trs_yearly$year,
    size    = n_abs,
    replace = TRUE,
    prob    = trs_yearly$se_year
  )

  n_abs_per_year <- as.data.frame(table(sampled_years), stringsAsFactors = FALSE)
  names(n_abs_per_year) <- c("year", "n")
  n_abs_per_year$year <- as.integer(n_abs_per_year$year)

  # ---- 4. For each year: sample TRS rows proportional to SE (keep geometry) ----
  abs_list <- lapply(seq_len(nrow(n_abs_per_year)), function(i) {
    this_year <- n_abs_per_year$year[i]
    this_n    <- n_abs_per_year$n[i]

    these_trs <- trs_daily |>
      dplyr::filter(.data$year == this_year, .data[[se_col]] > 0)

    if (nrow(these_trs) == 0) {
      return(NULL)
    }

    # Use slice_sample (plays nicer with sf than sample_n)
    sampled_trs <- these_trs |>
      dplyr::slice_sample(
        n        = this_n,
        replace  = TRUE,
        weight_by = .data[[se_col]]
      )

    cols_to_keep <- unique(c("year", date_col, se_cols))

    sampled_trs <- sampled_trs |>
      dplyr::select(dplyr::all_of(cols_to_keep))

    sampled_trs |>
      dplyr::mutate(presence = FALSE)
  })

  abs_list <- abs_list[!vapply(abs_list, is.null, logical(1))]

  if (length(abs_list) == 0) {
    stop("No pseudoabsences were generated.", call. = FALSE)
  }

  abs_sf <- do.call(rbind, abs_list)
  abs_sf <- sf::st_transform(abs_sf, sf::st_crs(D_sf))

  # Extract lon/lat from geometry to keep downstream helpers compatible
  abs_coords <- sf::st_coordinates(abs_sf)
  abs_sf$lon <- abs_coords[, 1]
  abs_sf$lat <- abs_coords[, 2]

  for (col in se_cols) {
    if (!col %in% names(D_sf)) {
      D_sf[[col]] <- NA_real_
    }
  }

  # ---- 5. Combine with presences ----
  pres_df <- sf::st_drop_geometry(D_sf) |> dplyr::mutate(presence = TRUE)
  abs_df  <- sf::st_drop_geometry(abs_sf)

  combined <- dplyr::bind_rows(pres_df, abs_df)

  # Save combined dataset (still sf; you can drop geometry / add grid later)
  base_attrs <- attributes(D)
  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_filename <- paste0(stem, "_se.Rds")
  output_path     <- file.path(data_dir, output_filename)
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) {
    attr(combined, nm) <- preserve[[nm]]
  }
  attr(combined, "output_path") <- output_path
  attr(combined, "location_slug") <- location_slug

  if (isTRUE(write_output)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(combined, output_path)
  }

  combined
}