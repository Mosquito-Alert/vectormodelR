#' Generate pseudoabsences using TRS effort (Mosquito Alert) and TGB (GBIF)
#'
#' Samples pseudoabsences separately for Mosquito Alert (TRS-based effort
#' surface) and GBIF (target-group background weights). The function expects the
#' presence dataset to include coordinates (configurable via `lon_col`/`lat_col`),
#' the specified date column, and a source column distinguishing Mosquito Alert vs
#' GBIF records. When supplied an in-memory dataset it must carry an `output_path`
#' attribute so the helper can infer the location slug and persist the augmented
#' output.
#'
#' @param dataset Either the in-memory modelling dataset or a path to the
#'   corresponding RDS file.
#' @param data_dir Directory holding processed datasets (TRS/TGB artefacts) and
#'   where the pseudoabsence dataset will be written. Defaults to "data/proc".
#' @param sampling_factor_ma Mosquito Alert pseudoabsences per MA presence
#'   (default 10).
#' @param sampling_factor_gbif GBIF pseudoabsences per GBIF presence (default 10).
#' @param date_col Name of the date column shared by the presence and effort
#'   tables (default `"date"`).
#' @param lon_col Name of longitude column in the presence dataset (default `"lon"`).
#' @param lat_col Name of latitude column in the presence dataset (default `"lat"`).
#' @param source_col Name of the data-source column in the presence dataset
#'   (default `"source"`).
#' @param se_col Name of the TRS sampling-effort column (default `"SE_expected"`).
#' @param tgb_col Name of the GBIF target-group background weight column (default
#'   `"tgb_w"`).
#' @param ma_source Label used for Mosquito Alert records in `source_col`
#'   (default `"malert"`).
#' @param gbif_source Label used for GBIF records in `source_col` (default
#'   `"gbif"`).
#' @param write_output Logical; when `TRUE` (default) persists the combined
#'   presence+pseudoabsence dataset to disk.
#'
#' @return A tibble combining presences (`presence = TRUE`) and pseudoabsences
#'   (`presence = FALSE`) with numeric coordinate columns (`lon_col`/`lat_col`)
#'   plus `pa_method` metadata. Attributes mirror the input dataset with updated
#'   `output_path` and `location_slug` values.
#' @export
add_pseudoabsences_se <- function(
    dataset,
    data_dir             = "data/proc",
    sampling_factor_ma   = 10,
    sampling_factor_gbif = 10,
    date_col             = "date",
    lon_col              = "longitude",
    lat_col              = "latitude",
    source_col           = "source",
    se_col               = "SE_expected",
    tgb_col              = "tgb_w",
    ma_source            = "malert",
    gbif_source          = "gbif",
    write_output         = TRUE
) {
  # deps
  for (pkg in c("dplyr", "sf", "lubridate")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install it with install.packages('", pkg, "').", call. = FALSE)
    }
  }
  
  infer_slug <- function(path) {
    fname <- basename(path)
    matches <- regexec("^model_prep_(.+?)_(?:malert|gbif)(?:_.*)?$", fname)
    parts <- regmatches(fname, matches)[[1]]
    if (length(parts) >= 2) parts[2] else NA_character_
  }
  
  # ---- 1) Load dataset ----
  dataset_is_path <- is.character(dataset) && length(dataset) == 1L
  if (dataset_is_path) {
    dataset_path <- dataset
    if (!file.exists(dataset_path)) stop("Model dataset not found at ", dataset_path, call. = FALSE)
    D <- readRDS(dataset_path)
  } else {
    D <- dataset
    dataset_path <- attr(D, "output_path", exact = TRUE)
    if (is.null(dataset_path) || !nzchar(dataset_path)) {
      stop("Input dataset must carry an `output_path` attribute or be a file path.", call. = FALSE)
    }
  }
  
  location_slug <- attr(D, "location_slug", exact = TRUE)
  if (is.null(location_slug) || !nzchar(location_slug)) location_slug <- infer_slug(dataset_path)
  if (is.na(location_slug) || !nzchar(location_slug)) {
    stop("Could not determine location slug; ensure `location_slug` attribute exists.", call. = FALSE)
  }
  attr(D, "location_slug") <- location_slug
  
  # columns
  req <- c(lon_col, lat_col, date_col)
  miss <- setdiff(req, names(D))
  if (length(miss) > 0) stop("Dataset missing: ", paste(miss, collapse = ", "), call. = FALSE)
  
  if (!source_col %in% names(D)) {
    warning("Dataset missing `", source_col, "`; treating all presences as Mosquito Alert.")
    D[[source_col]] <- ma_source
  }
  
  # Ensure numeric coordinates
  D[[lon_col]] <- as.numeric(D[[lon_col]])
  D[[lat_col]] <- as.numeric(D[[lat_col]])
  
  # sf + date/year
  D_sf <- sf::st_as_sf(D, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  D_sf[[date_col]] <- as.Date(D_sf[[date_col]])
  D_sf$year <- lubridate::year(D_sf[[date_col]])
  years_use <- sort(unique(D_sf$year))
  
  # ---- helper sampler ----
  sample_from_effort <- function(eff_sf, weight_col, n_abs, source_value, pa_method, extra_keep = character()) {
    if (n_abs <= 0) return(NULL)
    if (!inherits(eff_sf, "sf")) stop("Effort surface must be sf.", call. = FALSE)
    if (!date_col %in% names(eff_sf)) stop("Effort surface missing date column: ", date_col, call. = FALSE)
    if (!weight_col %in% names(eff_sf)) stop("Effort surface missing weight column: ", weight_col, call. = FALSE)
    
    eff_sf[[date_col]] <- as.Date(eff_sf[[date_col]])
    eff_sf$year <- lubridate::year(eff_sf[[date_col]])
    
    eff_sf <- eff_sf |>
      dplyr::filter(.data$year %in% years_use, .data[[weight_col]] > 0)
    
    if (nrow(eff_sf) == 0) stop("No positive-effort rows for source ", source_value, ".", call. = FALSE)
    
    eff_yearly <- eff_sf |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(w_year = sum(.data[[weight_col]], na.rm = TRUE), .groups = "drop") |>
      dplyr::filter(.data$w_year > 0)
    
    sampled_years <- sample(
      eff_yearly$year,
      size = n_abs,
      replace = TRUE,
      prob = eff_yearly$w_year
    )
    
    n_abs_per_year <- as.data.frame(table(sampled_years), stringsAsFactors = FALSE)
    names(n_abs_per_year) <- c("year", "n")
    n_abs_per_year$year <- as.integer(n_abs_per_year$year)
    
    abs_list <- lapply(seq_len(nrow(n_abs_per_year)), function(i) {
      this_year <- n_abs_per_year$year[i]
      this_n <- n_abs_per_year$n[i]
      
      these_eff <- eff_sf |>
        dplyr::filter(.data$year == this_year, .data[[weight_col]] > 0)
      
      if (nrow(these_eff) == 0) return(NULL)
      
      sampled <- these_eff |>
        dplyr::slice_sample(n = this_n, replace = TRUE, weight_by = .data[[weight_col]])
      
      keep <- unique(c("year", date_col, weight_col, extra_keep))
      keep <- intersect(keep, names(sampled))
      
      sampled |>
        dplyr::select(dplyr::all_of(keep)) |>
        dplyr::mutate(
          presence = FALSE,
          !!source_col := source_value,
          pa_method = pa_method
        )
    })
    
    abs_list <- abs_list[!vapply(abs_list, is.null, logical(1))]
    if (!length(abs_list)) return(NULL)
    
    abs_sf <- do.call(rbind, abs_list)
    
    # add coordinate columns (using lon_col/lat_col names)
    xy <- sf::st_coordinates(abs_sf)
    abs_sf[[lon_col]] <- xy[, 1]
    abs_sf[[lat_col]] <- xy[, 2]
    
    abs_sf
  }
  
  # ---- 2) Split presences ----
  D_ma   <- D_sf |> dplyr::filter(.data[[source_col]] == ma_source)
  D_gbif <- D_sf |> dplyr::filter(.data[[source_col]] == gbif_source)
  n_pres_ma <- nrow(D_ma)
  n_pres_gbif <- nrow(D_gbif)
  
  # ---- 3) MA pseudoabsences ----
  abs_ma <- NULL
  if (n_pres_ma > 0 && sampling_factor_ma > 0) {
    trs_path <- file.path(data_dir, sprintf("model_prep_%s_trs_daily.Rds", location_slug))
    if (!file.exists(trs_path)) stop("TRS dataset not found at ", trs_path, call. = FALSE)
    trs_daily <- readRDS(trs_path)
    if (!inherits(trs_daily, "sf")) stop("`trs_daily` must be an sf object.", call. = FALSE)
    
    se_cols <- unique(c(se_col, intersect(c("SE", "SE_expected"), names(trs_daily))))
    
    abs_ma <- sample_from_effort(
      eff_sf = trs_daily,
      weight_col = se_col,
      n_abs = sampling_factor_ma * n_pres_ma,
      source_value = ma_source,
      pa_method = "trs_effort",
      extra_keep = se_cols
    )
    
    if (!is.null(abs_ma)) {
      for (col in se_cols) if (!col %in% names(D_sf)) D_sf[[col]] <- NA_real_
    }
  }
  
  # ---- 4) GBIF pseudoabsences ----
  abs_gbif <- NULL
  if (n_pres_gbif > 0 && sampling_factor_gbif > 0) {
    tgb_path <- file.path(data_dir, sprintf("model_prep_%s_tgb_daily.Rds", location_slug))
    if (!file.exists(tgb_path)) stop("TGB dataset not found at ", tgb_path, call. = FALSE)
    tgb_daily <- readRDS(tgb_path)
    if (!inherits(tgb_daily, "sf")) stop("`tgb_daily` must be an sf object.", call. = FALSE)
    
    abs_gbif <- sample_from_effort(
      eff_sf = tgb_daily,
      weight_col = tgb_col,
      n_abs = sampling_factor_gbif * n_pres_gbif,
      source_value = gbif_source,
      pa_method = "gbif_tgb",
      extra_keep = c(tgb_col)
    )
  }
  
  # ---- 5) Combine ----
  pres_df <- sf::st_drop_geometry(D_sf) |>
    dplyr::mutate(presence = TRUE, pa_method = NA_character_)
  
  abs_df <- dplyr::bind_rows(
    if (!is.null(abs_ma)) sf::st_drop_geometry(abs_ma) else NULL,
    if (!is.null(abs_gbif)) sf::st_drop_geometry(abs_gbif) else NULL
  )
  
  combined <- dplyr::bind_rows(pres_df, abs_df)
  
  # ---- 6) Save + attrs ----
  base_attrs <- attributes(D)
  stem <- tools::file_path_sans_ext(basename(dataset_path))
  output_filename <- paste0(stem, "_se.Rds")
  output_path <- file.path(data_dir, output_filename)
  
  preserve <- base_attrs[setdiff(names(base_attrs), c("names", "row.names", "class"))]
  for (nm in names(preserve)) attr(combined, nm) <- preserve[[nm]]
  attr(combined, "output_path") <- output_path
  attr(combined, "location_slug") <- location_slug
  
  if (isTRUE(write_output)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(combined, output_path)
  }
  
  combined
}