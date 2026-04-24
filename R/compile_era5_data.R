#' Compile ERA5 monthly CSVs from multi-variable GRIB/ZIP files
#'
#' Scans `input_dir` for ERA5 files (GRIB or ZIP format, with all variables in one file per month) 
#' and converts each month into a single long-format CSV.GZ. Output is organized by dataset type:
#' - ERA5 Single Levels → `processed/single-levels/YYYY/`
#' - ERA5-Land → `processed/land/YYYY/`
#' 
#' This version handles the new CDS API format where all variables are bundled into a single
#' file per month (e.g., era5_ita_3_firenze_2022_12.grib containing all variables).
#' 
#' Also writes recent N-month summaries in `input_dir`.
#'
#' @param input_dir Directory containing multi-variable GRIB files. If NULL/empty and `iso3`
#'   supplied, defaults to `file.path("data/weather/grib", tolower(iso3))` or
#'   `file.path("data/weather/grib", <slug>)` when `admin_name` is provided.
#' @param processed_dir Output dir for monthly CSVs. Defaults to `<input_dir>/processed`.
#'   Within this, dataset-specific subdirectories are created: `land/` and `single-levels/`.
#' @param iso3 Optional ISO3 code (character). Used to resolve `input_dir` (when missing)
#'   and to build output filenames.
#' @param admin_level integer. GADM administrative level (0=country, 1=region, 2=province, ...).
#'   Used only when `admin_name` is supplied.
#' @param admin_name character. Exact `NAME_<level>` value to select within GADM.
#'   When provided, uses admin-specific subdirectory and file naming.
#' @param dataset character. ERA5 dataset name ("reanalysis-era5-single-levels" or 
#'   "reanalysis-era5-land"). Required parameter - specifies which dataset to process.
#' @param recent_n Number of most recent monthly CSVs to merge into a summary (default 3).
#' @param verbose Print progress.
#'
#' @return (invisibly) a list with summary info.
#' @importFrom terra rast time nlyr
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom withr local_options
#' @export
compile_era5_data <- function(
  input_dir = NULL,
  processed_dir = NULL,
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL,
  dataset = "reanalysis-era5-single-levels",
  recent_n  = 3,
  verbose   = TRUE
) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required.")

  # Validate dataset (required parameter)
  if (is.null(dataset) || !nzchar(dataset)) {
    stop("`dataset` is required. Must be one of: 'reanalysis-era5-single-levels', 'reanalysis-era5-land'")
  }
  valid_datasets <- c("reanalysis-era5-single-levels", "reanalysis-era5-land")
  if (!dataset %in% valid_datasets) {
    stop("`dataset` must be one of: ", paste(valid_datasets, collapse = ", "))
  }
  
  # Dataset-specific naming
  is_era5_land <- dataset == "reanalysis-era5-land"

  iso_fragment <- if (!is.null(iso3)) {
    if (!is.character(iso3) || length(iso3) != 1L || !nzchar(iso3)) {
      stop("`iso3` must be a non-empty character scalar when provided.")
    }
    tolower(iso3)
  } else {
    NULL
  }

  # Validate and build admin fragment
  if (!is.null(admin_name) && !nzchar(admin_name)) admin_name <- NULL
  if (is.null(admin_name)) {
    admin_level <- NULL
  } else {
    if (is.null(iso3)) {
      stop("When `admin_name` is supplied, you must also supply `iso3`.")
    }
    if (is.null(admin_level) || length(admin_level) != 1L || is.na(admin_level)) {
      stop("When `admin_name` is supplied, `admin_level` must be a single non-missing value.")
    }
    admin_level <- as.integer(admin_level)
  }

  admin_fragment <- NULL
  if (!is.null(admin_name)) {
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    admin_fragment <- paste0(ids$admin_level, "_", ids$admin_name)
  }

  if (is.null(input_dir) || !nzchar(input_dir)) {
    if (is.null(iso_fragment)) {
      stop("Provide `iso3` when `input_dir` is missing or empty.")
    }
    if (!is.null(admin_name)) {
      ids <- build_location_identifiers(iso3, admin_level, admin_name)
      input_dir <- file.path("data/weather/grib", ids$slug)
    } else {
      input_dir <- file.path("data/weather/grib", iso_fragment)
    }
  }

  # ---- paths & metadata ----
  input_dir     <- path.expand(input_dir)
  if (!dir.exists(input_dir)) {
    stop(sprintf("Input directory does not exist: %s", input_dir))
  }
  if (is.null(iso_fragment)) {
    maybe_iso <- tolower(basename(input_dir))
    if (nchar(maybe_iso) == 3L && grepl("^[a-z]{3}$", maybe_iso)) {
      iso_fragment <- maybe_iso
    }
  }
  if (is.null(processed_dir) || !nzchar(processed_dir)) {
    processed_dir <- file.path(input_dir, "processed")
  }
  processed_dir <- path.expand(processed_dir)
  
  # Create dataset-specific subdirectory
  dataset_subdir <- if (is_era5_land) "land" else "single-levels"
  processed_dir <- file.path(processed_dir, dataset_subdir)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  
  metadata_suffix <- if (!is.null(admin_fragment)) {
    paste0(iso_fragment, "_", admin_fragment)
  } else {
    iso_fragment %||% "general"
  }
  metadata_suffix <- paste0(metadata_suffix, "_", dataset_subdir)
  metadata_file <- file.path(input_dir, sprintf("processed_%s_metadata.json", metadata_suffix))

  .load_metadata <- function() {
    if (file.exists(metadata_file)) {
      out <- tryCatch(jsonlite::fromJSON(metadata_file), error = function(e) NULL)
      if (is.list(out)) return(out)
    }
    list(processed_months = character(), last_updated = NULL, dataset = dataset)
  }
  .save_metadata <- function(meta) {
    meta$last_updated <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
    meta$dataset <- dataset
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), metadata_file)
  }
  .check_file <- function(fp) file.exists(fp) && file.info(fp)$size > 1024

  # ---- Helper: clean variable names from terra layer names ----
  .clean_variable_name <- function(layer_name) {
    # Input: "SFC (Ground or water surface); 2 metre temperature [C]"
    # Output: "2m_temperature"
    
    # Extract part after semicolon and before bracket
    parts <- strsplit(layer_name, ";", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(gsub("[^a-z0-9_]+", "_", tolower(trimws(layer_name))))
    
    var_part <- trimws(parts[2])
    # Remove unit in brackets
    var_part <- sub("\\s*\\[.*?\\]\\s*$", "", var_part)
    
    # Clean up common patterns
    var_part <- gsub("metre", "m", var_part, fixed = TRUE)
    var_part <- gsub(" ", "_", var_part, fixed = TRUE)
    var_part <- gsub("__+", "_", var_part)
    var_part <- tolower(var_part)
    
    return(var_part)
  }

  # ---- terra reader (multi-variable, processes all variables in one file) ----
  .read_grib_long <- function(filepath, year, month, parallel = FALSE) {
    # Handle .zip files (ERA5-Land) by extracting first
    file_to_read <- filepath
    temp_extracted <- NULL
    if (grepl("\\.zip$", filepath, ignore.case = TRUE)) {
      if (!requireNamespace("utils", quietly = TRUE)) stop("Package 'utils' required for .zip files.")
      temp_dir <- tempfile()
      dir.create(temp_dir)
      utils::unzip(filepath, exdir = temp_dir)
      extracted_files <- list.files(temp_dir, pattern = "\\.(grib|nc)$", full.names = TRUE, recursive = TRUE)
      if (length(extracted_files) == 0) {
        unlink(temp_dir, recursive = TRUE)
        stop("No GRIB or NetCDF files found in ZIP: ", basename(filepath))
      }
      file_to_read <- extracted_files[1]  # Use first file
      temp_extracted <- temp_dir
    }
    
    r <- terra::rast(file_to_read)
    
    # Get layer names and clean them for variable identification
    layer_names <- names(r)
    clean_names <- sapply(layer_names, .clean_variable_name, USE.NAMES = FALSE)

    # times (expect one per layer, but they repeat for each variable)
    tvals <- tryCatch(terra::time(r), error = function(e) NULL)
    if (!is.null(tvals) && !inherits(tvals, "POSIXct")) {
      tvals <- suppressWarnings(as.POSIXct(as.character(tvals), tz = "UTC"))
    }
    n <- terra::nlyr(r)
    if (is.null(tvals) || length(tvals) != n) {
      if (verbose) cat(sprintf("  ⚠ time() returned %s; using NA times for %s\n",
                               if (is.null(tvals)) "NULL" else length(tvals),
                               basename(filepath)))
      tvals <- rep(as.POSIXct(NA, tz = "UTC"), n)
    }

    # build long table by reading each layer's grid
    process_layer <- function(i) {
      df <- as.data.frame(r[[i]], xy = TRUE, na.rm = FALSE)
      data.table::setDT(df)
      data.table::setnames(df, c("x","y", names(df)[3]), c("longitude","latitude","value"))
      data.table::set(df, j = "time", value = tvals[i])
      data.table::set(df, j = "variable_name", value = clean_names[i])
      data.table::set(df, j = "grib_variable_name", value = layer_names[i])
      data.table::set(df, j = "year", value = as.integer(year))
      data.table::set(df, j = "month", value = as.integer(month))
      data.table::set(df, j = "dataset", value = dataset)
      df
    }

    if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
      lst <- future.apply::future_lapply(seq_len(n), process_layer)
    } else {
      lst <- lapply(seq_len(n), process_layer)
    }
    out <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    base_cols <- c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month","dataset")
    data.table::setcolorder(out, base_cols[base_cols %in% names(out)])
    
    # Clean up temp extraction if needed
    if (!is.null(temp_extracted) && dir.exists(temp_extracted)) {
      unlink(temp_extracted, recursive = TRUE)
    }
    
    out[!is.na(value)]
  }

  # ---- discovery ----
  .discover <- function() {
    if (verbose) {
      cat("ERA5 Monthly Compiler (Multi-Variable Format)\n", strrep("=", 60), "\n", sep = "")
      cat(sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      cat("🔍 Scanning directory for ERA5 multi-variable files...\n")
      cat(sprintf("   Dataset: %s\n", dataset))
    }
    
    # Build pattern for multi-variable files (NO variable name in filename)
    # ERA5-Land: era5land_<iso3>_<admin>_YYYY_MM.zip
    # ERA5 Single: era5_<iso3>_<admin>_YYYY_MM.grib
    prefix <- if (is_era5_land) "era5land" else "era5"
    ext <- if (is_era5_land) "zip" else "grib"
    
    patt <- if (!is.null(admin_fragment)) {
      sprintf("^%s_%s_%s_\\d{4}_\\d{2}\\.%s$", prefix, iso_fragment, gsub("_", "_", admin_fragment, fixed = TRUE), ext)
    } else if (!is.null(iso_fragment)) {
      sprintf("^%s_%s_\\d{4}_\\d{2}\\.%s$", prefix, iso_fragment, ext)
    } else {
      sprintf("^%s_[a-z]{3}_\\d{4}_\\d{2}\\.%s$", prefix, ext)
    }
    
    files <- list.files(input_dir, pattern = patt, full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) return(list())

    by_month <- list(); count <- 0L
    for (fp in files) {
      fn <- basename(fp)
      # Remove extension (.grib or .zip)
      stem <- sub("\\.(grib|zip)$", "", fn, ignore.case = TRUE)
      parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
      
      # Check for era5 or era5land prefix
      if (length(parts) >= 4 && parts[1] %in% c("era5", "era5land")) {
        file_dataset_type <- if (parts[1] == "era5land") "reanalysis-era5-land" else "reanalysis-era5-single-levels"
        
        # Skip if dataset doesn't match
        if (file_dataset_type != dataset) next
        
        iso_part <- tolower(parts[2])
        if (!is.null(iso_fragment) && !identical(iso_part, iso_fragment)) next
        
        # Determine offset based on admin fragment presence
        # Format: era5[land]_<iso3>_YYYY_MM OR era5[land]_<iso3>_<level>_<name>_YYYY_MM
        has_admin <- !is.null(admin_fragment)
        offset <- if (has_admin) {
          # parts[3]=level, parts[4]=name, parts[5]=year, parts[6]=month
          4
        } else {
          # parts[3]=year, parts[4]=month
          2
        }
        
        if (length(parts) < (offset + 2)) next
        
        yy <- suppressWarnings(as.integer(parts[offset + 1]))
        mm <- suppressWarnings(as.integer(parts[offset + 2]))
        if (!is.na(yy) && !is.na(mm) && yy >= 1900 && mm >= 1 && mm <= 12) {
          key_suffix <- if (has_admin) paste0("_", admin_fragment) else ""
          key <- sprintf("%s%s_%04d_%02d", iso_part, key_suffix, yy, mm)
          if (is.null(by_month[[key]])) {
            by_month[[key]] <- list(
              iso = iso_part, 
              year = yy, 
              month = mm, 
              admin_fragment = admin_fragment, 
              file_dataset = file_dataset_type,
              file = fp  # Single file path (contains all variables)
            )
            count <- count + 1L
          }
        }
      }
    }
    if (verbose) {
      file_type <- if (is_era5_land) "ZIP" else "GRIB"
      cat(sprintf("✓ Found %d %s files (multi-variable format)\n", count, file_type))
    }
    by_month
  }

  # ---- month processor ----
  .process_month <- function(iso_code, year, month, file_path, admin_frag = NULL, file_dataset = NULL) {
    iso_local <- if (!is.null(iso_code) && nzchar(iso_code)) {
      tolower(iso_code)
    } else if (!is.null(iso_fragment)) {
      iso_fragment
    } else {
      "iso"
    }
    key_suffix <- if (!is.null(admin_frag)) paste0("_", admin_frag) else ""
    key <- sprintf("%s%s_%04d_%02d", iso_local, key_suffix, year, month)
    display_suffix <- if (!is.null(admin_frag)) paste0(" [", admin_frag, "]") else ""
    display_key <- sprintf("%s%s %04d-%02d", toupper(iso_local), display_suffix, year, month)
    if (verbose) {
      cat("\n📅 Processing ", display_key, "\n", strrep("-", 40), "\n", sep = "")
    }
    
    year_dir <- file.path(processed_dir, sprintf("%04d", year))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Determine file prefix based on dataset type
    dataset_prefix <- if (is_era5_land) "era5land" else "era5"
    
    file_prefix <- if (!is.null(admin_frag)) {
      sprintf("%s_%s_%s", dataset_prefix, iso_local, admin_frag)
    } else {
      sprintf("%s_%s", dataset_prefix, iso_local)
    }
    out_file <- file.path(year_dir, sprintf("%s_%04d_%02d_all_variables.csv.gz", file_prefix, year, month))

    if (file.exists(out_file) && file.info(out_file)$size > 1024) {
      if (verbose) cat(sprintf("✓ Already processed: %s (%.1f MB)\n",
                               basename(out_file), file.info(out_file)$size/1024^2))
      return(list(success = TRUE, key = key, files_processed = 0L))
    }

    # Process single multi-variable file
    if (!.check_file(file_path)) { 
      if (verbose) cat("⚠ Missing/empty:", basename(file_path), "\n")
      return(list(success = FALSE, key = key, files_processed = 0L))
    }
    
    if (verbose) cat(sprintf("  ✓ Reading multi-variable file: %s\n", basename(file_path)))
    
    df_month <- tryCatch(
      .read_grib_long(file_path, year, month),
      error = function(e) { 
        if (verbose) cat("  ✗ Error:", e$message, "\n")
        data.frame()
      }
    )

    if (nrow(df_month) == 0) {
      if (verbose) cat(sprintf("  ✗ No data processed for %s\n", display_key))
      return(list(success = FALSE, key = key, files_processed = 0L))
    }

    if (verbose) {
      n_vars <- length(unique(df_month$variable_name))
      cat(sprintf("  📊 Extracted %d variables from file\n", n_vars))
    }

    if (verbose) cat(sprintf("  💾 Saving to: %s\n", basename(out_file)))
    withr::local_options(list(datatable.fread.datatable = TRUE))
    data.table::fwrite(df_month, file = out_file)

    if (verbose) {
      size_mb <- file.info(out_file)$size/1024^2
      cat(sprintf("  ✓ Saved: %s rows, %.1f MB\n",
                  format(nrow(df_month), big.mark = ","), size_mb))
    }
    rm(df_month); gc()
    list(success = TRUE, key = key, files_processed = 1L)
  }

  # ---- recent combiner ----
  .combine_recent <- function(n = 3L) {
    if (n <= 0) return(invisible())
    if (verbose) cat("\n📋 Creating recent data summary...\n")
    
    year_dirs <- list.dirs(processed_dir, full.names = TRUE, recursive = FALSE)
    if (!length(year_dirs)) {
      if (verbose) cat("  ⚠ No year directories found.\n")
      return(invisible())
    }
    
    # Match files based on dataset type
    dataset_prefix <- if (is_era5_land) "era5land" else "era5"
    
    recent_files <- character(0)
    file_patt <- if (!is.null(admin_fragment)) {
      sprintf("^%s_%s_%s_.*_all_variables\\.csv\\.gz$", dataset_prefix, iso_fragment, gsub("_", "_", admin_fragment, fixed = TRUE))
    } else if (!is.null(iso_fragment)) {
      sprintf("^%s_%s_.*_all_variables\\.csv\\.gz$", dataset_prefix, iso_fragment)
    } else {
      sprintf("^%s_[a-z]{3}_.*_all_variables\\.csv\\.gz$", dataset_prefix)
    }
    
    for (yd in rev(sort(year_dirs))) {
      mfiles <- list.files(yd, pattern = file_patt, full.names = TRUE)
      if (length(mfiles)) recent_files <- c(recent_files, rev(sort(mfiles)))
      if (length(recent_files) >= n) break
    }
    recent_files <- head(recent_files, n)
    if (!length(recent_files)) {
      if (verbose) cat("  ⚠ No recent files found.\n")
      return(invisible())
    }
    
    combined_suffix <- if (!is.null(admin_fragment)) {
      paste0(iso_fragment, "_", admin_fragment)
    } else {
      iso_fragment %||% "all"
    }
    combined_file <- file.path(input_dir, sprintf("era5_%s_%s_recent_%dmonths.csv.gz", combined_suffix, dataset_subdir, n))
    
    if (verbose) cat(sprintf("  📊 Combining %d files from %s...\n", length(recent_files), dataset_subdir))
    lst <- lapply(recent_files, function(f) data.table::fread(f, showProgress = FALSE, data.table = FALSE))
    df  <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::fwrite(df, combined_file)
    if (verbose) {
      size_mb <- file.info(combined_file)$size/1024^2
      cat(sprintf("  💾 Saved %s: %s rows, %.1f MB\n", basename(combined_file), format(nrow(df), big.mark=","), size_mb))
    }
    invisible()
  }

  # ---- driver ----
  meta <- .load_metadata()
  processed <- unique(as.character(meta$processed_months %||% character()))
  processed <- tolower(processed)
  if (!is.null(iso_fragment) && length(processed)) {
    needs_prefix <- !grepl("^[a-z]{3}_\\d{4}_\\d{2}$", processed)
    processed[needs_prefix] <- sprintf("%s_%s", iso_fragment, processed[needs_prefix])
  }
  by_month <- .discover()
  if (!length(by_month)) {
    if (verbose) cat("✗ No valid ERA5 multi-variable files found.\n")
    return(invisible(list(months_processed = 0L, files_processed = 0L)))
  }
  if (verbose) {
    cat(sprintf("📊 Found data for %d months\n", length(by_month)))
    cat(sprintf("📋 Previously processed: %d months\n", length(processed)))
  }

  total_months <- 0L; total_files <- 0L
  for (key in sort(names(by_month))) {
    if (key %in% processed) { if (verbose) cat(sprintf("⏭ Skipping %s (already processed)\n", key)); next }
    mi  <- by_month[[key]]
    res <- .process_month(mi$iso, mi$year, mi$month, mi$file, mi$admin_fragment, mi$file_dataset)
    if (isTRUE(res$success)) {
      processed     <- unique(c(processed, res$key))
      total_months  <- total_months + 1L
      total_files   <- total_files  + res$files_processed
    }
  }

  meta$processed_months <- processed
  .save_metadata(meta)
  .combine_recent(recent_n)

  if (verbose) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("FINAL SUMMARY\n", strrep("=", 60), "\n", sep = "")
    cat(sprintf("Months processed this run: %d\n", total_months))
    cat(sprintf("Files processed this run: %d\n", total_files))
    cat(sprintf("Total months available: %d\n", length(by_month)))
    cat(sprintf("Dataset: %s\n", dataset))
    cat(sprintf("Output directory: %s\n", processed_dir))
    cat(strrep("=", 60), "\n")
    cat("✓ ERA5 monthly processing completed!\n")
  }

  invisible(list(
    months_processed = total_months,
    files_processed  = total_files,
    output_dir       = processed_dir,
    dataset = dataset
  ))
}

`%||%` <- function(a, b) if (is.null(a)) b else a
