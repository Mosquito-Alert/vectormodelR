#' Compile ERA5 monthly CSVs from GRIB files (terra-only)
#'
#' Scans `input_dir` for files named `era5_<iso3>_YYYY_MM_<var>.grib`, converts each
#' month into a single long-format CSV.GZ under `processed/YYYY/`, and writes a
#' recent N-month summary in `input_dir`.
#'
#' @param input_dir Directory containing per-variable GRIB files. If NULL/empty and `iso3`
#'   supplied, defaults to `file.path("data/weather/grib", tolower(iso3))`.
#' @param processed_dir Output dir for monthly CSVs. Defaults to `<input_dir>/processed`.
#' @param iso3 Optional ISO3 code (character). Used to resolve `input_dir` (when missing)
#'   and to build output filenames.
#' @param recent_n Number of most recent monthly CSVs to merge into a summary (default 3).
#' @param verbose Print progress.
#'
#' @return (invisibly) a list with summary info.
#' @importFrom terra rast time nlyr
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom withr local_options
#' @export
compile_era5_data_v2 <- function(
  input_dir = NULL,
  processed_dir = NULL,
  iso3 = NULL,
  recent_n  = 3,
  verbose   = TRUE
) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required.")

  iso_fragment <- if (!is.null(iso3)) {
    if (!is.character(iso3) || length(iso3) != 1L || !nzchar(iso3)) {
      stop("`iso3` must be a non-empty character scalar when provided.")
    }
    tolower(iso3)
  } else {
    NULL
  }

  if (is.null(input_dir) || !nzchar(input_dir)) {
    if (is.null(iso_fragment)) {
      stop("Provide `iso3` when `input_dir` is missing or empty.")
    }
    input_dir <- file.path("data/weather/grib", iso_fragment)
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
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  metadata_file <- file.path(input_dir, sprintf("processed_%s_metadata.json", iso_fragment %||% "general"))

  .load_metadata <- function() {
    if (file.exists(metadata_file)) {
      out <- tryCatch(jsonlite::fromJSON(metadata_file), error = function(e) NULL)
      if (is.list(out)) return(out)
    }
    list(processed_months = character(), last_updated = NULL)
  }
  .save_metadata <- function(meta) {
    meta$last_updated <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), metadata_file)
  }
  .check_file <- function(fp) file.exists(fp) && file.info(fp)$size > 1024

  # ---- terra reader (per-layer, robust) ----
  .read_grib_long <- function(filepath, variable_name, year, month, parallel = FALSE) {
    r <- terra::rast(filepath)

    # times (expect one per layer)
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
      df[, `:=`(
        time               = tvals[i],
        variable_name      = variable_name,
        grib_variable_name = variable_name,   # one name; time carries timestamps
        year               = as.integer(year),
        month              = as.integer(month)
      )]
      df[]
    }

    if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
      lst <- future.apply::future_lapply(seq_len(n), process_layer)
    } else {
      lst <- lapply(seq_len(n), process_layer)
    }
    out <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::setcolorder(out,
      c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))
    out[!is.na(value)]
  }

  # ---- discovery ----
  .discover <- function() {
    if (verbose) {
      cat("ERA5 (GRIB) Monthly Compiler\n", strrep("=", 60), "\n", sep = "")
      cat(sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      cat("🔍 Scanning directory for GRIB files...\n")
    }
    patt  <- if (!is.null(iso_fragment)) {
      sprintf("^era5_%s_\\d{4}_\\d{2}_.+\\.grib$", iso_fragment)
    } else {
      "^era5_[a-z]{3}_\\d{4}_\\d{2}_.+\\.grib$"
    }
    files <- list.files(input_dir, pattern = patt, full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) return(list())

    by_month <- list(); count <- 0L
    for (fp in files) {
      fn <- basename(fp)
      stem <- sub("\\.grib$", "", fn, ignore.case = TRUE)
      parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
      if (length(parts) >= 5 && parts[1] == "era5") {
        iso_part <- tolower(parts[2])
        if (!is.null(iso_fragment) && !identical(iso_part, iso_fragment)) next
        yy <- suppressWarnings(as.integer(parts[3]))
        mm <- suppressWarnings(as.integer(parts[4]))
        if (!is.na(yy) && !is.na(mm) && yy >= 1900 && mm >= 1 && mm <= 12) {
          variable <- paste(parts[-(1:4)], collapse = "_")
          key <- sprintf("%s_%04d_%02d", iso_part, yy, mm)
          if (is.null(by_month[[key]])) by_month[[key]] <- list(iso = iso_part, year = yy, month = mm, variables = list())
          by_month[[key]]$variables[[variable]] <- fp
          count <- count + 1L
        }
      }
    }
    if (verbose) cat(sprintf("✓ Found %d GRIB files across %d months\n", count, length(by_month)))
    by_month
  }

  # ---- month processor ----
  .process_month <- function(iso_code, year, month, var_list) {
    iso_local <- if (!is.null(iso_code) && nzchar(iso_code)) {
      tolower(iso_code)
    } else if (!is.null(iso_fragment)) {
      iso_fragment
    } else {
      "iso"
    }
    key <- sprintf("%s_%04d_%02d", iso_local, year, month)
    display_key <- sprintf("%s %04d-%02d", toupper(iso_local), year, month)
    if (verbose) {
      cat("\n📅 Processing ", display_key, "\n", strrep("-", 40), "\n", sep = "")
    }
    year_dir <- file.path(processed_dir, sprintf("%04d", year))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(year_dir, sprintf("era5_%s_%04d_%02d_all_variables.csv.gz", iso_local, year, month))

    if (file.exists(out_file) && file.info(out_file)$size > 1024) {
      if (verbose) cat(sprintf("✓ Already processed: %s (%.1f MB)\n",
                               basename(out_file), file.info(out_file)$size/1024^2))
      return(list(success = TRUE, key = key, files_processed = 0L))
    }

    monthly <- list(); k <- 0L
    for (var in names(var_list)) {
      fp <- var_list[[var]]
      if (!.check_file(fp)) { if (verbose) cat("⚠ Missing/empty:", basename(fp), "\n"); next }
      if (verbose) cat(sprintf("  ✓ %s\n", var))
      dt <- tryCatch(.read_grib_long(fp, var, year, month),
                     error = function(e) { if (verbose) cat("  ✗ Error:", e$message, "\n"); data.frame() })
      if (nrow(dt)) { k <- k + 1L; monthly[[k]] <- dt }
    }

    if (k == 0L) {
      if (verbose) cat(sprintf("  ✗ No data processed for %s\n", display_key))
      return(list(success = FALSE, key = key, files_processed = 0L))
    }

    if (verbose) cat(sprintf("  📊 Combining %d variables...\n", k))
    df_month <- data.table::rbindlist(monthly, use.names = TRUE, fill = TRUE)

    if (verbose) cat(sprintf("  💾 Saving to: %s\n", basename(out_file)))
    withr::local_options(list(datatable.fread.datatable = TRUE))
    data.table::fwrite(df_month, file = out_file)

    if (verbose) {
      size_mb <- file.info(out_file)$size/1024^2
      cat(sprintf("  ✓ Saved: %s rows, %.1f MB\n",
                  format(nrow(df_month), big.mark = ","), size_mb))
    }
    rm(df_month, monthly); gc()
    list(success = TRUE, key = key, files_processed = k)
  }

  # ---- recent combiner ----
  .combine_recent <- function(n = 3L) {
    if (n <= 0) return(invisible())
    if (verbose) cat("\n📋 Creating recent data summary...\n")
    year_dirs <- list.dirs(processed_dir, full.names = TRUE, recursive = FALSE)
    if (!length(year_dirs)) { if (verbose) cat("  ⚠ No year directories found.\n"); return(invisible()) }

    recent_files <- character(0)
    file_patt <- if (!is.null(iso_fragment)) {
      sprintf("^era5_%s_.*_all_variables\\.csv\\.gz$", iso_fragment)
    } else {
      "^era5_[a-z]{3}_.*_all_variables\\.csv\\.gz$"
    }
    for (yd in rev(sort(year_dirs))) {
      mfiles <- list.files(yd, pattern = file_patt, full.names = TRUE)
      if (length(mfiles)) recent_files <- c(recent_files, rev(sort(mfiles)))
      if (length(recent_files) >= n) break
    }
    recent_files <- head(recent_files, n)
    if (!length(recent_files)) { if (verbose) cat("  ⚠ No recent monthly files found.\n"); return(invisible()) }

    combined_file <- file.path(input_dir, sprintf("era5_%s_recent_%dmonths.csv.gz", iso_fragment %||% "all", n))
    if (verbose) cat(sprintf("  📊 Combining %d files...\n", length(recent_files)))
    lst <- lapply(recent_files, function(f) data.table::fread(f, showProgress = FALSE, data.table = FALSE))
    df  <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::fwrite(df, combined_file)
    if (verbose) {
      size_mb <- file.info(combined_file)$size/1024^2
      cat(sprintf("  💾 Saved: %s rows, %.1f MB\n", format(nrow(df), big.mark=","), size_mb))
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
    if (verbose) cat("✗ No valid ERA5 GRIB files found.\n")
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
    res <- .process_month(mi$iso, mi$year, mi$month, mi$variables)
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
    cat(sprintf("Output directory: %s\n", processed_dir), "\n")
    cat(strrep("=", 60), "\n")
    cat("✓ ERA5 GRIB monthly processing completed!\n")
  }

  invisible(list(months_processed = total_months,
                 files_processed  = total_files,
                 output_dir       = processed_dir))
}

`%||%` <- function(a, b) if (is.null(a)) b else a