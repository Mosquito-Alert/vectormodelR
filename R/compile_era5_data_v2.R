#' Compile ERA5 monthly CSVs from GRIB files (terra-only)
#'
#' Scans `input_dir` for files named `era5_YYYY_MM_<var>.grib`, converts each
#' month into a single long-format CSV.GZ under `processed/YYYY/`, and writes a
#' `era5_recent_<n>months.csv.gz` in `input_dir`.
#'
#' @param input_dir     Directory containing per-variable GRIB files.
#' @param processed_dir Output directory for monthly CSVs (default: file.path(input_dir, "processed")).
#' @param recent_n      Number of most recent monthly CSVs to merge into a summary (default 3).
#' @param verbose       Logical; print progress.
#'
#' @return (Invisibly) a list with summary info.
#' @import data.table
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom withr local_options
#' @export
#'
#' @examples
#' \dontrun{
#' compile_era5_data_v2(
#'   input_dir     = "data/era5/grib",
#'   processed_dir = "data/era5/grib/processed",
#'   recent_n      = 3,
#'   verbose       = TRUE
#' )
#' }
compile_era5_data_v2 <- function(
  input_dir,
  processed_dir = file.path(input_dir, "processed"),
  recent_n      = 3,
  verbose       = TRUE
) {
  # ---- deps & basic checks ----
  stopifnot(dir.exists(path.expand(input_dir)))
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required.")
  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required.")
  if (!requireNamespace("withr", quietly = TRUE))
    stop("Package 'withr' is required.")

  # ---- paths & metadata ----
  input_dir     <- path.expand(input_dir)
  processed_dir <- path.expand(processed_dir)
  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
  metadata_file <- file.path(input_dir, "processing_metadata.json")

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
  .check_file <- function(fp) file.exists(fp) && isTRUE(file.info(fp)$size > 1024)

  # ---- terra reader (robust: per-layer → long) ----
  .read_with_terra <- function(filepath, variable_name, year, month, parallel = FALSE) {
    r <- terra::rast(filepath)

    # time vector
    tvals <- tryCatch(terra::time(r), error = function(e) NULL)
    if (!is.null(tvals) && !inherits(tvals, "POSIXct")) {
      # some GDAL builds give character; coerce safely
      tvals <- suppressWarnings(as.POSIXct(as.character(tvals), tz = "UTC"))
    }

    n <- terra::nlyr(r)
    if (is.null(tvals) || length(tvals) != n) {
      if (verbose) cat(sprintf("    ⚠ time length (%s) != layers (%d); assigning NA.\n",
                               if (is.null(tvals)) "NULL" else length(tvals), n))
      tvals <- rep(as.POSIXct(NA, tz="UTC"), n)
    }

    # per-layer loop avoids the wide->long mismatch that produced 1 column earlier
    one_layer <- function(i) {
      df <- as.data.frame(r[[i]], xy = TRUE, na.rm = FALSE)
      data.table::setDT(df)
      # ensure column names
      valcol <- setdiff(names(df), c("x","y"))[1]
      data.table::setnames(df, c("x","y", valcol), c("longitude","latitude","value"))
      df[, `:=`(
        time              = tvals[i],
        variable_name     = variable_name,
        grib_variable_name= paste0(variable_name, "_layer", i),
        year              = as.integer(year),
        month             = as.integer(month)
      )]
      df[]
    }

    # no extra deps by default
    lst <- lapply(seq_len(n), one_layer)
    out <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::setcolorder(out, c("latitude","longitude","time",
                                   "variable_name","grib_variable_name",
                                   "value","year","month"))
    out[!is.na(value)]
  }

  # ---- discovery (GRIB only) ----
  .discover_available <- function() {
    if (verbose) {
      cat("ERA5 Data Wrangling - Monthly File Organization (GRIB, terra)\n")
      cat(strrep("=", 60), "\n")
      cat(sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      cat("🔍 Scanning directory for available GRIB files...\n")
    }
    files <- list.files(input_dir, pattern = "^era5_.*\\.grib$", full.names = TRUE)
    if (!length(files)) return(list())

    data_by_month <- list(); file_count <- 0L
    for (fp in files) {
      fn <- basename(fp)
      stem <- sub("\\.grib$", "", fn, ignore.case = TRUE)
      parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
      if (length(parts) >= 4 && parts[1] == "era5") {
        yy <- suppressWarnings(as.integer(parts[2]))
        mm <- suppressWarnings(as.integer(parts[3]))
        if (!is.na(yy) && !is.na(mm) && yy >= 1900 && mm >= 1 && mm <= 12) {
          variable <- paste(parts[-(1:3)], collapse = "_")
          key <- sprintf("%04d_%02d", yy, mm)
          if (is.null(data_by_month[[key]])) {
            data_by_month[[key]] <- list(year = yy, month = mm, variables = list())
          }
          data_by_month[[key]]$variables[[variable]] <- fp
          file_count <- file_count + 1L
        }
      }
    }
    if (verbose) cat(sprintf("✓ Found %d GRIB files organized into %d months\n",
                             file_count, length(data_by_month)))
    data_by_month
  }

  # ---- month processor ----
  .process_month <- function(year, month, var_list) {
    key <- sprintf("%04d_%02d", year, month)
    if (verbose) {
      cat(sprintf("\n📅 Processing %s\n", gsub("_", "-", key)))
      cat(strrep("-", 40), "\n")
    }

    year_dir <- file.path(processed_dir, sprintf("%04d", year))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(year_dir, sprintf("era5_%04d_%02d_all_variables.csv.gz", year, month))

    if (file.exists(out_file) && file.info(out_file)$size > 1024) {
      if (verbose) cat(sprintf("✓ Already processed: %s (%.1f MB)\n",
                               basename(out_file), file.info(out_file)$size/1024^2))
      return(list(success = TRUE, key = key, files_processed = 0L))
    }

    monthly <- list(); k <- 0L
    for (var in names(var_list)) {
      fp <- var_list[[var]]
      if (!.check_file(fp)) {
        if (verbose) cat("⚠ Missing/too-small file:", basename(fp), "\n")
        next
      }
      if (verbose) {
        # quick peek for logging only (no heavy conversion yet)
        rr <- try(terra::rast(fp), silent = TRUE)
        if (!inherits(rr, "try-error")) {
          cat(sprintf("  ✓ %s — layers: %d, time stamps: %s\n",
                      var, terra::nlyr(rr),
                      tryCatch(length(terra::time(rr)), error = function(e) "NA")))
        }
      }
      dt <- try(.read_with_terra(fp, var, year, month), silent = TRUE)
      if (inherits(dt, "try-error") || !nrow(dt)) {
        if (verbose) cat(sprintf("  ⚠ No data from: %s\n", var))
      } else {
        k <- k + 1L; monthly[[k]] <- dt
      }
    }

    if (k > 0) {
      if (verbose) cat(sprintf("  📊 Combining %d variables...\n", k))
      df_month <- data.table::rbindlist(monthly, use.names = TRUE, fill = TRUE)

      if (verbose) cat(sprintf("  💾 Saving to: %s\n", basename(out_file)))
      withr::local_options(list(datatable.fread.datatable = TRUE))
      data.table::fwrite(df_month, file = out_file)

      if (verbose) {
        size_mb <- file.info(out_file)$size/1024^2
        cat(sprintf("  ✓ Saved: %s rows, %.1f MB\n",
                    format(nrow(df_month), big.mark=","), size_mb))
      }
      rm(df_month, monthly); gc()
      return(list(success = TRUE, key = key, files_processed = k))
    } else {
      if (verbose) cat(sprintf("  ✗ No data processed for %s\n", gsub("_","-", key)))
      return(list(success = FALSE, key = key, files_processed = 0L))
    }
  }

  # ---- recent combiner ----
  .create_recent <- function(n = 3L) {
    if (n <= 0) return(invisible())
    if (verbose) cat("\n📋 Creating recent data summary...\n")

    year_dirs <- list.dirs(processed_dir, full.names = TRUE, recursive = FALSE)
    if (!length(year_dirs)) {
      if (verbose) cat("  ⚠ No year directories found.\n")
      return(invisible())
    }
    recent_files <- character(0)
    for (yd in rev(sort(year_dirs))) {
      mfiles <- list.files(yd, pattern = "^era5_.*_all_variables\\.csv\\.gz$", full.names = TRUE)
      if (length(mfiles)) recent_files <- c(recent_files, rev(sort(mfiles)))
      if (length(recent_files) >= n) break
    }
    recent_files <- head(recent_files, n)
    if (!length(recent_files)) {
      if (verbose) cat("  ⚠ No recent monthly files found to combine.\n")
      return(invisible())
    }

    combined_file <- file.path(input_dir, sprintf("era5_recent_%dmonths.csv.gz", n))
    if (verbose) cat(sprintf("  📊 Combining %d recent files...\n", length(recent_files)))
    lst <- lapply(recent_files, function(p) data.table::fread(p, showProgress = FALSE, data.table = FALSE))
    df  <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::fwrite(df, combined_file)
    if (verbose) {
      size_mb <- file.info(combined_file)$size/1024^2
      cat(sprintf("  💾 Saved recent summary: %s rows, %.1f MB\n",
                  format(nrow(df), big.mark=","), size_mb))
    }
    invisible()
  }

  # ---- driver ----
  meta <- .load_metadata()
  processed <- unique(as.character(meta$processed_months %||% character()))
  data_by_month <- .discover_available()
  if (!length(data_by_month)) {
    if (verbose) cat("✗ No valid ERA5 GRIB files found.\n")
    return(invisible(list(months_processed = 0L, files_processed = 0L)))
  }
  if (verbose) {
    cat(sprintf("📊 Found data for %d months\n", length(data_by_month)))
    cat(sprintf("📋 Previously processed: %d months\n", length(processed)))
  }

  total_months <- 0L
  total_files  <- 0L
  for (key in sort(names(data_by_month))) {
    if (key %in% processed) {
      if (verbose) cat(sprintf("⏭ Skipping %s (already processed)\n", key))
      next
    }
    mi  <- data_by_month[[key]]
    res <- .process_month(mi$year, mi$month, mi$variables)
    if (isTRUE(res$success)) {
      processed     <- unique(c(processed, res$key))
      total_months  <- total_months + 1L
      total_files   <- total_files  + res$files_processed
    }
  }

  meta$processed_months <- processed
  .save_metadata(meta)
  .create_recent(recent_n)

  if (verbose) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("FINAL SUMMARY\n")
    cat(strrep("=", 60), "\n")
    cat(sprintf("Months processed this run: %d\n", total_months))
    cat(sprintf("Files processed this run: %d\n", total_files))
    cat(sprintf("Total months available: %d\n", length(data_by_month)))
    cat(sprintf("Output directory: %s\n", processed_dir), "\n")
    cat(strrep("=", 60), "\n")
    cat("✓ ERA5 monthly processing completed!\n")
  }

  invisible(list(months_processed = total_months,
                 files_processed  = total_files,
                 output_dir       = processed_dir))
}

`%||%` <- function(a, b) if (is.null(a)) b else a