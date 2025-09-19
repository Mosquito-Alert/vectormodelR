#' Compile ERA5 monthly CSVs from GRIB/NetCDF files
#'
#' Scans `input_dir` for `era5_YYYY_MM_<var>.<ext>` files, converts each month
#' into a single long-format CSV.GZ under `processed/YYYY/`, and writes a
#' 3-month recency file in `input_dir`.
#'
#' @param input_dir  Directory containing per-variable files.
#' @param processed_dir Output directory for monthly CSVs (default: file.path(input_dir, "processed")).
#' @param file_ext   "grib" or "nc".
#' @param prefer     "terra" or "stars" (primary reader; the other is fallback).
#' @param recent_n   Number of most recent monthly CSVs to merge into a summary (default 3).
#' @param verbose    Logical; print progress.
#'
#' @return (Invisibly) a list with summary info.
#' @import data.table
#' @importFrom stars read_stars
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom withr local_options
#' @export
compile_era5_monthly <- function(
  input_dir,
  processed_dir = file.path(input_dir, "processed"),
  file_ext  = c("grib", "nc"),
  prefer    = c("terra", "stars"),
  recent_n  = 3,
  verbose   = TRUE
) {
  # ---- deps (namespaced calls; no need to import) ----
  stopifnot(dir.exists(path.expand(input_dir)))
  file_ext <- match.arg(tolower(file_ext), c("grib", "nc"))
  prefer   <- match.arg(tolower(prefer),   c("terra", "stars"))

  if (!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Package 'jsonlite' is required.")
  if (!requireNamespace("withr", quietly = TRUE))
    stop("Package 'withr' is required.")
  if (!requireNamespace("stars", quietly = TRUE) && prefer == "stars")
    stop("prefer='stars' but package 'stars' is not installed.")
  if (!requireNamespace("terra", quietly = TRUE) && prefer == "terra")
    stop("prefer='terra' but package 'terra' is not installed.")

  # ---- paths & metadata helpers ----
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
  .check_file <- function(fp) file.exists(fp) && file.info(fp)$size > 1024

  # ---- robust time parser (for stars + odd strings) ----
  .parse_time_vec <- function(x) {
    if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
    if (is.numeric(x)) return(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))
    if (is.character(x)) {
      if (requireNamespace("fasttime", quietly = TRUE)) {
        out <- try(fasttime::fastPOSIXct(x, tz = "UTC"), silent = TRUE)
        if (!inherits(out, "try-error")) return(out)
      }
      if (requireNamespace("lubridate", quietly = TRUE)) {
        out <- try(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC"), silent = TRUE)
        if (!inherits(out, "try-error") && any(!is.na(out))) return(as.POSIXct(out))
        out <- try(lubridate::parse_date_time(
          x,
          orders = c("Ymd HMS","Ymd HM","Ymd H","Ymd",
                     "Y-m-d H:M:S","Y-m-d H:M","Y/m/d H:M:S","Y/m/d H:M"),
          tz = "UTC", quiet = TRUE
        ), silent = TRUE)
        if (!inherits(out, "try-error") && any(!is.na(out))) return(as.POSIXct(out))
      }
      return(suppressWarnings(as.POSIXct(x, tz = "UTC")))
    }
    as.POSIXct(NA)
  }

  # ---- readers (return long data.frame) ----
  .read_with_stars <- function(fp, variable_name, year, month) {
    s <- stars::read_stars(fp, proxy = TRUE)

    # Try to get time values out of stars dimensions (time or band)
    dims <- tryCatch(stars::st_dimensions(s), error = function(e) NULL)
    time_vals <- NULL
    if (!is.null(dims)) {
      if ("time" %in% names(dims) && !is.null(dims$time$values)) {
        time_vals <- dims$time$values
      } else if ("band" %in% names(dims) && !is.null(dims$band$values)) {
        time_vals <- dims$band$values
      }
    }

    # Combine multiple arrays into one long table
    s <- tryCatch(do.call(c, c(unclass(s), along = "grib_variable_name")),
                  error = function(e) s)
    df <- as.data.frame(s, long = TRUE, na.rm = FALSE)
    data.table::setDT(df)

    # coordinates: geometry → lon/lat if present
    if ("geometry" %in% names(df)) {
      if (!requireNamespace("sf", quietly = TRUE))
        stop("stars returned a geometry column but package 'sf' is not installed.")
      coords <- sf::st_coordinates(df$geometry)
      df$longitude <- coords[, 1]
      df$latitude  <- coords[, 2]
      df$geometry  <- NULL
    }
    # rename common x/y/lat/lon options
    if (!"longitude" %in% names(df) && "x"   %in% names(df)) data.table::setnames(df, "x", "longitude")
    if (!"latitude"  %in% names(df) && "y"   %in% names(df)) data.table::setnames(df, "y", "latitude")
    if (!"longitude" %in% names(df) && "lon" %in% names(df)) data.table::setnames(df, "lon", "longitude")
    if (!"latitude"  %in% names(df) && "lat" %in% names(df)) data.table::setnames(df, "lat", "latitude")

    # ensure time column
    if (!"time" %in% names(df)) {
      if ("band" %in% names(df) && length(time_vals)) {
        max_band <- min(max(df$band, na.rm = TRUE), length(time_vals))
        tmap <- data.table::data.table(
          band = seq_len(max_band),
          time = .parse_time_vec(time_vals[seq_len(max_band)])
        )
        df <- merge(df, tmap, by = "band", all.x = TRUE, sort = FALSE)
        df$band <- NULL
      } else {
        df$time <- as.POSIXct(NA)
      }
    } else if (!inherits(df$time, "POSIXt")) {
      df$time <- .parse_time_vec(df$time)
    }

    # variable name column
    if (!"grib_variable_name" %in% names(df)) {
      if ("variable" %in% names(df)) {
        data.table::setnames(df, "variable", "grib_variable_name")
      } else {
        df$grib_variable_name <- variable_name
      }
    }
    # value column
    if (!"value" %in% names(df)) {
      data.table::setnames(df, tail(names(df), 1L), "value")
    }

    # keep and coerce
    keep <- intersect(c("latitude","longitude","time","grib_variable_name","value"), names(df))
    if (!length(keep)) return(data.frame())
    df <- df[, keep, drop = FALSE]

    if (!all(c("latitude","longitude") %in% names(df)))
      stop("Latitude/longitude columns not found after reading file.")

    df$latitude  <- suppressWarnings(as.numeric(df$latitude))
    df$longitude <- suppressWarnings(as.numeric(df$longitude))
    if (inherits(df$value, "difftime")) {
      df$value <- as.numeric(df$value, units = "secs")
    } else if (is.list(df$value)) {
      df$value <- suppressWarnings(as.numeric(unlist(df$value)))
    } else {
      df$value <- suppressWarnings(as.numeric(df$value))
    }

    # add metadata
    df$variable_name <- variable_name
    df$year  <- as.integer(year)
    df$month <- as.integer(month)

    # reorder
    data.table::setcolorder(df, c("latitude","longitude","time","variable_name",
                                  "grib_variable_name","value","year","month"))

    n0 <- nrow(df)
    df <- df[!is.na(df$value), , drop = FALSE]
    if (verbose && nrow(df) < n0) {
      cat(sprintf("  ⚠ Removed %d rows with invalid values from %s\n",
                  n0 - nrow(df), basename(fp)))
    }
    df
  }

  .read_with_terra <- function(fp, variable_name, year, month) {
    r <- terra::rast(fp)
    tvals <- tryCatch(terra::time(r), error = function(e) NULL)

    # pull all values to a data.frame (xy + layers)
    df <- as.data.frame(r, xy = TRUE, cells = FALSE, na.rm = FALSE)
    data.table::setDT(df)
    data.table::setnames(df, c("x","y"), c("longitude","latitude"))

    data_cols <- setdiff(names(df), c("longitude","latitude"))
    if (!length(data_cols)) return(data.frame())

    # long pivot (namespaced to avoid data.table import)
    df_long <- data.table::melt.data.table(
      df,
      id.vars = c("latitude","longitude"),
      measure.vars = data_cols,
      variable.name = "grib_variable_name",
      value.name = "value"
    )

    # time mapping
    df_long$time <- as.POSIXct(NA)
    if (!is.null(tvals) && length(tvals) == length(data_cols)) {
      layermap <- data.table::data.table(grib_variable_name = data_cols, tval = tvals)
      df_long <- merge(df_long, layermap, by = "grib_variable_name", all.x = TRUE, sort = FALSE)
      data.table::setnames(df_long, "tval", "time")
    }

    df_long$variable_name <- variable_name
    df_long$year  <- as.integer(year)
    df_long$month <- as.integer(month)

    data.table::setcolorder(df_long, c("latitude","longitude","time","variable_name",
                                       "grib_variable_name","value","year","month"))
    df_long <- df_long[!is.na(df_long$value), ]
    df_long
  }

  .load_grib_to_long_format <- function(filepath, variable_name, year, month) {
    primary   <- if (prefer == "terra") .read_with_terra else .read_with_stars
    secondary <- if (prefer == "terra") .read_with_stars else .read_with_terra

    out <- tryCatch(primary(filepath, variable_name, year, month),
                    error = function(e) {
                      if (verbose) cat(sprintf("  ⚠ %s read failed for %s (%s). Trying fallback...\n",
                                               prefer, basename(filepath), e$message))
                      tryCatch(secondary(filepath, variable_name, year, month),
                               error = function(e2) {
                                 if (verbose) cat(sprintf("✗ Error loading %s with both readers.\n",
                                                          basename(filepath)))
                                 data.frame()
                               })
                    })
    out
  }

  # ---- discovery ----
  .discover_available <- function() {
    if (verbose) {
      cat("ERA5 Data Wrangling - Monthly File Organization (R)\n")
      cat(strrep("=", 60), "\n")
      cat(sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      cat("🔍 Scanning directory for available data files...\n")
    }
    patt <- sprintf("^era5_.*\\.%s$", file_ext)
    files <- list.files(input_dir, pattern = patt, full.names = TRUE)
    if (!length(files)) return(list())

    data_by_month <- list()
    file_count <- 0L

    for (fp in files) {
      fn <- basename(fp)
      stem <- sub(sprintf("\\.%s$", file_ext), "", fn, ignore.case = TRUE)
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
    if (verbose) cat(sprintf("✓ Found %d %s files organized into %d months\n",
                             file_count, toupper(file_ext), length(data_by_month)))
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
        if (verbose) cat("⚠ Missing file:", basename(fp), "\n")
        next
      }
      if (verbose) cat(sprintf("  ✓ Processing: %s\n", var))
      dt <- .load_grib_to_long_format(fp, var, year, month)
      if (nrow(dt)) {
        k <- k + 1L
        monthly[[k]] <- dt
      } else if (verbose) {
        cat(sprintf("  ⚠ No data from: %s\n", var))
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
      if (length(mfiles)) {
        recent_files <- c(recent_files, rev(sort(mfiles)))
      }
      if (length(recent_files) >= n) break
    }
    recent_files <- head(recent_files, n)
    if (!length(recent_files)) {
      if (verbose) cat("  ⚠ No recent monthly files found to combine.\n")
      return(invisible())
    }

    combined_file <- file.path(input_dir, sprintf("era5_recent_%dmonths.csv.gz", n))
    if (verbose) cat(sprintf("  📊 Combining %d recent files...\n", length(recent_files)))
    lst <- vector("list", length(recent_files))
    for (i in seq_along(recent_files)) {
      if (verbose) cat(sprintf("  ✓ Loaded: %s\n", basename(recent_files[i])))
      lst[[i]] <- data.table::fread(recent_files[i], showProgress = FALSE, data.table = FALSE)
    }
    df <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
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
    if (verbose) {
      cat("✗ No valid ERA5 files found.\n")
    }
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
    mi <- data_by_month[[key]]
    res <- .process_month(mi$year, mi$month, mi$variables)
    if (isTRUE(res$success)) {
      processed <- unique(c(processed, res$key))
      total_months <- total_months + 1L
      total_files  <- total_files  + res$files_processed
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

# small helper so we don't import rlang in the package
`%||%` <- function(a, b) if (is.null(a)) b else a