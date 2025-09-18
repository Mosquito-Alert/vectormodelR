#' Compile ERA5 monthly CSVs from GRIB/NetCDF files
#'
#' Scans an input directory for files named like \code{era5_YYYY_MM_<variable>.grib}
#' (or \code{.nc}), reads each file to a long table, combines variables per month,
#' writes \code{processed/YYYY/era5_YYYY_MM_all_variables.csv.gz}, and optionally
#' writes a 3-month combined quick-look file.
#'
#' @param input_dir Character. Folder that contains the ERA5 monthly files.
#' @param processed_dir Character. Output folder for monthly CSVs.
#'   Default \code{file.path(input_dir, "processed")}.
#' @param metadata_file Character. JSON file to track processed months.
#'   Default \code{file.path(input_dir, "processing_metadata.json")}.
#' @param file_ext Character. "grib" or "nc". Default "grib".
#' @param prefer Character. Reader preference, "terra" or "stars". Default "terra".
#' @param recent_n Integer. Number of most-recent monthly CSVs to combine
#'   into \code{era5_recent_<n>months.csv.gz} (0 = skip). Default 3.
#' @param skip_if_exists Logical. If TRUE, skip writing a monthly CSV when an
#'   existing output > 1 KB is present. Default TRUE.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return (Invisibly) a list with summary counts.
#' @import data.table
#' @importFrom stars read_stars
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom withr local_options
#' @export
compile_era5_monthly <- function(
  input_dir,
  processed_dir = file.path(input_dir, "processed"),
  metadata_file = file.path(input_dir, "processing_metadata.json"),
  file_ext      = c("grib","nc"),
  prefer        = c("terra","stars"),
  recent_n      = 3L,
  skip_if_exists = TRUE,
  verbose       = TRUE
) {
  stopifnot(dir.exists(input_dir))
  file_ext <- match.arg(tolower(file_ext), c("grib","nc"))
  prefer   <- match.arg(tolower(prefer),   c("terra","stars"))

  op <- options(datatable.print.class = TRUE)
  on.exit(options(op), add = TRUE)

  if (verbose) {
    cat("ERA5 Data Wrangling - Monthly File Organization (R)\n")
    cat(strrep("=", 60), "\n")
    cat(sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  }

  # ---- packages we'll use ----
  requireNamespace("data.table")
  requireNamespace("jsonlite")
  # stars/terra are optional; we check inside the readers

  `%||%` <- function(a, b) if (is.null(a)) b else a

  # --------------------------
  # Helpers: metadata
  # --------------------------
  load_metadata <- function() {
    if (file.exists(metadata_file)) {
      out <- tryCatch(jsonlite::fromJSON(metadata_file), error = function(e) NULL)
      if (is.list(out)) return(out)
    }
    list(processed_months = character(), last_updated = NULL)
  }

  save_metadata <- function(meta) {
    meta$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), metadata_file)
  }

  # --------------------------
  # Discover available data
  # --------------------------
  discover_available_data <- function() {
    if (verbose) cat("🔍 Scanning directory for available data files...\n")
    patt <- sprintf("^era5_.*\\.%s$", file_ext)
    files <- list.files(input_dir, pattern = patt, full.names = TRUE)
    if (!length(files)) {
      if (verbose) cat("✗ No files matched in:", input_dir, "\n")
      return(list())
    }

    now_y <- as.integer(format(Sys.time(), "%Y"))
    data_by_month <- list()
    file_count <- 0L

    for (fp in files) {
      fn <- basename(fp)
      parts <- strsplit(sub(sprintf("\\.%s$", file_ext), "", fn), "_", fixed = FALSE)[[1]]
      # Expect: era5, YYYY, MM, <variable...>
      if (length(parts) >= 4 && identical(parts[1], "era5")) {
        year  <- suppressWarnings(as.integer(parts[2]))
        month <- suppressWarnings(as.integer(parts[3]))
        if (!is.na(year) && !is.na(month) &&
            year >= 1900 && year <= now_y && month >= 1 && month <= 12) {
          variable  <- paste(parts[-(1:3)], collapse = "_")
          month_key <- sprintf("%04d_%02d", year, month)
          if (is.null(data_by_month[[month_key]])) {
            data_by_month[[month_key]] <- list(
              year = year, month = month, variables = list()
            )
          }
          data_by_month[[month_key]]$variables[[variable]] <- fp
          file_count <- file_count + 1L
        }
      }
    }
    if (verbose) cat(sprintf("✓ Found %d %s files organized into %d months\n",
                             file_count, toupper(file_ext), length(data_by_month)))
    data_by_month
  }

  check_file_exists <- function(fp) {
    file.exists(fp) && isTRUE(unname(file.info(fp)$size > 1024))
  }

  # --------------------------
  # GRIB/NetCDF → long-format data.table (inner helper)
  # --------------------------
  # IMPORTANT: we *avoid* giant joins; we map layer → time by index.
  load_grib_to_long_format <- function(filepath, variable_name, year, month,
                                       prefer = c("terra","stars"),
                                       drop_na_values = TRUE,
                                       verbose = FALSE) {
    prefer <- match.arg(prefer, c("terra","stars"))

    # ---- Reader: stars ----
    read_with_stars <- function() {
      if (!requireNamespace("stars", quietly = TRUE))
        stop("Package 'stars' is not installed.")
      s <- stars::read_stars(filepath, proxy = TRUE)

      # try to discover a time dimension before going long
      dims <- try(stars::st_dimensions(s), silent = TRUE)
      time_vals <- NULL
      time_dim  <- NULL
      if (!inherits(dims, "try-error")) {
        dn <- names(dims)
        if ("time" %in% dn) {
          time_vals <- try(stars::st_get_dimension_values(s, "time"), silent = TRUE)
          if (!inherits(time_vals, "try-error")) time_dim <- "time"
        } else if ("band" %in% dn) {
          # sometimes band carries times (one per layer)
          tv <- try(stars::st_get_dimension_values(s, "band"), silent = TRUE)
          if (!inherits(tv, "try-error")) {
            # keep; may be POSIXct or numeric indices
            time_vals <- tv
            time_dim  <- "band"
          }
        }
      }

      df <- as.data.frame(s, long = TRUE, na.rm = FALSE)
      data.table::setDT(df)

      # geometry → lon/lat (reduced grids)
      if ("geometry" %in% names(df)) {
        if (!requireNamespace("sf", quietly = TRUE))
          stop("stars returned geometry but package 'sf' is not installed.")
        coords <- sf::st_coordinates(df$geometry)
        df[, longitude := coords[, 1]]
        df[, latitude  := coords[, 2]]
        df[, geometry  := NULL]
      }

      # x/y or lon/lat naming fix
      if (!"longitude" %in% names(df) && "x"   %in% names(df)) data.table::setnames(df, "x",   "longitude")
      if (!"latitude"  %in% names(df) && "y"   %in% names(df)) data.table::setnames(df, "y",   "latitude")
      if (!"longitude" %in% names(df) && "lon" %in% names(df)) data.table::setnames(df, "lon", "longitude")
      if (!"latitude"  %in% names(df) && "lat" %in% names(df)) data.table::setnames(df, "lat", "latitude")

      # stars long output usually has a column named after the attribute (value)
      # It may also include 'band' if multiple slices
      val_col <- if ("value" %in% names(df)) "value" else tail(names(df), 1)
      if (!"value" %in% names(df)) data.table::setnames(df, val_col, "value")

      # derive a time column:
      # 1) if a proper POSIXct column already exists, use it
      time_col <- NULL
      cand <- intersect(c("time","valid_time","date","datetime"), names(df))
      if (length(cand)) time_col <- cand[1]

      # 2) else, map from band/time_dim by index (no joins)
      if (is.null(time_col) && "band" %in% names(df) && !is.null(time_vals)) {
        # band indexes map 1..n to time_vals
        if (inherits(time_vals, "POSIXct")) {
          df[, time := time_vals[as.integer(.data$band)]]
        } else {
          # coerce to POSIXct if it looks like datetimes; otherwise leave NA
          poss <- suppressWarnings(as.POSIXct(time_vals, tz = "UTC"))
          if (!anyNA(poss)) {
            df[, time := poss[as.integer(.data$band)]]
          } else {
            df[, time := as.POSIXct(NA)]
          }
        }
        time_col <- "time"
        df[, band := NULL]
      } else if (is.null(time_col)) {
        df[, time := as.POSIXct(NA)]
        time_col <- "time"
      }

      # ensure id columns
      if (!all(c("latitude","longitude") %in% names(df)))
        stop("Latitude/longitude columns not found after reading file.")

      # variable label
      if (!"grib_variable_name" %in% names(df)) {
        if ("variable" %in% names(df)) {
          data.table::setnames(df, "variable", "grib_variable_name")
        } else {
          df[, grib_variable_name := variable_name]
        }
      }

      # types
      df[, latitude  := as.numeric(latitude)]
      df[, longitude := as.numeric(longitude)]
      if (!inherits(df[[time_col]], "POSIXct")) {
        df[, (time_col) := suppressWarnings(as.POSIXct(get(time_col), tz = "UTC"))]
      }
      if (inherits(df$value, "difftime")) {
        df[, value := as.numeric(value, units = "secs")]
      } else if (is.list(df$value)) {
        df[, value := suppressWarnings(as.numeric(unlist(value)))]
      } else {
        df[, value := suppressWarnings(as.numeric(value))]
      }

      # pack columns
      keep <- intersect(c("latitude","longitude", time_col, "grib_variable_name", "value"), names(df))
      if (!length(keep)) return(data.table::data.table())
      df <- df[, ..keep]
      data.table::setnames(df, time_col, "time")
      df[, `:=`(variable_name = variable_name,
                year = as.integer(year),
                month = as.integer(month))]

      data.table::setcolorder(df,
        c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))

      if (drop_na_values) {
        n0 <- nrow(df)
        df <- df[!is.na(value)]
        if (verbose && nrow(df) < n0) {
          cat(sprintf("  ⚠ Removed %d rows with invalid values from %s\n",
                      n0 - nrow(df), basename(filepath)))
        }
      }
      df
    }

    # ---- Reader: terra ----
    read_with_terra <- function() {
      if (!requireNamespace("terra", quietly = TRUE))
        stop("Package 'terra' is not installed.")
      r <- terra::rast(filepath)  # multi-layer (time)
      tvals <- tryCatch(terra::time(r), error = function(e) NULL)

      # Make a data.frame wide: columns: x,y, layer1, layer2, ...
      dfw <- as.data.frame(r, xy = TRUE, cells = FALSE, na.rm = FALSE)
      data.table::setDT(dfw)
      data.table::setnames(dfw, c("x","y"), c("longitude","latitude"))

      # melt without joins; map layer index → time by position
      data_cols <- setdiff(names(dfw), c("longitude","latitude"))
      if (!length(data_cols)) return(data.table::data.table())

      dfl <- data.table::melt(
        dfw,
        id.vars = c("latitude","longitude"),
        measure.vars = data_cols,
        variable.name = "grib_variable_name",
        value.name = "value",
        variable.factor = TRUE
      )

      # layer index by factor level
      lyr_idx <- as.integer(dfl[["grib_variable_name"]])  # 1..n_layers
      if (!is.null(tvals) && length(tvals) == length(data_cols)) {
        # map by index; NO join
        tt <- tryCatch(as.POSIXct(tvals, tz = "UTC"), error = function(e) NULL)
        if (!is.null(tt) && !anyNA(tt)) {
          dfl[, time := tt[lyr_idx]]
        } else {
          dfl[, time := as.POSIXct(NA)]
        }
      } else {
        dfl[, time := as.POSIXct(NA)]
      }

      # numeric coercions
      dfl[, latitude  := as.numeric(latitude)]
      dfl[, longitude := as.numeric(longitude)]
      if (inherits(dfl$value, "difftime")) {
        dfl[, value := as.numeric(value, units = "secs")]
      } else if (is.list(dfl$value)) {
        dfl[, value := suppressWarnings(as.numeric(unlist(value)))]
      } else {
        dfl[, value := suppressWarnings(as.numeric(value))]
      }

      dfl[, `:=`(variable_name = variable_name,
                 year = as.integer(year),
                 month = as.integer(month))]

      data.table::setcolorder(dfl,
        c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))

      if (drop_na_values) dfl <- dfl[!is.na(value)]
      dfl
    }

    out <- tryCatch(
      if (prefer == "terra") read_with_terra() else read_with_stars(),
      error = function(e1) {
        if (verbose) cat(sprintf("  ⚠ %s read failed for %s (%s). Trying fallback...\n",
                                 prefer, basename(filepath), e1$message))
        tryCatch(
          if (prefer == "terra") read_with_stars() else read_with_terra(),
          error = function(e2) {
            if (verbose) cat(sprintf("✗ Error loading %s with both readers.\n", basename(filepath)))
            data.table::data.table()
          }
        )
      }
    )
    out
  }

  # --------------------------
  # Process a month (inner)
  # --------------------------
  process_month <- function(year, month, variables_list) {
    month_key <- sprintf("%04d_%02d", year, month)
    if (verbose) {
      cat(sprintf("\n📅 Processing %d-%02d\n", year, month))
      cat(strrep("-", 40), "\n")
    }

    dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
    year_dir <- file.path(processed_dir, sprintf("%04d", year))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)

    output_file <- file.path(year_dir, sprintf("era5_%04d_%02d_all_variables.csv.gz", year, month))

    if (isTRUE(skip_if_exists) &&
        file.exists(output_file) && isTRUE(unname(file.info(output_file)$size > 1024))) {
      if (verbose) cat(sprintf("✓ Already processed: %s (%.1f MB)\n",
                               basename(output_file), file.info(output_file)$size / 1024^2))
      return(list(success = TRUE, key = month_key, files_processed = 0L))
    }

    monthly_list <- list()
    files_processed <- 0L

    for (variable_name in names(variables_list)) {
      fp <- variables_list[[variable_name]]
      if (!check_file_exists(fp)) {
        if (verbose) cat("⚠ Missing file:", basename(fp), "\n")
        next
      }
      if (verbose) cat(sprintf("  ✓ Processing: %s\n", variable_name))
      dt <- load_grib_to_long_format(
        filepath = fp, variable_name = variable_name,
        year = year, month = month,
        prefer = prefer, drop_na_values = TRUE, verbose = verbose
      )
      if (nrow(dt)) {
        monthly_list[[length(monthly_list) + 1L]] <- dt
        files_processed <- files_processed + 1L
      } else if (verbose) {
        cat(sprintf("  ⚠ No data from: %s\n", variable_name))
      }
    }

    if (!length(monthly_list)) {
      if (verbose) cat(sprintf("  ✗ No data processed for %d-%02d\n", year, month))
      return(list(success = FALSE, key = month_key, files_processed = 0L))
    }

    if (verbose) cat(sprintf("  📊 Combining %d variables...\n", length(monthly_list)))
    df_month <- data.table::rbindlist(monthly_list, use.names = TRUE, fill = TRUE)

    if (verbose) cat(sprintf("  💾 Saving to: %s\n", basename(output_file)))
    data.table::fwrite(df_month, file = output_file)  # .gz triggers compression

    file_size_mb <- file.info(output_file)$size / 1024^2
    if (verbose) cat(sprintf("  ✓ Saved: %s rows, %.1f MB\n",
                             format(nrow(df_month), big.mark=","), file_size_mb))

    rm(df_month, monthly_list); gc()
    list(success = TRUE, key = month_key, files_processed = files_processed)
  }

  # --------------------------
  # Create recent combined file (inner)
  # --------------------------
  create_recent_combined_file <- function(n = 3L) {
    n <- as.integer(n)
    if (n <= 0L) return(invisible())
    if (verbose) cat("\n📋 Creating recent data summary...\n")

    # newest first
    year_dirs <- list.dirs(processed_dir, full.names = TRUE, recursive = FALSE)
    if (!length(year_dirs)) {
      if (verbose) cat("  ⚠ No yearly subfolders in processed dir.\n")
      return(invisible())
    }
    year_dirs <- rev(sort(year_dirs))

    recent_files <- character()
    for (yd in year_dirs) {
      mfiles <- list.files(yd, pattern = "^era5_.*_all_variables\\.csv\\.gz$", full.names = TRUE)
      if (length(mfiles)) {
        mfiles <- rev(sort(mfiles)) # newest first inside year
        recent_files <- c(recent_files, mfiles)
        if (length(recent_files) >= n) break
      }
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
      lst[[i]] <- data.table::fread(recent_files[i], showProgress = FALSE)
    }
    df_combined <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::fwrite(df_combined, combined_file)

    file_size_mb <- file.info(combined_file)$size / 1024^2
    if (verbose) cat(sprintf("  💾 Saved recent summary: %s rows, %.1f MB\n",
                             format(nrow(df_combined), big.mark=","), file_size_mb))
    invisible()
  }

  # --------------------------
  # Run
  # --------------------------
  meta <- load_metadata()
  processed_months <- unique(as.character(meta$processed_months %||% character()))
  data_by_month <- discover_available_data()
  if (!length(data_by_month)) {
    if (verbose) {
      cat("✗ No valid ERA5 files found.\n")
      cat(strrep("=", 60), "\n")
    }
    return(invisible(list(summary = list(
      months_processed_this_run = 0L,
      files_processed_this_run  = 0L,
      total_months_available    = 0L,
      output_directory          = processed_dir
    ))))
  }

  if (verbose) {
    cat(sprintf("📊 Found data for %d months\n", length(data_by_month)))
    cat(sprintf("📋 Previously processed: %d months\n", length(processed_months)))
  }

  total_processed <- 0L
  total_files     <- 0L

  for (month_key in sort(names(data_by_month))) {
    if (month_key %in% processed_months) {
      if (verbose) cat(sprintf("⏭ Skipping %s (already processed)\n", month_key))
      next
    }
    mi <- data_by_month[[month_key]]
    res <- process_month(mi$year, mi$month, mi$variables)
    if (isTRUE(res$success)) {
      processed_months <- unique(c(processed_months, res$key))
      total_processed  <- total_processed + 1L
      total_files      <- total_files + res$files_processed
    }
  }

  meta$processed_months <- processed_months
  save_metadata(meta)

  create_recent_combined_file(recent_n)

  if (verbose) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("FINAL SUMMARY\n")
    cat(strrep("=", 60), "\n")
    cat(sprintf("Months processed this run: %d\n", total_processed))
    cat(sprintf("Files processed this run: %d\n", total_files))
    cat(sprintf("Total months available: %d\n", length(data_by_month)))
    cat(sprintf("Output directory: %s\n", processed_dir), "\n")
    cat(strrep("=", 60), "\n")
    cat("✓ ERA5 monthly processing completed!\n")
  }

  invisible(list(
    summary = list(
      months_processed_this_run = total_processed,
      files_processed_this_run  = total_files,
      total_months_available    = length(data_by_month),
      output_directory          = processed_dir
    )
  ))
}