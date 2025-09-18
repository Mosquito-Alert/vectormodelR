#' Wrangle ERA5 monthly GRIB/NetCDF files into long-format CSVs
#'
#' Scans an input directory for files named like `era5_YYYY_MM_<variable>.(grib|nc)`,
#' converts each month into a single long-format `.csv.gz`, and writes a recent-N-months
#' combined file. Persists basic progress in a JSON metadata file so repeated runs skip
#' already processed months.
#'
#' @param input_dir Character. Directory containing raw ERA5 files (GRIB or NetCDF).
#' @param processed_dir Character. Where to write per-month CSVs (default: `file.path(input_dir, "processed")`).
#' @param metadata_file Character. Path to metadata JSON (default: `file.path(input_dir, "processing_metadata.json")`).
#' @param file_ext Character. One of `"grib"` or `"nc"`. Used for discovery pattern. Defaults to `"grib"`.
#' @param recent_n Integer. How many most recent monthly CSVs to combine (default `3`). Set `0` to skip.
#' @param overwrite Logical. Recompute months even if output `.csv.gz` already exists (default `FALSE`).
#' @param prefer Character. Reader preference: `"stars"` or `"terra"` (default `"stars"`). Fallback used automatically.
#' @param drop_na_values Logical. Drop rows with `NA` in `value` (default `TRUE`).
#' @param verbose Logical. Print progress to console (default `TRUE`).
#'
#' @return A list with elements:
#'   - `summary`: list of counts and paths
#'   - `processed_months`: character vector of YYYY_MM processed/recorded
#'   - `recent_file`: path to combined recent file (if created), otherwise `NULL`
#'   - `per_month_outputs`: named character vector of monthly CSV paths
#'
#' @importFrom data.table fwrite fread melt rbindlist setDT setnames setcolorder
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom withr local_options
#' @importFrom stars read_stars
#' @export
compile_era5_monthly <- function(
    input_dir,
    processed_dir  = file.path(normalizePath(input_dir, mustWork = FALSE), "processed"),
    metadata_file  = file.path(normalizePath(input_dir, mustWork = FALSE), "processing_metadata.json"),
    file_ext       = c("grib", "nc"),
    recent_n       = 3L,
    overwrite      = FALSE,
    prefer         = c("stars", "terra"),
    drop_na_values = TRUE,
    verbose        = TRUE
) {
  stopifnot(dir.exists(input_dir))
  file_ext <- match.arg(tolower(file_ext), c("grib","nc"))
  prefer   <- match.arg(tolower(prefer),   c("stars","terra"))

  now <- Sys.time()
  currentMonth <- as.integer(format(now, "%m"))
  currentYear  <- as.integer(format(now, "%Y"))

  dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

  vcat <- function(...) if (isTRUE(verbose)) cat(...)

  # --- metadata helpers ---
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

  # --- discover available ---
  discover_available_data <- function() {
    vcat("🔍 Scanning directory for available data files...\n")
    pat <- sprintf("^era5_.*\\.%s$", file_ext)
    files <- list.files(input_dir, pattern = pat, full.names = TRUE)
    if (!length(files)) {
      vcat("✗ No files matched in: ", input_dir, "\n")
      return(list())
    }
    data_by_month <- list(); file_count <- 0L
    for (fp in files) {
      fn <- basename(fp)
      parts <- strsplit(sub(sprintf("\\.%s$", file_ext), "", fn), "_")[[1]]
      if (length(parts) >= 4 && identical(parts[1], "era5")) {
        yr  <- suppressWarnings(as.integer(parts[2]))
        mo  <- suppressWarnings(as.integer(parts[3]))
        if (!is.na(yr) && !is.na(mo) && yr >= 1900 && yr <= currentYear && mo >= 1 && mo <= 12) {
          variable <- paste(parts[-(1:3)], collapse = "_")
          key <- sprintf("%04d_%02d", yr, mo)
          if (is.null(data_by_month[[key]])) {
            data_by_month[[key]] <- list(year = yr, month = mo, variables = list())
          }
          data_by_month[[key]]$variables[[variable]] <- fp
          file_count <- file_count + 1L
        }
      }
    }
    vcat(sprintf("✓ Found %d %s files organized into %d months\n", file_count, toupper(file_ext), length(data_by_month)))
    data_by_month
  }

  check_file_exists <- function(fp) file.exists(fp) && file.info(fp)$size > 1024

  # --- readers (internal) ---
  try_stars <- function(filepath, variable_name, year, month, drop_na_values) {
    s <- stars::read_stars(filepath, proxy = TRUE)
    s <- tryCatch(do.call(c, c(unclass(s), along = "grib_variable_name")), error = function(e) s)
    df <- as.data.frame(s, long = TRUE, na.rm = FALSE)
    data.table::setDT(df)

    # geometry → lon/lat
    if ("geometry" %in% names(df)) {
      if (!requireNamespace("sf", quietly = TRUE)) stop("stars returned geometry, but {sf} is not installed.")
      coords <- sf::st_coordinates(df$geometry)
      df[, `:=`(longitude = coords[,1], latitude = coords[,2])]
      df[, geometry := NULL]
    }
    # rename coords
    if (!"longitude" %in% names(df) && "x"   %in% names(df)) data.table::setnames(df, "x",   "longitude")
    if (!"latitude"  %in% names(df) && "y"   %in% names(df)) data.table::setnames(df, "y",   "latitude")
    if (!"longitude" %in% names(df) && "lon" %in% names(df)) data.table::setnames(df, "lon", "longitude")
    if (!"latitude"  %in% names(df) && "lat" %in% names(df)) data.table::setnames(df, "lat", "latitude")

    # time column
    time_col <- NULL
    candidates <- intersect(c("valid_time","time","date","datetime"), names(df))
    if (length(candidates)) time_col <- candidates[1]
    if (is.null(time_col)) {
      posix_cols <- names(df)[vapply(df, inherits, logical(1), what = "POSIXct")]
      if (length(posix_cols)) time_col <- posix_cols[1]
    }
    if (is.null(time_col)) { df[, time := as.POSIXct(NA)]; time_col <- "time" }

    # variable + value
    if (!"grib_variable_name" %in% names(df)) {
      if ("variable" %in% names(df)) data.table::setnames(df, "variable", "grib_variable_name") else df[, grib_variable_name := variable_name]
    }
    if (!"value" %in% names(df)) {
      data.table::setnames(df, tail(names(df),1), "value")
    }

    keep <- intersect(c("latitude","longitude", time_col, "grib_variable_name", "value"), names(df))
    if (!length(keep)) return(data.table::data.table())
    df <- df[, ..keep]

    if (!all(c("latitude","longitude") %in% names(df))) stop("Latitude/longitude columns not found after reading file.")

    df[, `:=`(latitude = as.numeric(latitude), longitude = as.numeric(longitude))]
    if (!inherits(df[[time_col]], "POSIXct")) df[, (time_col) := as.POSIXct(get(time_col), tz = "UTC")]

    if (inherits(df$value, "difftime")) {
      df[, value := as.numeric(value, units = "secs")]
    } else if (is.list(df$value)) {
      df[, value := suppressWarnings(as.numeric(unlist(value)))]
    } else {
      df[, value := suppressWarnings(as.numeric(value))]
    }

    data.table::setnames(df, time_col, "time")
    df[, `:=`(variable_name = variable_name, year = as.integer(year), month = as.integer(month))]
    data.table::setcolorder(df, c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))

    if (drop_na_values) {
      n0 <- nrow(df); df <- df[!is.na(value)]
      if (verbose && nrow(df) < n0) vcat(sprintf("  ⚠ Removed %d rows with invalid values from %s\n", n0 - nrow(df), basename(filepath)))
    }
    df
  }

  try_terra <- function(filepath, variable_name, year, month, drop_na_values) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("terra not installed and stars failed.")
    r <- terra::rast(filepath)

    # time vector, or attempt to infer hourly sequence if monthly hourly stack
    tvals <- tryCatch(terra::time(r), error = function(e) NULL)
    nlyr  <- terra::nlyr(r)
    if (is.null(tvals) || length(tvals) != nlyr) {
      d0 <- as.POSIXct(sprintf("%04d-%02d-01 00:00:00", year, month), tz = "UTC")
      end <- as.POSIXct(format(seq(as.Date(d0), by = "month", length.out = 2)[2], "%Y-%m-01"), tz = "UTC")
      days_in_mo <- as.integer(difftime(end, d0, units = "days"))
      expected <- 24L * days_in_mo
      if (nlyr == expected) tvals <- seq(d0, by = "hour", length.out = nlyr)
    }

    df <- as.data.frame(r, xy = TRUE, cells = FALSE, na.rm = FALSE)
    data.table::setDT(df)
    data.table::setnames(df, c("x","y"), c("longitude","latitude"))
    df[, `:=`(longitude = as.numeric(longitude), latitude = as.numeric(latitude))]

    data_cols <- setdiff(names(df), c("longitude","latitude"))
    df_long <- data.table::melt(df, id.vars = c("latitude","longitude"),
                                measure.vars = data_cols,
                                variable.name = "layer_name",
                                value.name   = "value")

    # map time by layer position (robust)
    layermap <- data.table::data.table(
      layer_name = data_cols,
      band_id    = seq_along(data_cols),
      tval       = if (!is.null(tvals)) tvals else as.POSIXct(NA)
    )
    df_long[, band_id := match(layer_name, layermap$layer_name)]
    df_long <- merge(df_long, layermap[, .(band_id, tval)], by = "band_id", all.x = TRUE)
    df_long[, `:=`(band_id = NULL, time = tval, tval = NULL)]

    # clean variable naming (compact)
    df_long[, `:=`(
      grib_variable_name = variable_name,
      value = suppressWarnings(as.numeric(value)),
      variable_name = variable_name,
      year = as.integer(year),
      month = as.integer(month)
    )]

    if (!inherits(df_long$time, "POSIXct")) df_long[, time := as.POSIXct(time, tz = "UTC")]

    data.table::setcolorder(df_long, c("latitude","longitude","time","variable_name","grib_variable_name","value","year","month"))
    if (drop_na_values) df_long <- df_long[!is.na(value)]
    df_long
  }

  load_to_long <- function(filepath, variable_name, year, month) {
    reader_order <- if (prefer == "stars") c("stars","terra") else c("terra","stars")
    for (rd in reader_order) {
      out <- try(
        switch(rd,
               stars = try_stars(filepath, variable_name, year, month, drop_na_values),
               terra = try_terra(filepath, variable_name, year, month, drop_na_values)),
        silent = TRUE
      )
      if (!inherits(out, "try-error")) return(out)
      vcat(sprintf("  ⚠ %s read failed for %s (%s).%s\n",
                   rd, basename(filepath),
                   tryCatch(conditionMessage(attr(out, "condition")), error = function(...) "error"),
                   if (rd == tail(reader_order,1)) "" else " Trying fallback..."))
    }
    vcat(sprintf("✗ Error loading %s with both readers.\n", basename(filepath)))
    data.table::data.table()
  }

  # --- process a month ---
  process_month <- function(year, month, variables_list) {
    key <- sprintf("%04d_%02d", year, month)
    vcat(sprintf("\n📅 Processing %d-%02d\n%s\n", year, month, strrep("-", 40)))
    year_dir <- file.path(processed_dir, sprintf("%04d", year))
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(year_dir, sprintf("era5_%04d_%02d_all_variables.csv.gz", year, month))

    if (!overwrite && file.exists(out_file) && file.info(out_file)$size > 1024) {
      vcat(sprintf("✓ Already processed: %s (%.1f MB)\n", basename(out_file), file.info(out_file)$size / 1024^2))
      return(list(success = TRUE, key = key, files_processed = 0L, out_file = out_file))
    }

    monthly_list <- list(); files_done <- 0L
    for (variable_name in names(variables_list)) {
      fp <- variables_list[[variable_name]]
      if (!check_file_exists(fp)) { vcat("⚠ Missing file: ", basename(fp), "\n"); next }
      vcat(sprintf("  ✓ Processing: %s\n", variable_name))
      dt <- load_to_long(fp, variable_name, year, month)
      if (nrow(dt)) { monthly_list[[length(monthly_list)+1L]] <- dt; files_done <- files_done + 1L }
      else vcat(sprintf("  ⚠ No data from: %s\n", variable_name))
    }

    if (!length(monthly_list)) {
      vcat(sprintf("  ✗ No data processed for %d-%02d\n", year, month))
      return(list(success = FALSE, key = key, files_processed = 0L, out_file = NULL))
    }

    vcat(sprintf("  📊 Combining %d variables...\n", length(monthly_list)))
    df_month <- data.table::rbindlist(monthly_list, use.names = TRUE, fill = TRUE)

    vcat(sprintf("  💾 Saving to: %s\n", basename(out_file)))
    withr::local_options(list(datatable.fread.datatable = TRUE))
    data.table::fwrite(df_month, file = out_file)
    fsz <- file.info(out_file)$size / 1024^2
    vcat(sprintf("  ✓ Saved: %s rows, %.1f MB\n", format(nrow(df_month), big.mark=","), fsz))
    rm(df_month, monthly_list); gc()
    list(success = TRUE, key = key, files_processed = files_done, out_file = out_file)
  }

  # --- recent combiner ---
  create_recent_combined_file <- function(n = 3L) {
    if (n <= 0L) return(NULL)
    vcat("\n📋 Creating recent data summary...\n")
    year_dirs <- list.dirs(processed_dir, full.names = TRUE, recursive = FALSE)
    year_dirs <- rev(sort(year_dirs))
    recent_files <- character()
    for (yd in year_dirs) {
      mfiles <- list.files(yd, pattern = "^era5_.*_all_variables\\.csv\\.gz$", full.names = TRUE)
      mfiles <- rev(sort(mfiles))
      if (length(mfiles)) {
        recent_files <- c(recent_files, mfiles)
        if (length(recent_files) >= n) break
      }
    }
    recent_files <- head(recent_files, n)
    if (!length(recent_files)) { vcat("  ⚠ No recent monthly files found to combine.\n"); return(NULL) }

    out <- file.path(input_dir, sprintf("era5_recent_%dmonths.csv.gz", length(recent_files)))
    vcat(sprintf("  📊 Combining %d recent files...\n", length(recent_files)))
    lst <- vector("list", length(recent_files))
    for (i in seq_along(recent_files)) {
      vcat(sprintf("  ✓ Loaded: %s\n", basename(recent_files[i])))
      lst[[i]] <- data.table::fread(recent_files[i], showProgress = FALSE)
    }
    dfc <- data.table::rbindlist(lst, use.names = TRUE, fill = TRUE)
    data.table::fwrite(dfc, out)
    vcat(sprintf("  💾 Saved recent summary: %s rows, %.1f MB\n",
                 format(nrow(dfc), big.mark=","), file.info(out)$size / 1024^2))
    out
  }

  # --- run ---
  vcat("ERA5 Data Wrangling - Monthly File Organization (R)\n", strrep("=", 60), "\n",
       sprintf("Run time: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  meta <- load_metadata()
  processed_prev <- unique(as.character(meta$processed_months %||% character()))
  data_by_month <- discover_available_data()
  if (!length(data_by_month)) {
    return(list(
      summary = list(message = "No matching files", output_directory = processed_dir),
      processed_months = processed_prev,
      recent_file = NULL,
      per_month_outputs = character()
    ))
  }

  vcat(sprintf("📊 Found data for %d months\n", length(data_by_month)))
  vcat(sprintf("📋 Previously processed: %d months\n", length(processed_prev)))

  total_processed <- 0L; total_files <- 0L
  per_month_outputs <- c()

  for (month_key in sort(names(data_by_month))) {
    if (!overwrite && month_key %in% processed_prev) {
      vcat(sprintf("⏭ Skipping %s (already processed)\n", month_key))
      next
    }
    mi <- data_by_month[[month_key]]
    res <- process_month(mi$year, mi$month, mi$variables)
    if (isTRUE(res$success)) {
      processed_prev <- unique(c(processed_prev, res$key))
      total_processed <- total_processed + 1L
      total_files <- total_files + res$files_processed
      if (!is.null(res$out_file)) per_month_outputs[month_key] <- res$out_file
    }
  }

  meta$processed_months <- processed_prev
  save_metadata(meta)

  recent_file <- create_recent_combined_file(recent_n)

  vcat("\n", strrep("=", 60), "\n",
       "FINAL SUMMARY\n",
       strrep("=", 60), "\n",
       sprintf("Months processed this run: %d\n", total_processed),
       sprintf("Files processed this run: %d\n", total_files),
       sprintf("Total months available: %d\n", length(data_by_month)),
       sprintf("Output directory: %s\n", processed_dir), "\n",
       strrep("=", 60), "\n",
       "✓ ERA5 monthly processing completed!\n")

  list(
    summary = list(
      months_processed_run = total_processed,
      files_processed_run  = total_files,
      months_available     = length(data_by_month),
      output_directory     = processed_dir
    ),
    processed_months  = processed_prev,
    recent_file       = recent_file,
    per_month_outputs = per_month_outputs
  )
}

# internal null-coalescing
`%||%` <- function(a, b) if (is.null(a)) b else a
