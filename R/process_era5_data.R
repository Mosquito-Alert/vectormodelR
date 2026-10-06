#' Build daily & lagged weather features from processed ERA5 CSVs
#'
#' Reads monthly CSV.GZ files created by compile_era5_data_v2 from the appropriate
#' dataset-specific subdirectory, clips by a GADM admin polygon, aggregates
#' to hourly area means or cell-level daily summaries, derives daily weather
#' summaries, rolling-window weather features, accumulated precipitation windows,
#' lagged precipitation windows, and saves RDS outputs with informative names.
#'
#' @param iso3 Character. ISO3 code for the country (e.g., "ESP").
#' @param admin_level Integer. GADM administrative level (0=country, 1=region, 2=province, ...).
#' @param admin_name Character or NULL. Exact `NAME_<level>` to match (e.g., "Barcelona").
#'   If NULL, the whole level geometry is used (unioned).
#' @param dataset Character. ERA5 dataset: "reanalysis-era5-single-levels" or "reanalysis-era5-land".
#'   Required. Determines which processed folder to read from (processed/single-levels/ or processed/land/).
#' @param out_dir Character. Directory to write RDS outputs. Created if missing.
#' @param start_date Date. Earliest date to include (UTC). Default NULL, inferred from data.
#' @param end_date Date. Latest date to include (UTC). Default NULL, inferred from data.
#' @param wind_calm_kmh Numeric. Calm-wind threshold in km/h for MWI logic. Default 6.
#' @param round_ll Integer. Rounding decimal places applied to lon/lat before reshaping to wide.
#' @param verbose Logical. If TRUE, prints progress messages. Default TRUE.
#' @param attach_to_global Logical. If TRUE, assigns output data.frames to .GlobalEnv.
#' @param aggregation_unit Character. Choose "region", "cell", or "hourly".
#' @param polygon_buffer_km Numeric. If no ERA5 centroids fall inside the admin
#'   polygon, or too few are captured, expand it by this distance in kilometers.
#'
#' @return For `aggregation_unit = "region"` or `"cell"`, invisibly returns a list with:
#'   `daily`, `lags_3d`, `lags_7d`, `lags_14d`, `lags_21d`, `lags_30d`,
#'   `lags_3d_lag7`, `lags_7d_lag7`, `lags_14d_lag7`, `lags_21d_lag7`,
#'   `lags_30d_lag7`, `ppt_lags`, and `paths`.
#'   `ppt_lags` contains accumulated precipitation windows of 3, 7, 14, 21,
#'   and 30 days, plus the same windows lagged by 7 days. For
#'   `aggregation_unit = "hourly"`, returns a list with `hourly` and `paths`.
#'   Missing hours are inserted as NA and reported with message(). Incomplete
#'   windows remain NA. Daily rainfall uses UTC interval boundaries.
#'   The hourly table includes current-hour weather values plus short-window
#'   precipitation and temperature/humidity summaries for each cell.
#'
#' @details
#' ERA5-Land columns are renamed after `dcast()` to match `era5_single_level`,
#' so downstream calculations use one standard variable schema.
#'
#' Temperature and dewpoint values are treated as Kelvin and converted to Celsius
#' with `x - 273.15`. This is intentional: in some GRIBs, `terra` may label the
#' layers as `[C]`, but the values can still be Kelvin, e.g. 272–295.
#'
#' Input precipitation must be from the standard hourly CDS GRIB products,
#' with GRIB validity timestamps, not already de-accumulated ERA5-Land data.
#' ERA5-Land hourly rain is differenced within forecast cycles; its daily total
#' uses the following midnight accumulation. ERA5 single-level rain is already
#' hourly and is assigned to UTC days using interval-ending timestamps.
#' Missing cell-hours are inserted, reported, and propagated through windows.
#' Missing hourly values also make daily temperature/humidity/wind summaries NA.
#' Padding is read from available CSVs; this function does not download it.
#'
#' Rolling weather summaries use rolling means for temperature, humidity, wind,
#' and MWI-type indices. Accumulated precipitation is handled separately in
#' `ppt_lags` using rolling sums.
#'
#' Hourly short-window features are right-aligned within each cell and include
#' the current ERA5 hour in the window.
#'
#' Requires helper objects:
#' - `era5_single_level`
#' - `era5_land_to_single_level`
#'
#' @importFrom lubridate parse_date_time floor_date ceiling_date
#' @importFrom RcppRoll roll_mean roll_sum
#' @importFrom sf st_make_valid st_union st_bbox st_transform st_buffer st_intersects
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate case_when transmute arrange lag select all_of group_by ungroup
#' @importFrom readr write_rds
#' @importFrom data.table fread setnames as.data.table rbindlist dcast setorder set
#' @export
#' @examples
#' \dontrun{
#' result <- process_era5_data(
#'   iso3 = "ESP",
#'   admin_level = 2,
#'   admin_name = "Barcelona",
#'   dataset = "reanalysis-era5-land",
#'   aggregation_unit = "region"
#' )
#'
#' result <- process_era5_data(
#'   iso3 = "ITA",
#'   admin_level = 0,
#'   admin_name = NULL,
#'   dataset = "reanalysis-era5-single-levels",
#'   start_date = as.Date("2020-01-01"),
#'   end_date = as.Date("2023-12-31")
#' )
#' }
process_era5_data <- function(
    iso3,
    admin_level,
    admin_name,
    dataset,
    out_dir = "data/proc",
    start_date = NULL,
    end_date   = NULL,
    wind_calm_kmh = 6,
    round_ll = 3,
    verbose  = TRUE,
    attach_to_global = FALSE,
    aggregation_unit = c("region", "cell", "hourly"),
    polygon_buffer_km = 10
) {
  report_missing <- function(x, cols, label) {
    cols <- intersect(cols, names(x))
    counts <- vapply(cols, function(nm) sum(!is.finite(x[[nm]])), integer(1))
    counts <- counts[counts > 0L]
    if (length(counts)) {
      message("INCOMPLETE ", label, ": ",
              paste(names(counts), counts, sep = "=", collapse = "; "),
              ". Values remain NA; incomplete windows are not treated as zero rainfall.")
    }
  }

  # Allow this replacement to be sourced without exposing package internals.
  if (!exists("build_location_identifiers", mode = "function", inherits = TRUE)) {
    build_location_identifiers <- getFromNamespace("build_location_identifiers", "vectormodelR")
  }
  if (!exists("get_gadm_data", mode = "function", inherits = TRUE)) {
    get_gadm_data <- getFromNamespace("get_gadm_data", "vectormodelR")
  }
  .say  <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  .fmtI <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE)
  aggregation_unit <- match.arg(aggregation_unit)
  
  sanitize_slug <- function(x) {
    if (is.null(x) || !length(x) || is.na(x) || !nzchar(x)) return(character())
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }
  
  # ---- deps & args ----
  if (!is.character(iso3) || length(iso3) != 1L || !nzchar(iso3)) {
    stop("`iso3` must be a non-empty character scalar.")
  }
  
  iso3_upper   <- toupper(iso3)
  iso_fragment <- tolower(iso3_upper)
  
  if (is.null(dataset) || !nzchar(dataset)) {
    stop("`dataset` is required. Use 'reanalysis-era5-single-levels' or 'reanalysis-era5-land'.")
  }
  
  valid_datasets <- c("reanalysis-era5-single-levels", "reanalysis-era5-land")
  if (!dataset %in% valid_datasets) {
    stop("`dataset` must be one of: ", paste(valid_datasets, collapse = ", "))
  }
  
  if (!exists("era5_single_level", inherits = TRUE)) {
    stop("Object `era5_single_level` was not found. Load your helper file first.")
  }
  
  if (!exists("era5_land_to_single_level", inherits = TRUE)) {
    stop("Object `era5_land_to_single_level` was not found. Load your helper file first.")
  }
  
  dataset_subdir <- if (dataset == "reanalysis-era5-land") "land" else "single-levels"
  
  wanted <- if (dataset == "reanalysis-era5-land") {
    names(era5_land_to_single_level)
  } else {
    era5_single_level
  }
  
  standard_names <- era5_single_level
  
  # ---- paths ----
  admin_fragment <- NULL
  
  if (!is.null(admin_name) && nzchar(admin_name)) {
    if (is.null(admin_level) || is.na(admin_level)) {
      stop("When `admin_name` is provided, `admin_level` must be specified.")
    }
    
    ids <- build_location_identifiers(iso3, admin_level, admin_name)
    admin_fragment <- paste0(ids$admin_level, "_", ids$admin_name)
    processed_dir <- file.path("data/weather/grib", ids$slug, "processed", dataset_subdir)
  } else {
    processed_dir <- file.path("data/weather/grib", iso_fragment, "processed", dataset_subdir)
  }
  
  processed_dir <- path.expand(processed_dir)
  
  if (!dir.exists(processed_dir)) {
    stop("Processed directory not found: ", processed_dir)
  }
  
  .say("Reading from: %s", processed_dir)
  
  out_dir <- path.expand(out_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  list_month_files <- function(dir, iso_slug, admin_frag = NULL, ds = NULL) {
    prefix <- if (!is.null(ds) && ds == "reanalysis-era5-land") "era5land" else "era5"
    
    if (!is.null(admin_frag) && nzchar(admin_frag)) {
      pat <- sprintf(
        "^%s_%s_%s_\\d{4}_\\d{2}_all_variables\\.csv\\.gz$",
        prefix, iso_slug, admin_frag
      )
    } else {
      pat <- sprintf(
        "^%s_%s_\\d{4}_\\d{2}_all_variables\\.csv\\.gz$",
        prefix, iso_slug
      )
    }
    
    list.files(dir, pattern = pat, full.names = TRUE, recursive = TRUE, ignore.case = FALSE)
  }
  
  K_to_C <- function(x) x - 273.15
  
  rh_from_T_Td <- function(Tc, TDc, a = 17.625, b = 243.04) {
    100 * exp((a * TDc / (b + TDc)) - (a * Tc / (b + Tc)))
  }
  
  files <- list_month_files(processed_dir, iso_fragment, admin_fragment, dataset)
  
  if (!length(files)) {
    stop(
      "No processed monthly CSVs found for ISO '", iso_fragment,
      "' dataset '", dataset,
      "' under: ", processed_dir
    )
  }
  
  .say("Found %s monthly files (%s).", .fmtI(length(files)), dataset_subdir)
  
  # ---- admin geometry & bbox window ----
  .say("Loading GADM geometry: %s level %d ...", iso3_upper, admin_level)

  if (!is.null(admin_name)) {
    .say("Filtering admin unit by exact name match: '%s' ...", admin_name)

    g <- get_gadm_data(
      iso3 = iso3_upper,
      level = admin_level,
      name = admin_name,
      path = file.path(out_dir, "gadm"),
      rds = FALSE,
      perimeter = FALSE,
      union = FALSE,
      match_type = "exact",
      verbose = FALSE
    )
  } else {
    .say("No admin_name provided; using union of all geometries at level %d.", admin_level)

    g <- get_gadm_data(
      iso3 = iso3_upper,
      level = admin_level,
      path = file.path(out_dir, "gadm"),
      rds = FALSE,
      perimeter = FALSE,
      union = TRUE,
      verbose = FALSE
    )
  }
  
  bb <- sf::st_bbox(g)
  
  read_margin <- 0.5
  
  lon_min <- as.numeric(bb["xmin"]) - read_margin
  lon_max <- as.numeric(bb["xmax"]) + read_margin
  lat_min <- as.numeric(bb["ymin"]) - read_margin
  lat_max <- as.numeric(bb["ymax"]) + read_margin
  
  .say(
    "Bounding box: lon[%.4f, %.4f], lat[%.4f, %.4f] (with read margin).",
    lon_min, lon_max, lat_min, lat_max
  )
  
  # ---- read & prefilter by bbox/time/vars ----
  .say("Reading and prefiltering files (bbox + variables), parsing time ...")
  
  pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
  on.exit(try(close(pb), silent = TRUE), add = TRUE)
  
  DT <- data.table::rbindlist(
    lapply(seq_along(files), function(i) {
      f <- files[i]
      dt <- data.table::fread(f, showProgress = FALSE)
      
      nm_lower <- tolower(names(dt))
      candidate_cols <- c("variable_name", "grib_variable_name", "variable", "var_name", "var")
      match_idx <- which(nm_lower %in% candidate_cols)
      
      if (length(match_idx)) {
        col_idx <- match_idx[1]
        orig_name <- names(dt)[col_idx]
        
        if (!identical(orig_name, "variable_name")) {
          data.table::setnames(dt, orig_name, "variable_name")
        }
      } else {
        stop("File ", basename(f), " is missing 'variable_name' column.")
      }
      
      keep_var_idx <- which(dt[["variable_name"]] %in% wanted)
      if (!length(keep_var_idx)) return(data.table::data.table())
      dt <- dt[keep_var_idx, , drop = FALSE]
      
      keep_bbox_idx <- which(
        dt[["longitude"]] >= lon_min & dt[["longitude"]] <= lon_max &
          dt[["latitude"]] >= lat_min & dt[["latitude"]] <= lat_max
      )
      
      if (!length(keep_bbox_idx)) return(data.table::data.table())
      dt <- dt[keep_bbox_idx, , drop = FALSE]
      
      time_vals <- dt[["time"]]
      
      if (inherits(time_vals, "POSIXct")) {
        attr(time_vals, "tzone") <- "UTC"
      } else {
        time_char <- gsub(" UTC$", "", as.character(time_vals))
        time_vals <- lubridate::parse_date_time(
          time_char,
          orders = c("ymd HMS", "ymd HM", "ymd"),
          tz = "UTC",
          exact = FALSE,
          quiet = TRUE
        )
        time_vals <- as.POSIXct(time_vals, tz = "UTC")
      }
      
      data.table::set(dt, j = "time", value = time_vals)
      
      utils::setTxtProgressBar(pb, i)
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  DT <- data.table::as.data.table(DT)
  
  if (!nrow(DT)) {
    stop("No rows after bbox/var filtering. Check inputs.")
  }
  
  .say("\nAfter bbox/var filter: %s rows.", .fmtI(nrow(DT)))
  
  found_vars <- sort(unique(DT$variable_name))
  missing_vars <- setdiff(wanted, found_vars)
  
  if (length(missing_vars)) {
    stop(
      "Missing required variables for dataset ", dataset, ": ",
      paste(missing_vars, collapse = ", "),
      "\nFound variables: ",
      paste(found_vars, collapse = ", ")
    )
  }
  
  # ---- date filtering ----
  if (is.null(start_date)) {
    start_date <- as.Date(lubridate::floor_date(min(DT$time, na.rm = TRUE), unit = "day"))
  }
  
  if (is.null(end_date)) {
    end_date <- as.Date(lubridate::ceiling_date(max(DT$time, na.rm = TRUE), unit = "day") - 1)
  }
  
  .say("Date window: %s to %s (inclusive).", format(start_date), format(end_date))
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    stop("Invalid requested date range.")
  }
  # 30-day window ending seven days earlier starts 36 days earlier.
  # Keep following midnight for the final day's rainfall.
  input_from <- as.POSIXct(start_date - 36, tz = "UTC")
  input_to <- as.POSIXct(end_date + 1, tz = "UTC")
  keep_range <- DT$time >= input_from & DT$time <= input_to
  
  DT <- DT[keep_range]
  
  if (!nrow(DT)) {
    stop("No rows after time filtering. Check date window.")
  }
  
  .say("After time filter: %s rows.", .fmtI(nrow(DT)))
  
  # ---- polygon mask ----
  .say("Applying exact polygon mask ...")
  
  poly <- g |>
    sf::st_make_valid() |>
    sf::st_union()
  
  cells <- unique(DT[, .(longitude, latitude)])
  pts_sf <- sf::st_as_sf(cells, coords = c("longitude", "latitude"), crs = 4326)
  
  inside_cells <- as.logical(sf::st_intersects(pts_sf, poly, sparse = FALSE))
  inside_cells[is.na(inside_cells)] <- FALSE
  n_inside <- sum(inside_cells)
  
  min_cells_no_buffer <- 2L
  use_buffer <- FALSE
  
  if (polygon_buffer_km > 0) {
    poly_buffer <- poly |>
      sf::st_transform(3857) |>
      sf::st_buffer(polygon_buffer_km * 1000) |>
      sf::st_transform(4326)
    
    inside_cells_buf <- as.logical(sf::st_intersects(pts_sf, poly_buffer, sparse = FALSE))
    inside_cells_buf[is.na(inside_cells_buf)] <- FALSE
    n_inside_buf <- sum(inside_cells_buf)
    
    if (n_inside == 0L && n_inside_buf > 0L) {
      use_buffer <- TRUE
    } else if (n_inside > 0L && n_inside < min_cells_no_buffer && n_inside_buf > n_inside) {
      use_buffer <- TRUE
    }
    
    if (isTRUE(use_buffer)) {
      inside_cells <- inside_cells_buf
      n_inside <- n_inside_buf
      
      .say(
        "Using buffered polygon (%.1f km): %s ERA5 centroids selected.",
        polygon_buffer_km,
        .fmtI(n_inside)
      )
    }
  }
  
  if (n_inside == 0L) {
    stop("No ERA5 centroids inside polygon or buffer. Check coordinates / buffer size.")
  }
  
  keep_cells <- cells[inside_cells, .(longitude, latitude)]
  DT <- DT[keep_cells, on = .(longitude, latitude), nomatch = 0L]
  
  rm(cells, pts_sf, inside_cells, keep_cells)
  gc()
  
  .say("Points inside polygon: %s rows kept.", .fmtI(nrow(DT)))
  
  # ---- round lon/lat ----
  data.table::set(DT, j = "lat", value = round(DT$latitude, round_ll))
  data.table::set(DT, j = "lon", value = round(DT$longitude, round_ll))
  
  .say("Rounded lon/lat to %d decimals.", round_ll)
  
  # ---- wide per cell & hour ----
  .say("Casting to wide per (lon, lat, time) ...")
  
  if (anyDuplicated(DT[, .(lon, lat, time, variable_name)])) {
    stop("Duplicate cell/time/variable entries before reshaping; inspect input files.")
  }

  wide <- data.table::dcast(
    DT[, .(lon, lat, time, variable_name, value)],
    lon + lat + time ~ variable_name,
    value.var = "value"
  )
  
  if (dataset == "reanalysis-era5-land") {
    old_names <- names(era5_land_to_single_level)
    new_names <- unname(era5_land_to_single_level)
    present <- old_names %in% names(wide)
    
    data.table::setnames(
      wide,
      old = old_names[present],
      new = new_names[present]
    )
  }
  
  .say("Wide table: %s rows, %d columns.", .fmtI(nrow(wide)), ncol(wide))
  
  missing_wide <- setdiff(standard_names, names(wide))
  
  if (length(missing_wide)) {
    stop(
      "Wide table is missing required columns after renaming: ",
      paste(missing_wide, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(wide), collapse = ", ")
    )
  }
  
  # Complete the hourly timeline before calculating differences.
  wide <- data.table::copy(data.table::as.data.table(wide))
  if (anyDuplicated(wide[, .(lon, lat, time)])) {
    stop("Duplicate weather cell/timestamps: resolve duplicates before processing.")
  }
  cells <- unique(wide[, .(lon, lat)])
  times <- seq(input_from, input_to, by = "hour")
  if (any(!wide$time %in% times)) {
    stop("Weather timestamps must lie on the requested whole-hour UTC grid.")
  }
  grid <- cells[, .(time = times), by = .(lon, lat)]
  wide[, .present := TRUE]
  out <- merge(grid, wide, by = c("lon", "lat", "time"), all.x = TRUE, sort = TRUE)
  absent <- out[is.na(.present)]
  if (nrow(absent)) {
    message(
      "INCOMPLETE WEATHER SERIES: inserted ", nrow(absent),
      " missing cell-hours as NA across ",
      nrow(unique(absent[, .(lon, lat)])), " cells. Range: ",
      min(absent$time), " to ", max(absent$time), " UTC. ",
      "This includes unavailable history/following-midnight padding."
    )
    print(utils::head(as.data.frame(absent[, .(lon, lat, time)]), 10L),
          row.names = FALSE)
  }
  out[, .present := NULL]
  wide <- out
  rm(out, grid, absent, cells, times)
  # Treat non-finite source values as missing, not as valid observations.
  for (nm in standard_names) {
    bad <- which(!is.finite(wide[[nm]]))
    if (length(bad)) data.table::set(wide, i = bad, j = nm, value = NA_real_)
  }
  report_missing(wide, standard_names, "RAW WEATHER (INCLUDING PADDING)")

  # ---- derived hourly features ----
  .say("Computing hourly derived features ...")
  
  ws10_vals <- sqrt(
    wide[["10m_u_component_of_wind"]]^2 +
      wide[["10m_v_component_of_wind"]]^2
  )
  
  data.table::set(wide, j = "ws10", value = ws10_vals)
  
  t2m_vals <- K_to_C(wide[["2m_temperature"]])
  d2m_vals <- K_to_C(wide[["2m_dewpoint_temperature"]])
  
  data.table::set(wide, j = "t2m_C", value = t2m_vals)
  data.table::set(wide, j = "d2m_C", value = d2m_vals)
  
  rh_vals <- pmin(pmax(rh_from_T_Td(t2m_vals, d2m_vals), 0), 100)
  data.table::set(wide, j = "RH", value = rh_vals)
  
  # Convert source precipitation into hourly increments.
  data.table::setorder(wide, lon, lat, time)
  wide[, ppt_accum_mm := total_precipitation * 1000]
  wide[!is.finite(ppt_accum_mm), ppt_accum_mm := NA_real_]
  if (dataset == "reanalysis-era5-land") {
    wide[, ppt_mm := {
      previous <- data.table::shift(ppt_accum_mm)
      contiguous <- as.numeric(difftime(time, data.table::shift(time),
                                       units = "hours")) == 1
      amount <- ppt_accum_mm - previous
      amount[is.na(contiguous) | !contiguous] <- NA_real_
      reset <- format(time, "%H", tz = "UTC") == "01"
      amount[reset] <- ppt_accum_mm[reset]
      amount
    }, by = .(lon, lat)]
  } else if (dataset == "reanalysis-era5-single-levels") {
    wide[, ppt_mm := ppt_accum_mm]
  } else stop("Unsupported ERA5 dataset.")

  # Do not silently turn material negative amounts into dry weather.
  bad <- which(!is.na(wide$ppt_mm) & wide$ppt_mm < -1e-6)
  if (length(bad)) {
    message("INVALID PRECIPITATION: ", length(bad),
            " negative hourly amounts set to NA. Inspect source values/timestamps.")
    print(utils::head(as.data.frame(wide[bad, .(lon, lat, time, ppt_mm)]), 10L),
          row.names = FALSE)
    wide[bad, ppt_mm := NA_real_]
  }
  wide[!is.na(ppt_mm) & ppt_mm < 0, ppt_mm := 0]

  
  wide_small <- wide[, .(lon, lat, time, t2m_C, d2m_C, RH, ws10, ppt_mm, ppt_accum_mm)]
  
  hourly_cells <- data.table::copy(wide_small)
  data.table::set(hourly_cells, j = "date", value = as.Date(hourly_cells[["time"]], tz = "UTC"))
  data.table::setorderv(hourly_cells, c("lon", "lat", "time"))
  cell_groups <- split(seq_len(nrow(hourly_cells)), list(hourly_cells$lon, hourly_cells$lat), drop = TRUE)
  roll_by_cell <- function(values, n, roller) {
    out <- rep(NA_real_, length(values))
    for (idx in cell_groups) {
      out[idx] <- roller(values[idx], n = n, align = "right", fill = NA, na.rm = FALSE)
    }
    out
  }
  
  data.table::set(hourly_cells, j = "t2m_C_hour", value = hourly_cells[["t2m_C"]])
  data.table::set(hourly_cells, j = "RH_hour", value = hourly_cells[["RH"]])
  data.table::set(hourly_cells, j = "ws10_hour", value = hourly_cells[["ws10"]])
  data.table::set(hourly_cells, j = "ppt_mm_hour", value = hourly_cells[["ppt_mm"]])
  data.table::set(hourly_cells, j = "ppt_mm_prev_6h", value = roll_by_cell(hourly_cells[["ppt_mm"]], 6, RcppRoll::roll_sum))
  data.table::set(hourly_cells, j = "ppt_mm_prev_24h", value = roll_by_cell(hourly_cells[["ppt_mm"]], 24, RcppRoll::roll_sum))
  data.table::set(hourly_cells, j = "t2m_C_mean_prev_6h", value = roll_by_cell(hourly_cells[["t2m_C"]], 6, RcppRoll::roll_mean))
  data.table::set(hourly_cells, j = "RH_mean_prev_6h", value = roll_by_cell(hourly_cells[["RH"]], 6, RcppRoll::roll_mean))
  
  if (identical(aggregation_unit, "region")) {
    .say("Aggregating to hourly area means ...")
    
    hourly <- hourly_cells[
      ,
      .(
        TM    = mean(t2m_C, na.rm = FALSE),
        HRM   = mean(RH, na.rm = FALSE),
        VVM10 = mean(ws10, na.rm = FALSE),
        PPT   = mean(ppt_mm, na.rm = FALSE)
      ),
      by = .(time)
    ]
    
    data.table::set(hourly, j = "date", value = as.Date(hourly[["time"]], tz = "UTC"))
    
    .say("Hourly table: %s rows.", .fmtI(nrow(hourly)))
    .say("Aggregating to daily summaries ...")
    
    daily_dt <- hourly[
      ,
      .(
        meanTM    = mean(TM, na.rm = FALSE),
        maxTM     = max(TM, na.rm = FALSE),
        minTM     = min(TM, na.rm = FALSE),
        
        meanHRM   = mean(HRM, na.rm = FALSE),
        maxHRM    = max(HRM, na.rm = FALSE),
        minHRM    = min(HRM, na.rm = FALSE),
        
        meanVVM10 = mean(VVM10, na.rm = FALSE),
        maxVVX10  = max(VVM10, na.rm = FALSE),
        minVVX10  = min(VVM10, na.rm = FALSE),
        
        meanPPT24H = sum(PPT, na.rm = FALSE)
      ),
      by = .(date)
    ][order(date)]
    
  } else if (identical(aggregation_unit, "cell")) {
    .say("Keeping hourly values per cell (no area aggregation).")
    
    data.table::setorder(hourly_cells, lon, lat, time)
    hourly <- hourly_cells
    
    .say("Hourly cell table: %s rows.", .fmtI(nrow(hourly)))
    .say("Aggregating to daily summaries per cell ...")
    
    daily_dt <- hourly[
      ,
      .(
        meanTM    = mean(t2m_C, na.rm = FALSE),
        maxTM     = max(t2m_C, na.rm = FALSE),
        minTM     = min(t2m_C, na.rm = FALSE),
        
        meanHRM   = mean(RH, na.rm = FALSE),
        maxHRM    = max(RH, na.rm = FALSE),
        minHRM    = min(RH, na.rm = FALSE),
        
        meanVVM10 = mean(ws10, na.rm = FALSE),
        maxVVX10  = max(ws10, na.rm = FALSE),
        minVVX10  = min(ws10, na.rm = FALSE),
        
        meanPPT24H = sum(ppt_mm, na.rm = FALSE)
      ),
      by = .(lon, lat, date)
    ][order(lon, lat, date)]
    
  } else {
    .say("Returning hourly per-cell series without aggregation.")
    
    data.table::setorder(hourly_cells, lon, lat, time)
    hourly <- hourly_cells
  }
  
  # Report unavailable hourly rainfall windows in the requested period.
  requested_hourly <- hourly_cells[date >= start_date & date <= end_date]
  report_missing(
    requested_hourly,
    c("ppt_mm_hour", "ppt_mm_prev_6h", "ppt_mm_prev_24h",
      "t2m_C_mean_prev_6h", "RH_mean_prev_6h"),
    "HOURLY FEATURES"
  )

  if (!identical(aggregation_unit, "hourly")) {
    # Replace timestamp-date sums with correctly aligned UTC daily totals.
    rain_daily <- data.table::copy(data.table::as.data.table(hourly_cells))
    if (dataset == "reanalysis-era5-land") {
      # Midnight is the completed accumulation for the PREVIOUS UTC day.
      rain_daily <- rain_daily[format(time, "%H", tz = "UTC") == "00"]
      rain_daily[, date := as.Date(time, tz = "UTC") - 1]
      rain_daily[, meanPPT24H := ppt_accum_mm]
      rain_daily[!is.finite(meanPPT24H) | meanPPT24H < -1e-6, meanPPT24H := NA_real_]
      rain_daily[!is.na(meanPPT24H) & meanPPT24H < 0, meanPPT24H := 0]
      daily_ppt <- rain_daily[, .(lon, lat, date, meanPPT24H)]
    } else {
    # An hour ending at midnight belongs to the preceding calendar day.
    rain_daily[, date := as.Date(time - 1, tz = "UTC")]
    daily_ppt <- rain_daily[, .(meanPPT24H =
            if (.N == 24L && all(is.finite(ppt_mm))) sum(ppt_mm) else NA_real_),
      by = .(lon, lat, date)]
    }
    rm(rain_daily)
    if (aggregation_unit == "region") {
      # Regional rainfall depth is a cell mean, not a sum of cell depths.
      daily_ppt <- daily_ppt[, .(meanPPT24H = mean(meanPPT24H, na.rm = FALSE)),
                             by = date]
      keys <- "date"
    } else keys <- c("lon", "lat", "date")
    daily_dt[, meanPPT24H := NULL]
    daily_dt <- merge(daily_dt, daily_ppt, by = keys, all.x = TRUE, sort = TRUE)
    .say("Daily table: %s rows.", .fmtI(nrow(daily_dt)))
    
    # ---- MWI logic ----
    .say("Computing MWI indices (calm threshold = %.2f km/h) ...", wind_calm_kmh)
    
    calm_ms <- wind_calm_kmh / 3.6
    daily <- as.data.frame(daily_dt)
    
    if (identical(aggregation_unit, "cell")) {
      daily <- daily |>
        dplyr::group_by(lon, lat)
    }
    
    daily <- daily |>
      dplyr::mutate(
        FW = as.integer(meanVVM10 <= calm_ms),
        FH = dplyr::case_when(
          meanHRM < 40 ~ 0,
          meanHRM > 95 ~ 0,
          TRUE ~ (meanHRM / 55) - (40 / 55)
        ),
        FT = dplyr::case_when(
          meanTM <= 15 ~ 0,
          meanTM > 30 ~ 0,
          meanTM > 15 & meanTM <= 20 ~ 0.2 * meanTM - 3,
          meanTM > 20 & meanTM <= 25 ~ 1,
          meanTM > 25 & meanTM <= 30 ~ -0.2 * meanTM + 6
        ),
        mwi = FW * FH * FT,
        
        FWx = as.integer(maxVVX10 <= calm_ms),
        FHx = dplyr::case_when(
          minHRM < 40 ~ 0,
          maxHRM > 95 ~ 0,
          TRUE ~ (meanHRM / 55) - (40 / 55)
        ),
        FTx = dplyr::case_when(
          maxTM <= 15 ~ 0,
          maxTM > 30 ~ 0,
          maxTM > 15 & maxTM <= 20 ~ 0.2 * maxTM - 3,
          maxTM > 20 & maxTM <= 25 ~ 1,
          maxTM > 25 & maxTM <= 30 ~ -0.2 * maxTM + 6
        ),
        mwix = FWx * FHx * FTx,
        
        mwi_zero = mwi == 0,
        FH_zero = FH == 0,
        mwi_zeros_past_14d = RcppRoll::roll_sum(
          mwi_zero,
          n = 14,
          align = "right",
          fill = NA,
          na.rm = FALSE
        ),
        FH_zeros_past_14d = RcppRoll::roll_sum(
          FH_zero,
          n = 14,
          align = "right",
          fill = NA,
          na.rm = FALSE
        )
      )
    
    if (identical(aggregation_unit, "cell")) {
      daily <- dplyr::ungroup(daily)
    }
    
    # ---- rolling-window helpers ----
    .say("Computing rolling weather windows and precipitation lag windows ...")
    
    mk_roll <- function(dt, n, suffix) {
      long <- dt |>
        tidyr::pivot_longer(cols = -date, names_to = "weather_type", values_to = "val") |>
        dplyr::group_by(weather_type) |>
        dplyr::arrange(date, .by_group = TRUE) |>
        dplyr::mutate(
          roll = RcppRoll::roll_mean(
            val,
            n = n,
            align = "right",
            fill = NA,
            na.rm = FALSE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(date, weather_type, roll) |>
        tidyr::pivot_wider(names_from = weather_type, values_from = roll)
      
      out <- long |>
        dplyr::transmute(
          date,
          FW = as.integer(meanVVM10 <= calm_ms),
          FH = dplyr::case_when(
            meanHRM < 40 ~ 0,
            meanHRM > 95 ~ 0,
            TRUE ~ (meanHRM / 55) - (40 / 55)
          ),
          FT = dplyr::case_when(
            meanTM <= 15 ~ 0,
            meanTM > 30 ~ 0,
            meanTM > 15 & meanTM <= 20 ~ 0.2 * meanTM - 3,
            meanTM > 20 & meanTM <= 25 ~ 1,
            meanTM > 25 & meanTM <= 30 ~ -0.2 * meanTM + 6
          ),
          minTM = minTM,
          maxTM = maxTM,
          meanTM = meanTM,
          meanHRM = meanHRM,
          meanVVM10 = meanVVM10,
          mwi = FW * FH * FT
        )
      
      names(out)[names(out) != "date"] <- paste0(names(out)[names(out) != "date"], "_", suffix)
      out
    }
    
    mk_ppt_lags <- function(dt) {
      dt |>
        dplyr::transmute(date, PPT = meanPPT24H) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          PPT_3d  = RcppRoll::roll_sum(PPT, n = 3,  align = "right", fill = NA, na.rm = FALSE),
          PPT_7d  = RcppRoll::roll_sum(PPT, n = 7,  align = "right", fill = NA, na.rm = FALSE),
          PPT_14d = RcppRoll::roll_sum(PPT, n = 14, align = "right", fill = NA, na.rm = FALSE),
          PPT_21d = RcppRoll::roll_sum(PPT, n = 21, align = "right", fill = NA, na.rm = FALSE),
          PPT_30d = RcppRoll::roll_sum(PPT, n = 30, align = "right", fill = NA, na.rm = FALSE),
          
          PPT_3d_lag7  = dplyr::lag(PPT_3d, 7),
          PPT_7d_lag7  = dplyr::lag(PPT_7d, 7),
          PPT_14d_lag7 = dplyr::lag(PPT_14d, 7),
          PPT_21d_lag7 = dplyr::lag(PPT_21d, 7),
          PPT_30d_lag7 = dplyr::lag(PPT_30d, 7)
        ) |>
        dplyr::select(-PPT)
    }
    
    sel_cols <- c(
      "date",
      "meanTM",
      "maxTM",
      "minTM",
      "meanHRM",
      "meanVVM10"
    )
    
    if (identical(aggregation_unit, "region")) {
      sel <- dplyr::select(daily, dplyr::all_of(sel_cols))
      
      lags_3d  <- mk_roll(sel,  3, "3d")
      lags_7d  <- mk_roll(sel,  7, "7d")
      lags_14d <- mk_roll(sel, 14, "14d")
      lags_21d <- mk_roll(sel, 21, "21d")
      lags_30d <- mk_roll(sel, 30, "30d")
      
      lags_3d_lag7 <- mk_roll(sel, 3, "3d_lag7") |>
        dplyr::mutate(date = date + 7)
      
      lags_7d_lag7 <- mk_roll(sel, 7, "7d_lag7") |>
        dplyr::mutate(date = date + 7)
      
      lags_14d_lag7 <- mk_roll(sel, 14, "14d_lag7") |>
        dplyr::mutate(date = date + 7)
      
      lags_21d_lag7 <- mk_roll(sel, 21, "21d_lag7") |>
        dplyr::mutate(date = date + 7)
      
      lags_30d_lag7 <- mk_roll(sel, 30, "30d_lag7") |>
        dplyr::mutate(date = date + 7)
      
      ppt_lags <- mk_ppt_lags(daily)
      
    } else {
      cell_split <- split(daily, list(daily$lon, daily$lat), drop = TRUE)
      
      build_lag <- function(n, suffix) {
        pieces <- lapply(cell_split, function(df) {
          core <- dplyr::select(df, dplyr::all_of(sel_cols))
          res <- mk_roll(core, n, suffix)
          
          res$lon <- df$lon[1]
          res$lat <- df$lat[1]
          
          res <- res[, c("lon", "lat", setdiff(names(res), c("lon", "lat"))), drop = FALSE]
          res
        })
        
        out <- data.table::rbindlist(pieces)
        data.table::setorder(out, lon, lat, date)
        out
      }
      
      shift_lag_dates <- function(x) {
        if (nrow(x)) {
          data.table::set(
            x,
            j = "date",
            value = x[["date"]] + 7
          )
        }
        x
      }
      
      build_ppt_lags <- function(df) {
        out <- mk_ppt_lags(df)
        
        out$lon <- df$lon[1]
        out$lat <- df$lat[1]
        
        out <- out[, c("lon", "lat", setdiff(names(out), c("lon", "lat"))), drop = FALSE]
        out
      }
      
      lags_3d  <- build_lag(3,  "3d")
      lags_7d  <- build_lag(7,  "7d")
      lags_14d <- build_lag(14, "14d")
      lags_21d <- build_lag(21, "21d")
      lags_30d <- build_lag(30, "30d")
      
      lags_3d_lag7  <- shift_lag_dates(build_lag(3,  "3d_lag7"))
      lags_7d_lag7  <- shift_lag_dates(build_lag(7,  "7d_lag7"))
      lags_14d_lag7 <- shift_lag_dates(build_lag(14, "14d_lag7"))
      lags_21d_lag7 <- shift_lag_dates(build_lag(21, "21d_lag7"))
      lags_30d_lag7 <- shift_lag_dates(build_lag(30, "30d_lag7"))
      
      ppt_pieces <- lapply(cell_split, build_ppt_lags)
      
      ppt_lags <- data.table::rbindlist(ppt_pieces)
      data.table::setorder(ppt_lags, lon, lat, date)
    }
    
    trim_dates <- function(x) {
      x <- as.data.frame(x)
      x[x$date >= start_date & x$date <= end_date, , drop = FALSE]
    }
    daily <- trim_dates(daily)
    lags_3d <- trim_dates(lags_3d)
    lags_7d <- trim_dates(lags_7d)
    lags_14d <- trim_dates(lags_14d)
    lags_21d <- trim_dates(lags_21d)
    lags_30d <- trim_dates(lags_30d)
    lags_3d_lag7 <- trim_dates(lags_3d_lag7)
    lags_7d_lag7 <- trim_dates(lags_7d_lag7)
    lags_14d_lag7 <- trim_dates(lags_14d_lag7)
    lags_21d_lag7 <- trim_dates(lags_21d_lag7)
    lags_30d_lag7 <- trim_dates(lags_30d_lag7)
    ppt_lags <- trim_dates(ppt_lags)
    report_missing(daily, c("maxTM", "minHRM", "meanVVM10", "meanPPT24H"),
                         "DAILY FEATURES")
    report_missing(ppt_lags, setdiff(names(ppt_lags), c("lon", "lat", "date")),
                         "DAILY RAINFALL WINDOWS")

    # ---- write outputs ----
    admin_tokens <- sanitize_slug(admin_name)
    if (!length(admin_tokens)) admin_tokens <- "all"
    
    dataset_token <- dataset_subdir
    
    base_prefix <- paste0(
      "weather_",
      iso_fragment,
      "_",
      admin_level,
      "_",
      paste(admin_tokens, collapse = "_"),
      "_",
      dataset_token
    )
    
    prefix <- if (identical(aggregation_unit, "cell")) {
      paste0(base_prefix, "_cell")
    } else {
      paste0(base_prefix, "_region")
    }
    
    p_daily        <- file.path(out_dir, paste0(prefix, "_daily.Rds"))
    p_lags_3       <- file.path(out_dir, paste0(prefix, "_lags_3d.Rds"))
    p_lags_7       <- file.path(out_dir, paste0(prefix, "_lags_7d.Rds"))
    p_lags_14      <- file.path(out_dir, paste0(prefix, "_lags_14d.Rds"))
    p_lags_21      <- file.path(out_dir, paste0(prefix, "_lags_21d.Rds"))
    p_lags_30      <- file.path(out_dir, paste0(prefix, "_lags_30d.Rds"))
    p_lags_3_lag7  <- file.path(out_dir, paste0(prefix, "_lags_3d_lag7.Rds"))
    p_lags_7_lag7  <- file.path(out_dir, paste0(prefix, "_lags_7d_lag7.Rds"))
    p_lags_14_lag7 <- file.path(out_dir, paste0(prefix, "_lags_14d_lag7.Rds"))
    p_lags_21_lag7 <- file.path(out_dir, paste0(prefix, "_lags_21d_lag7.Rds"))
    p_lags_30_lag7 <- file.path(out_dir, paste0(prefix, "_lags_30d_lag7.Rds"))
    p_ppt_lags     <- file.path(out_dir, paste0(prefix, "_ppt_lags.Rds"))
    
    .say("Writing RDS files to %s ...", out_dir)
    
    readr::write_rds(daily,         p_daily)
    readr::write_rds(lags_3d,       p_lags_3)
    readr::write_rds(lags_7d,       p_lags_7)
    readr::write_rds(lags_14d,      p_lags_14)
    readr::write_rds(lags_21d,      p_lags_21)
    readr::write_rds(lags_30d,      p_lags_30)
    readr::write_rds(lags_3d_lag7,  p_lags_3_lag7)
    readr::write_rds(lags_7d_lag7,  p_lags_7_lag7)
    readr::write_rds(lags_14d_lag7, p_lags_14_lag7)
    readr::write_rds(lags_21d_lag7, p_lags_21_lag7)
    readr::write_rds(lags_30d_lag7, p_lags_30_lag7)
    readr::write_rds(ppt_lags,      p_ppt_lags)
    
    .say("Done writing.")
    
    if (isTRUE(attach_to_global)) {
      .say("Attaching outputs to .GlobalEnv ...")
      
      assign(paste0(prefix, "_daily"),         daily,         envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_3d"),       lags_3d,       envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_7d"),       lags_7d,       envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_14d"),      lags_14d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_21d"),      lags_21d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_30d"),      lags_30d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_3d_lag7"),  lags_3d_lag7,  envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_7d_lag7"),  lags_7d_lag7,  envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_14d_lag7"), lags_14d_lag7, envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_21d_lag7"), lags_21d_lag7, envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_30d_lag7"), lags_30d_lag7, envir = .GlobalEnv)
      assign(paste0(prefix, "_ppt_lags"),      ppt_lags,      envir = .GlobalEnv)
      
      .say("Attached objects with prefix '%s_*'.", prefix)
    }
    
    result <- list(
      daily         = daily,
      lags_3d       = lags_3d,
      lags_7d       = lags_7d,
      lags_14d      = lags_14d,
      lags_21d      = lags_21d,
      lags_30d      = lags_30d,
      lags_3d_lag7  = lags_3d_lag7,
      lags_7d_lag7  = lags_7d_lag7,
      lags_14d_lag7 = lags_14d_lag7,
      lags_21d_lag7 = lags_21d_lag7,
      lags_30d_lag7 = lags_30d_lag7,
      ppt_lags      = ppt_lags,
      paths = list(
        daily         = p_daily,
        lags_3d       = p_lags_3,
        lags_7d       = p_lags_7,
        lags_14d      = p_lags_14,
        lags_21d      = p_lags_21,
        lags_30d      = p_lags_30,
        lags_3d_lag7  = p_lags_3_lag7,
        lags_7d_lag7  = p_lags_7_lag7,
        lags_14d_lag7 = p_lags_14_lag7,
        lags_21d_lag7 = p_lags_21_lag7,
        lags_30d_lag7 = p_lags_30_lag7,
        ppt_lags      = p_ppt_lags
      )
    )
    
  } else {
    admin_tokens <- sanitize_slug(admin_name)
    if (!length(admin_tokens)) admin_tokens <- "all"
    
    dataset_token <- dataset_subdir
    
    base_prefix <- paste0(
      "weather_",
      iso_fragment,
      "_",
      admin_level,
      "_",
      paste(admin_tokens, collapse = "_"),
      "_",
      dataset_token
    )
    
    hourly <- hourly[date >= start_date & date <= end_date]
    prefix <- paste0(base_prefix, "_hourly")
    p_hourly <- file.path(out_dir, paste0(prefix, ".Rds"))
    
    .say("Writing RDS files to %s ...", out_dir)
    
    readr::write_rds(hourly, p_hourly)
    
    .say("Done writing.")
    
    if (isTRUE(attach_to_global)) {
      .say("Attaching outputs to .GlobalEnv ...")
      assign(prefix, hourly, envir = .GlobalEnv)
      .say("Attached object '%s'.", prefix)
    }
    
    result <- list(
      hourly = hourly,
      paths  = list(hourly = p_hourly)
    )
  }
  
  .say("All done ✅")
  invisible(result)
}