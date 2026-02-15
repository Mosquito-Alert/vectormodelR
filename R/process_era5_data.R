#' Build daily & lagged weather features from processed ERA5 CSVs
#'
#' Reads monthly CSV.GZ files created by your ERA5 compiler (one file per
#' month with multiple variables named like `era5_<iso3>_YYYY_MM_all_variables.csv.gz`), clips by a GADM admin polygon, aggregates
#' to hourly area means, derives daily summaries and rolling-window features,
#' and saves RDS outputs with informative names.
#'
#' @param processed_dir Character. Root dir containing `processed/YYYY/era5_<iso3>_YYYY_MM_all_variables.csv.gz`.
#'   If NULL, defaults to `file.path("data/weather/grib", tolower(iso3), "processed")`.
#' @param iso3 Character. ISO3 code for the country (e.g., "ESP").
#' @param admin_level Integer. GADM administrative level (0=country, 1=region, 2=province, ...).
#' @param admin_name Character or NULL. Exact `NAME_<level>` to match (e.g., "Barcelona").
#'   If NULL, the whole level geometry is used (unioned).
#' @param out_dir Character. Directory to write RDS outputs. Created if missing.
#' @param start_date Date. Earliest date to include (UTC). Default "2014-01-01".
#' @param end_date Date. Latest date to include (UTC). Default is today.
#' @param wind_calm_kmh Numeric. Calm-wind threshold in km/h for MWI logic. Default 6.
#' @param round_ll Integer. Rounding (decimal places) applied to lon/lat before
#'   reshaping to wide. Default 3 (≈100–120 m grid at mid-latitudes).
#' @param verbose Logical. If TRUE, prints progress messages. Default TRUE.
#' @param attach_to_global Logical. If TRUE, assigns output data.frames to .GlobalEnv
#'   with names based on the file prefix (e.g., `weather_esp_lvl2_barcelona_daily`). Default FALSE.
#' @param aggregation_unit Character. Choose "region" (default) to produce
#'   polygon-wide means, "cell" for per-ERA5-cell summaries, or "hourly" to return the
#'   raw per-cell hourly series without further aggregation.
#' @param polygon_buffer_km Numeric. If no ERA5 centroids fall inside the admin
#'   polygon, expand it by this distance (kilometers) and retry. Default 10.
#'
#' @return For `aggregation_unit = "region"` or `"cell"`, (invisibly) a list with:
#'   `daily`, `lags_7d`, `lags_14d`, `lags_30d`, `lags_21d_lag7`, `ppt_lags`, and
#'   `paths` (written file paths). For `aggregation_unit = "hourly"`, returns a list
#'   with `hourly` (raw per-cell series) and `paths`.
#'
#' @details
#' Expects variables in the CSVs:
#' - "10m_u_component_of_wind", "10m_v_component_of_wind"
#' - "2m_temperature", "2m_dewpoint_temperature"
#' - "surface_pressure", "total_precipitation"
#'
#' Units assumed (ERA5 defaults): temperature in Kelvin, precipitation in meters
#' per step; wind components in m/s.
#'
#' @importFrom lubridate ymd_hms
#' @importFrom RcppRoll roll_mean roll_sum
#' @importFrom sf st_as_sf st_make_valid st_union st_within st_drop_geometry st_bbox st_transform st_buffer st_intersects
#' @importFrom geodata gadm
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate case_when transmute arrange lag select all_of
#' @importFrom readr write_rds
#' @importFrom data.table fread setDT setnames as.data.table rbindlist dcast copy setorder set
#' @export
process_era5_data <- function(
  processed_dir = NULL,
  iso3,
  admin_level,
  admin_name,
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
  .say  <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  .fmtI <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE)
  aggregation_unit <- match.arg(aggregation_unit)

  # ---- deps & args ----
  if (!is.character(iso3) || length(iso3) != 1L || !nzchar(iso3)) {
    stop("`iso3` must be a non-empty character scalar.")
  }
  iso3_upper   <- toupper(iso3)
  iso_fragment <- tolower(iso3_upper)

  if (is.null(processed_dir) || !nzchar(processed_dir)) {
    processed_dir <- file.path("data/weather/grib", iso_fragment, "processed")
  }
  processed_dir <- path.expand(processed_dir)
  if (!dir.exists(processed_dir)) {
    stop("Processed directory not found: ", processed_dir)
  }
  out_dir       <- path.expand(out_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # helper: list monthlies
  list_month_files <- function(dir, iso_slug) {
    pat <- sprintf("^era5_%s_\\d{4}_\\d{2}_all_variables\\.csv\\.gz$", iso_slug)
    list.files(dir, pattern = pat, full.names = TRUE, recursive = TRUE, ignore.case = FALSE)
  }

  # Kelvin -> Celsius
  K_to_C <- function(x) x - 273.15

  # RH from T & Td (C) (Magnus)
  rh_from_T_Td <- function(Tc, TDc, a = 17.625, b = 243.04) {
    100 * exp((a*TDc/(b + TDc)) - (a*Tc/(b + Tc)))
  }

  files <- list_month_files(processed_dir, iso_fragment)
  if (!length(files)) stop("No processed monthly CSVs found for ISO '", iso_fragment, "' under: ", processed_dir)
  .say("Found %s monthly files.", .fmtI(length(files)))


  # ---- admin geometry & bbox window ----
  .say("Loading GADM geometry: %s level %d ...", iso3_upper, admin_level)
  g <- geodata::gadm(country = iso3_upper, level = admin_level, path = file.path(out_dir, "gadm")) |>
    sf::st_as_sf()

  if (!is.null(admin_name)) {
    nmcol <- paste0("NAME_", admin_level)
    .say("Filtering admin unit by name column %s == '%s' ...", nmcol, admin_name)
    g <- g[g[[nmcol]] == admin_name, , drop = FALSE]
    if (nrow(g) == 0) stop("Admin name '", admin_name, "' not found at level ", admin_level, " for ", iso3_upper)
  } else {
    .say("No admin_name provided; using union of all geometries at level %d.", admin_level)
    g <- sf::st_union(g)
  }

  bb <- sf::st_bbox(g)  # xmin, ymin, xmax, ymax
  
  # Apply a safety margin to the initial READ filter.
  read_margin <- 0.5 # degrees, approx 50km

  lon_min <- as.numeric(bb["xmin"]) - read_margin
  lon_max <- as.numeric(bb["xmax"]) + read_margin
  lat_min <- as.numeric(bb["ymin"]) - read_margin
  lat_max <- as.numeric(bb["ymax"]) + read_margin

  .say("Bounding box: lon[%.4f, %.4f], lat[%.4f, %.4f] (with read margin).", lon_min, lon_max, lat_min, lat_max)

  # ---- read & prefilter by bbox/time/vars ----
  wanted <- c("10m_u_component_of_wind",
              "10m_v_component_of_wind",
              "2m_temperature",
              "2m_dewpoint_temperature",
              "surface_pressure",
              "total_precipitation")

  .say("Reading and prefiltering files (bbox + variables), parsing time ...")
  pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
  on.exit(try(close(pb), silent = TRUE), add = TRUE)

  DT <- data.table::rbindlist(
    lapply(seq_along(files), function(i) {
      f <- files[i]
      dt <- data.table::fread(f, showProgress = FALSE)
      # OPTIMIZATION: Removed as.data.frame(dt) to save memory/speed for large inputs
      
      nm_lower <- tolower(names(dt))
      candidate_cols <- c("variable_name","grib_variable_name","variable","var_name","var")
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
      if (!length(keep_var_idx)) return(data.table::data.table()) # Return empty DT
      dt <- dt[keep_var_idx, , drop = FALSE]

      keep_bbox_idx <- which(
        dt[["longitude"]] >= lon_min & dt[["longitude"]] <= lon_max &
        dt[["latitude"]]  >= lat_min & dt[["latitude"]]  <= lat_max
      )
      if (!length(keep_bbox_idx)) return(data.table::data.table()) # Return empty DT
      dt <- dt[keep_bbox_idx, , drop = FALSE]

      # Parse time directly in DT
      # Some ERA5 exports append " UTC"
      time_char <- gsub(" UTC$", "", dt[["time"]])
      time_vals <- lubridate::ymd_hms(time_char, tz = "UTC", quiet = TRUE)
      data.table::set(dt, j = "time", value = time_vals)
      
      utils::setTxtProgressBar(pb, i)
      dt
    }),
    use.names = TRUE, fill = TRUE
  )
  
  DT <- data.table::as.data.table(DT)
  
  if (!nrow(DT)) stop("No rows after bbox/var filtering. Check inputs.")
  .say("\nAfter bbox/var filter: %s rows.", .fmtI(nrow(DT)))

  # ---- set default dates if missing, then filter once ----
  if (is.null(start_date)) start_date <- as.Date(lubridate::floor_date(min(DT$time, na.rm = TRUE), unit = "day"))
  if (is.null(end_date))   end_date   <- as.Date(lubridate::ceiling_date(max(DT$time, na.rm = TRUE), unit = "day") - 1)
  .say("Date window: %s to %s (inclusive).", format(start_date), format(end_date))

  keep_range <- DT$time >= as.POSIXct(start_date, tz = "UTC") &
    DT$time < as.POSIXct(end_date + 1, tz = "UTC")
  DT <- DT[keep_range]
  if (!nrow(DT)) stop("No rows after time filtering. Check date window.")
  .say("After time filter: %s rows.", .fmtI(nrow(DT)))

  # ---- polygon mask (exact clip) [FIXED SECTION] ----
  .say("Applying exact polygon mask ...")
  poly <- g |> sf::st_make_valid() |> sf::st_union()

  # 1. Create a temporary SF object for spatial matching ONLY
  # We do this to find the indices, then we'll throw it away to free memory
  temp_sf <- sf::st_as_sf(DT, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # 2. Get logical vector of points inside
  # sparse = FALSE returns a simple logical vector
  inside_idx <- as.logical(sf::st_intersects(temp_sf, poly, sparse = FALSE))
  inside_idx[is.na(inside_idx)] <- FALSE # Safety for coordinate edge cases

  # 3. Handle buffering if no points found
  if (!any(inside_idx) && polygon_buffer_km > 0) {
    .say("No points inside polygon; buffering by %.1f km and retrying ...", polygon_buffer_km)
    poly_buffer <- poly |>
      sf::st_transform(3857) |>
      sf::st_buffer(polygon_buffer_km * 1000) |>
      sf::st_transform(4326)
    inside_idx <- as.logical(sf::st_intersects(temp_sf, poly_buffer, sparse = FALSE))
    inside_idx[is.na(inside_idx)] <- FALSE
    
    if (any(inside_idx)) {
      .say("Buffer captured %s rows.", .fmtI(sum(inside_idx)))
    }
  }

  if (!any(inside_idx)) stop("No rows inside the polygon. Check coordinates.")

  # 4. THE CRITICAL FIX: Subset the data.table 'DT' using which() integer indices
  # This avoids the 'undefined columns' error triggered by sf/data.frame methods
  DT <- DT[which(inside_idx), ]

  # 5. Cleanup memory immediately
  rm(temp_sf, inside_idx); gc()
  .say("Points inside polygon: %s rows kept.", .fmtI(nrow(DT)))

  # ---- round lon/lat (stabilize keys for dcast) ----
  data.table::set(DT, j = "lat", value = round(DT$latitude,  round_ll))
  data.table::set(DT, j = "lon", value = round(DT$longitude, round_ll))
  .say("Rounded lon/lat to %d decimals.", round_ll)

  # ---- wide per cell & hour ----
  .say("Casting to wide per (lon, lat, time) ...")
  wide <- data.table::dcast(
    DT[, .(lon, lat, time, variable_name, value)],
    lon + lat + time ~ variable_name,
    value.var = "value"
  )
  .say("Wide table: %s rows, %d columns.", .fmtI(nrow(wide)), ncol(wide))

  # ---- derived hourly features ----
  .say("Computing hourly derived features ...")
  ws10_vals <- sqrt(wide[["10m_u_component_of_wind"]]^2 + wide[["10m_v_component_of_wind"]]^2)
  data.table::set(wide, j = "ws10", value = ws10_vals)

  t2m_vals <- K_to_C(wide[["2m_temperature"]])
  d2m_vals <- K_to_C(wide[["2m_dewpoint_temperature"]])
  data.table::set(wide, j = "t2m_C", value = t2m_vals)
  data.table::set(wide, j = "d2m_C", value = d2m_vals)

  rh_vals <- pmin(pmax(rh_from_T_Td(t2m_vals, d2m_vals), 0), 100)
  data.table::set(wide, j = "RH", value = rh_vals)

  ppt_vals <- wide[["total_precipitation"]] * 1000
  data.table::set(wide, j = "ppt_mm", value = ppt_vals)  # m -> mm

  wide_small <- wide[, .(lon, lat, time, t2m_C, d2m_C, RH, ws10, ppt_mm)]
  hourly_cells <- data.table::copy(wide_small)
  data.table::set(hourly_cells, j = "date", value = as.Date(hourly_cells[["time"]], tz = "UTC"))

  if (identical(aggregation_unit, "region")) {
    .say("Aggregating to hourly area means ...")
    hourly <- hourly_cells[
      ,
      .(
        TM    = mean(t2m_C, na.rm = TRUE),
        HRM   = mean(RH,    na.rm = TRUE),
        VVM10 = mean(ws10,  na.rm = TRUE),
        PPT   = sum(ppt_mm, na.rm = TRUE)
      ),
      by = .(time)
    ]
    data.table::set(hourly, j = "date", value = as.Date(hourly[["time"]], tz = "UTC"))
    .say("Hourly table: %s rows.", .fmtI(nrow(hourly)))

    .say("Aggregating to daily summaries ...")
    daily_dt <- hourly[
      ,
      .(
        meanTM    = mean(TM,    na.rm = TRUE),
        maxTM     = max(TM,     na.rm = TRUE),
        minTM     = min(TM,     na.rm = TRUE),

        meanHRM   = mean(HRM,   na.rm = TRUE),
        maxHRM    = max(HRM,    na.rm = TRUE),
        minHRM    = min(HRM,    na.rm = TRUE),

        meanVVM10 = mean(VVM10, na.rm = TRUE),
        maxVVX10  = max(VVM10,  na.rm = TRUE),
        minVVX10  = min(VVM10,  na.rm = TRUE),

        meanPPT24H = sum(PPT,   na.rm = TRUE)
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
        meanTM    = mean(t2m_C, na.rm = TRUE),
        maxTM     = max(t2m_C, na.rm = TRUE),
        minTM     = min(t2m_C, na.rm = TRUE),

        meanHRM   = mean(RH,    na.rm = TRUE),
        maxHRM    = max(RH,     na.rm = TRUE),
        minHRM    = min(RH,     na.rm = TRUE),

        meanVVM10 = mean(ws10,  na.rm = TRUE),
        maxVVX10  = max(ws10,   na.rm = TRUE),
        minVVX10  = min(ws10,   na.rm = TRUE),

        meanPPT24H = sum(ppt_mm, na.rm = TRUE)
      ),
      by = .(lon, lat, date)
    ][order(lon, lat, date)]
  } else {
    .say("Returning hourly per-cell series without aggregation.")
    data.table::setorder(hourly_cells, lon, lat, time)
    hourly <- hourly_cells
  }

  if (!identical(aggregation_unit, "hourly")) {
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
        FW  = as.integer(meanVVM10 <= calm_ms),
        FH  = dplyr::case_when(
          meanHRM < 40 ~ 0,
          meanHRM > 95 ~ 0,
          TRUE         ~ (meanHRM/55) - (40/55)
        ),
        FT  = dplyr::case_when(
          meanTM <= 15 ~ 0,
          meanTM >  30 ~ 0,
          meanTM > 15 & meanTM <= 20 ~ 0.2*meanTM - 3,
          meanTM > 20 & meanTM <= 25 ~ 1,
          meanTM > 25 & meanTM <= 30 ~ -0.2*meanTM + 6
        ),
        mwi = FW*FH*FT,

        FWx = as.integer(maxVVX10 <= calm_ms),
        FHx = dplyr::case_when(
          minHRM < 40 ~ 0,
          maxHRM > 95 ~ 0,
          TRUE        ~ (meanHRM/55) - (40/55)
        ),
        FTx = dplyr::case_when(
          maxTM <= 15 ~ 0,
          maxTM >  30 ~ 0,
          maxTM > 15 & maxTM <= 20 ~ 0.2*meanTM - 3,
          maxTM > 20 & meanTM <= 25 ~ 1,
          maxTM > 25 & maxTM <= 30  ~ -0.2*meanTM + 6
        ),
        mwix = FWx*FHx*FTx,

        mwi_zero = mwi == 0,
        FH_zero  = FH  == 0,
        mwi_zeros_past_14d = RcppRoll::roll_sum(mwi_zero, n = 14, align = "right", fill = NA, na.rm = TRUE),
        FH_zeros_past_14d  = RcppRoll::roll_sum(FH_zero,  n = 14, align = "right", fill = NA, na.rm = TRUE)
      )
    if (identical(aggregation_unit, "cell")) {
      daily <- dplyr::ungroup(daily)
    }

    # ---- rolling windows helper ----
    .say("Computing rolling windows (7, 14, 21(+lag7), 30 days) ...")
    mk_roll <- function(dt, n, suffix) {
      long <- dt |>
        tidyr::pivot_longer(cols = -date, names_to = "weather_type", values_to = "val") |>
        dplyr::group_by(weather_type) |>
        dplyr::arrange(date, .by_group = TRUE) |>
        dplyr::mutate(roll = RcppRoll::roll_mean(val, n = n, align = "right", fill = NA, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::select(date, weather_type, roll) |>
        tidyr::pivot_wider(names_from = weather_type, values_from = roll)

      out <- long |>
        dplyr::transmute(
          date,
          FW  = as.integer(meanVVM10 <= calm_ms),
          FH  = dplyr::case_when(
            meanHRM < 40 ~ 0,
            meanHRM > 95 ~ 0,
            TRUE         ~ (meanHRM/55) - (40/55)
          ),
          FT  = dplyr::case_when(
            meanTM <= 15 ~ 0,
            meanTM >  30 ~ 0,
            meanTM > 15 & meanTM <= 20 ~ 0.2*meanTM - 3,
            meanTM > 20 & meanTM <= 25 ~ 1,
            meanTM > 25 & meanTM <= 30 ~ -0.2*meanTM + 6
          ),
          minTM = minTM,
          maxTM = maxTM,
          mwi = FW*FH*FT,
          PPT = meanPPT24H
        )

      names(out)[names(out) != "date"] <- paste0(names(out)[names(out) != "date"], suffix)
      out
    }

    sel_cols <- c("date", "meanTM", "maxTM", "minTM", "meanHRM", "meanVVM10", "meanPPT24H")

    if (identical(aggregation_unit, "region")) {
      sel <- dplyr::select(daily, dplyr::all_of(sel_cols))
      lags_7d   <- mk_roll(sel,  7, "7")
      lags_14d  <- mk_roll(sel, 14, "14")
      lags_30d  <- mk_roll(sel, 30, "30")
      lags_21d_lag7 <- mk_roll(sel, 21, "21") |>
        dplyr::mutate(date = date + 7)

      ppt_lags <- daily |>
        dplyr::transmute(date, PPT = meanPPT24H) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          PPT_7d          = RcppRoll::roll_sum(PPT, n = 7,  align = "right", fill = NA, na.rm = TRUE),
          PPT_30d         = RcppRoll::roll_sum(PPT, n = 30, align = "right", fill = NA, na.rm = TRUE),
          PPT_7d_8daysago = dplyr::lag(PPT_7d, 8)
        ) |>
        dplyr::select(-PPT)
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

      lags_7d   <- build_lag(7,  "7")
      lags_14d  <- build_lag(14, "14")
      lags_30d  <- build_lag(30, "30")
      lags_21d_lag7 <- build_lag(21, "21")
      if (nrow(lags_21d_lag7)) {
        data.table::set(lags_21d_lag7, j = "date", value = lags_21d_lag7[["date"]] + 7)
      }

      ppt_pieces <- lapply(cell_split, function(df) {
        out <- df |>
          dplyr::transmute(date, PPT = meanPPT24H) |>
          dplyr::arrange(date) |>
          dplyr::mutate(
            PPT_7d          = RcppRoll::roll_sum(PPT, n = 7,  align = "right", fill = NA, na.rm = TRUE),
            PPT_30d         = RcppRoll::roll_sum(PPT, n = 30, align = "right", fill = NA, na.rm = TRUE),
            PPT_7d_8daysago = dplyr::lag(PPT_7d, 8)
          ) |>
          dplyr::select(-PPT)
        out$lon <- df$lon[1]
        out$lat <- df$lat[1]
        out <- out[, c("lon", "lat", setdiff(names(out), c("lon", "lat"))), drop = FALSE]
        out
      })
      ppt_lags <- data.table::rbindlist(ppt_pieces)
      data.table::setorder(ppt_lags, lon, lat, date)
    }

    # ---- write outputs with informative names ----
    admin_tokens <- sanitize_slug(admin_name)
    if (!length(admin_tokens)) admin_tokens <- "all"
    base_prefix <- paste0("weather_", iso_fragment, "_", admin_level, "_", paste(admin_tokens, collapse = "-"))
    prefix <- if (identical(aggregation_unit, "cell")) paste0(base_prefix, "_cell") else paste0(base_prefix, "_region")

    p_daily         <- file.path(out_dir, paste0(prefix, "_daily.Rds"))
    p_lags_7        <- file.path(out_dir, paste0(prefix, "_lags_7d.Rds"))
    p_lags_14       <- file.path(out_dir, paste0(prefix, "_lags_14d.Rds"))
    p_lags_30       <- file.path(out_dir, paste0(prefix, "_lags_30d.Rds"))
    p_lags_21_lag7  <- file.path(out_dir, paste0(prefix, "_lags_21d_lag7.Rds"))
    p_ppt_lags      <- file.path(out_dir, paste0(prefix, "_ppt_lags.Rds"))

    .say("Writing RDS files to %s ...", out_dir)
    readr::write_rds(daily,         p_daily)
    readr::write_rds(lags_7d,       p_lags_7)
    readr::write_rds(lags_14d,      p_lags_14)
    readr::write_rds(lags_30d,      p_lags_30)
    readr::write_rds(lags_21d_lag7, p_lags_21_lag7)
    readr::write_rds(ppt_lags,      p_ppt_lags)
    .say("Done writing.")

    if (isTRUE(attach_to_global)) {
      .say("Attaching outputs to .GlobalEnv ...")
      assign(paste0(prefix, "_daily"),          daily,         envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_7d"),        lags_7d,       envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_14d"),       lags_14d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_30d"),       lags_30d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_21d_lag7"),  lags_21d_lag7, envir = .GlobalEnv)
      assign(paste0(prefix, "_ppt_lags"),       ppt_lags,      envir = .GlobalEnv)
      .say("Attached objects with prefix '%s_*'.", prefix)
    }

    result <- list(
      daily         = daily,
      lags_7d       = lags_7d,
      lags_14d      = lags_14d,
      lags_30d      = lags_30d,
      lags_21d_lag7 = lags_21d_lag7,
      ppt_lags      = ppt_lags,
      paths = list(
        daily         = p_daily,
        lags_7d       = p_lags_7,
        lags_14d      = p_lags_14,
        lags_30d      = p_lags_30,
        lags_21d_lag7 = p_lags_21_lag7,
        ppt_lags      = p_ppt_lags
      )
    )
  } else {
    admin_tokens <- sanitize_slug(admin_name)
    if (!length(admin_tokens)) admin_tokens <- "all"
    base_prefix <- paste0("weather_", iso_fragment, "_", admin_level, "_", paste(admin_tokens, collapse = "-"))
    prefix <- paste0(base_prefix, "_hourly")
    p_hourly <- file.path(out_dir, paste0(prefix, ".Rds"))

    .say("Writing RDS files to %s ...", out_dir)
    readr::write_rds(hourly, p_hourly)
    .say("Done writing.")

    if (isTRUE(attach_to_global)) {
      .say("Attaching outputs to .GlobalEnv ...")
      assign(paste0(prefix, ""), hourly, envir = .GlobalEnv)
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