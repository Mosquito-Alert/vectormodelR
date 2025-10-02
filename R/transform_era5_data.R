#' Build daily & lagged weather features from processed ERA5 CSVs
#'
#' Reads monthly CSV.GZ files created by your ERA5 compiler (one file per
#' month with multiple variables), clips by a GADM admin polygon, aggregates
#' to hourly area means, derives daily summaries and rolling-window features,
#' and saves RDS outputs with informative names.
#'
#' @param processed_dir Character. Root dir containing `processed/YYYY/era5_YYYY_MM_all_variables.csv.gz`.
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
#'
#' @return (Invisibly) a list with: `daily`, `lags_7d`, `lags_14d`, `lags_30d`,
#'   `lags_21d_lag7`, `ppt_lags`, and `paths` (written file paths).
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
#' @import data.table
#' @importFrom lubridate ymd_hms
#' @importFrom RcppRoll roll_mean roll_sum
#' @importFrom sf st_as_sf st_make_valid st_union st_within st_drop_geometry st_bbox
#' @importFrom geodata gadm
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate case_when transmute arrange lag select
#' @importFrom readr write_rds
#' @export
#'
#' @examples
#' \dontrun{
#' # Barcelona province
#' transform_era5_data(
#'   iso3 = "ESP",
#'   admin_level = 2,
#'   admin_name = "Barcelona",
#'   processed_dir = "~/data/weather/grib/processed",
#'   out_dir = "data/proc"
#' )
#'
#' # Catalonia region (level 1)
#' transform_era5_data(
#'   iso3 = "ESP",
#'   admin_level = 1,
#'   admin_name = "Cataluña",
#'   processed_dir = "~/data/weather/grib/processed",
#'   out_dir = "data/weather_catalonia"
#' )
#'
#' # Stricter calm wind threshold (4 km/h)
#' transform_era5_data(
#'   iso3 = "ESP",
#'   admin_level = 2,
#'   admin_name = "Barcelona",
#'   processed_dir = "~/data/weather/grib/processed",
#'   out_dir = "data/proc_strict",
#'   wind_calm_kmh = 4
#' )
#' }
transform_era5_data <- function(
  processed_dir,
  iso3,
  admin_level,
  admin_name,
  out_dir = "data/proc",
  start_date = NULL,
  end_date   = NULL,
  wind_calm_kmh = 6,
  round_ll = 3,
  verbose  = TRUE,
  attach_to_global = FALSE
) {
  .say  <- function(...) if (isTRUE(verbose)) message(sprintf(...))
  .fmtI <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE)

  # ---- deps & args ----
  stopifnot(dir.exists(path.expand(processed_dir)))
  processed_dir <- path.expand(processed_dir)
  out_dir       <- path.expand(out_dir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # helper: list monthlies
  list_month_files <- function(dir) {
    list.files(dir, pattern = "^era5_\\d{4}_\\d{2}_all_variables\\.csv\\.gz$",
               full.names = TRUE, recursive = TRUE)
  }

  # Kelvin -> Celsius
  K_to_C <- function(x) x - 273.15

  # RH from T & Td (C) (Magnus)
  rh_from_T_Td <- function(Tc, TDc, a = 17.625, b = 243.04) {
    100 * exp((a*TDc/(b + TDc)) - (a*Tc/(b + Tc)))
  }

  files <- list_month_files(processed_dir)
  if (!length(files)) stop("No processed monthly CSVs found under: ", processed_dir)
  .say("Found %s monthly files.", .fmtI(length(files)))


  # ---- admin geometry & bbox window ----
  .say("Loading GADM geometry: %s level %d ...", iso3, admin_level)
  g <- geodata::gadm(country = iso3, level = admin_level, path = file.path(out_dir, "gadm")) |>
    sf::st_as_sf()

  if (!is.null(admin_name)) {
    nmcol <- paste0("NAME_", admin_level)
    .say("Filtering admin unit by name column %s == '%s' ...", nmcol, admin_name)
    g <- g[g[[nmcol]] == admin_name, , drop = FALSE]
    if (nrow(g) == 0) stop("Admin name '", admin_name, "' not found at level ", admin_level, " for ", iso3)
  } else {
    .say("No admin_name provided; using union of all geometries at level %d.", admin_level)
    g <- sf::st_union(g)
  }

  bb <- sf::st_bbox(g)  # xmin, ymin, xmax, ymax
  lon_min <- as.numeric(bb["xmin"]); lon_max <- as.numeric(bb["xmax"])
  lat_min <- as.numeric(bb["ymin"]); lat_max <- as.numeric(bb["ymax"])
  .say("Bounding box: lon[%.4f, %.4f], lat[%.4f, %.4f].", lon_min, lon_max, lat_min, lat_max)

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
      dt <- dt[variable_name %in% wanted]
      dt <- dt[longitude >= lon_min & longitude <= lon_max &
               latitude  >= lat_min & latitude  <= lat_max]
      dt[, time := lubridate::ymd_hms(time, tz = "UTC")]
      utils::setTxtProgressBar(pb, i)
      dt
    }),
    use.names = TRUE, fill = TRUE
  )
  if (!nrow(DT)) stop("No rows after bbox/var filtering. Check inputs.")
  .say("\nAfter bbox/var filter: %s rows.", .fmtI(nrow(DT)))

  # ---- set default dates if missing, then filter once ----
  if (is.null(start_date)) start_date <- as.Date(min(DT$time, na.rm = TRUE))
  if (is.null(end_date))   end_date   <- as.Date(max(DT$time, na.rm = TRUE))
  .say("Date window: %s to %s (inclusive).", format(start_date), format(end_date))

  DT <- DT[
    time >= as.POSIXct(start_date, tz = "UTC") &
    time <  as.POSIXct(end_date + 1, tz = "UTC")
  ]
  if (!nrow(DT)) stop("No rows after time filtering. Check date window.")
  .say("After time filter: %s rows.", .fmtI(nrow(DT)))

  # ---- polygon mask (exact clip) ----
  .say("Applying exact polygon mask ...")
  poly <- g |> sf::st_make_valid() |> sf::st_union()
  DT_sf <- sf::st_as_sf(DT, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)
  inside_idx <- lengths(sf::st_within(DT_sf, poly, sparse = TRUE)) > 0
  kept_n <- sum(inside_idx)
  DT_sf <- DT_sf[inside_idx, , drop = FALSE]
  DT <- data.table::as.data.table(sf::st_drop_geometry(DT_sf))
  rm(DT_sf, inside_idx); gc()
  if (!nrow(DT)) stop("No rows inside the polygon. Check admin unit selection.")
  .say("Points inside polygon: %s rows kept.", .fmtI(kept_n))

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
  wide[, ws10 := sqrt(`10m_u_component_of_wind`^2 + `10m_v_component_of_wind`^2)]
  wide[, `:=`(
    t2m_C = K_to_C(`2m_temperature`),
    d2m_C = K_to_C(`2m_dewpoint_temperature`)
  )]
  wide[, RH := pmin(pmax(rh_from_T_Td(t2m_C, d2m_C), 0), 100)]
  wide[, ppt_mm := `total_precipitation` * 1000]  # m -> mm

  wide_small <- wide[, .(lon, lat, time, t2m_C, d2m_C, RH, ws10, ppt_mm)]

  # ---- hourly area aggregate ----
  .say("Aggregating to hourly area means ...")
  hourly <- wide_small[
    ,
    .(
      TM    = mean(t2m_C, na.rm = TRUE),
      HRM   = mean(RH,    na.rm = TRUE),
      VVM10 = mean(ws10,  na.rm = TRUE),
      PPT   = sum(ppt_mm, na.rm = TRUE)
    ),
    by = .(time)
  ]
  hourly[, date := as.Date(time, tz = "UTC")]
  .say("Hourly table: %s rows.", .fmtI(nrow(hourly)))

  # ---- daily summaries ----
  .say("Aggregating to daily summaries ...")
  daily <- hourly[
    ,
    .(
      meanTM    = mean(TM,    na.rm = TRUE),
      maxTX     = max(TM,     na.rm = TRUE),
      minTX     = min(TM,     na.rm = TRUE),

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
  .say("Daily table: %s rows.", .fmtI(nrow(daily)))

  # ---- MWI logic ----
  .say("Computing MWI indices (calm threshold = %.2f km/h) ...", wind_calm_kmh)
  calm_ms <- wind_calm_kmh / 3.6
  daily <- dplyr::mutate(
    daily,
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
      minTX <= 15 ~ 0,
      maxTX >  30 ~ 0,
      minTX > 15 & maxTX <= 20 ~ 0.2*meanTM - 3,
      minTX > 20 & meanTM <= 25 ~ 1,
      minTX > 25 & maxTX <= 30  ~ -0.2*meanTM + 6
    ),
    mwix = FWx*FHx*FTx,

    mwi_zero = mwi == 0,
    FH_zero  = FH  == 0,
    mwi_zeros_past_14d = RcppRoll::roll_sum(mwi_zero, n = 14, align = "right", fill = NA, na.rm = TRUE),
    FH_zeros_past_14d  = RcppRoll::roll_sum(FH_zero,  n = 14, align = "right", fill = NA, na.rm = TRUE)
  )

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
        mwi = FW*FH*FT,
        PPT = meanPPT24H
      )

    names(out)[names(out) != "date"] <- paste0(names(out)[names(out) != "date"], suffix)
    out
  }

  sel <- dplyr::select(daily, date, meanTM, meanHRM, meanVVM10, meanPPT24H)
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

  # ---- write outputs with informative names ----
  sanitize <- function(x) gsub("[^A-Za-z0-9]+", "", tolower(x))
  prefix <- paste0("weather_", tolower(iso3), "_", "lvl", admin_level, "_", sanitize(admin_name))

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

  # ---- optionally attach to .GlobalEnv ----
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

  .say("All done ✅")
  invisible(list(
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
  ))
}