#' Build daily & lagged weather features from processed ERA5 CSVs
#'
#' Reads monthly CSV.GZ files created by compile_era5_data_v2 from the appropriate
#' dataset-specific subdirectory, clips by a GADM admin polygon, aggregates
#' to hourly area means, derives daily summaries and rolling-window features,
#' and saves RDS outputs with informative names.
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
#'   `daily`, `lags_7d`, `lags_14d`, `lags_30d`, `lags_21d_lag7`, `ppt_lags`, and
#'   `paths`. For `aggregation_unit = "hourly"`, returns a list with `hourly` and `paths`.
#'
#' @details
#' ERA5-Land columns are renamed after `dcast()` to match `era5_single_level`,
#' so downstream calculations use one standard variable schema.
#'
#' Temperature and dewpoint values are treated as Kelvin and converted to Celsius
#' with `x - 273.15`. This is intentional: in some GRIBs, `terra` may label the
#' layers as `[C]`, but the values can still be Kelvin, e.g. 272–295.
#'
#' Requires helper objects:
#' - `era5_single_level`
#' - `era5_land_to_single_level`
#'
#' @importFrom lubridate parse_date_time floor_date ceiling_date
#' @importFrom RcppRoll roll_mean roll_sum
#' @importFrom sf st_as_sf st_make_valid st_union st_bbox st_transform st_buffer st_intersects
#' @importFrom geodata gadm
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom dplyr mutate case_when transmute arrange lag select all_of group_by ungroup
#' @importFrom readr write_rds
#' @importFrom data.table fread setDT setnames as.data.table rbindlist dcast copy setorder set
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

  # ERA5 temperature variables are handled as Kelvin.
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

  g <- geodata::gadm(
    country = iso3_upper,
    level = admin_level,
    path = file.path(out_dir, "gadm")
  ) |>
    sf::st_as_sf()

  if (!is.null(admin_name)) {
    nmcol <- paste0("NAME_", admin_level)
    .say("Filtering admin unit by name column %s == '%s' ...", nmcol, admin_name)

    g <- g[g[[nmcol]] == admin_name, , drop = FALSE]

    if (nrow(g) == 0) {
      stop("Admin name '", admin_name, "' not found at level ", admin_level, " for ", iso3_upper)
    }
  } else {
    .say("No admin_name provided; using union of all geometries at level %d.", admin_level)
    g <- sf::st_union(g)
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

  keep_range <- DT$time >= as.POSIXct(start_date, tz = "UTC") &
    DT$time < as.POSIXct(end_date + 1, tz = "UTC")

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

  ppt_vals <- wide[["total_precipitation"]] * 1000
  data.table::set(wide, j = "ppt_mm", value = ppt_vals)

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

        meanPPT24H = sum(PPT, na.rm = TRUE)
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
          TRUE         ~ (meanHRM / 55) - (40 / 55)
        ),
        FT  = dplyr::case_when(
          meanTM <= 15 ~ 0,
          meanTM >  30 ~ 0,
          meanTM > 15 & meanTM <= 20 ~ 0.2 * meanTM - 3,
          meanTM > 20 & meanTM <= 25 ~ 1,
          meanTM > 25 & meanTM <= 30 ~ -0.2 * meanTM + 6
        ),
        mwi = FW * FH * FT,

        FWx = as.integer(maxVVX10 <= calm_ms),
        FHx = dplyr::case_when(
          minHRM < 40 ~ 0,
          maxHRM > 95 ~ 0,
          TRUE        ~ (meanHRM / 55) - (40 / 55)
        ),
        FTx = dplyr::case_when(
          maxTM <= 15 ~ 0,
          maxTM >  30 ~ 0,
          maxTM > 15 & maxTM <= 20 ~ 0.2 * meanTM - 3,
          maxTM > 20 & meanTM <= 25 ~ 1,
          maxTM > 25 & maxTM <= 30  ~ -0.2 * meanTM + 6
        ),
        mwix = FWx * FHx * FTx,

        mwi_zero = mwi == 0,
        FH_zero  = FH  == 0,
        mwi_zeros_past_14d = RcppRoll::roll_sum(
          mwi_zero,
          n = 14,
          align = "right",
          fill = NA,
          na.rm = TRUE
        ),
        FH_zeros_past_14d = RcppRoll::roll_sum(
          FH_zero,
          n = 14,
          align = "right",
          fill = NA,
          na.rm = TRUE
        )
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
        dplyr::mutate(
          roll = RcppRoll::roll_mean(
            val,
            n = n,
            align = "right",
            fill = NA,
            na.rm = TRUE
          )
        ) |>
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
            TRUE         ~ (meanHRM / 55) - (40 / 55)
          ),
          FT  = dplyr::case_when(
            meanTM <= 15 ~ 0,
            meanTM >  30 ~ 0,
            meanTM > 15 & meanTM <= 20 ~ 0.2 * meanTM - 3,
            meanTM > 20 & meanTM <= 25 ~ 1,
            meanTM > 25 & meanTM <= 30 ~ -0.2 * meanTM + 6
          ),
          minTM = minTM,
          maxTM = maxTM,
          mwi = FW * FH * FT,
          PPT = meanPPT24H
        )

      names(out)[names(out) != "date"] <- paste0(names(out)[names(out) != "date"], suffix)
      out
    }

    sel_cols <- c("date", "meanTM", "maxTM", "minTM", "meanHRM", "meanVVM10", "meanPPT24H")

    if (identical(aggregation_unit, "region")) {
      sel <- dplyr::select(daily, dplyr::all_of(sel_cols))

      lags_7d  <- mk_roll(sel,  7, "7")
      lags_14d <- mk_roll(sel, 14, "14")
      lags_30d <- mk_roll(sel, 30, "30")

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

      lags_7d  <- build_lag(7,  "7")
      lags_14d <- build_lag(14, "14")
      lags_30d <- build_lag(30, "30")

      lags_21d_lag7 <- build_lag(21, "21")

      if (nrow(lags_21d_lag7)) {
        data.table::set(
          lags_21d_lag7,
          j = "date",
          value = lags_21d_lag7[["date"]] + 7
        )
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
      paste(admin_tokens, collapse = "-"),
      "_",
      dataset_token
    )

    prefix <- if (identical(aggregation_unit, "cell")) {
      paste0(base_prefix, "_cell")
    } else {
      paste0(base_prefix, "_region")
    }

    p_daily        <- file.path(out_dir, paste0(prefix, "_daily.Rds"))
    p_lags_7       <- file.path(out_dir, paste0(prefix, "_lags_7d.Rds"))
    p_lags_14      <- file.path(out_dir, paste0(prefix, "_lags_14d.Rds"))
    p_lags_30      <- file.path(out_dir, paste0(prefix, "_lags_30d.Rds"))
    p_lags_21_lag7 <- file.path(out_dir, paste0(prefix, "_lags_21d_lag7.Rds"))
    p_ppt_lags     <- file.path(out_dir, paste0(prefix, "_ppt_lags.Rds"))

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

      assign(paste0(prefix, "_daily"),         daily,         envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_7d"),       lags_7d,       envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_14d"),      lags_14d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_30d"),      lags_30d,      envir = .GlobalEnv)
      assign(paste0(prefix, "_lags_21d_lag7"), lags_21d_lag7, envir = .GlobalEnv)
      assign(paste0(prefix, "_ppt_lags"),      ppt_lags,      envir = .GlobalEnv)

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

    dataset_token <- dataset_subdir

    base_prefix <- paste0(
      "weather_",
      iso_fragment,
      "_",
      admin_level,
      "_",
      paste(admin_tokens, collapse = "-"),
      "_",
      dataset_token
    )

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