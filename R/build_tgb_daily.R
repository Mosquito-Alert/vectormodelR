#' Build GBIF target-group background data at original locations
#'
#' Loads GBIF target-group records, retains their original coordinates, and
#' aggregates duplicate records by location and time period. Spatial grid IDs
#' are assigned later by the model preparation function.
#'
#' @param iso3 Three-letter ISO3 country code.
#' @param admin_level Administrative level identifier.
#' @param admin_name Administrative unit name.
#' @param vector_dir Directory containing `vector_<slug>_gbif.Rds`.
#' @param data_dir Directory where the output should be written.
#' @param weight_col Name of the target-group weight column.
#' @param time_bin Either `"day"` or `"year"`.
#' @param write_output Whether to write the output RDS file.
#' @param overwrite Whether to replace an existing output file.
#'
#' @return An `sf` point object containing `lon`, `lat`, `date`, `year`, and
#'   the target-group weight column.
#' @export
build_tgb_daily <- function(
    iso3,
    admin_level,
    admin_name,
    vector_dir = "data/proc",
    data_dir = "data/proc",
    weight_col = "tgb_w",
    time_bin = c("day", "year"),
    write_output = TRUE,
    overwrite = FALSE
) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(
      "Package 'dplyr' is required.",
      call. = FALSE
    )
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required.",
      call. = FALSE
    )
  }
  
  time_bin <- match.arg(time_bin)
  
  ids <- build_location_identifiers(
    iso3,
    admin_level,
    admin_name
  )
  
  slug <- ids$slug
  
  gbif_path <- file.path(
    vector_dir,
    sprintf("vector_%s_gbif.Rds", slug)
  )
  
  if (!file.exists(gbif_path)) {
    stop(
      "GBIF vector file not found at ",
      gbif_path,
      call. = FALSE
    )
  }
  
  message("Loading GBIF occurrences from: ", gbif_path)
  
  gbif_data <- readRDS(gbif_path)
  
  if (!nrow(gbif_data)) {
    stop(
      "The GBIF dataset contains no rows.",
      call. = FALSE
    )
  }
  
  find_column <- function(data, candidates) {
    column_names <- names(data)
    
    matches <- match(
      tolower(candidates),
      tolower(column_names),
      nomatch = 0L
    )
    
    matches <- matches[matches > 0L]
    
    if (!length(matches)) {
      return(NA_character_)
    }
    
    column_names[matches[1L]]
  }
  
  lon_col <- find_column(
    gbif_data,
    c("decimal_longitude", "decimalLongitude", "longitude", "lon")
  )
  
  lat_col <- find_column(
    gbif_data,
    c("decimal_latitude", "decimalLatitude", "latitude", "lat")
  )
  
  date_col <- find_column(
    gbif_data,
    c("event_date", "eventDate", "date")
  )
  
  year_col <- find_column(
    gbif_data,
    "year"
  )
  
  if (is.na(lon_col) || is.na(lat_col)) {
    stop(
      "GBIF data must contain longitude and latitude columns.",
      call. = FALSE
    )
  }
  
  if (is.na(date_col) && is.na(year_col)) {
    stop(
      "GBIF data must contain a date or year column.",
      call. = FALSE
    )
  }
  
  gbif_data$lon <- suppressWarnings(
    as.numeric(gbif_data[[lon_col]])
  )
  
  gbif_data$lat <- suppressWarnings(
    as.numeric(gbif_data[[lat_col]])
  )
  
  if (!is.na(date_col)) {
    if (inherits(gbif_data[[date_col]], "Date")) {
      gbif_data$date <- as.Date(gbif_data[[date_col]])
    } else {
      date_text <- substr(
        as.character(gbif_data[[date_col]]),
        1L,
        10L
      )
      
      gbif_data$date <- suppressWarnings(
        as.Date(date_text)
      )
    }
  } else {
    gbif_data$date <- as.Date(NA)
  }
  
  if (!is.na(year_col)) {
    gbif_data$year <- suppressWarnings(
      as.integer(gbif_data[[year_col]])
    )
  } else {
    gbif_data$year <- NA_integer_
  }
  
  missing_year <- is.na(gbif_data$year) &
    !is.na(gbif_data$date)
  
  gbif_data$year[missing_year] <- as.integer(
    format(gbif_data$date[missing_year], "%Y")
  )
  
  if (time_bin == "year") {
    gbif_data$date <- as.Date(
      paste0(gbif_data$year, "-01-01")
    )
  } else {
    missing_date <- is.na(gbif_data$date) &
      !is.na(gbif_data$year)
    
    gbif_data$date[missing_date] <- as.Date(
      paste0(gbif_data$year[missing_date], "-01-01")
    )
  }
  
  gbif_data <- gbif_data[
    !is.na(gbif_data$lon) &
      !is.na(gbif_data$lat) &
      !is.na(gbif_data$date) &
      !is.na(gbif_data$year),
    ,
    drop = FALSE
  ]
  
  if (!nrow(gbif_data)) {
    stop(
      "No complete GBIF records remained after preparation.",
      call. = FALSE
    )
  }
  
  tgb_data <- dplyr::count(
    gbif_data,
    lon,
    lat,
    date,
    year,
    name = weight_col
  )
  
  tgb_data <- sf::st_as_sf(
    tgb_data,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  output_path <- file.path(
    data_dir,
    sprintf("model_prep_%s_tgb_daily.Rds", slug)
  )
  
  if (isTRUE(write_output)) {
    if (file.exists(output_path) && !isTRUE(overwrite)) {
      stop(
        "Output already exists at ",
        output_path,
        ". Set `overwrite = TRUE` to replace it.",
        call. = FALSE
      )
    }
    
    dir.create(
      dirname(output_path),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    saveRDS(tgb_data, output_path)
    message("Target-group background saved to: ", output_path)
  }
  
  attr(tgb_data, "output_path") <- output_path
  attr(tgb_data, "gbif_source") <- gbif_path
  
  tgb_data
}