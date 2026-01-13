 #' Get country bounding boxes for climate data requests
#'
#' @param scale String. Map scale resolution. Options: "small", "medium", "large". Defaults to "medium"
#' @param format String. Output format for bounding box. Options: "dataframe", "cds_string", "vector". Defaults to "dataframe"
#' @param countries Vector of strings. ISO3 country codes to filter results. If NULL, returns all countries
#' @returns A data frame with country bounding boxes, or formatted strings/vectors depending on format parameter
#' @import sf
#' @import rnaturalearth
#' @import rnaturalearthdata
#' @export
#' @examples
#' # Get all country bounding boxes
#' all_boxes <- get_bounding_boxes()
#'
#' # Get specific countries
#' selected <- get_bounding_boxes(countries = c("BGD", "ESP", "USA"))
#'
#' # Get CDS-formatted strings for specific countries
#' cds_strings <- get_bounding_boxes(countries = "BGD", format = "cds_string")

get_bounding_boxes <- function(scale = "medium",
                               format = "dataframe",
                               countries = NULL) {

  # Suppress R CMD check notes for dplyr variables
  iso3 <- name <- north <- west <- south <- east <- cds_area <- bbox <- NULL
  iso_a3 <- name_long <- geometry <- NULL

  # Validate inputs
  if (!scale %in% c("small", "medium", "large")) {
    stop("scale must be one of: 'small', 'medium', 'large'")
  }

  if (!format %in% c("dataframe", "cds_string", "vector")) {
    stop("format must be one of: 'dataframe', 'cds_string', 'vector'")
  }

  # 1) Get country polygons (WGS84 lon/lat)
  world <- rnaturalearth::ne_countries(scale = scale, returnclass = "sf")

  # 2) Compute per-country bbox and convert to CDS area string (N/W/S/E)
  bboxes <- world %>%
    dplyr::transmute(
      iso3 = iso_a3,
      name = name_long,
      bbox = purrr::map(geometry, sf::st_bbox)
    ) %>%
    dplyr::mutate(
      west  = purrr::map_dbl(bbox, ~ .x["xmin"]),   # lon min
      south = purrr::map_dbl(bbox, ~ .x["ymin"]),   # lat min
      east  = purrr::map_dbl(bbox, ~ .x["xmax"]),   # lon max
      north = purrr::map_dbl(bbox, ~ .x["ymax"]),   # lat max
      cds_area = sprintf("%.6f/%.6f/%.6f/%.6f", north, west, south, east)  # N/W/S/E
    ) %>%
    dplyr::select(iso3, name, north, west, south, east, cds_area)

  # Filter by countries if specified
  if (!is.null(countries)) {
    bboxes <- bboxes %>% dplyr::filter(iso3 %in% countries)

    if (nrow(bboxes) == 0) {
      warning("No countries found matching the provided ISO3 codes")
      return(NULL)
    }
  }

  # Return in requested format
  switch(format,
    "dataframe" = bboxes,
    "cds_string" = bboxes$cds_area,
    "vector" = {
      # Return as named list of vectors c(north, west, south, east)
      if (nrow(bboxes) == 0) return(list())
      result <- purrr::map(seq_len(nrow(bboxes)), function(i) {
        c(north = bboxes$north[i],
          west = bboxes$west[i],
          south = bboxes$south[i],
          east = bboxes$east[i])
      })
      names(result) <- bboxes$iso3
      result
    }
  )
}
