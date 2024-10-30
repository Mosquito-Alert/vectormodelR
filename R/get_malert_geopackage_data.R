#' Generates mosquito alert reports with geopackage
#'
#' @param filter_year String. The year(s) you would like to filter for. Default is all. "2014,2"
#' @param country_code String. Country you would like to select
#' @param file_layer String. the layer of the geopackage to access
#' @returns The aggregated data.
#' @import sf
#' @import dplyr
#' @import stringr
#' @import httr
#' @export
#' @examples
#' malert_reports = get_malert_data(source = "github")
#' malert_reports


get_malert_geopackage_data <- function(filter_year, country_code, file_layer) {

  malerts_reports_github = get_malert_data(source = "github")

  # Handle multiple years or year range
  if (!is.null(filter_year)) {

    if (grepl("-", filter_year)) { # Check if it's a range (e.g., "2011-2015")
      years <- as.numeric(unlist(strsplit(filter_year, "-")))
      filter_year <- seq(years[1], years[2])

    } else if (grepl(",", filter_year)) { # Check if it's a comma-separated list (e.g., "2021,2024,2023,2022")
      filter_year <- as.numeric(unlist(strsplit(filter_year, ",")))
    } else {
      filter_year <- as.numeric(filter_year) # Single year
    }

    malerts_reports_github <- malerts_reports_github %>%
      filter(creation_year %in% filter_year)
  }

    url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/"
    url <- paste0(url, "gadm41_",country_code, ".gpkg")
    file_path <- tempfile(fileext = ".gpkg")

    response <- httr::HEAD(url)

    if (status_code(response) == 200) {
      # Download the file
      download.file(url, file_path, mode = "wb")
    } else {
      print("INVALID COUNTRY CODE")
    }

  malerts_reports_github <- malerts_reports_github %>%
    filter(country == country_code)


  gpkg_layers <- st_layers(file_path)
  num_rows <- nrow(gpkg_layers)-1

  if (file_layer>num_rows)
  {
    file_layer<-num_rows
  }

  gpkg_layer <- paste0("ADM_ADM_", file_layer)
  polygon_file <- st_read(file_path, layer = gpkg_layer)


  malerts_reports_github <- st_as_sf(malerts_reports_github,
                                       coords = c("lon", "lat"),
                                       crs = 4326)

  malert_geopackage_data <- st_join(malerts_reports_github,polygon_file)

  return(malert_geopackage_data)

}



