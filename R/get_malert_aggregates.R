#' Aggregates mosquito alert report data by country or city
#'
#' @param aggregate_type String. The type of aggregation to perform. Options are "country" or "city".
#' @param filter_year String. The year(s) to filter the data. Options include a single year (e.g., "2014"),
#'   a comma-separated list of years (e.g., "2014,2015"), or a range (e.g., "2014-2016"). Defaults to NULL (all years).
#' @param country_code String. The ISO country code (required if aggregate_type is "city").
#' @param file_layer Integer. The layer of the shapefile/geopackage to access (for city aggregation).
#' @returns A data frame containing the aggregated mosquito alert report counts.
#' @import sf
#' @import dplyr
#' @import stringr
#' @import httr
#' @export
#' @examples
#' # Aggregate mosquito reports by country for 2015, 2016 and 2018
#' get_malert_aggregates(aggregate_type = "country", filter_year = "2015,2016,2018")
#'
#' # Aggregate mosquito reports by city for Spain in 2014-2024
#' get_malert_aggregates(aggregate_type = "city", filter_year = "2014-2024", country_code = "ESP", file_layer = 2)


get_malert_aggregates <- function(aggregate_type, filter_year, country_code, file_layer) {

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

if(aggregate_type == "country")
{

  aggregated_data <- malerts_reports_github %>%
    group_by(country) %>%
    summarise(count = n()) %>%
  arrange(desc(count))

} else if (aggregate_type == "city")
{

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

  file_ext <- tools::file_ext(file_path)

  # Test if the file is a shapefile (.shp) or a GPKG file (.gpkg)
  if (file_ext == "shp") {

    last_digit <- str_extract(file_path, "_\\d+\\.") %>%
      str_extract("\\d+")

    if(last_digit != file_layer)
    {
      file_layer <- last_digit
    }

    file_layer <- paste0("NAME_", file_layer)
    polygon_file <- st_read(file_path)
  } else if (file_ext == "gpkg") {
    gpkg_layers <- st_layers(file_path)
    num_rows <- nrow(gpkg_layers)-1

    if (file_layer>num_rows)
    {
      file_layer<-num_rows
    }


    gpkg_layer <- paste0("ADM_ADM_", file_layer)
    polygon_file <- st_read(file_path, layer = gpkg_layer)
  } else {
    return("Unknown file type.")
  }

  malerts_reports_github <- st_as_sf(malerts_reports_github,
                                     coords = c("lon", "lat"),
                                     crs = 4326)

  malerts_reports_github <- st_join(malerts_reports_github,polygon_file)
  file_layer <- paste0("NAME_", file_layer)
  aggregated_data <- malerts_reports_github %>%
    group_by_at(vars(file_layer)) %>%
    summarise(count = n()) %>%
  arrange(desc(count))

}

return(aggregated_data)

}



