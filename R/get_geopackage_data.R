#' Retrieves geopackage integration from GADM (https://gadm.org)
#'
#' @param country_code String. The ISO country code for the desired country.
#' @param file_layer Integer. The layer of the geopackage to access. Defaults to last available layer.
#' @returns The geopackage for the specified country and layer.
#' @import sf
#' @export
#' @examples
#' # Retrieve geopackage data for Spain (ESP) and layer 4
#' get_geopackage_data(country_code = "ESP", file_layer = 4)


get_geopackage_data <- function(country_code, file_layer) {

  url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/"
  url <- paste0(url, "gadm41_", country_code, ".gpkg")
  file_path <- tempfile(fileext = ".gpkg")

  response <- httr::HEAD(url)

  if (httr::status_code(response) == 200) {
    # Download the file with a 10-minute timeout
    download.file(url, file_path, mode = "wb")
  } else {
    stop("INVALID COUNTRY CODE")
  }

  gpkg_layers <- sf::st_layers(file_path)
  num_rows <- nrow(gpkg_layers)-1

  if (file_layer > num_rows)
  {
    file_layer <- num_rows
  }

  gpkg_layer <- paste0("ADM_ADM_", file_layer)
  polygon_file <- sf::st_read(file_path, layer = gpkg_layer)


  return(polygon_file)

}