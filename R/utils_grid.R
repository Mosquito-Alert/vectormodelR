#' Round a given number x downward to the nearest n.
#'
#' @param x The number to be rounded.
#' @param n The number to be rounded.
#' @returns The numeric result of the downward rounding.
#' @noRd
#' @examples
#' round_down(4.569, 0.05)
round_down = function(x, n) round( floor( (x*1000)/ (n*1000))*n, decimal_places(n))


#' Creates standard sampling cell IDs by masking a set of longitude and latitude values.
#'
#' @param lon A vector of longitudes to be masked
#' @param lat A vector of latitudes to be masked
#' @param mask The masking value.
#' @returns A character vector of sampling cell IDs.
#' @noRd
#' @examples
#' make_samplingcell_ids(lon=c(2.1686, 2.1032), lat=c(41.3874, 41.2098), 0.05)
make_samplingcell_ids = function(lon, lat, mask=0.05){
  masked_lon = round_down(as.numeric(lon), mask)
  masked_lat = round_down(as.numeric(lat), mask)
  return(paste(masked_lon, masked_lat, sep="_"))
}

#' Return the number of decimal places in a given value
#'
#' @param x The value for which the number of decimal places will be returned.
#' @returns An integer representing the number of decimal places in x.
#' @noRd
#' @examples
#' decimal_places(4.56)
decimal_places <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#' Resolve the grid identifier column
#'
#' Converts a numeric hex-grid cell size or an H3 specification into the
#' corresponding grid identifier column name.
#'
#' @param cellsize A positive numeric hex-grid cell size in metres, or an H3
#'   specification such as `"h3_9"`.
#'
#' @return A character string containing the grid identifier column name.
#' @noRd
#'
#' @examples
#' resolve_grid_col(800)
#' resolve_grid_col("h3_9")
resolve_grid_col <- function(cellsize) {
  is_h3 <- (
    is.character(cellsize) &&
      length(cellsize) == 1L &&
      grepl("^h3_[0-9]+$", tolower(cellsize))
  )

  if (is_h3) {
    resolution <- as.integer(
      sub("^h3_", "", tolower(cellsize))
    )

    if (!resolution %in% 0:15) {
      stop(
        "H3 resolution must be between 0 and 15.",
        call. = FALSE
      )
    }

    return(
      paste0("h3_id_", resolution)
    )
  }

  if (
    !is.numeric(cellsize) ||
      length(cellsize) != 1L ||
      is.na(cellsize) ||
      cellsize <= 0
  ) {
    stop(
      "`cellsize` must be a positive number or an H3 value such as `h3_9`.",
      call. = FALSE
    )
  }

  cellsize_token <- gsub(
    "\\.",
    "_",
    format(
      cellsize,
      trim = TRUE,
      scientific = FALSE
    )
  )

  paste0("grid_id_", cellsize_token)
}