#' Get GADM attributes for a country/level
#'
#' Downloads a GADM layer via \pkg{geodata}, returns its attributes as either
#' a regular data.frame or an interactive \pkg{DT} datatable (if available).
#'
#' @param country Character scalar. Country name as accepted by \code{geodata::gadm()} (e.g. "Spain").
#' @param level Integer. Administrative level (0 = country, 1 = region, 2 = province, 3 = municipality, etc.).
#' @param path Character. Directory where GADM files will be cached/downloaded.
#' @param view Either \code{"datatable"} (default, requires \pkg{DT}) or \code{"table"} for a plain data.frame.
#'
#' @return A \code{data.frame} of GADM attributes, or an interactive datatable
#'   object if \code{view = "datatable"} and \pkg{DT} is installed.
#'
#' @examples
#' \dontrun{
#'   # Provinces of Spain as interactive table
#'   explore_gadm_names("Spain", level = 2)
#'
#'   # Municipalities of Spain as a regular data.frame
#'   df <- explore_gadm_names("Spain", level = 3, view = "table")
#'   head(df)
#' }
#'
#' @export
#' @importFrom geodata gadm
#' @importFrom terra as.data.frame
get_gadm_names <- function(country = "Spain",
                               level = 2,
                               path = "data/gadm",
                               view = c("datatable", "table")) {
  view <- match.arg(view)

  gadm_obj <- geodata::gadm(country, level = level, path = path)
  gadm_df  <- as.data.frame(gadm_obj)

  if (view == "datatable") {
    if (requireNamespace("DT", quietly = TRUE)) {
      return(
        DT::datatable(
          gadm_df,
          options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
          filter = "top",
          rownames = FALSE
        )
      )
    } else {
      message("Package 'DT' not installed; returning a regular data.frame instead.")
      return(gadm_df)
    }
  } else {
    return(gadm_df)
  }
}
