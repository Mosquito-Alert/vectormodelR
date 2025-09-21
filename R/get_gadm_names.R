#' Get GADM attributes (names) for a country/level
#'
#' Downloads a GADM layer via \pkg{geodata}, returns its attributes as a data.frame,
#' and (optionally) shows an interactive \pkg{DT} datatable if available.
#'
#' @param country Character scalar. Country name as accepted by \code{geodata::gadm()} (e.g., "Spain").
#' @param level Integer. Administrative level (0 = country, 1 = region, 2 = province, 3+ finer).
#' @param path Character. Directory where GADM files will be cached/downloaded.
#' @param view One of \code{"datatable"} or \code{"table"}.
#'   If \code{"datatable"}, tries to render an interactive table using \pkg{DT};
#'   falls back to a regular data.frame when \pkg{DT} is not installed.
#' @param search Optional character string. If provided, filters rows where ANY NAME_* column
#'   contains this pattern (case-insensitive).
#' @param names_only Logical. If \code{TRUE}, keep only common name/id columns
#'   (e.g., \code{GID_*}, \code{NAME_*}, \code{TYPE_*}, \code{ENGTYPE_*}).
#' @param sort Logical. If \code{TRUE}, attempts to sort by the first \code{NAME_*} column.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{data}: \code{data.frame} of attributes (possibly filtered/trimmed).
#'   \item \code{spat}: the \code{terra::SpatVector} (for spatial subsetting/plotting).
#' }
#' The function also *prints* an interactive table when \code{view = "datatable"} and \pkg{DT} is available.
#'
#' @examples
#' \dontrun{
#'   # Provinces of Spain (level 2) as an interactive table
#'   res <- explore_gadm_names(country = "Spain", level = 2)
#'   # Access the data.frame or SpatVector:
#'   head(res$data)
#'   terra::plot(res$spat)
#'
#'   # Municipalities, only name/id columns, filtered for "Barc"
#'   res <- explore_gadm_names("Spain", level = 3, names_only = TRUE, search = "Barc")
#'
#'   # Plain data.frame view (no DT widget)
#'   res <- explore_gadm_names("Spain", level = 2, view = "table")
#' }
#'
#' @export
#' @importFrom geodata gadm
#' @importFrom terra vect
get_gadm_names <- function(country = "Spain",
                               level = 2,
                               path = "data/",
                               view = c("datatable", "table"),
                               search = NULL,
                               names_only = FALSE,
                               sort = TRUE) {
  view <- match.arg(view)

  # --- download/load GADM (SpatVector) ---
  gadm_obj <- geodata::gadm(country = country, level = level, path = path)

  # --- attributes as data.frame ---
  df <- as.data.frame(gadm_obj)

  # Keep only common name/id cols if requested
  if (isTRUE(names_only)) {
    keep_pat <- "^(GID_|CC_|NAME_|TYPE_|ENGTYPE_)"
    keep_cols <- grep(keep_pat, names(df), value = TRUE)
    if (length(keep_cols) > 0) df <- df[, keep_cols, drop = FALSE]
  }

  # Optional search across NAME_* columns
  if (!is.null(search) && nzchar(search)) {
    name_cols <- grep("^NAME_", names(df), value = TRUE)
    if (length(name_cols) > 0) {
      hits <- Reduce(`|`, lapply(name_cols, function(col) {
        grepl(search, df[[col]], ignore.case = TRUE)
      }))
      df <- df[hits %in% TRUE, , drop = FALSE]
    }
  }

  # Optional sort by first NAME_* column
  if (isTRUE(sort)) {
    name_cols <- grep("^NAME_", names(df), value = TRUE)
    if (length(name_cols) > 0) {
      ord <- order(tolower(as.character(df[[name_cols[1]]])), na.last = TRUE)
      df <- df[ord, , drop = FALSE]
    }
  }

  # Render datatable if requested and DT is available
  if (identical(view, "datatable")) {
    if (requireNamespace("DT", quietly = TRUE)) {
      DT::datatable(
        df,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
    } else {
      message("Package 'DT' not installed; returning a regular data.frame instead.")
      print(utils::head(df, 10))
    }
  }

  # Return both the data and the SpatVector for downstream use
  return(list(data = df, spat = gadm_obj))
}