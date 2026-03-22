#' Sanitize strings for use in file names or slugs
#'
#' Converts input strings to lowercase ASCII, replaces non-alphanumeric
#' characters with a configurable separator, collapses repeated separators,
#' and trims separators from the ends. Empty or missing inputs are dropped
#' from the output.
#'
#' @param x Character vector of values to sanitize.
#' @param separator Single character string used to replace disallowed
#'   characters. Defaults to "-".
#' @param max_length Optional positive integer limiting the number of
#'   characters in the returned slug. When `NULL` (default) no truncation is
#'   applied.
#' @return A character vector of sanitized slug strings with empty results
#'   removed.
#' @keywords internal
#' @noRd
sanitize_slug <- function(x, separator = "-", max_length = NULL) {
  if (missing(x) || is.null(x)) {
    return(character())
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  if (length(separator) != 1L || !nzchar(separator)) {
    stop("`separator` must be a single non-empty character string.", call. = FALSE)
  }

  x[is.na(x)] <- ""
  x <- trimws(x)

  transliterated <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  transliterated[is.na(transliterated)] <- ""
  transliterated <- gsub("['`\"]", "", transliterated)

  escape_regex <- function(y) gsub("([][{}()+*^$|\\.?\\\\])", "\\\\\\1", y)

  slugs <- tolower(transliterated)
  pattern <- "[^a-z0-9]+"
  slugs <- gsub(pattern, separator, slugs, perl = TRUE)
  sep_re <- escape_regex(separator)
  slugs <- gsub(paste0("(", sep_re, ")+"), separator, slugs, perl = TRUE)
  slugs <- gsub(paste0("^", sep_re), "", slugs, perl = TRUE)
  slugs <- gsub(paste0(sep_re, "$"), "", slugs, perl = TRUE)

  if (!is.null(max_length)) {
    if (!is.numeric(max_length) || length(max_length) != 1L || max_length <= 0) {
      stop("`max_length` must be a positive numeric scalar when supplied.", call. = FALSE)
    }
    slugs <- substr(slugs, 1L, max_length)
  }

  slugs[nzchar(slugs)]
}


#' Build reusable identifiers for administrative inputs
#'
#' Normalises ISO3, administrative level, and administrative name values into a
#' consistent lowercase slug that can be reused for constructing filenames.
#'
#' @param iso3 Three-letter ISO3 code.
#' @param admin_level Administrative level (numeric or character).
#' @param admin_name Administrative unit name.
#' @return A list with normalised `iso3`, `admin_level`, `admin_name`, and
#'   combined `slug` values.
#' @keywords internal
#' @noRd
build_location_identifiers <- function(iso3, admin_level, admin_name) {
  if (length(iso3) != 1 || nchar(iso3) != 3) {
    stop("`iso3` must be a single three-character ISO3 code.", call. = FALSE)
  }

  iso3_slug <- tolower(iso3)
  admin_level_slug <- as.character(admin_level)
  admin_name_slug <- sanitize_slug(admin_name, separator = "_")
  if (!length(admin_name_slug)) {
    admin_name_slug <- ""
  }

  list(
    iso3 = iso3_slug,
    admin_level = admin_level_slug,
    admin_name = admin_name_slug,
    slug = paste(iso3_slug, admin_level_slug, admin_name_slug, sep = "_")
  )
}
