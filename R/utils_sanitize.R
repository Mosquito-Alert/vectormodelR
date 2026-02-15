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
