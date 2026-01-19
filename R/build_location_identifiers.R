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
