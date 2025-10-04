#' Make a TIGA-style global tile grid (e.g., 0.035° ~ 3–4 km)
#'
#' Builds a rectangular graticule aligned to the global origin (-180, -90) with a
#' chosen resolution (default 0.035 degrees). Optionally clips to a country or
#' admin boundary (via GADM) or to a user-provided polygon. Outputs an `sf`
#' polygon layer with a stable `TigacellID` like `"1.400_41.200"` (lon_lat of cell
#' centroid, rounded to the grid precision).
#'
#' @param res_deg Numeric. Grid resolution in degrees. Default `0.035`.
#' @param bbox Numeric length-4 `c(xmin, ymin, xmax, ymax)` in lon/lat (EPSG:4326).
#'   If `NULL`, it is inferred from `clip` (if given) or from the selected country.
#' @param clip An `sf`/`sp` polygon to clip against (optional). If `NULL` and
#'   `iso3` is supplied, a GADM polygon is fetched instead.
#' @param iso3 Character ISO3 country code (e.g. `"ESP"`). Used only if `clip` is `NULL`.
#' @param gadm_level Integer GADM level (0=country, 1=region, 2=province, ...).
#'   Only used when `iso3` is provided. Default `0`.
#' @param gadm_name Character or `NULL`. If given, filter the GADM polygons by the
#'   `NAME_<level>` column to a single admin unit. If `NULL`, union all polygons at
#'   that level. Only used when `iso3` is provided.
#' @param add_cols Logical. Add helper columns (`lon_center`, `lat_center`,
#'   `area_km2`). Default `TRUE`.
#' @param id_from Character. Which coordinate to base `TigacellID` on:
#'   `"centroid"` (default) or `"lowerleft"`. Lower-left gives fully deterministic
#'   IDs even for oddly shaped cells; centroid is usually fine.
#' @param digits Integer. Decimal places for IDs; if `NULL`, inferred from `res_deg`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return An `sf` polygon layer (EPSG:4326) with columns:
#'   - `TigacellID` (character)
#'   - optionally `lon_center`, `lat_center`, `area_km2` if `add_cols=TRUE`
#'
#' @details
#' The grid is aligned to the global origin (-180, -90) and snapped so cell
#' edges fall on exact multiples of `res_deg`. This ensures reproducibility across
#' runs and AOIs. The output CRS is geographic (EPSG:4326); area is computed after
#' projecting to an equal-area CRS (EPSG:6933).
#'
#' @examples
#' \dontrun{
#' # 1) Simple Spain-wide grid at 0.035°
#' g1 <- build_tigacell_grid(iso3 = "ESP", gadm_level = 0)
#'
#' # 2) Barcelona province only (level 2)
#' g2 <- build_tigacell_grid(iso3 = "ESP", gadm_level = 2, gadm_name = "Barcelona")
#'
#' # 3) Custom bbox over Iberia with 0.05°
#' iberia_bbox <- c(-10, 35, 4, 44)  # xmin, ymin, xmax, ymax
#' g3 <- build_tigacell_grid(res_deg = 0.05, bbox = iberia_bbox)
#'
#' # 4) Use your own polygon (sf) and finer grid
#' # my_poly <- sf::st_read("my_region.geojson")
#' # g4 <- build_tigacell_grid(res_deg = 0.025, clip = my_poly)
#'
#' # Inspect
#' # plot(sf::st_geometry(g2), col = NA, border = 'grey')
#' # head(g2)
#' }
#' @export
build_tigacell_grid <- function(
  res_deg    = 0.035,
  bbox       = NULL,
  clip       = NULL,
  iso3       = NULL,
  gadm_level = 0,
  gadm_name  = NULL,
  add_cols   = TRUE,
  id_from    = c("centroid", "lowerleft"),
  digits     = NULL,
  verbose    = TRUE
) {
  stopifnot(is.numeric(res_deg), res_deg > 0)
  id_from <- match.arg(id_from)

  say <- function(...) if (isTRUE(verbose)) message(sprintf(...))

  # --- helper: decimal places from res (e.g., 0.035 -> 3) ---
  if (is.null(digits)) {
    # infer digits from res as character (robust to FP rounding)
    res_chr <- format(res_deg, scientific = FALSE, trim = TRUE)
    if (grepl("\\.", res_chr)) {
      digits <- nchar(sub("^[^.]*\\.", "", res_chr))
    } else {
      digits <- 0
    }
  }

  # --- get/normalize clip polygon if requested via ISO3 ---
  if (is.null(clip) && !is.null(iso3)) {
    if (!requireNamespace("geodata", quietly = TRUE))
      stop("Package {geodata} is required when using iso3=... (install it).")
    say("Fetching GADM for %s level %d ...", iso3, gadm_level)
    g <- geodata::gadm(country = iso3, level = gadm_level, path = tempdir())
    clip <- sf::st_as_sf(g)
    if (!is.null(gadm_name)) {
      nmcol <- paste0("NAME_", gadm_level)
      clip <- clip[clip[[nmcol]] == gadm_name, , drop = FALSE]
      if (nrow(clip) == 0)
        stop("Admin name '", gadm_name, "' not found at level ", gadm_level, " for ", iso3)
      say("Filtered to %s in %s L%d.", gadm_name, iso3, gadm_level)
    } else {
      # union all units at this level
      clip <- sf::st_union(clip)
    }
  }

  # Accept sp polygon too
  if (!is.null(clip) && inherits(clip, "Spatial")) {
    clip <- sf::st_as_sf(clip)
  }

  # --- set bbox ---
  if (is.null(bbox)) {
    if (!is.null(clip)) {
      bb <- sf::st_bbox(clip)
      bbox <- c(unname(bb["xmin"]), unname(bb["ymin"]), unname(bb["xmax"]), unname(bb["ymax"]))
    } else if (!is.null(iso3)) {
      # if iso3 but clip somehow failed, use country bbox from rnaturalearth
      if (!requireNamespace("rnaturalearth", quietly = TRUE))
        stop("Need {rnaturalearth} or provide bbox/clip.")
      country <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      row <- country[country$iso_a3 == iso3, ]
      if (nrow(row) == 0) stop("ISO3 '", iso3, "' not found in Natural Earth.")
      bb <- sf::st_bbox(row)
      bbox <- c(unname(bb["xmin"]), unname(bb["ymin"]), unname(bb["xmax"]), unname(bb["ymax"]))
    } else {
      stop("Provide one of: clip (sf/sp), iso3, or bbox.")
    }
  }
  stopifnot(is.numeric(bbox), length(bbox) == 4)
  names(bbox) <- c("xmin","ymin","xmax","ymax")

  # --- align bbox to global grid anchored at (-180, -90) ---
  snap <- function(val, origin, step, fun = floor) fun((val - origin) / step) * step + origin
  xmin <- snap(bbox["xmin"], -180, res_deg, floor)
  ymin <- snap(bbox["ymin"],  -90, res_deg, floor)
  xmax <- snap(bbox["xmax"], -180, res_deg, ceiling)
  ymax <- snap(bbox["ymax"],  -90, res_deg, ceiling)

  # make an sf bbox polygon
  bbox_sfc <- sf::st_as_sfc(sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 4326))

  # --- build grid ---
  say("Making grid at %.6f° over [%0.3f,%0.3f]x[%0.3f,%0.3f] ...", res_deg, xmin, xmax, ymin, ymax)
  gr <- sf::st_make_grid(
    bbox_sfc,
    cellsize = c(res_deg, res_deg),
    what = "polygons",
    square = TRUE
  )
  gr <- sf::st_as_sf(gr)

  # optional clip
  if (!is.null(clip)) {
    clip <- sf::st_make_valid(clip)
    clip <- sf::st_union(clip)  # robust
    say("Clipping grid to polygon ...")
    gr <- sf::st_intersection(gr, clip)
  }

  # --- add IDs and helper cols ---
  # center or lower-left corner for ID
  if (id_from == "centroid") {
    ctr <- sf::st_centroid(gr)
    coords <- sf::st_coordinates(ctr)
    lon_id <- round(coords[,1], digits)
    lat_id <- round(coords[,2], digits)
  } else {
    # lower-left from each cell's bbox
    bbs <- sf::st_bbox(gr)
    # st_bbox on a collection returns a single bbox—so compute per feature:
    bbm <- t(vapply(sf::st_geometry(gr), function(geom) {
      bb <- sf::st_bbox(geom)
      c(bb["xmin"], bb["ymin"])
    }, numeric(2)))
    lon_id <- round(bbm[,1], digits)
    lat_id <- round(bbm[,2], digits)
  }

  TigacellID <- paste0(
    format(lon_id, nsmall = digits, trim = TRUE, scientific = FALSE), "_",
    format(lat_id, nsmall = digits, trim = TRUE, scientific = FALSE)
  )

  gr$TigacellID <- TigacellID

  if (isTRUE(add_cols)) {
    # centers for convenience (even if id_from is lowerleft)
    ctr <- sf::st_centroid(gr)
    cc  <- sf::st_coordinates(ctr)
    gr$lon_center <- round(cc[,1], digits)
    gr$lat_center <- round(cc[,2], digits)

    # area in km2 (equal-area proj)
    gr_eq <- sf::st_transform(gr, 6933)  # NSIDC EASE-Grid 2.0 (global equal-area)
    gr$area_km2 <- as.numeric(sf::st_area(gr_eq)) / 1e6
  }

  sf::st_set_crs(gr, 4326)
}