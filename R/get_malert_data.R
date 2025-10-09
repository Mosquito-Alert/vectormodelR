#' Download Mosquito Alert report data from GitHub or Zenodo
#'
#' @param source String. Source to download from. Options are "github" or "zenodo".
#' @param doi String. Zenodo DOI if downloading from Zenodo.
#'   Default is the DOI that will always point to the most recent version:
#'   10.5281/zenodo.597466.
#' @param clean Logical. If TRUE, the dataset is cleaned and standardized
#'   (renamed columns, formatted dates, and adds Tigacell fields). If FALSE,
#'   returns the raw downloaded data as-is. Default: FALSE.
#'
#' @returns A tibble (raw or cleaned, depending on \code{clean}).
#' @export
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tibble as_tibble
#' @importFrom lubridate year today ymd_hms
#' @importFrom magrittr %>%
#' @examples
#' # Download raw data
#' malert_reports <- get_malert_data(source = "github", clean = FALSE)
#'
#' # Download and clean data
#' malert_clean <- get_malert_data(source = "github", clean = TRUE)
get_malert_data <- function(source = "zenodo",
                            doi = "10.5281/zenodo.597466",
                            clean = FALSE) {

  this_temp_file <- tempfile()

  # --- 1) Download from GitHub or Zenodo -------------------------------------
  if (source == "github") {
    temp <- this_temp_file
    download.file(
      "https://github.com/MosquitoAlert/Data/raw/master/all_reports.zip",
      destfile = temp
    )

  } else if (source == "zenodo" && !is.na(doi)) {
    dir.create(this_temp_file, showWarnings = FALSE)
    download_zenodo(doi = doi, path = this_temp_file)
    this_file <- list.files(this_temp_file)
    this_temp_file_zip <- file.path(this_temp_file, list.files(this_temp_file))
    outer_file_name <- unzip(this_temp_file_zip, exdir = this_temp_file, list = TRUE)[1, 1]
    unzip(this_temp_file_zip, exdir = this_temp_file)
    temp <- file.path(this_temp_file, outer_file_name, "all_reports.zip")

  } else {
    stop("Error: This function currently only supports downloads from GitHub or Zenodo")
  }

  # --- 2) Load JSON for all years --------------------------------------------
  reports <- bind_rows(lapply(2014:lubridate::year(lubridate::today()), function(this_year) {
    message("Loading year: ", this_year)
    this_file <- paste0("home/webuser/webapps/tigaserver/static/all_reports", this_year, ".json")
    jsonlite::fromJSON(unz(temp, file = this_file), flatten = TRUE) %>% as_tibble()
  }))

  unlink(this_temp_file)

  # --- 3) Optional cleaning pipeline -----------------------------------------
  if (clean) {
    reports <- reports %>%
      dplyr::select(
        creation_time,
        country,
        lat,
        lon,
        site_cat,
        version_UUID,
        date = creation_date,
        movelab_certainty_category_euro_class_label = movelab_annotation_euro.class_label,
        movelab_certainty_category_euro_class_value = movelab_annotation_euro.class_value,
        movelab_certainty_category = movelab_annotation_euro.site_certainty_category,
        movelab_certainty_category_aegypti = movelab_annotation.aegypti_certainty_category,
        crowdcrafting_n_response = movelab_annotation.crowdcrafting_n_response,
        crowd_certainty_category = movelab_annotation.crowdcrafting_score_cat,
        user_tigaprob_cat = tigaprob_cat,
        site_q1_response_new = tiger_responses.q1_response,
        site_q2_response_new = tiger_responses.q2_response,
        site_q3_response_new = tiger_responses.q3_response,
        report_type = type
      ) %>%
      dplyr::mutate(
        creation_time = lubridate::ymd_hms(creation_time, tz = "UTC"),
        creation_time = format(creation_time, "%Y-%m-%d %H:%M:%S"),
        tigacell_lon = round_down(lon, 0.05),
        tigacell_lat = round_down(lat, 0.05),
        TigacellID   = make_samplingcell_ids(lon, lat, 0.05)
      )
  }

  return(reports)
}