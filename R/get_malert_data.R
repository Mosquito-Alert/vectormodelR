#' Download Mosquito Alert report data from GitHub
#'
#' @param source String. Source to download from. Options are github or zenodo.
#' @param doi String. Zenodo doi if downloading from Zenodo. Default is the doi that will always point to the most recent version: 10.5281/zenodo.597466.
#' @returns A tibble.
#' @export
#' @examples
#' \dontrun{
#' malert_reports = get_malert_data(source = "github")
#' malert_reports
#' }
get_malert_data = function(source = "zenodo", doi = "10.5281/zenodo.597466"){
    this_temp_file <- tempfile()
    if(source == "github"){
      temp = this_temp_file
    download.file("https://github.com/MosquitoAlert/Data/raw/master/all_reports.zip", destfile = temp)
    } else if(source == "zenodo" & !is.na(doi)){
      dir.create(this_temp_file, showWarnings = FALSE)
      download_zenodo(doi = doi, path = this_temp_file)
      this_file = list.files(this_temp_file)
      this_temp_file_zip = file.path(this_temp_file, list.files(this_temp_file))
      outer_file_name = unzip(this_temp_file_zip, exdir = this_temp_file, list = TRUE)[1,1]
      unzip(this_temp_file_zip, exdir = this_temp_file)
      temp = file.path(this_temp_file, outer_file_name, "all_reports.zip")
    } else{
  stop("Error: This function currently only supports downloads from Github or Zenodo")
}

    extract_dir <- tempfile("malert_extract_")
    dir.create(extract_dir, showWarnings = FALSE)
    unzip(temp, exdir = extract_dir)

    reports_file_list = list.files(
      extract_dir,
      recursive = TRUE,
      pattern = "all_reports[0-9]{4}\\.json$",
      full.names = TRUE
    )

    reports = bind_rows(lapply(reports_file_list, function(this_file){
      print(regmatches(basename(this_file), regexpr("[0-9]{4}", basename(this_file))))
      flush.console()
      RcppSimdJson::fload(this_file, flatten = TRUE, max_simplify_lvl = "data_frame") %>% as_tibble()
    }))

    unlink(extract_dir, recursive = TRUE)
    unlink(this_temp_file, recursive = TRUE)
    return(reports)
}
