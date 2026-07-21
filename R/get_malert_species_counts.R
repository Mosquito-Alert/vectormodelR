#' Count Mosquito Alert reports by species
#'
#' Downloads Mosquito Alert report data using [get_malert_data()] and returns
#' the number of reports for each species.
#'
#' @param source Source to download from: `"github"` or `"zenodo"`.
#' @param doi Zenodo DOI. Defaults to the DOI for the latest version.
#' @param iso3 Optional three-letter ISO country code for spatial filtering.
#' @param admin_level Optional administrative level for spatial filtering.
#' @param admin_name Optional administrative name for spatial filtering.
#'
#' @return A tibble containing
#'   `movelab_annotation_euro.class_name` and `count`.
#'
#' @export
get_malert_species_counts <- function(
    source = "zenodo",
    doi = "10.5281/zenodo.597466",
    iso3 = NULL,
    admin_level = NULL,
    admin_name = NULL
) {
    #Okay, so I was lazy and hardcoded this. But at some point, I need to make this more flexible. Some day... Eventually...
  species_col <- "movelab_annotation_euro.class_name"

  reports <- vectormodelR::get_malert_data(
    source = source,
    doi = doi,
    iso3 = iso3,
    admin_level = admin_level,
    admin_name = admin_name
  )

  species_counts <- reports |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(species_col)
      )
    ) |>
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "drop"
    )
  
  species_counts[
    order(
      species_counts$count,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]
}
