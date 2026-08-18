# Count Mosquito Alert reports by species

Downloads Mosquito Alert report data using
[`get_malert_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_malert_data.md)
and returns the number of reports for each species.

## Usage

``` r
get_malert_species_counts(
  source = "zenodo",
  doi = "10.5281/zenodo.597466",
  iso3 = NULL,
  admin_level = NULL,
  admin_name = NULL
)
```

## Arguments

- source:

  Source to download from: `"github"` or `"zenodo"`.

- doi:

  Zenodo DOI. Defaults to the DOI for the latest version.

- iso3:

  Optional three-letter ISO country code for spatial filtering.

- admin_level:

  Optional administrative level for spatial filtering.

- admin_name:

  Optional administrative name for spatial filtering.

## Value

A tibble containing `movelab_annotation_euro.class_name` and `count`.
