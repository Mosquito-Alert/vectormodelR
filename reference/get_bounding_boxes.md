# Get country bounding boxes for climate data requests

Get country bounding boxes for climate data requests

## Usage

``` r
get_bounding_boxes(scale = "medium", format = "dataframe", countries = NULL)
```

## Arguments

- scale:

  String. Map scale resolution. Options: "small", "medium", "large".
  Defaults to "medium"

- format:

  String. Output format for bounding box. Options: "dataframe",
  "cds_string", "vector". Defaults to "dataframe"

- countries:

  Vector of strings. ISO3 country codes to filter results. If NULL,
  returns all countries

## Value

A data frame with country bounding boxes, or formatted strings/vectors
depending on format parameter

## Examples

``` r
# Get all country bounding boxes
all_boxes <- get_bounding_boxes()

# Get specific countries
selected <- get_bounding_boxes(countries = c("BGD", "ESP", "USA"))

# Get CDS-formatted strings for specific countries
cds_strings <- get_bounding_boxes(countries = "BGD", format = "cds_string")
```
