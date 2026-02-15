# Transform a geometry to its local UTM zone

Computes the centroid of the supplied geometry (after ensuring it is
expressed in WGS84), determines the corresponding UTM zone, and
reprojects the geometry to that metric CRS. Works for geometries in
either hemisphere.

## Usage

``` r
convert_to_utm(x)
```

## Arguments

- x:

  An `sf` or `sfc` object with a known CRS (usually EPSG:4326).

## Value

The input geometry transformed to the appropriate UTM CRS.

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
poly <- st_as_sfc(st_bbox(c(xmin = 2.05, xmax = 2.25,
                             ymin = 41.3, ymax = 41.5)), crs = 4326)
convert_to_utm(poly)
} # }
```
