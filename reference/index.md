# Package index

## All functions

- [`add_elevation_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_elevation_features.md)
  : Add elevation to Mosquito Alert model inputs
- [`add_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_features.md)
  : Sequentially enrich Mosquito Alert model-preparation datasets
- [`add_hex_grid()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_hex_grid.md)
  : Attach hex-grid identifiers to Mosquito Alert model inputs
- [`add_landcover_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_landcover_features.md)
  : Add land-cover codes and classes to Mosquito Alert model inputs
- [`add_ndvi_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_ndvi_features.md)
  : Add high-NDVI proximity metrics to Mosquito Alert model inputs
- [`add_popdensity_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_popdensity_features.md)
  : Add population density to Mosquito Alert model inputs
- [`add_pseudoabsences_se()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_pseudoabsences_se.md)
  : Generate pseudoabsences using TRS effort (Mosquito Alert) and TGB
  (GBIF)
- [`add_weather_features()`](https://labs.mosquitoalert.com/mosquitoR/reference/add_weather_features.md)
  : Add ERA5 weather features to Mosquito Alert model inputs
- [`build_grid_adjacency()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_grid_adjacency.md)
  : Build adjacency matrix for a grid (global-safe)
- [`build_spatial_grid()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_spatial_grid.md)
  : Build a regular grid for a supplied polygon
- [`build_tgb_daily()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_tgb_daily.md)
  : Build GBIF target-group background daily weights on the TRS/Tigacell
  geometry
- [`build_tigacell_grid()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_tigacell_grid.md)
  : Build a TIGA-like grid over a GADM admin unit
- [`build_trs_daily()`](https://labs.mosquitoalert.com/mosquitoR/reference/build_trs_daily.md)
  : Build TRS daily sampling effort surface for a location
- [`compile_era5_data_v2()`](https://labs.mosquitoalert.com/mosquitoR/reference/compile_era5_data_v2.md)
  : Compile ERA5 monthly CSVs from GRIB files (terra-only)
- [`compile_era5_monthly()`](https://labs.mosquitoalert.com/mosquitoR/reference/compile_era5_monthly.md)
  : Compile ERA5 monthly CSVs from GRIB/NetCDF files
- [`convert_to_utm()`](https://labs.mosquitoalert.com/mosquitoR/reference/convert_to_utm.md)
  : Transform a geometry to its local UTM zone
- [`decimal_places()`](https://labs.mosquitoalert.com/mosquitoR/reference/decimal_places.md)
  : Return the number of decimal places in a given value
- [`download_zenodo()`](https://labs.mosquitoalert.com/mosquitoR/reference/download_zenodo.md)
  : Get data from a Zenodo archive
- [`get_adm_perimeter()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_adm_perimeter.md)
  : Compute ADM polygon perimeters using local UTM projections
- [`get_bounding_boxes()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_bounding_boxes.md)
  : Get country bounding boxes for climate data requests
- [`get_elevation_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_elevation_data.md)
  : Get ~30 m SRTM DEM for a GADM admin unit or full country
- [`get_era5_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_era5_data.md)
  : Download ERA5 climate data from the Copernicus Climate Data Store
- [`get_gadm_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gadm_data.md)
  : Retrieve GADM polygons using geodata::gadm()
- [`get_gadm_names()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gadm_names.md)
  : Get GADM attributes for a country/level
- [`get_gbif_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_gbif_data.md)
  : Download GBIF occurrence data for a taxon and country
- [`get_geopackage_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_geopackage_data.md)
  : Retrieves geopackage integration from GADM (https://gadm.org)
- [`get_malert_aggregates()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_malert_aggregates.md)
  : Aggregates mosquito alert report data by country or city
- [`get_malert_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_malert_data.md)
  : Download Mosquito Alert report data from GitHub or Zenodo
- [`get_malert_geopackage_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_malert_geopackage_data.md)
  : Retrieves mosquito alert reports with geopackage integration
- [`get_senscape_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_senscape_data.md)
  : Download data from Senscape server using http get request.
- [`get_senscape_devices()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_senscape_devices.md)
  : Download device information from Senscape server using http get
  request.
- [`get_vector_counts()`](https://labs.mosquitoalert.com/mosquitoR/reference/get_vector_counts.md)
  : Aggregate Mosquito Alert + GBIF occurrence counts by GADM
  administrative level
- [`initialize_ma_dataset()`](https://labs.mosquitoalert.com/mosquitoR/reference/initialize_ma_dataset.md)
  : Prepare Mosquito Alert reports for modelling
- [`initialize_vector_dataset()`](https://labs.mosquitoalert.com/mosquitoR/reference/initialize_vector_dataset.md)
  : Build a unified vector presence dataset
- [`interpret_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/interpret_brms_model.md)
  : Generate a diagnostic report for a fitted brms model
- [`invstd2()`](https://labs.mosquitoalert.com/mosquitoR/reference/invstd2.md)
  : Inverse double-standardize a variable
- [`make_aggregated_time_series()`](https://labs.mosquitoalert.com/mosquitoR/reference/make_aggregated_time_series.md)
  : Make complete time series aggregated by a given interval
- [`make_lonlat_from_samplingcell_ids()`](https://labs.mosquitoalert.com/mosquitoR/reference/make_lonlat_from_samplingcell_ids.md)
  : Extract longitudes or latitudes from sampling cell IDs.
- [`make_mask()`](https://labs.mosquitoalert.com/mosquitoR/reference/make_mask.md)
  : Mask a number by rounding it down to the nearest mask value. Used by
  'make_sampling_cells' for transforming exact locations into masked
  sampling cells. This is basically the same as the round_down function
  but it returns a character instead of a numeric.
- [`make_metadata_template()`](https://labs.mosquitoalert.com/mosquitoR/reference/make_metadata_template.md)
  : Make metadata template ready for data portal
- [`make_samplingcell_ids()`](https://labs.mosquitoalert.com/mosquitoR/reference/make_samplingcell_ids.md)
  : Creates standard sampling cell IDs by masking a set of longitude and
  latitude values.
- [`not_covered_by()`](https://labs.mosquitoalert.com/mosquitoR/reference/not_covered_by.md)
  : Simple predicate function for sf to filter all objects that are not
  covered by another one
- [`prepare_bym2_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/prepare_bym2_data.md)
  : Prepare data and adjacency matrix for BYM2 modelling
- [`process_era5_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_era5_data.md)
  : Build daily & lagged weather features from processed ERA5 CSVs
- [`process_landcover_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_landcover_data.md)
  : Process land-cover raster by clipping to a GADM boundary
- [`process_ndvi_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_ndvi_data.md)
  : Process NDVI raster by clipping to an administrative boundary
- [`process_popdensity_data()`](https://labs.mosquitoalert.com/mosquitoR/reference/process_popdensity_data.md)
  : Process population raster by clipping to a GADM boundary and
  computing density
- [`round_down()`](https://labs.mosquitoalert.com/mosquitoR/reference/round_down.md)
  : Round a given number x downward to the nearest n.
- [`run_brms_bym2_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_bym2_model.md)
  : Fit a BYM2 Mosquito Alert occupancy model with brms
- [`run_brms_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_brms_model.md)
  : Fit a baseline Mosquito Alert occupancy model with brms
- [`run_maxent_model()`](https://labs.mosquitoalert.com/mosquitoR/reference/run_maxent_model.md)
  : Fit a baseline Mosquito Alert MaxEnt model (maxnet)
- [`std2()`](https://labs.mosquitoalert.com/mosquitoR/reference/std2.md)
  : Center and double-standardize a variable
