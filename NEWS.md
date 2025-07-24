
# Version 0.4.0

## New features

* `fd_landcover_esri()`: models from 2024 are now available

* `fd_canopy_height(model = "amazon")`: new model from the Amazon Forest added with a spatial resolution of ~5 meters ([Publication](https://doi.org/10.48550/arXiv.2501.10600))

* `fd_forest_eu_biomass()`: new function to access aboveground biomass, belowground biomass, and growing volumne stock raster data in Europe. Visit [Forest Carbon Monitoring](https://www.forestcarbonplatform.org/) for more information.

## Deprecated

* `fd_landcover_copernicus()`: Global Land Cover now requires authentication for accessing the data, and therefore, this function will be eliminated in the next version.

## Enhancements

* `fd_forest_chorological()`: now searches for the latest version of the data more efficiently using the API.

# Version 0.3.1

## New features

* `fd_occ_euforest()`: new function to access the [EU-Forest dataset](https://doi.org/10.6084/m9.figshare.c.3288407.v1) with more than 500,000 forest tree species occurrences in Europe.

## Bugs

* `fd_inventory_spain()`: returned wrong results with `process_level = 1` or `process_level = 2` combined with `ifn = 4` for regions that contain more than 1 province inside the same `province` argument. For instance, `province = "Canarias"` includes 2 provinces (35 and 38), but previously it was treated as one.

## Enhancements

* `fd_inventory_spain()`: some plots have tree data on the field but no coordinates recorded. Before this version, this data was ignored. Now, a row with empty geometry and warning were added.

# Version 0.3.0

## New features

* `fd_forest_glad()` and `fd_canopy_height()`: gain a new argument `merge = FALSE`. If the tiles are not merged, the function will return the result much faster as a `SpatRasterCollection`.

* `fd_forest_extent_glad()`: eliminated. Functionality extended with `fd_forest_glad()`.

* `fd_inventory_spain()`: gains a new argument `process_level`, which can be:

    -   `process_level = 0`: raw data is downloaded
    
    -   `process_level = 1`: data is processed at the tree level
    
    -   `process_level = 2`: data is processed at the stand level
    

## Enhancements

* Better functions feedback with `cli` R package

* Make the package lighter eliminating useless internal functions. `purrr`, `rvest`, `rlang`, and `crayon` dependencies are not needed anymore.

## Bugs

* Fix `foresdata` url issues according to #9 and CRAN policies.


# Version 0.2.1

-   `fd_canopy_height()`: fix an error that was provoking the cached rasters to be deleted in the current session.

# Version 0.2.0

## New features

-   `fd_allometry_tallo()`: new function to retrieve data from the TALLO database (thanks to @i-c-grant in #1).

-   `fd_pathogens_defid2()`:

    -   It has now `quiet` argument, which defaults to `FALSE`.

-   `metadata_forestdata`:

    -   List of chorological species updated

    -   New list containing metadata of possible options for `fd_pathogens_defid2()`.

    -   New list containing metadata of codes for discrete and forest layers in `fd_landcover_copernicus()`.

-   `fd_landcover_esri()`: now includes land cover data for 2023.

-   `fd_canopy_height()`: gains a new argument `model` with two possible options:

    -   `model = "eth"`: default value. It returns the ETH Global Canopy Model with 10 meters of spatial resolution.

    -   `model = "meta"`: it returns the Meta Global Canopy Model with 1 meter of spatial resolution. Thanks to @Wycology in #2.

-   `fd_forest_glad()`: new function that deprecates `fd_forest_extent_glad()`. It includes a new argument `model` to choose forest extent, forest height, or land cover models from GLAD. It also fixes #7.

## Enhancements

-   Properly name land cover instead of land use to `fd_landcover_copernicus()` and `fd_landcover_esri()` in their documentation.

-   `fd_forest_eutrees4f()`: now gives an error with a better message when `distrib == "nat" & period != 2005`.

-   Improve documentation, fix mistakes in urls to other functions, add unit tests for all functions, correct wrong `fd_forest_extent_glad()` reference.

-   Remove dependency on `RODBC`, and use `DBI` and `odbc` instead in `fd_inventory_spain()`. Improve error message when data is not available.

-   `fd_canopy_height()`: now crops and then merges multiples tiles, so it's much faster when using `crop = TRUE`. It acquires argument `mask`, and losses `...` passed to `terra::crop()` because it fails to mask when used within `crop()`.

-   Now every function have the argument `quiet = FALSE` by default, and retrieves the citation of the dataset.

## Bugs

-   `fd_forest_chorological()`: fixes #5. Species *Cedrus atlantica* and *Phoenix theophrasti* are eliminated from the metadata since they have no range data. *Chamaerops humilis* is now well spelled. The function now retrieve an error message when data is not available (fixes #5).

-   `fd_inventory_spain()`

    -   It was returning metadata with the same name always. Now it will be different depending on the dataset that we download, and it will be checked if it exists before downloading it (according to #3).

    -   Some provinces in IFN4 were incorrectly georreferenced. Now they are correctly georreferenced based on the *Documentador.* IFN3 provinces are now also georreferenced

-   `fd_landcover_copernicus()` and `fd_canopy_height()` now crop data regardless of the CRS.

-   `fd_forest_extent_glad()`: incorrectly crops to the argument `x` (fixes #7).

# Version 0.1.0

-   Initial release.
