# Development version

## New features

-   `fd_allometry_tallo()`: new function to retrieve data from the TALLO database (thanks to @i-c-grant in #1).

-   `fd_pathogens_defid2()`:

    -   It has now `quiet` argument, which defaults to `TRUE`.

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

-   `fd_inventory_spain()` was returning metadata with the same name always. Now it will be different depending on the dataset that we download, and it will be checked if it exists before downloading it (according to #3).

-   `fd_landcover_copernicus()` and `fd_canopy_height()` now crop data regardless of the CRS.

-   `fd_forest_extent_glad()`: incorrectly crops to the argument `x` (fixes #7).

# Version 0.1.0

-   Initial release.
