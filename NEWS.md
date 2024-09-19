# Development version

## New features

-   `fd_allometry_tallo()`: new function to retrieve data from the TALLO database (thanks to @i-c-grant in #1).

-   `fd_pathogens_defid2()`:

    -   It has now `quiet` argument, which defaults to `TRUE`.

-   `metadata_forestdata`:

    -   List of chorological species updated

    -   New list containing metadata of possible options for `fd_pathogens_defid2()`.

-   `fd_landcover_esri()`: now includes land cover data for 2023.

## Enhancements

-   Properly name land cover instead of land use to `fd_landcover_copernicus()` and `fd_landcover_esri()` in their documentation.

-   `fd_forest_eutrees4f()`: now gives an error with a better message when `distrib == "nat" & period != 2005`

-   Improve documentation, fix mistakes in urls to other functions, add unit tests for all functions, correct wrong `fd_forest_extent_glad()` reference

## Bugs

-   `fd_forest_chorological()`: fixes #5. Species *Cedrus atlantica* and *Phoenix theophrasti* are eliminated from the metadata since they have no range data. *Chamaerops humilis* is now well spelled. The function now retrieve an error message when data is not available (fixes #5).

-   `fd_inventory_spain()` was returning metadata with the same name always. Now it will be different depending on the dataset that we download, and it will be checked if it exists before downloading it (according to #3).

# Version 0.1.0

-   Initial release.
