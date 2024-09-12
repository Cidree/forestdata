
# Development version

## Enhancements

* `fd_inventory_spain()` now checks if metadata exists before downloading it.

* Properly name land cover instead of land use to `fd_landcover_copernicus()` and `fd_landcover_esri()` in their documentation.

* Improve documentation, fix mistake in urls to other functions.

## Bugs

* `fd_inventory_spain()` was returning metadata with the same name always. Now it will be different depending on the dataset that we download.


# Version 0.1.0

* Initial release.
