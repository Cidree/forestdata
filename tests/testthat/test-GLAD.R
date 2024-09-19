
# 1. Forest Extent --------------------------------------------------------

## 1.1. Get data --------------------

## Using coords (all years)
galicia_coords_all_years <- fd_forest_extent_glad(
  lat  = -7,
  lon  = 43,
  year = "all"
)

## 1.2. Unit tests --------------------

## Check that data is properly downloaded and tiles are merged
test_that("Data is properly downloaded", {
  expect_s4_class(galicia_coords_all_years, "SpatRaster")
})

test_that("Tiles are the correct ones", {

  expect_equal(
    basename(terra::sources(galicia_coords_all_years)),
    rep("00N_040E.tif", 2)
  )

  expect_equal(
    names(galicia_coords_all_years),
    c("tile_2000", "tile_2020")
  )
})

