
# 1. Forest Extent --------------------------------------------------------

## 1.1. Get data --------------------

## Get study area
ccaa_sf <- sf::st_bbox(
  c(xmin = -7.54293,
    ymin = 36.02270,
    xmax = -1.63003,
    ymax = 40.46953)
) |>
  sf::st_as_sfc() |>
  sf::st_as_sf(crs = 4326)

## Using coords (all years)
test_that("download with coords works", {
  skip_on_cran()
  ## get data
  galicia_coords <- fd_forest_glad(
    lat   = -7,
    lon   = 43,
    year  = 2020,
    model = "landcover-change"
  )
  ## check spatraster
  expect_s4_class(galicia_coords, "SpatRaster")
  ## check tiles are correct ones
  expect_equal(basename(terra::sources(galicia_coords)), "00N_040E.tif")
  expect_equal(names(galicia_coords), "landcover-change_2020")
})

## 2 tiles crop
test_that("download two tiles and crop", {
  skip_on_cran()
  ## get data
  ccaa_landcover_crop_sr <- fd_forest_glad(
    x     = ccaa_sf,
    year  = 2020,
    model = "landcover",
    crop  = TRUE,
    mask  = TRUE
  )
  ## Check extent of cropped
  expect_equal(
    as.vector(terra::ext(ccaa_landcover_crop_sr)), as.vector(terra::ext(ccaa_sf)),
    tolerance = 0.01
  )
})


## 2 tiles no crop
test_that("download two tiles without crop", {
  skip_on_cran()
  ## get data
  ccaa_landcover_sr <- fd_forest_glad(
    x     = ccaa_sf,
    year  = 2020,
    model = "landcover"
  )
  ## check spatraster
  expect_s4_class(ccaa_landcover_sr, "SpatRaster")
  ## check tiles are correct ones
  expect_equal(names(ccaa_landcover_sr), "landcover_2020")
  ## Check extent of non-cropped
  expect_equal(as.vector(terra::ext(ccaa_landcover_sr)),
               c(xmin = -10, xmax = 0, ymin = 30, ymax = 50))
})

