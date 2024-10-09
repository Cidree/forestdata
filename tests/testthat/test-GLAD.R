
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
galicia_coords <- fd_forest_glad(
  lat   = -7,
  lon   = 43,
  year  = 2020,
  model = "landcover-change"
)

## 2 tiles crop
ccaa_landcover_crop_sr <- fd_forest_glad(
  x     = ccaa_sf,
  year  = 2020,
  model = "landcover",
  crop  = TRUE,
  mask  = TRUE
)

## 2 tiles no crop
ccaa_landcover_sr <- fd_forest_glad(
  x     = ccaa_sf,
  year  = 2020,
  model = "landcover"
)

## 1.2. Unit tests --------------------

## Check that data is properly downloaded and tiles are merged
test_that("Data is properly downloaded", {
  expect_s4_class(galicia_coords, "SpatRaster")
  expect_s4_class(ccaa_landcover_sr, "SpatRaster")
})

test_that("Tiles are the correct ones", {

  expect_equal(basename(terra::sources(galicia_coords)), "00N_040E.tif")

  expect_equal(names(galicia_coords), "landcover-change_2020")
  expect_equal(names(ccaa_landcover_sr), "landcover_2020")
})

test_that("Raster extents are correct", {

  ## Check extent of non-cropped
  expect_equal(as.vector(terra::ext(ccaa_landcover_sr)),
               c(xmin = -10, xmax = 0, ymin = 30, ymax = 50))

  ## Check extent of cropped
  expect_equal(
    as.vector(terra::ext(ccaa_landcover_crop_sr)), as.vector(terra::ext(ccaa_sf)),
    tolerance = 0.01
  )


})
