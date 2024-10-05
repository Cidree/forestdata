
library(terra)

# 1. Sentinel 10m ---------------------------------------------------------

## 1.1. Get data ------------------

## Create polygon spanning 2 tiles
polygon_2tiles_sf <- sf::st_sfc(sf::st_point(c(-7, 42)), crs = 4326) |>
  sf::st_buffer(1000) |>
  sf::st_as_sf()

polygon_sf <- sf::st_sfc(sf::st_point(c(-8, 43)), crs = 4326) |>
  sf::st_buffer(1000) |>
  sf::st_as_sf() |>
  terra::vect()

## Download 2 tiles
ch_2tiles_sr <- fd_canopy_height(polygon_2tiles_sf)

## Download 1 tile
ch_polygon_sr <- fd_canopy_height(polygon_sf)
ch_polygon_crop_sr <- fd_canopy_height(polygon_sf, crop = TRUE)

## Download 1 tiles using coords
ch_coords_sr <- fd_canopy_height(lon = -8, lat = 43)

## Download other layers
std_coords_sr <- fd_canopy_height(lon = -8, lat = 43, layer = "std")
all_coords_sr <- fd_canopy_height(lon = -8, lat = 43, layer = "all")

## 1.2. Unit tests ------------------

## Check that data is properly downloaded (tiles are merged)
test_that("Data is properly downloaded", {
  expect_s4_class(ch_2tiles_sr, "SpatRaster")
  expect_s4_class(ch_polygon_sr, "SpatRaster")
  expect_s4_class(ch_polygon_crop_sr, "SpatRaster")
  expect_s4_class(ch_coords_sr, "SpatRaster")
  expect_s4_class(std_coords_sr, "SpatRaster")
  expect_s4_class(all_coords_sr, "SpatRaster")
})

## Check that polygon was properly cropped
test_that("Crop works", {
  expect_equal(terra::ext(polygon_sf)[1], terra::ext(ch_polygon_crop_sr)[1], tolerance = 1e-1)
  expect_equal(terra::ext(polygon_sf)[2], terra::ext(ch_polygon_crop_sr)[2], tolerance = 1e-1)
  expect_equal(terra::ext(polygon_sf)[3], terra::ext(ch_polygon_crop_sr)[3], tolerance = 1e-1)
  expect_equal(terra::ext(polygon_sf)[4], terra::ext(ch_polygon_crop_sr)[4], tolerance = 1e-1)
})

## Test that not cropping and taking coordinate of same tile is same object
test_that("Objects are the same", {
  expect_equal(terra::ext(ch_polygon_sr)[1], terra::ext(ch_coords_sr)[1], tolerance = 1e-1)
  expect_equal(terra::ext(ch_polygon_sr)[2], terra::ext(ch_coords_sr)[2], tolerance = 1e-1)
  expect_equal(terra::ext(ch_polygon_sr)[3], terra::ext(ch_coords_sr)[3], tolerance = 1e-1)
  expect_equal(terra::ext(ch_polygon_sr)[4], terra::ext(ch_coords_sr)[4], tolerance = 1e-1)
})

## Check layer argument
test_that("Layers are the same", {
  expect_equal(
    names(all_coords_sr),
    names(c(ch_coords_sr, std_coords_sr))
  )
  expect_equal(
    names(all_coords_sr),
    c("chm", "std")
  )
})

# 2. Meta 1m --------------------------------------------------------------

## 2.1. Get data ---------------------

## 2 tiles in meta
meta_2_tiles_sf <- sf::st_bbox(
  c(xmin = -3.56707,
    ymin = 36.69458,
    xmax = -3.41038,
    ymax = 36.79233)
) |>
  sf::st_as_sfc() |>
  sf::st_as_sf(crs = 4326)

## Download 1 tiles using coords
meta_coords_sr <- fd_canopy_height(lon = -8, lat = 43, model = "meta")

## Download 1 tile
meta_x_sr <- fd_canopy_height(polygon_2tiles_sf, model = "meta")

## Download 1 tile with crop
meta_crop_sr <- fd_canopy_height(polygon_2tiles_sf, model = "meta", crop = TRUE)

## Download 2 tiles with crop
meta_crop2_sf <- fd_canopy_height(
  x     = meta_2_tiles_sf,
  model = "meta",
  crop  = TRUE
)


## 2.2. Tests -----------------------

## Check that data is properly downloaded (tiles are merged)
test_that("Data is properly downloaded", {
  expect_s4_class(meta_coords_sr, "SpatRaster")
  expect_s4_class(meta_x_sr, "SpatRaster")
  expect_s4_class(meta_crop_sr, "SpatRaster")
  expect_s4_class(meta_crop2_sf, "SpatRaster")
})


## Check that polygon was properly cropped
test_that("Crop works", {
  meta_2_tiles_3857_sf <- sf::st_transform(meta_2_tiles_sf, "EPSG:3857")
  expect_equal(terra::ext(meta_2_tiles_3857_sf)[1], terra::ext(meta_crop2_sf)[1], tolerance = 1e-1)
  expect_equal(terra::ext(meta_2_tiles_3857_sf)[2], terra::ext(meta_crop2_sf)[2], tolerance = 1e-1)
  expect_equal(terra::ext(meta_2_tiles_3857_sf)[3], terra::ext(meta_crop2_sf)[3], tolerance = 1e-1)
  expect_equal(terra::ext(meta_2_tiles_3857_sf)[4], terra::ext(meta_crop2_sf)[4], tolerance = 1e-1)
})









