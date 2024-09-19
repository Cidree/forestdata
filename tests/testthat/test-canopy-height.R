
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
