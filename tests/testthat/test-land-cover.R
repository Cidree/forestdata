
# 1. Copernicus GLC -------------------------------------------------------

## 1.1. Load data ---------------

## LC for coords
copernicus_coords <- fd_landcover_copernicus(
  lon = -7,
  lat = 43
)

## LC coords several layers
copernicus_coords_2_layers <- fd_landcover_copernicus(
  lon   = -7,
  lat   = 43,
  layer = c("crops", "tree")
)

## LC coords several layers and years
copernicus_years <- fd_landcover_copernicus(
  lon   = -7,
  lat   = 43,
  year  = "all",
  layer = c("crops", "tree")
)

## Create a polygon for 1 tile
polygon_1tile_vect <- sf::st_sfc(sf::st_point(c(-7, 44)), crs = 4326) |>
  sf::st_buffer(100000) |>
  sf::st_as_sf() |>
  terra::vect()

## Create a polygon for 2 tiles
polygon_2tiles_sf <- sf::st_sfc(sf::st_point(c(-7, 44)), crs = 4326) |>
  sf::st_buffer(500000) |>
  sf::st_as_sf()

## LC polygon one layer (merging 2 tiles)
copernicus_polygon <- fd_landcover_copernicus(
  x     = polygon_2tiles_sf,
  layer = "builtup"
)

## LC polygon 2 layers (merging 2 tiles)
copernicus_polygon_2_layers <- fd_landcover_copernicus(
  x     = polygon_2tiles_sf,
  layer = c("snow", "bare")
)

## Crop
copernicus_crop <- fd_landcover_copernicus(
  x     = polygon_1tile_vect,
  layer = "grass",
  year  = 2016,
  crop  = TRUE
)


## 1.2. Unit tests --------------

## Check that data is properly downloaded (tiles are merged)
test_that("Data is properly downloaded", {
  expect_s4_class(copernicus_coords, "SpatRaster")
  expect_s4_class(copernicus_coords_2_layers, "SpatRaster")
  expect_s4_class(copernicus_years, "SpatRaster")
  expect_s4_class(copernicus_polygon, "SpatRaster")
  expect_s4_class(copernicus_polygon_2_layers, "SpatRaster")
  expect_s4_class(copernicus_crop, "SpatRaster")
})

## Check that layers are correctly downloaded
test_that("Layers are correctly downloaded and named", {

  expect_equal(
    copernicus_coords |> terra::sources() |> basename(),
    "W020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
  )

  expect_equal(
    copernicus_coords_2_layers |> terra::sources() |> basename(),
    c(
      "W020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Crops-CoverFraction-layer_EPSG-4326.tif",
      "W020N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"
    )
  )

  expect_equal(
    copernicus_years |> names(),
    c(
      paste0("crops_", 2015:2019),
      paste0("tree_", 2015:2019)
    )
  )

  expect_equal(copernicus_polygon |> names(), "builtup_2019")
  expect_equal(copernicus_polygon_2_layers |> names(), c("bare_2019", "snow_2019"))
  expect_equal(copernicus_crop |> names(), "grass_2016")


})


## Check extent when cropping
test_that("Crop works", {
  expect_equal(terra::ext(copernicus_crop)[1], terra::ext(polygon_1tile_vect)[1], tolerance = 1e-1)
  expect_equal(terra::ext(copernicus_crop)[2], terra::ext(polygon_1tile_vect)[2], tolerance = 1e-1)
  expect_equal(terra::ext(copernicus_crop)[3], terra::ext(polygon_1tile_vect)[3], tolerance = 1e-1)
  expect_equal(terra::ext(copernicus_crop)[4], terra::ext(polygon_1tile_vect)[4], tolerance = 1e-1)
})


# 2. Esri LCE -------------------------------------------------------------

## 2.1. Load data ---------------------

# Download Land Cover for UTM tile 29T year 2022
lc_base <- fd_landcover_esri("29T", year = 2020, quiet = FALSE)

## Download LC for all the years
lc_years <- fd_landcover_esri("29N", year = "all", quiet = FALSE)


## 2.2. Unit tests --------------------

## Check that data is properly downloaded
test_that("Data is properly downloaded", {
  expect_s4_class(lc_base, "SpatRaster")
  expect_s4_class(lc_years, "SpatRaster")
})

## Check that layers are the correct ones, and with correct name



