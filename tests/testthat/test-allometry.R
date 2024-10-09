
# 1. TALLO database -------------------------------------------------------

## Get data
tallo_tbl        <- fd_allometry_tallo()
tallo_sf         <- fd_allometry_tallo(spatial = TRUE)
tallo_country_sf <- fd_allometry_tallo(country = "Spain")
tallo_iso2_sf    <- fd_allometry_tallo(country = "ES")
tallo_iso3_sf    <- fd_allometry_tallo(country = "ESP", quiet = TRUE)
tallo_2countries <- fd_allometry_tallo(country = c("ES", "PO"))

## Check formats
test_that("Database is downloaded in correct format", {
  expect_equal(class(tallo_tbl)[1], "tbl_df")
  expect_equal(class(tallo_sf)[1], "sf")
  expect_equal(class(tallo_country_sf)[1], "sf")
  expect_equal(class(tallo_iso2_sf)[1], "sf")
  expect_equal(class(tallo_iso3_sf)[1], "sf")
  expect_equal(class(tallo_2countries)[1], "sf")
})

## Data is downloaded
test_that("Data is downloaded", {
  expect_gt(nrow(tallo_tbl), 1)
  expect_gt(nrow(tallo_sf), 1)
  expect_gt(nrow(tallo_country_sf), 1)
  expect_gt(nrow(tallo_iso2_sf), 1)
  expect_gt(nrow(tallo_iso3_sf), 1)
  expect_gt(nrow(tallo_2countries), 1)
})

## ISO2, ISO3 and country works properly
test_that("Data is the same", {
  expect_equal(tallo_country_sf, tallo_iso2_sf)
  expect_equal(tallo_iso3_sf, tallo_iso2_sf)
})


