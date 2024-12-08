
# 1. TALLO database -------------------------------------------------------

## Get data
tallo_tbl <- fd_allometry_tallo()
tallo_sf  <- fd_allometry_tallo(spatial = TRUE, quiet = TRUE)

## Check formats
test_that("Database is downloaded in correct format", {
  expect_equal(class(tallo_tbl)[1], "tbl_df")
  expect_equal(class(tallo_sf)[1], "sf")
})

## Data is downloaded
test_that("Data is downloaded", {
  expect_gt(nrow(tallo_tbl), 1)
  expect_gt(nrow(tallo_sf), 1)
})



