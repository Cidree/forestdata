
# 1. TALLO database -------------------------------------------------------

## Get data
tallo_tbl <- fd_allometry_tallo()

## Check formats
test_that("Database is downloaded in correct format", {
  expect_equal(class(tallo_tbl)[1], "tbl_df")
})

## Data is downloaded
test_that("Data is downloaded", {
  expect_gt(nrow(tallo_tbl), 1)
})



