
# 1. Spain ----------------------------------------------------------------

## 1.1. Get data -------------------

## Get 1 file
lugo_mfe <- fd_forest_spain_mfe50(
  province = "Lugo",
  quiet    = TRUE
)

## 1.2. Unit tests ------------------

## Check one file
test_that("Data is downloaded properly", {
  expect_equal(class(lugo_mfe)[1], "sf")
  expect_gt(nrow(lugo_mfe), 10)
})

## Check all files (~10min)
## Check only 5
test_that("All files are downloaded", {
  expect_no_error(
    purrr::map(
      metadata_forestdata$mfe_provinces[1:3],
      fd_forest_spain_mfe50
    )
  )
})

# 2. France ---------------------------------------------------------------


## Check data type is correct, and data is download
test_that("Data is downloaded properly for DB Foret v2", {
  skip_on_cran()
  ## get  data
  hautes_alpes_forest_v2 <- fd_forest_france(
    department = metadata_forestdata$bdforet_tbl_departments[5],
    quiet    = FALSE
  )
  ## test
  expect_equal(class(hautes_alpes_forest_v2)[1], "sf")
  expect_gt(nrow(hautes_alpes_forest_v2), 10)
  expect_equal(
    names(hautes_alpes_forest_v2),
    c("ID", "CODE_TFV", "TFV", "TFV_G11", "ESSENCE", "geometry")
  )
})

test_that("Data is downloaded properly for DB Foret v1", {
  ## Get 1 file from DB Foret 1
  hautes_alpes_forest_v1 <- fd_forest_france(
    department = metadata_forestdata$bdforet_tbl_departments[5],
    version   = 1,
    quiet     = TRUE
  )
  ## test
  expect_equal(class(hautes_alpes_forest_v1)[1], "sf")
  expect_gt(nrow(hautes_alpes_forest_v1), 10)
  expect_equal(
    names(hautes_alpes_forest_v1),
    c("DEP", "CYCLE", "ANREF", "TFIFN", "LIBELLE", "LIBELLE2", "TYPN", "NOM_TYPN", "geometry")
  )
})


