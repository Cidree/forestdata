
# 1. Defid2 ---------------------------------------------------------------

## 1.1. Get data ------------------

## Get full DB
defid2_full <- fd_pathogens_defid2()

## Test filters
defid_filter_agent <- fd_pathogens_defid2(
  agent = metadata_forestdata$defid2_list$pathogens[50]
)

defid_filter_host <- fd_pathogens_defid2(
  host = metadata_forestdata$defid2_list$hosts[1]
)

defid_filter_country <- fd_pathogens_defid2(
  country = metadata_forestdata$defid2_list$countries[2]
)

## Test geometry
defid_pt <- fd_pathogens_defid2(
  geometry = "point",
  quiet    = FALSE
)

## 1.2. Unit tests ----------------

## Check that database is downloaded and contains data
test_that("Database is downloaded in correct format", {
  expect_equal(class(defid2_full)[1], "sf")
  expect_equal(class(defid_filter_agent)[1], "sf")
  expect_equal(class(defid_filter_host)[1], "sf")
  expect_equal(class(defid_filter_country)[1], "sf")
  expect_equal(class(defid_pt)[1], "sf")
})

test_that("Database contains data", {
  expect_gt(nrow(defid2_full), 1)
  expect_gt(nrow(defid_filter_agent), 1)
  expect_gt(nrow(defid_filter_host), 1)
  expect_gt(nrow(defid_filter_country), 1)
  expect_gt(nrow(defid_pt), 1)
})

## Check geometry type
test_that("Geometry type is correct", {

  ## Polygon
  expect_equal(
    sf::st_geometry_type(defid2_full, by_geometry = FALSE) |> as.character(),
    "MULTIPOLYGON"
  )

  ## Point
  expect_equal(
    sf::st_geometry_type(defid_pt, by_geometry = FALSE) |> as.character(),
    "POINT"
  )

})
