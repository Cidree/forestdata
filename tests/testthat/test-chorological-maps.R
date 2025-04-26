
# 1. Chronological maps -----------------------------------------------------

## 1.1. Get data --------------------

cs_syn <- fd_forest_chorological(
  species = "Castanea sativa",
  range   = "syn"
)

cs_nat <- fd_forest_chorological(
  species = "Castanea sativa",
  range   = "nat",
  quiet   = TRUE
)

## 1.2. Unit tests ------------------

test_that("All natural range data is downloaded", {
  expect_no_error(
    purrr::map(
      metadata_forestdata$chorological_species[1:3],
      fd_forest_chorological
    )
  )
})

test_that("Synanthropic range data is downloaded", {
  expect_no_error(
    fd_forest_chorological(
      species = "Castanea sativa",
      range   = "syn"
    )
  )
})

test_that("Polygons are downloaded, not points", {

  if (inherits(cs_nat, "sf")) {
    expect_in(
      sf::st_geometry_type(cs_nat, by_geometry = FALSE) |> as.character(),
      c("POLYGON", "MULTIPOLYGON")
    )

    expect_in(
      sf::st_geometry_type(cs_syn, by_geometry = FALSE) |> as.character(),
      c("POLYGON", "MULTIPOLYGON")
    )
  } else {
    skip("Not a sf object, skipping test on atlas")
  }


})
