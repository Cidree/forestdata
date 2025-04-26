
# 1. EU-Trees4F -----------------------------------------------------------

## 1.1. Get data ------------------------

## Base with all periods
abies_alba_base <- fd_forest_eutrees4f(
  species = metadata_forestdata$eutrees4f_species[1]
)

## Download only one period
acer_campestre_1_period <- purrr::map(
  c("2005", "2035", "2065", "2065"),
  \(x) fd_forest_eutrees4f(
    species = metadata_forestdata$eutrees4f_species[2],
    period  = x,
    quiet   = TRUE
  )
)

## Scenario argument
betula_pendula_scenario <- purrr::map(
  c("rcp45", "rcp85"),
  \(x) fd_forest_eutrees4f(
    species = metadata_forestdata$eutrees4f_species[10],
    period  = "2035",
    scenario = x
  )
)

## Type and model argument
quercus_ilex_type <- purrr::map(
  c("bin", "prob", "std"),
  \(x) fd_forest_eutrees4f(
    species = "Quercus ilex",
    period  = "2095",
    model   = "sdms",
    type    = x
  )
)

## Distrib argument
tilia_cordata_distrib <- purrr::map(
  c("disp_lu", "disp", "pot"),
  \(x) fd_forest_eutrees4f(
    species = "Tilia cordata",
    period  = "2035",
    type    = "bin",
    distrib = x
  )
)

tilia_cordata_distrib_nat <-
  fd_forest_eutrees4f(
    "Abies alba",
    type = "bin",
    period = "2005",
    distrib = "nat"
  )


## 1.2. Unit tests ----------------------

## Check that all periods are downloaded properly
test_that("4 layers are downloaded", {

  if (inherits(abies_alba_base, "SpatRaster")) {

    expect_equal(terra::nlyr(abies_alba_base), 4)

    expect_equal(
      basename(terra::sources(abies_alba_base)),
      c(
        "Abies_alba_ens-clim_cur2005_bin_pot.tif",
        "Abies_alba_ens-clim_rcp45_fut2035_bin_pot.tif",
        "Abies_alba_ens-clim_rcp45_fut2065_bin_pot.tif",
        "Abies_alba_ens-clim_rcp45_fut2095_bin_pot.tif"
      )
    )

  } else {
    skip("Not a raster object, skipping test on atlas")
  }





})

## Check that only 1 layer is downloaded when 1 year specified
test_that("There is only 1 layer per raster", {

  if (inherits(acer_campestre_1_period, "SpatRaster")) {
    expect_equal(
      purrr::map_int(acer_campestre_1_period, terra::nlyr),
      rep(1, 4)
    )
  } else {
    skip("Not a raster object, skipping test on atlas")
  }


})

## Check scenario argument
test_that("Scenario argument works properly", {

  if (inherits(betula_pendula_scenario, "SpatRaster")) {
    expect_equal(
      purrr::map_chr(betula_pendula_scenario, \(x) basename(terra::sources(x))),
      c(
        "Betula_pendula_ens-clim_rcp45_fut2035_bin_pot.tif",
        "Betula_pendula_ens-clim_rcp85_fut2035_bin_pot.tif"
      )
    )
  } else {
    skip("Not a raster object, skipping test on atlas")
  }



})

## Check type and model argument
test_that("Type and model arguments work properly", {

  if (inherits(quercus_ilex_type, "SpatRaster")) {
    expect_equal(
      purrr::map_chr(quercus_ilex_type, \(x) basename(terra::sources(x))),
      c(
        "Quercus_ilex_ens-sdms_rcp45_fut2095_bin_pot.tif",
        "Quercus_ilex_ens-sdms_rcp45_fut2095_prob_pot.tif",
        "Quercus_ilex_ens-sdms_rcp45_fut2095_std_pot.tif"
      )
    )
  } else {
    skip("Not a raster object, skipping test on atlas")
  }



})

## Check distrib argument
test_that("Distrib argument behaves correctly", {

  if (inherits(tilia_cordata_distrib, "SpatRaster")) {
    expect_equal(
      purrr::map_chr(tilia_cordata_distrib, \(x) basename(terra::sources(x))),
      c(
        "Tilia_cordata_ens-clim_rcp45_fut2035_bin_disp_lu.tif",
        "Tilia_cordata_ens-clim_rcp45_fut2035_bin_disp.tif",
        "Tilia_cordata_ens-clim_rcp45_fut2035_bin_pot.tif"
      )
    )

    expect_equal(
      basename(terra::sources(tilia_cordata_distrib_nat)),
      "Abies_alba_ens-clim_cur2005_bin_nat.tif"
    )
  } else {
    skip("Not a raster object, skipping test on atlas")
  }


})

## Test errors
test_that("Errors are working properly", {

  expect_error(fd_forest_eutrees4f("Abies alba", model = "clim", type = "std"))
  expect_error(fd_forest_eutrees4f("Abies alba", type = "prob", distrib = "nat"))
  expect_error(fd_forest_eutrees4f("Abies alba", type = "std", distrib = "nat"))
  expect_error(fd_forest_eutrees4f("Abies alba", type = "std", period = 2005))
  expect_error(fd_forest_eutrees4f("Abies alba", distrib = "disp", type = "std"))
  expect_error(fd_forest_eutrees4f("Abies alba", distrib = "disp_lu", type = "std"))
  expect_error(fd_forest_eutrees4f("Abies alba", distrib = "nat", period = 2065))

})


























