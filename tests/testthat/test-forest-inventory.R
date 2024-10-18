
# 1. Spain ----------------------------------------------------------------

## 1.1. Get data -------------------



## 1.2. Unit tests ------------------

drivers <- odbc::odbcListDrivers()
if (!"Microsoft Access Driver (*.mdb, *.accdb)" %in% drivers$name) {

  test_that("Error is returned when driver not available", {
    expect_error(fd_inventory_spain("Albacete", ifn = 4))
    expect_error(fd_inventory_spain("Albacete", ifn = 4, database = "gis"))
    expect_error(fd_inventory_spain("Albacete", ifn = 3))
    expect_error(fd_inventory_spain("Albacete", ifn = 3, database = "gis"))
  })

} else {

  ## Get 1 file for IFN4
  albacete_ifn4 <- fd_inventory_spain(
    province = "Albacete",
    ifn      = 4
  )

  albacete_ifn4_gis <- fd_inventory_spain(
    province = "Albacete",
    ifn      = 4,
    database = "gis"
  )

  ## Get 1 file for IFN3
  albacete_ifn3 <- fd_inventory_spain(
    province = "Albacete",
    ifn      = 3
  )

  albacete_ifn3_gis <- fd_inventory_spain(
    province = "Albacete",
    ifn      = 3,
    database = "gis"
  )

  ## Get 1 file for IFN2
  albacete_ifn2 <- fd_inventory_spain(
    province = "Albacete",
    ifn      = 2,
    quiet    = TRUE
  )

  albacete_ifn2_gis <- fd_inventory_spain(
    province = "Albacete",
    ifn      = 2,
    database = "gis"
  )

  test_that("File is downloaded", {
    ## IFN 4
    expect_equal(class(albacete_ifn4), "list")
    expect_equal(length(albacete_ifn4), 10)

    expect_equal(class(albacete_ifn4_gis), "list")
    expect_equal(length(albacete_ifn4_gis), 9)

    ## IFN 3
    expect_equal(class(albacete_ifn3), "list")
    expect_equal(length(albacete_ifn3), 14)

    expect_equal(class(albacete_ifn3_gis), "list")
    expect_equal(length(albacete_ifn3_gis), 11)

    ## IFN 2
    expect_equal(class(albacete_ifn2), "list")
    expect_equal(length(albacete_ifn2), 9)

    expect_equal(class(albacete_ifn2_gis)[1], "sf")

  })

}

## Check that file is downloaded
##


