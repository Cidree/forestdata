
## GLOBAL LAND COVER DATA ----------

# get_glc_tbl

#'  Global Land Cover Table
#'  Get a table with the year, coordinates, and URL of the Global Land Cover
#'  tiles.
#'
#' @return A \code{tibble}
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_glc_tbl()
#' }

get_glc_tbl <- function() {

  # 1. Vector with possible longitudes
  lon <- c(
    paste0("W", sprintf("%03d", seq(180, 20, by = -20))),
    paste0("E", sprintf("%03d", seq(0, 160, by = 20)))
  )
  # 2. Vector with possible latitudes
  lat <- c(
    paste0("N", sprintf("%02d", seq(0, 80, by = 20))),
    paste0("S", sprintf("%02d", seq(20, 40, by = 20)))
  )
  # 3. Vector with possible years
  years_url <- c("2015-base", "2016-conso", "2017-conso", "2018-conso", "2019-nrt")
  # 4. Vector with possible land covers
  layers <- c("Discrete-Classification-map", "Classification-proba",
              "Bare-CoverFraction-layer", "BuiltUp-CoverFraction-layer", "Crops-CoverFraction-layer",
              "Tree-CoverFraction-layer", "Grass-CoverFraction-layer", "MossLichen-CoverFraction-layer",
              "SeasonalWater-CoverFraction-layer", "Shrub-CoverFraction-layer", "Snow-CoverFraction-layer",
              "PermanentWater-CoverFraction-layer", "Forest-Type", "DataDensityIndicator")
  # 5. Create the grid
  grid_urls <- expand.grid(
    lon = lon,
    lat = lat,
    years_url = years_url,
    layers = layers
  )
  # 6. Prepare grid and urls
  grid_urls <- grid_urls %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      year = stringr::str_extract(years_url, "([0-9]{4})")
    ) %>%
    dplyr::mutate(
      lonlat = paste0(lon, lat),
      url = stringr::str_glue("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/{year}/{lonlat}/{lonlat}_PROBAV_LC100_global_v3.0.1_{years_url}_{layers}_EPSG-4326.tif")
    )
  return(grid_urls)
}


# fd_landcover_copernicus ----

#' Download data form the Global Land Cover
#'
#' Download a SpatRater from the Global Land Cover from the Copernicus Global
#' Land Service.
#'
#'
#' @param x An \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lat A number specifying the latitude of the area where we want the tile
#' @param lon A number specifying the longitude of the area where we want the tile
#' @param year Year of the forest extent data. One of 2000, 2020 or 'all'
#' @param crop When \code{x} is specified, whether to crop the tiles(s) to the
#'             object
#' @param ... additional arguments passed to the \code{terra::crop} function
#'
#' @return \code{SpatRaster} object
#' @export
#'
#' @details
#' The Forest Extent Map is a product offered by the Global Land Analysis &
#' Discovery organization. The spatial resolution of the product is 0.00025º
#' (approximately 30 meters at the Equator), and it's distributed in tiles of
#' 10ºx10º. Pixels with forest height > 5 meters are classified as the forest class.
#'
#' Note that each tile is stored as a raster file of 1.5 GB, so for
#' big extensions the function might take some time to retrieve the data.
#'
#' @references P. Potapov, X. Li, A. Hernandez-Serna, A. Tyukavina, M.C. Hansen,
#'             A. Kommareddy, A. Pickens, S. Turubanova, H. Tang, C.E. Silva,
#'             J. Armston, R. Dubayah, J. B. Blair, M. Hofton (2020) Mapping
#'             and monitoring global forest canopy height through integration
#'             of GEDI and Landsat data. Remote Sensing of Environment,
#'             112165. https://doi.org/10.1016/j.rse.2020.112165
#'
#' @examples
#' \dontrun{
#'  # Get tile for Galicia (Spain)
#'  galicia_forest_extent <- fd_landcover_copernicus(lat = 42.7, lon = -7.8, year = 2020)
#'  # Get masked tile for Galicia (Spain)
#'  galicia_forest_extent <- fd_landcover_copernicus(
#'  lat  = 42.7,
#'  lon  = -7.8,
#'  year = 2020,
#'  crop = TRUE,
#'  mask = TRUE)
#' }

fd_landcover_copernicus <- function(x,
                                    lon = NULL,
                                    lat = NULL,
                                    year = 2019,
                                    layer = NULL) {

  # 0. Handle errors
  print(x)



}
