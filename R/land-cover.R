
## GLOBAL LAND COVER  COPERNICUS ----------

# get_glc_tbl

#'  Global Land Cover Table
#'  Get a table with the year, coordinates, and URL of the Global Land Cover
#'  tiles.
#'
#' @return A \code{tibble}
#' @keywords internal
#' @references <https://lcviewer.vito.be/download>
get_glc_tbl <- function() {

  # 1. Vector with possible longitudes
  lon_code <- c(
    paste0("W", sprintf("%03d", seq(180, 20, by = -20))),
    paste0("E", sprintf("%03d", seq(0, 160, by = 20)))
  )
  # 2. Vector with possible latitudes
  lat_code <- c(
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
              "PermanentWater-CoverFraction-layer", "Forest-Type-layer", "DataDensityIndicator")
  # 5. Create the grid
  grid_urls <- expand.grid(
    lon_code  = lon_code,
    lat_code  = lat_code,
    years_url = years_url,
    layers    = layers
  )
  # 6. Prepare grid and urls
  grid_urls <- grid_urls |>
    tibble::as_tibble() |>
    dplyr::mutate(
      year = stringr::str_extract(years_url, "([0-9]{4})") |> as.numeric()
    ) |>
    dplyr::mutate(
      lonlat     = paste0(lon_code, lat_code),
      lon        = stringr::str_sub(lon_code, 2, 4) |> as.numeric(),
      lon        = ifelse(stringr::str_detect(lon_code, "E([0-9]{3})"), lon, -lon),
      lat        = stringr::str_sub(lat_code, 2, 3) |> as.numeric(),
      lat        = ifelse(stringr::str_detect(lat_code, "N([0-9]{2})"), lat, -lat),
      url        = stringr::str_glue("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/{year}/{lonlat}/{lonlat}_PROBAV_LC100_global_v3.0.1_{years_url}_{layers}_EPSG-4326.tif"),
      layer_shrt = stringr::str_split(layers, "-"),
      layer_shrt = purrr::map_chr(layer_shrt, 1) |> stringr::str_to_lower()
      )
  return(grid_urls)
}


# fd_landcover_copernicus

#' Global Land Cover
#'
#' Download a \code{SpatRaster} from the Global Land Cover from the Copernicus Global
#' Land Service.
#'
#' @param x an \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param year year of the forest extent data. One of 2015:2019 or 'all'
#' @param layer a character vector of the layer(s) to use from the Global
#'              Land Cover. See details
#' @param crop when \code{x} is specified, whether to crop the tiles(s) to the
#'             object
#' @param ... additional arguments passed to the \link[terra]{crop} function
#'
#' @return \code{SpatRaster} object
#' @export
#' @include utils-not-exported.R
#'
#'
#' @references Buchhorn, M.; Smets, B.; Bertels, L.; De Roo, B.; Lesiv, M.;
#'             Tsendbazar, N. - E.; Herold, M.; Fritz, S. Copernicus Global
#'             Land Service: Land Cover 100m: collection 3: epoch 2019: Globe
#'             2020. DOI 10.5281/zenodo.3939050
#'
#' @details
#' There are 14 different layers that can be downloaded:
#'
#' - \strong{"discrete"}: land cover discrete classification
#'
#' - \strong{"classification"}: land cover classification probability
#'
#' - \strong{"bare"}: cover fraction of bare and sparse vegetation
#'
#' - \strong{"builtup"}: cover fraction of builtup
#'
#' - \strong{"crops"}: cover fraction of cropland
#'
#' - \strong{"tree"}: cover fraction of forest
#'
#' - \strong{"grass"}: cover fraction of herbaceous vegetation
#'
#' - \strong{"mosslichen"}: cover fraction of moss and lichen
#'
#' - \strong{"seasonalwater"}: cover fraction of seasonal inland water
#'
#' - \strong{"shrub"}: cover fraction of shrubland
#'
#' - \strong{"snow"}: cover fraction of snow and ice
#'
#' - \strong{"permanentwater"}: cover fraction of permanent inland water
#'
#' - \strong{"forest"} (default): forest types. (0): unknown; (1): evergreen needle leaf forest;
#' (2): evergreen broad leaf forest; (3): deciduous needle leaf; (4): deciduous
#' broad leaf; (5): mix of forest types
#'
#' - \strong{"datadensityindicator"}: input data density
#'
#'
#' @examples
#' \donttest{
#'  # Get tile for Galicia (Spain) and year 2019
#'  galicia_forest_extent <- fd_landcover_copernicus(
#'   lat  = 42.7,
#'   lon  = -7.8,
#'   year = 2019
#'  )
#'  # Get forest and discrete classification tiles for all years
#'  galicia_forest_extent <- fd_landcover_copernicus(
#'   lat  = 42.7,
#'   lon  = -7.8,
#'   year = "all",
#'   layer = c("forest", "discrete")
#'  )
#'  }

fd_landcover_copernicus <- function(x,
                                    lon   = NULL,
                                    lat   = NULL,
                                    year  = 2019,
                                    layer = "forest",
                                    crop  = FALSE, ...) {

  # 0. Handle errors
  if (!year %in% 2015:2019 & year != "all") stop("Invalid year")
  if (!is.null(lon) & !is.null(lat)) {
    if (lon > 180 | lon < -180) stop("Invalid longitude coordinate value")
    if (lat > 80 | lat < -60) stop("Invalid latitude coordinate value")
  } else {
    if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  }
  sel_year <- year
  if (!all(layer %in% unique(get_glc_tbl()$layer_shrt))) stop("Invalid layer name(s)")

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile coordinates
    new_lat <- ceiling(lat / 20) * 20
    new_lon <- floor(lon / 20) * 20
    ## 1.2. Filter file
    tile_tbl <- glc_tbl |>
      dplyr::filter(lat == new_lat & lon == new_lon)
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon and get bbox
    xwgs84 <- sf::st_transform(x, crs = "epsg:4326")
    xbbox  <- sf::st_bbox(xwgs84)
    ### 1.3.2 Get tile coordinates
    new_lon <- floor(xbbox[c(1,3)]/20) * 20
    new_lat <- ceiling(xbbox[c(2,4)]/20) * 20
    ### 1.3.3. Filter file
    tile_tbl <- glc_tbl |>
      dplyr::filter(lat %in% new_lat & lon %in% new_lon)
  }

  # 2. Rest of the filters
  ## 2.1. Filter years
  if (year != "all") tile_tbl <- dplyr::filter(tile_tbl, year %in% sel_year)
  ## 2.2. Filter layer
  tile_tbl <- dplyr::filter(tile_tbl, layer_shrt %in% tolower(layer))

  # 3. Manage different tiles
  ## 3.1. Get one element per year and layer
  id_year <- unique(tile_tbl$year)
  id_lyr  <- unique(tile_tbl$layer_shrt)
  ids <- expand.grid(year = id_year, layer = id_lyr)
  ids$layer_names <- paste0(ids$layer, "_", ids$year)
  ## 3.2. Get the combined rasters per year
  message(stringr::str_glue("{nrow(tile_tbl)} tile(s) were found."))
  combined_sr <- purrr::map2(ids$year, ids$layer, get_combined_raster_2l, url_table = tile_tbl)
  ## 3.3. Convert to SpatRaster if it's a list
  glad_sr <- terra::rast(combined_sr)
  ## 3.4. Rename layers
  names(glad_sr) <- ids$layer_names

  # 4. Manage crop
  if (crop) glad_sr <- terra::crop(glad_sr, x, ...)

  # 5. Return
  return(glad_sr)

}



## LAND COVER EXPLORER Esri ------------


## get_landcoverexplorer_tbl

#' (Internal) Get codes table of Land Cover Explorer
#'
#' Get a table with the Land Cover explorer codes and urls
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils-not-exported.R
get_landcoverexplorer_tbl <- function() {

  # 1. Possible options
  years   <- 2017:2022
  letters <- LETTERS[-c(1, 2, 9, 15, 25:26)]
  nmbrs   <- 1:60
  nmbrs   <- sprintf("%02d", nmbrs)

  # 2. Table with options
  expand.grid(Year = years, Number = nmbrs, Letter = letters) |>
    dplyr::mutate(download_url =
                    stringr::str_glue("https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc{Year}/{Number}{Letter}_{Year}0101-{Year + 1}0101.tif")
    )
}


## fd_landcover_esri

#' Download data from the ESRI Land Cover Explorer
#'
#' Download an UTM tile of the ESRI Land Cover Explorer for a specified year
#'
#' @param utm_code a character string of length 1 with an UTM code (e.g. "29N")
#' @param year an integer or vector of integers corresponding to the base year
#'             of the Land Cover image. The option \code{year = 'all'} downloads all
#'             the available images
#' @param quiet if \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
#'
#' @return A \code{SpatRaster}
#' @export
#'
#'
#' @references \url{https://livingatlas.arcgis.com/en/home/}
#'
#' @examples
#' \donttest{
#' # Download Land Cover for UTM tile 29N year 2022
#' lc <- fd_landcover_esri("29N", year = 2020)
#'
#' # Download Land Cover for UTM time 29N for all years
#' lc <- fd_landcover_esri("29N", year = "all")
#' }
fd_landcover_esri <- function(utm_code,
                              year,
                              quiet = TRUE) {

  # 0. Handle year error
  if (!(year %in% seq(2017, 2022, 1)) & year != "all") stop("The indicated year is incorrect. Please, use a year between 2017 and 2022, or the 'all' function to retrieve all.")

  # 1. Get number and letter
  nmbr <- stringr::str_sub(utm_code, 1, 2)
  lttr <- stringr::str_sub(utm_code, 3, 3)

  # 2. Get url
  if (year == "all") {
    download_url <- landcover_explorer_tbl |>
      dplyr::filter(Number == nmbr & Letter == lttr) |>
      dplyr::pull(download_url)
  } else {
    download_url <- landcover_explorer_tbl |>
      dplyr::filter(Year %in% year & Number == nmbr & Letter == lttr) |>
      dplyr::pull(download_url)
  }

  # 3. Handle error
  if (length(download_url) == 0) stop("The UTM Code doesn't exist, is incorrect or fell into the sea. Please, use two numbers and one letter (e.g. 05T)")

  # 4. Download
  ## 4.1. Tiff file
  tif_path <- paste0(tempdir(), "/", basename(download_url))
  ## Check for user's timeout
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout))
  ## Download file
  options(timeout = 10000)
  purrr::map2(
    download_url,
    tif_path,
    \(x, y) download.file(
      url      = x,
      destfile = y,
      mode     = "wb",
      quiet    = quiet
    )
  )

  # 5. Read into R
  ## 5.1. Read file
  lc_sr <- terra::rast(tif_path)
  ## 5.2. Layer names
  lc_names <- stringr::str_sub(basename(download_url), 1, 8)
  names(lc_sr) <- lc_names
  ## 5.3. Return object
  return(lc_sr)

}

