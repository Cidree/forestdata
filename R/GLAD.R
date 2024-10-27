
## GLOBAL LAND ANALYSIS & DISCOVERY

# get_glad_tiles

#'  Global Land Analysis & Discovery tiles
#'  Get an sf object with the tiles of GLAD datasets
#'
#' @return An \code{sf} object
#' @keywords internal
get_glad_tiles <- function() {
  download_url <- "https://glad.umd.edu/users/Potapov/GLCLUC2020/10d_tiles.zip"
  dir_zip      <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_unzip    <- stringr::str_remove(dir_zip, ".zip")
  fdi_download_unzip(
    download_url = download_url,
    dir_unzip    = dir_zip,
    dir_zip      = dir_unzip
  )

  ## Read tiles
  sf::read_sf(stringr::str_glue("{dir_zip}/10d_tiles.shp"))
}




#' Global Land Analysis & Discovery datasets
#'
#' Download data from GLAD database including forest extent, forest height, and
#' land cover at ~30m spatial resolution
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the necessary
#' tiles to cover the area (if \code{lat} and \code{lon} are specified, this
#' argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param model a character vector of length 1 indicating the model to retrieve (see details)
#' @param year year of the data (see details)
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#' @param mask when \code{x} is specified, whether to mask the tile(s) to the object
#'
#' @include utils-not-exported.R
#' @return \code{SpatRaster} object
#' @export
#'
#' @details
#' The Global Land Analysis & Discovery (GLAD) includes several datasets which
#' can be accessed through the \code{model} argument:
#'
#' - __landcover__: global land cover and land use dataset. Dataset divided into
#' 10ºx10º tiles containing measures of bare ground and tree height inside and
#' outside of wetlands, seasonal water percent, binary labels of built-up,
#' permanent ice/snow, and cropland. Available for the years 2000, 2005, 2010,
#' 2015, and 2020.
#'
#' - __landcover-change__: changes of landcover from 2000 to 2020. Argument
#' \code{year} is ignored.
#'
#' - __extent__: dataset showing presence of forest, defined as wildland, managed,
#' and planted tree cover including agroforestry and orchards. Includes areas where
#' the vegetation is taller than 5 meters. Available for the years 2000 and 2020.
#'
#' - __height__: dataset measuring the height of woody vegetation taller
#' than 3 meters. Available for the years 2000 and 2020.
#'
#' The spatial resolution of the product is 0.00025º (approximately 30 meters
#' at the Equator), and it's distributed in tiles of 10ºx10º.
#'
#' Note that each tile is stored as a raster file of 1.5 GB, so for
#' big extensions the function might take some time to retrieve the data.
#'
#' @references Potapov P., Hansen M.C., Pickens A., Hernandez-Serna A., Tyukavina A.,
#' Turubanova S., Zalles V., Li X., Khan A., Stolle F., Harris N., Song X.-P.,
#' Baggett A., Kommareddy I., Kommareddy A. (2022) The global 2000-2020 land cover
#' and land use change dataset derived from the Landsat archive: first results.
#' Frontiers in Remote Sensing \doi{10.3389/frsen.2022.856903}
#'
#' P. Potapov, X. Li, A. Hernandez-Serna, A. Tyukavina, M.C. Hansen, A. Kommareddy,
#' A. Pickens, S. Turubanova, H. Tang, C.E. Silva, J. Armston, R. Dubayah, J. B.
#' Blair, M. Hofton (2020) Mapping and monitoring global forest canopy height
#' through integration of GEDI and Landsat data. Remote Sensing of Environment,
#' 112165.\doi{10.1016/j.rse.2020.112165}
#'
#' @examples
#' \donttest{
#'  # Get tile for Galicia (Spain)
#'  galicia_forest_extent <- fd_forest_glad(lon = -7.8, lat = 42.7, year = 2020)
#' }
fd_forest_glad <- function(x     = NULL,
                           lon   = NULL,
                           lat   = NULL,
                           model = "extent",
                           year  = 2020,
                           crop  = FALSE,
                           mask  = FALSE,
                           quiet = FALSE) {

  # 0. Handle errors
  if (year != "all" & !year %in% seq(2000, 2020, 5)) stop("Invalid year. Please, use year 2000, 2020 or 'all'")
  if (is.na(sf::st_crs(x)) & !is.null(x)) stop("The object x is not georreferenced.")
  if (!class(x)[1] %in% c("sf", "SpatVector") & is.null(lat) & is.null(lon)) stop("Invalid x format, or lat&lon not specified.")
  ## 0.1. Handle formats
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  ## 0.2. Handle incompatible arguments (crop = TRUE & coords)
  if (crop & !is.null(lon) & !is.null(lat)) {
    stop("`crop = TRUE` is only available when `x` is specified.")
  }

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile
    user_point_sf <- sf::st_as_sf(
      x      = data.frame(lon = lon, lat = lat),
      coords = c("lon", "lat"),
      crs    = "EPSG:4326"
    )
    ## 1.2. Filter file
    tile_vec <- glad_tiles_sf |>
      sf::st_filter(user_point_sf) |>
      dplyr::pull(NAME)
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon
    xwgs84 <- sf::st_transform(x, crs = "EPSG:4326")
    ### 1.3.2 Filter tiles(s)
    tile_vec <- glad_tiles_sf |>
      sf::st_filter(xwgs84) |>
      dplyr::pull(NAME)
  }

  # 2. Create URL's
  tiles_vec <- switch(model,
    "extent"           = stringr::str_glue("https://glad.umd.edu/users/Potapov/GLCLUC2020/Forest_extent_{year}/{tile_vec}.tif"),
    "height"           = stringr::str_glue("https://glad.umd.edu/users/Potapov/GLCLUC2020/Forest_height_{year}/{year}_{tile_vec}.tif"),
    "landcover"        = stringr::str_glue("https://storage.googleapis.com/earthenginepartners-hansen/GLCLU2000-2020/v2/{year}/{tile_vec}.tif"),
    "landcover-change" = stringr::str_glue("https://storage.googleapis.com/earthenginepartners-hansen/GLCLU2000-2020/v2/2000-2020change/{tile_vec}.tif"),
    stop("Invalid model. Please, use <extent>, <height> or <landcover>")
  )

  # 3. Manage different tiles
  if (model %in% c("extent", "height") & !quiet) {
    message(stringr::str_glue("{length(tiles_vec)} tile(s) were found. A total of {length(tiles_vec)*1.5} GB of data will be read into R. This may take a while."))
  }
  ## 3.1. Read rasters
  if (length(tiles_vec) > 1) {
    ### Read tiles
    r <- purrr::map(tiles_vec, terra::rast)
    ### Crop
    if (crop) r <- purrr::map(r, \(x) terra::crop(x, xwgs84))
    if (mask) r <- purrr::map(r, \(x) terra::mask(x, xwgs84))
    ### Merge
    r <- do.call(terra::merge, r)
  } else {
    ## Read tile
    r <- terra::rast(tiles_vec)
    ### Crop
    if (crop) r <- terra::crop(r, xwgs84, mask = mask)
  }
  ## 3.2. Rename layers
  names(r) <- paste0(model, "_", year)

  # 4. Return (TODO: use right citations)
  if (!quiet) {
    switch(model,
           "extent"           = message(crayon::cyan("Cite this dataset using <https://doi.org/10.1016/j.rse.2020.112165>")),
           "height"           = message(crayon::cyan("Cite this dataset using <https://doi.org/10.1016/j.rse.2020.112165>")),
           "landcover"        = message(crayon::cyan("Cite this dataset using <https://doi.org/10.3389/frsen.2022.856903>")),
           "landcover-change" = message(crayon::cyan("Cite this dataset using <https://doi.org/10.3389/frsen.2022.856903>"))
    )
  }

  return(r)

}























# get_forest_extent_tbl

#'  Forest Extent Table
#'  Get a table with the year, coordinates, and URL of the Forest Extent files
#'  from the Global Land Analysis & Discovery (GLAD)
#'
#' @return A \code{tibble}
#' @keywords internal
get_forest_extent_tbl <- function() {
  # 1. Get possible lat/long
  ## 1.1. Read html
  url <- "https://glad.umd.edu/users/Potapov/GLCLUC2020/Forest_extent_2000/"
  forext_html <- rvest::read_html(url)
  ## 1.2. Get the href attributes
  attr_vec <- forext_html |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  ## 1.3. Filter those ending in .tif
  tif_vec <- grep(".tif$", attr_vec, value = TRUE)

  # 2. Create possible url's
  forext_tbl <- expand.grid(
    year      = c(2000, 2020),
    extension = tif_vec
  ) |>
    dplyr::mutate(
      lat = stringr::str_sub(extension, 1, 2) |> as.numeric(),
      lat = ifelse(stringr::str_detect(extension, "([0-9]{2})N"), lat, -lat),
      lon = stringr::str_sub(extension, 5, 7) |> as.numeric(),
      lon = ifelse(stringr::str_detect(extension, "([0-9]{2})E"), lon, -lon),
      url = stringr::str_glue("https://glad.umd.edu/users/Potapov/GLCLUC2020/Forest_extent_{year}/{extension}")
    ) |>
    tibble::as_tibble()

  # 3. Return object
  return(forext_tbl)
}


# fd_forest_extent_glad ----

#' Forest Extent
#'
#' `r lifecycle::badge("deprecated")`
#' This function is deprecated in favour of \link{fd_forest_glad}. Download the Forest
#' Extent raster from the Global Land Analysis & Discovery by
#' using a vectorial object or a pair of coordinates (latitude, longitude).
#'
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param year year of the forest extent data. One of 2000, 2020 or 'all'
#' @param crop when \code{x} is specified, whether to crop the tiles(s) to the object
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#' @param ... additional arguments passed to the \link[terra]{crop} function
#'
#' @include utils-not-exported.R
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
#' @references Potapov P., Hansen M.C., Pickens A., Hernandez-Serna A., Tyukavina A.,
#' Turubanova S., Zalles V., Li X., Khan A., Stolle F., Harris N., Song X.-P.,
#' Baggett A., Kommareddy I., Kommareddy A. (2022) The global 2000-2020 land cover
#' and land use change dataset derived from the Landsat archive: first results.
#' Frontiers in Remote Sensing \doi{10.3389/frsen.2022.856903}
#'
#' @examples
#' \donttest{
#'  # Get tile for Galicia (Spain)
#'  galicia_forest_extent <- fd_forest_extent_glad(lon = -7.8, lat = 42.7, year = 2020)
#' }

fd_forest_extent_glad <- function(x     = NULL,
                                  lon   = NULL,
                                  lat   = NULL,
                                  year  = 2020,
                                  crop  = FALSE,
                                  quiet = FALSE,
                                  ...) {

  # WARNING deprecated function
  lifecycle::deprecate_warn("0.2.0", "fd_forest_extent_glad()", "fd_forest_glad(model = 'extent')")

  # 0. Handle errors
  if (year != "all" & !year %in% c(2000, 2020)) stop("Invalid year. Please, use year 2000, 2020 or 'all'")
  if (is.na(sf::st_crs(x)) & !is.null(x)) stop("The object x is not georreferenced.")
  if (!class(x)[1] %in% c("sf", "SpatVector") & is.null(lat) & is.null(lon)) stop("Invalid x format, or lat&lon not specified.")
  ## 0.1. Handle formats
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  ## 0.2. Handle incompatible arguments (crop = TRUE & coords)
  if (crop & !is.null(lon) & !is.null(lat)) {
    stop("`crop = TRUE` is only available when `x` is specified.")
  }

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile coordinates
    new_lat <- ceiling(lat/10)*10
    new_lon <- floor(lon/10)*10
    ## 1.2. Filter file
    tile_tbl <- forest_extent_tbl |>
      dplyr::filter(lat == new_lat & lon == new_lon)
    ## 1.3. Avoid error in get_combined_raster
    xwgs84 <- NULL
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon and get bbox
    xwgs84 <- sf::st_transform(x, crs = "epsg:4326")
    xbbox  <- sf::st_bbox(xwgs84)
    ### 1.3.2 Get tile coordinates
    new_lat <- ceiling(xbbox[c(2,4)]/10)*10
    new_lon <- floor(xbbox[c(1,3)]/10)*10
    ### 1.3.3. Filter file
    tile_tbl <- forest_extent_tbl |>
      dplyr::filter(lat %in% new_lat & lon %in% new_lon)
  }

  # 2. Filter years
  if (year == 2000) {
    urls <- tile_tbl |>
      dplyr::filter(year == 2000)
  } else if (year == 2020) {
    urls <- tile_tbl |>
      dplyr::filter(year == 2020)
  } else {
    urls <- tile_tbl
  }

  # 3. Manage different tiles
  ## 3.1. Get one element per year
  ids <- unique(urls$year)
  ## 3.2. Get the combined rasters per year
  message(stringr::str_glue("{nrow(urls)} tile(s) were found. A total of {nrow(urls)*1.5} GB of data will be read into R. This may take a while."))
  forext_combined_sr <- purrr::map(
    ids,
    \(id) get_combined_raster(
      year_i    = id,
      url_table = urls,
      area      = xwgs84,
      crop      = crop, ...
    )
  )
  ## 3.3. Convert to SpatRaster if it's a list
  forext_sr <- terra::rast(forext_combined_sr)
  ## 3.4. Rename layers
  names(forext_sr) <- paste0("tile_", ids)

  # 4. Return
  if (!quiet) message(crayon::cyan("Cite this dataset using <https://doi.org/10.3389/frsen.2022.856903>"))
  return(forext_sr)

}











