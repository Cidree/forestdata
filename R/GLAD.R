
## FOREST EXTENT DATA ----------

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
#' Download the Forest Extent raster from the Global Land Analysis & Discovery by
#' using a vectorial object or a pair of coordinates (latitude, longitude).
#'
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile

#' @param year year of the forest extent data. One of 2000, 2020 or 'all'
#' @param crop when \code{x} is specified, whether to crop the tiles(s) to the
#'             object
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
#' Frontiers in Remote Sensing \url{https://doi.org/10.3389/frsen.2022.856903}
#'
#' @examples
#' \donttest{
#'  # Get tile for Galicia (Spain)
#'  galicia_forest_extent <- fd_forest_extent_glad(lon = -7.8, lat = 42.7, year = 2020)
#' }

fd_forest_extent_glad <- function(x    = NULL,
                                  lon  = NULL,
                                  lat  = NULL,
                                  year = 2020,
                                  crop = FALSE, ...) {

  # 0. Handle errors
  if (year != "all" & !year %in% c(2000, 2020)) stop("Invalid year. Please, use year 2000, 2020 or 'all'")
  if (is.na(sf::st_crs(x)) & !is.null(x)) stop("The object x is not georreferenced.")
  if (!class(x) %in% c("sf", "SpatVector") & is.null(lat) & is.null(lon)) stop("Invalid x format, or lat&lon not specified.")
  ## 0.1. Handle formats
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile coordinates
    new_lat <- ceiling(lat/10)*10
    new_lon <- floor(lon/10)*10
    ## 1.2. Filter file
    tile_tbl <- forest_extent_tbl |>
      dplyr::filter(lat == new_lat & lon == new_lon)
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon and get bbox
    xwgs84 <- sf::st_transform(x, crs = "epsg:4326")
    xbbox  <- sf::st_bbox(xwgs84)
    ### 1.3.2 Get tile coordinates
    new_lat <- ceiling(xbbox[c(1,3)]/10)*10
    new_lon <- floor(xbbox[c(2,4)]/10)*10
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
  forext_combined_sr <- purrr::map(ids, get_combined_raster, url_table = urls)
  ## 3.3. Convert to SpatRaster if it's a list
  forext_sr <- terra::rast(forext_combined_sr)
  ## 3.4. Rename layers
  names(forext_sr) <- paste0("tile_", ids)

  # 4. Manage crop
  if (crop) forext_sr <- crop(forext_sr, x, ...)

  # 5. Return
  return(forext_sr)

}











