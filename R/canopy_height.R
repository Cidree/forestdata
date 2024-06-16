
# get_gch_tbl

#'  Global Canopy Height Table
#'  Get a table with the coordinates and URL of the Global Canopy Height tiles
#'
#' @return A \code{tibble}
#' @keywords internal
#' @references <https://gee-community-catalog.org/projects/canopy/>
#'
#' @examples
#' \dontrun{
#' get_gch_tbl()
#' }
get_gch_tbl <- function() {
  # 1. Vector with possible longitudes
  lon_code <- c(
    paste0("W", sprintf("%03d", seq(180, 3, by = -3))),
    paste0("E", sprintf("%03d", seq(0, 177, by = 3)))
  )
  # 2. Vector with possible latitudes
  lat_code <- c(
    paste0("N", sprintf("%02d", seq(0, 81, by = 3))),
    paste0("S", sprintf("%02d", seq(3, 60, by = 3)))
  )
  # 3. Create the grid
  grid_urls <- tidyr::expand_grid(
    lon_code = lon_code,
    lat_code = lat_code,
    layer    = c("", "_SD")
  )
  # 4. Prepare urls
  grid_urls <- grid_urls %>%
    dplyr::mutate(
      lon = stringr::str_sub(lon_code, 2, 4) %>% as.numeric(),
      lon = ifelse(stringr::str_detect(lon_code, "E([0-9]{3})"), lon, -lon),
      lat = stringr::str_sub(lat_code, 2, 3) %>% as.numeric(),
      lat = ifelse(stringr::str_detect(lat_code, "N([0-9]{2})"), lat, -lat),
      url = paste0(
        "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_",
        lat_code,
        lon_code,
        "_Map",
        layer,
        ".tif"
      )
    )
  # 5. Return grid
  return(grid_urls)
}


# fd_canopy_height

#'  Download forest canopy height
#'  Download the ETH Global Sentinel-2 10m Canopy Height (2020)
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @param x An \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lon A number specifying the longitude of the area where we want the tile
#' @param lat A number specifying the latitude of the area where we want the tile
#'
#' @param layer A string for the layer to download. The default "\code{chm}"
#'              downloads the Canopy Height Model, while "\code{std}" downloads
#'              the standard deviation. If you want both layers, use "\code{all}"
#' @param crop When \code{x} is specified, whether to crop the tiles(s) to the
#'             object
#' @param ... additional arguments passed to the \code{terra::crop} function
#'
#' @include utils_notExported.R
#' @return A \code{SpatRaster}
#' @export
#'
#'
#' @references Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk
#'              Wegner. "A high-resolution canopy height model of the Earth."
#'              arXiv preprint arXiv:2204.08322 (2022).
#'
#' @examples
#' \dontrun{
#' fd_canopy_height(lon = -7.27, lat = 42.43)
#' }
fd_canopy_height <- function(x,
                             lon   = NULL,
                             lat   = NULL,
                             layer = "chm",
                             crop  = FALSE, ...) {

  # 0. Handle errors
  if (!is.null(lon) & !is.null(lat)) {
    if (lon > 180 | lon < -180) stop("Invalid longitude coordinate value")
    if (lat > 80 | lat < -80) stop("Invalid latitude coordinate value")
  } else {
    if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  }

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile coordinates
    new_lat <- floor(lat / 3) * 3
    new_lon <- floor(lon / 3) * 3
    ## 1.2. Filter file
    tile_tbl <- gch_tbl %>%
      dplyr::filter(lat == new_lat & lon == new_lon)
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon and get bbox
    xwgs84 <- sf::st_transform(x, crs = "epsg:4326")
    xbbox  <- sf::st_bbox(xwgs84)
    ### 1.3.2 Get bbox coordinates
    new_lon <- floor(xbbox[c(1,3)]/3) * 3
    new_lat <- floor(xbbox[c(2,4)]/3) * 3
    ### 1.3.3. Get all tiles
    new_lon <- seq(new_lon[1], new_lon[2], 3)
    new_lat <- seq(new_lat[1], new_lat[2], 3)
    ### 1.3.4. Filter file
    tile_tbl <- gch_tbl %>%
      dplyr::filter(lat %in% new_lat & lon %in% new_lon)
  }

  # 2. Rest of the filters
  ## 2.1. Filter layers
  if (layer == "chm") tile_tbl <- dplyr::filter(tile_tbl, layer == "")
  if (layer == "std") tile_tbl <- dplyr::filter(tile_tbl, layer == "_SD")

  # 3. Manage different tiles
  ## 3.1. Get URLs
  ids <- tile_tbl$url
  ## 3.2. Get the combined rasters per year
  message(stringr::str_glue("{nrow(tile_tbl)} tile(s) were found."))
  tiles_list <- purrr::map(
    .x = ids,
    .f = \(tile_url) fdi_download_raster(
      url   = tile_url,
      start = 38,
      end   = 80
    )
  )
  ## 3.3. Merge depending if there is chm, std or all
  if (layer == "all") {
    ## Number of tiles
    n_tiles <- 1:length(tiles_list)
    ## Filter even tiles (chm)
    chm_tiles <- tiles_list[[n_tiles[n_tiles %% 2 == 0]]]
    ## Filter uneven tiles (std)
    std_tiles <- tiles_list[[n_tiles[n_tiles %% 2 != 0]]]
    ## Merge them
    if (length(tiles_list) > 2) {
      chm_tiles <- do.call(terra::merge, chm_tiles)
      std_tiles <- do.call(terra::merge, std_tiles)
    }
    ## Join in SpatRaster
    ch_sr <- c(chm_tiles, std_tiles)
  } else {
    ## 3.3. Convert to SpatRaster if it's a list
    ch_sr <- do.call(terra::merge, tiles_list)
  }

  ## 3.4. Rename layers
  if (layer == "all") {
    names(ch_sr) <- c("chm", "std")
  } else {
    names(ch_sr) <- layer
  }


}



