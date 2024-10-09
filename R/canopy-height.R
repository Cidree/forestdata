
# get_gch_tbl

#'  Global Canopy Height Table
#'  Get a table with the coordinates and URL of the Global Canopy Height tiles
#'
#' @return A \code{tibble}
#' @keywords internal
#' @references <https://gee-community-catalog.org/projects/canopy/>
#' <https://www.research-collection.ethz.ch/handle/20.500.11850/609802>
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
  grid_urls <- grid_urls |>
    dplyr::mutate(
      lon = stringr::str_sub(lon_code, 2, 4) |> as.numeric(),
      lon = ifelse(stringr::str_detect(lon_code, "E([0-9]{3})"), lon, -lon),
      lat = stringr::str_sub(lat_code, 2, 3) |> as.numeric(),
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

# get_meta_tiles

#'  Meta Global Canopy Height tiles
#'  Get a vectorial object with the tiles of the dataset
#'
#' @return An \code{sf} object
#' @keywords internal
#' @references <https://registry.opendata.aws/dataforgood-fb-forests/>
get_meta_tiles <- function() {
  # 0. Handle suggested package
  if (!requireNamespace("aws.s3", quietly = TRUE)) stop("Package `aws.s3` is required to access the Canopy Height Maps. Please, install it.")

  # 1. Get data
  ## 1.1. Specify the bucket and the prefix
  bucket <- "dataforgood-fb-data"
  ## 1.2. Download a specific file
  s3_file <- "forests/v1/alsgedi_global_v6_float/tiles.geojson"
  ## 1.3. Save the file in tempdir
  out_file <- tempfile(fileext = ".geojson")
  aws.s3::save_object(
    object = s3_file,
    bucket = bucket,
    file   = out_file,
    region = "us-east-1"
  )
  # 2. Read and return object
  sf::read_sf(out_file)
}


# fd_canopy_height_eth

#'  Forest Canopy Height
#'
#'  Download the ETH Global Sentinel-2 10m Canopy Height (2020)
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#'
#' @param layer a string for the layer to download. The default "\code{chm}"
#'              downloads the Canopy Height Model, while "\code{std}" downloads
#'              the standard deviation. If you want both layers, use "\code{all}"
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param mask when \code{x} is specified, whether to mask the tile(s) to the object
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @return A \code{SpatRaster}
#' @keywords internal
#' @export
#'
#' @details
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk
#'              Wegner. "A high-resolution canopy height model of the Earth."
#'              arXiv preprint arXiv:2204.08322 (2022).
#'
#' @examples
#' \donttest{
#' fd_canopy_height_eth(lon = -7.27, lat = 42.43)
#' }
fd_canopy_height_eth <- function(x     = NULL,
                                 lon   = NULL,
                                 lat   = NULL,
                                 layer = "chm",
                                 crop  = FALSE,
                                 mask  = FALSE,
                                 quiet = FALSE) {

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile coordinates
    new_lat <- floor(lat / 3) * 3
    new_lon <- floor(lon / 3) * 3
    ## 1.2. Filter file
    tile_tbl <- gch_tbl |>
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
    tile_tbl <- gch_tbl |>
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
  if (!quiet) message(stringr::str_glue("{nrow(tile_tbl)} tile(s) were found."))
  tiles_list <- list()
  for (i in 1:length(ids)) {
    if (!quiet) message(crayon::green(stringr::str_glue("Downloading tile {i}...")))
    tiles_list[[i]] <- fdi_download_raster(
      url   = ids[i],
      start = 38,
      end   = 80,
      quiet = quiet
    )
  }
  ## DELETE OR CHANGE IN THE FUTURE
  # tiles_list <- purrr::map(
  #   .x = ids,
  #   .f = \(tile_url) fdi_download_raster(
  #     url   = tile_url,
  #     start = 38,
  #     end   = 80
  #   )
  # )
  ## 3.3. Crop layers
  if (crop) tiles_list <- purrr::map(tiles_list, \(x) terra::crop(x, xwgs84))
  if (mask) tiles_list <- purrr::map(tiles_list, \(x) terra::mask(x, xwgs84))
  ## 3.4. Merge depending if there is chm, std or all
  if (layer == "all") {
    ## Number of tiles
    n_tiles <- 1:length(tiles_list)
    ## Filter even tiles (chm)
    chm_tiles <- tiles_list[[n_tiles[n_tiles %% 2 == 0]]]
    ## Filter uneven tiles (std)
    std_tiles <- tiles_list[[n_tiles[n_tiles %% 2 != 0]]]
    ## Merge them if there are multiple tiles
    if (length(tiles_list) > 2) {
      message(crayon::green("Merging tiles..."))
      chm_tiles <- do.call(terra::merge, chm_tiles)
      std_tiles <- do.call(terra::merge, std_tiles)
    }
    ## Join in SpatRaster
    ch_sr <- c(chm_tiles, std_tiles)
  } else {
    ## 3.5. Convert to SpatRaster if it's a list
    ## Merge them if there are multiple tiles
    if (length(tiles_list) > 1) {
      message(crayon::green("Merging tiles..."))
      ch_sr <- do.call(terra::merge, tiles_list)
    } else {
      ch_sr <- tiles_list[[1]]
    }

  }

  # 4. Rename layers
  if (layer == "all") {
    names(ch_sr) <- c("chm", "std")
  } else {
    names(ch_sr) <- layer
  }

  # 5. Return
  if (!quiet) message(crayon::cyan("Cite this dataset using <https://doi.org/10.1038/s41559-023-02206-6>"))
  return(ch_sr)


}




# fd_canopy_height_meta

#'  Forest Canopy Height
#'
#'  Download the High Resolution 1m Global Canopy Height Map
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param mask when \code{x} is specified, whether to mask the tile(s) to the object
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @keywords internal
#' @return A \code{SpatRaster}
#' @export
#'
#' @details
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references <https://registry.opendata.aws/dataforgood-fb-forests/>
#'
#' @examples
#' \donttest{
#' fd_canopy_height_meta(lon = -7.27, lat = 42.43)
#' }
fd_canopy_height_meta <- function(x     = NULL,
                                  lon   = NULL,
                                  lat   = NULL,
                                  crop  = FALSE,
                                  mask  = FALSE,
                                  quiet = FALSE) {

  # 0. Install aws.s3 if not installed
  if (!requireNamespace("aws.s3", quietly = TRUE)) stop("Package `aws.s3` is required to access the inventory data. Please, install it.")

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile
    user_point_sf <- sf::st_as_sf(
      x      = data.frame(lon = lon, lat = lat),
      coords = c("lon", "lat"),
      crs    = "EPSG:4326"
    )
    ## 1.2. Filter file
    tile_vec <- meta_tiles_sf |>
      sf::st_filter(user_point_sf) |>
      dplyr::pull("tile")
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon
    xwgs84 <- sf::st_transform(x, crs = "EPSG:4326")
    ### 1.3.2 Filter tiles(s)
    tile_vec <- meta_tiles_sf |>
      sf::st_filter(xwgs84) |>
      dplyr::pull("tile")
  }

  # 2. Download tile(s)
  ## 2.1. Save into tempdir
  message(stringr::str_glue("{length(tile_vec)} tile(s) were found."))
  out_file <- paste0(tempdir(), "\\", tile_vec, ".tif")
  for (i in 1:length(out_file)) {
    ## If it already exists, go next
    if (file.exists(out_file[i])) {
      if (!quiet) message(crayon::green(stringr::str_glue("Tile {tile_vec[i]} cached.")))
      next
    }
    if (!quiet) message(crayon::green(stringr::str_glue("Downloading tile {i}...")))
    aws.s3::save_object(
      object = paste0("forests/v1/alsgedi_global_v6_float/chm/", tile_vec[i], ".tif"),
      bucket = "dataforgood-fb-data",
      file   = out_file[i],
      region = "us-east-1"
    )
  }
  ## 2.2. Merge and crop
  if (length(out_file) > 1) {
    ### Read tiles
    r <- purrr::map(out_file, terra::rast)
    ### Crop
    if (crop) r <- purrr::map(r, \(x) terra::crop(x, sf::st_transform(xwgs84, "EPSG:3857")))
    if (mask) r <- purrr::map(r, \(x) terra::mask(x, sf::st_transform(xwgs84, "EPSG:3857")))
    ### Merge
    r <- do.call(terra::merge, r)
  } else {
    ## Read tile
    r <- terra::rast(out_file)
    ### Crop
    if (crop) r <- terra::crop(r, sf::st_transform(xwgs84, "EPSG:3857"), mask = mask)
  }

  # 3. Rename and return
  names(r) <- "canopy_height"
  if (!quiet) message(crayon::cyan("Cite this dataset using <https://doi.org/10.1016/j.rse.2023.113888>"))
  return(r)


}




# fd_canopy_height

#' Forest Canopy Height
#'
#' Download the ETH Global Sentinel-2 10m Canopy Height (2020) or the
#' Meta High Resolution 1m Global Canopy Height Map
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#' necessary tiles to cover the area (if \code{lat} and \code{lon} are
#' specified, this argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param model a string specifying the model to download. One of "\code{eth}"
#' or "\code{meta}" (see details)
#' @param layer a string for the layer to download (valid only for eth). The default "\code{chm}"
#' downloads the Canopy Height Model, while "\code{std}" downloads the standard
#' deviation. If you want both layers, use "\code{all}"
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param mask when \code{x} is specified, whether to mask the tile(s) to the object
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @return A \code{SpatRaster}
#' @export
#'
#' @details
#' There are currently two global canopy height models available within this function.
#'
#' - \strong{eth}: the ETH Global Sentinel-2 10m Canopy Height from the year 2020. Visit
#' \url{https://www.research-collection.ethz.ch/handle/20.500.11850/609802} for more
#' information
#'
#' - \strong{meta}: the Meta High Resolution 1m Global Canopy Height. Visit
#' \url{https://doi.org/10.1016/j.rse.2023.113888} for more information
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk Wegner.
#' "A high-resolution canopy height model of the Earth." arXiv preprint arXiv:2204.08322 (2022).
#'
#' Tolan, J., Yang, H.I., Nosarzewski, B., Couairon, G., Vo, H.V., Brandt, J., Spore, J., Majumdar, S., Haziza, D., Vamaraju, J. and Moutakanni, T.,
#' 2024. Very high resolution canopy height maps from RGB imagery using self-supervised vision transformer and convolutional decoder trained on aerial
#' lidar. Remote Sensing of Environment, 300, p.113888.
#'
#' @examples
#' \donttest{
#' ## Get 10m resolution CHM
#' eth_model <- fd_canopy_height(lon = -7.27, lat = 42.43)
#'
#' ## Get 1m resolution CHM
#' meta_model <- fd_canopy_height(lon = -7.27, lat = 42.43, model = "meta")
#' }
fd_canopy_height <- function(x     = NULL,
                             lon   = NULL,
                             lat   = NULL,
                             model = "eth",
                             layer = "chm",
                             crop  = FALSE,
                             mask  = FALSE,
                             quiet = FALSE) {

  # 0. Handle errors
  ## 0.1. Handle all NULL
  if (is.null(x) & is.null(lon) & is.null(lat)) stop("No coordinates or object were specified")
  ## 0.2. Handle non-existing coordinates
  if (!is.null(lon) & !is.null(lat)) {
    if (lon > 180 | lon < -180) stop("Invalid longitude coordinate value")
    if (lat > 80 | lat < -80) stop("Invalid latitude coordinate value")
  } else {
    if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  }
  ## 0.3. Handle incompatible arguments
  if (!is.null(lon) & !is.null(lat) & !is.null(x)) {
    stop("Both coordinates (`lon` and `lat`) and object (`x`) were specified. Specify only one of them.")
  }
  ## 0.4. Handle incompatible arguments (crop = TRUE & coords)
  if (crop & !is.null(lon) & !is.null(lat)) {
    stop("`crop = TRUE` is only available when `x` is specified.")
  }
  ## 0.5. Handle model names
  if (!model %in% c("eth", "meta")) stop("model argument is not valid. Please, use either <eth> or <meta>.")

  # 1. Get data based on model
  if (model == "eth") {
    fd_canopy_height_eth(x = x, lon = lon, lat = lat, layer = layer, crop = crop, mask = mask, quiet = quiet)
  } else {
    fd_canopy_height_meta(x = x, lon = lon, lat = lat, crop = crop, mask = mask, quiet = quiet)
  }


}
