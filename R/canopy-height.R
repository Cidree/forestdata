


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
#' @param merge if \code{FALSE} (default), it will merge the tiles into one raster.
#' If \code{FALSE} a SpatRasterCollection will be returned.
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @return A \code{SpatRaster}
#' @keywords internal
#'
#' @details
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk
#'              Wegner. "A high-resolution canopy height model of the Earth."
#'              arXiv preprint arXiv:2204.08322 (2022).
#'
fd_canopy_height_eth <- function(x     = NULL,
                                 lon   = NULL,
                                 lat   = NULL,
                                 layer = "chm",
                                 crop  = FALSE,
                                 mask  = FALSE,
                                 merge = FALSE,
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
  if (!quiet) cli::cli_alert_info("Downloading {nrow(tile_tbl)} tile{?s}...")
  tiles_list <- list()
  # i <- 0
  # if (!quiet) cli::cli_progress_step("{i}/{nrow(tile_tbl)} {cli::qty(i)}tile{?s} downloaded.")
  if (!quiet) download_pb <- cli::cli_progress_bar(
    "Downloaded tiles",
    total       = nrow(tile_tbl),
    type        = "tasks",
    format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  for (i in 1:length(ids)) {
    tiles_list[[i]] <- fdi_download_raster(
      url   = ids[i],
      start = 38,
      end   = 80
    )
    ## stop if the download failed
    if (is.null(tiles_list[[i]])) {
      cli::cli_process_failed()
      return(cli::cli_alert_danger("`fd_canopy_height()` failed to retrieve the data. Service might be currently unavailable"))
    }
    if (!quiet) cli::cli_progress_update(id = download_pb)
  }
  if (!quiet) cli::cli_process_done(id = download_pb)
  ## 3.3. Crop layers
  ### eliminate terra bars
  user.opts <- terra::terraOptions(print = FALSE)
  terra::terraOptions(progress = 0)
  on.exit(terra::terraOptions(progressbar = user.opts$progress))
  ### crop
  if (crop) tiles_list <- crop_with_feedback(tiles_list, xwgs84, quiet)
  ## 3.4. Mask layers
  if (mask) tiles_list <- mask_with_feedback(tiles_list, xwgs84, quiet)
  ## 3.5. Merge when layer == "all" ---
  if (merge & layer == "all" & length(tiles_list) > 2) {
    ## user feedback
    if (!quiet) cli::cli_alert_info("Merging tiles...")
    ## Number of tiles
    n_tiles <- 1:length(tiles_list)
    ## Filter even tiles (chm)
    chm_tiles <- tiles_list[n_tiles[n_tiles %% 2 == 0]]
    ## Filter uneven tiles (std)
    std_tiles <- tiles_list[n_tiles[n_tiles %% 2 != 0]]
    ## merge
    chm_tiles <- do.call(terra::merge, chm_tiles)
    std_tiles <- do.call(terra::merge, std_tiles)
    ## store them in final raster
    ch_sr <- c(chm_tiles, std_tiles)
    names(ch_sr) <- c("chm", "std")


    ## manage when merge is not asked for ---
  } else if (!merge & layer == "all" & length(tiles_list) > 2) {
    ## Number of tiles
    n_tiles <- 1:length(tiles_list)
    ## Filter even tiles (chm)
    chm_tiles <- tiles_list[n_tiles[n_tiles %% 2 == 0]]
    ## Filter uneven tiles (std)
    std_tiles <- tiles_list[n_tiles[n_tiles %% 2 != 0]]
    ## create a sprc
    ch_sr <- list(chm = terra::sprc(chm_tiles), std = terra::sprc(chm_tiles))


    ## manage when merge is not needed ---
  } else if (layer == "all" & length(tiles_list) == 2) {
    ch_sr <- c(tiles_list[[1]], tiles_list[[2]])
    names(ch_sr) <- c("chm", "std")


    ## manage when individual layer and multiple tiles ---
  } else if (merge & layer != "all" & length(tiles_list) > 2) {
    ## user feedback
    if (!quiet) cli::cli_alert_info("Merging tiles...")
    if (!quiet) merge_pb <- cli::cli_progress_bar(
      "Merging tiles",
      total       = length(tiles_list) - 1, ,
      type        = "tasks",
      format_done = "{.alert-success Merge completed {.timestamp {cli::pb_elapsed}}}",
      clear       = FALSE
    )
    ## merge tiles iteratively
    ch_sr <- tiles_list[[1]]
    for (i in 2:length(tiles_list)) {
      ch_sr <- terra::merge(ch_sr, tiles_list[[i]])
      if (!quiet) cli::cli_progress_update(id = merge_pb)
    }
    ## finnish user feedback
    # cli::cli_progress_done(id = merge_pb)
    names(ch_sr) <- layer


    ## manage when merge is not asked for ---
  } else if (!merge & layer != "all" & length(tiles_list) > 1) {
    ch_sr <- terra::sprc(tiles_list)
    names(ch_sr) <- layer


    ## manage when there are only 2 tiles ---
  } else if (merge & layer != "all" & length(tiles_list) == 2) {
    if (!quiet) cli::cli_progress_step(
      "Merge tiles",
      msg_done = "Merge completed",
      spinner  = TRUE
    )
    ch_sr <- terra::merge(tiles_list[[1]], tiles_list[[2]])
    names(ch_sr) <- layer
    if (!quiet) cli::cli_progress_done()


    ## final case: when there's only 1 layer ---
  } else {
    ch_sr <- tiles_list[[1]]
    names(ch_sr) <- layer
  }

  # 4. Return
  if (!quiet) cli::cli_alert_success("Cite this dataset using {.url https://doi.org/10.1038/s41559-023-02206-6}")
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
#' @param merge if \code{FALSE} (default), it will merge the tiles into one raster.
#' If \code{FALSE} a SpatRasterCollection will be returned.
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @keywords internal
#' @return A \code{SpatRaster} or \code{SpatRasterCollection}
#'
#' @details
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references <https://registry.opendata.aws/dataforgood-fb-forests/>
#'
fd_canopy_height_meta <- function(x     = NULL,
                                  lon   = NULL,
                                  lat   = NULL,
                                  crop  = FALSE,
                                  mask  = FALSE,
                                  merge = FALSE,
                                  quiet = FALSE) {

  # 0. Install aws.s3 if not installed
  if (!requireNamespace("aws.s3", quietly = TRUE)) cli::cli_abort("Package `aws.s3` is required to access the inventory data. Please, install it.")

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
  ### stop if no tiles have been found
  if (length(tile_vec) == 0) cli::cli_abort("No tiles have been found in the selected area.")

  # 2. Download tile(s)
  ## 2.1. Save into tempdir
  out_file <- paste0(tempdir(), "\\", tile_vec, ".tif")
  ## user feedback
  if (!quiet) cli::cli_alert_info("Downloading {length(tile_vec)} tile{?s}...")
  if (!quiet) download_pb <- cli::cli_progress_bar(
    "Downloaded tiles",
    total       = length(tile_vec),
    type        = "tasks",
    format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## do download
  for (i in 1:length(out_file)) {
    ## download if it doesn't exist
    if (!file.exists(out_file[i])) {
      try({
        aws.s3::save_object(
          object = paste0("forests/v1/alsgedi_global_v6_float/chm/", tile_vec[i], ".tif"),
          bucket = "dataforgood-fb-data",
          file   = out_file[i],
          region = "us-east-1"
        )
      }, silent = TRUE)
    }
    ## stop if the download failed
    if (!file.exists(out_file[i])) {
      cli::cli_process_failed()
      return(cli::cli_alert_danger("`fd_canopy_height()` failed to retrieve the data. Service might be currently unavailable"))
    }
    ## close user feedback
    if (!quiet) cli::cli_progress_update(id = download_pb)
  }
  ## close user feedback
  if (!quiet) cli::cli_process_done(id = download_pb)
  ## read raster(s)
  r <- lapply(out_file, terra::rast)

  # 3. Crop
  if (crop) r <- crop_with_feedback(r, sf::st_transform(xwgs84, crs = "EPSG:3857"), quiet)

  # 4. Mask
  if (mask) r <- mask_with_feedback(r, sf::st_transform(xwgs84, crs = "EPSG:3857"), quiet)

  # 5. Merge
  ## manage merge = TRUE and multiple tiles ---
  if (merge & length(tile_vec) > 1) {
    ## user feedback
    if (!quiet) cli::cli_alert_info("Merging {length(tile_vec)} tile{?s}...")
    if (!quiet) merge_pb <- cli::cli_progress_bar(
      "Merged tiles",
      total       = length(tile_vec) - 1,
      type        = "tasks",
      format_done = "{.alert-success Merge completed {.timestamp {cli::pb_elapsed}}}",
      clear       = FALSE
    )
    ## do merge
    r_final <- r[[1]]
    for (i in 2:length(tile_vec)) {
      r_final <- terra::merge(r_final, r[[i]])
      if (!quiet) cli::cli_progress_update(id = merge_pb)
    }
    ## close user feedback
    cli::cli_process_done(id = merge_pb)


    ## manage merge = FALSE and multiple tiles ---
  } else if (!merge & length(tile_vec) > 1) {
    r_final <- terra::sprc(r)


    ## manage a single tile ---
  } else {
    r_final <- r[[1]]
  }

  # 6. Rename and return
  names(r_final) <- "canopy_height"
  if (!quiet) cli::cli_alert_success("Cite this dataset using {.url https://doi.org/10.1016/j.rse.2023.113888}")
  return(r_final)


}





# fd_canopy_height_amazon

#'  Forest Canopy Height
#'
#'  Download the High Resolution 5m for the Amazon Forest
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param mask when \code{x} is specified, whether to mask the tile(s) to the object
#' @param merge if \code{FALSE} (default), it will merge the tiles into one raster.
#' If \code{FALSE} a SpatRasterCollection will be returned.
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @keywords internal
#' @return A \code{SpatRaster} or \code{SpatRasterCollection}
#'
#' @details
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references <https://arxiv.org/abs/2501.10600>
#'
fd_canopy_height_amazon <- function(x     = NULL,
                                    lon   = NULL,
                                    lat   = NULL,
                                    crop  = FALSE,
                                    mask  = FALSE,
                                    merge = FALSE,
                                    quiet = FALSE) {

  # 0. Install aws.s3 if not installed
  if (!requireNamespace("aws.s3", quietly = TRUE)) cli::cli_abort("Package `aws.s3` is required to access the inventory data. Please, install it.")

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Get tile
    user_point_sf <- sf::st_as_sf(
      x      = data.frame(lon = lon, lat = lat),
      coords = c("lon", "lat"),
      crs    = "EPSG:4326"
    )
    ## 1.2. Filter file
    tile_vec <- amazon_tiles_sf |>
      sf::st_filter(user_point_sf) |>
      dplyr::pull("id")
  } else {
    ## 1.3. Get tiles for x
    ### 1.3.1. Transform to lat/lon
    xwgs84 <- sf::st_transform(x, crs = "EPSG:4326")
    ### 1.3.2 Filter tiles(s)
    tile_vec <- amazon_tiles_sf |>
      sf::st_filter(xwgs84) |>
      dplyr::pull("id")
  }
  ### stop if no tiles have been found
  if (length(tile_vec) == 0) cli::cli_abort("No tiles have been found in the selected area.")

  # 2. Download tile(s)
  ## 2.1. Save into tempdir
  out_file <- paste0(tempdir(), "\\", tile_vec)
  ## user feedback
  if (!quiet) cli::cli_alert_info("Downloading {length(tile_vec)} tile{?s}...")
  if (!quiet) download_pb <- cli::cli_progress_bar(
    "Downloaded tiles",
    total       = length(tile_vec),
    type        = "tasks",
    format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## do download
  for (i in 1:length(out_file)) {
    ## download if it doesn't exist
    if (!file.exists(out_file[i])) {
      try({
        aws.s3::save_object(
          object = paste0("v1/", tile_vec[i]),
          bucket = "ctrees-amazon-canopy-height",
          file   = out_file[i],
          region = "us-west-2"
        )
      }, silent = TRUE)
    }
    ## stop if the download failed
    if (!file.exists(out_file[i])) {
      cli::cli_process_failed()
      return(cli::cli_alert_danger("`fd_canopy_height()` failed to retrieve the data. Service might be currently unavailable"))
    }
    ## close user feedback
    if (!quiet) cli::cli_progress_update(id = download_pb)
  }
  ## close user feedback
  if (!quiet) cli::cli_process_done(id = download_pb)
  ## read raster(s)
  r <- lapply(out_file, terra::rast)

  # 3. Crop
  if (crop) r <- crop_with_feedback(r, sf::st_transform(xwgs84, crs = "EPSG:3857"), quiet)

  # 4. Mask
  if (mask) r <- mask_with_feedback(r, sf::st_transform(xwgs84, crs = "EPSG:3857"), quiet)

  # 5. Merge
  ## manage merge = TRUE and multiple tiles ---
  if (merge & length(tile_vec) > 1) {
    ## user feedback
    if (!quiet) cli::cli_alert_info("Merging {length(tile_vec)} tile{?s}...")
    if (!quiet) merge_pb <- cli::cli_progress_bar(
      "Merged tiles",
      total       = length(tile_vec) - 1,
      type        = "tasks",
      format_done = "{.alert-success Merge completed {.timestamp {cli::pb_elapsed}}}",
      clear       = FALSE
    )
    ## do merge
    r_final <- r[[1]]
    for (i in 2:length(tile_vec)) {
      r_final <- terra::merge(r_final, r[[i]])
      if (!quiet) cli::cli_progress_update(id = merge_pb)
    }
    ## close user feedback
    cli::cli_process_done(id = merge_pb)


    ## manage merge = FALSE and multiple tiles ---
  } else if (!merge & length(tile_vec) > 1) {
    r_final <- terra::sprc(r)


    ## manage a single tile ---
  } else {
    r_final <- r[[1]]
  }

  # 6. Rename and return
  names(r_final) <- "amazon_chm"
  if (!quiet) cli::cli_alert_success("Cite this dataset using {.url https://doi.org/10.48550/arXiv.2501.10600}")
  return(r_final)


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
#' @param merge if \code{FALSE} (default), it will merge the tiles into one raster.
#' If \code{FALSE} a SpatRasterCollection will be returned.
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @return A \code{SpatRaster} or \code{SpatRasterCollection}
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
#' \doi{10.1016/j.rse.2023.113888} for more information
#'
#' - \strong{amazon}: the high resolution tree height for the Amazon Forest using
#' Plnet NICFI Images and LiDAR-Informed U-Net model. Visit \url{https://arxiv.org/abs/2501.10600}
#' for more information
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @references Lang, Nico, Walter Jetz, Konrad Schindler, and Jan Dirk Wegner.
#' "A high-resolution canopy height model of the Earth." arXiv preprint
#' \url{https://arxiv.org/abs/2204.08322} (2022).
#'
#' Tolan, J., Yang, H.I., Nosarzewski, B., Couairon, G., Vo, H.V., Brandt, J., Spore, J., Majumdar, S., Haziza, D., Vamaraju, J. and Moutakanni, T.,
#' 2024. Very high resolution canopy height maps from RGB imagery using self-supervised vision transformer and convolutional decoder trained on aerial
#' lidar. Remote Sensing of Environment, 300, p.113888.
#'
#' Wagner, F. H., Dalagnol, R., Carter, G., Hirye, M. C. M., Gill, S., Sagang Takougoum,
#' L. B., Favrichon, S., Keller, M., Ometto, J. P. H. B., Alves, L., Creze, C.,
#' George-Chacon, S. P., Li, S., Liu, Z., Mullissa, A., Yang, Y., Santos, E. G.,
#' Worden, S. R., Brandt, M., Ciais, P., Hagen, S. C., & Saatchi, S. (2025). High
#' resolution tree height mapping of the Amazon Forest using Planet NICFI images and
#' LiDAR-informed U-Net model. arXiv. \url{https://arxiv.org/abs/2501.10600}
#'
#' @examples
#' \dontrun{
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
                             merge = FALSE,
                             quiet = FALSE) {

  # 0. Handle errors
  ## 0.1. Handle all NULL
  if (is.null(x) & is.null(lon) & is.null(lat)) cli::cli_abort("No coordinates or object were specified")
  ## 0.2. Handle non-existing coordinates
  if (!is.null(lon) & !is.null(lat)) {
    if (lon > 180 | lon < -180) cli::cli_abort("Invalid longitude coordinate value")
    if (lat > 80 | lat < -80) cli::cli_abort("Invalid latitude coordinate value")
  } else {
    if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  }
  ## 0.3. Handle incompatible arguments
  if (!is.null(lon) & !is.null(lat) & !is.null(x)) {
    cli::cli_abort("Both coordinates (`lon` and `lat`) and object (`x`) were specified. Specify only one of them.")
  }
  ## 0.4. Handle incompatible arguments (crop = TRUE & coords)
  if ((crop | mask | merge) & !is.null(lon) & !is.null(lat)) {
    v <- if (crop) "crop" else if (mask) "mask" else if (merge) "merge"
    cli::cli_abort("`{v} = TRUE` is only available when `x` is specified.")
  }
  ## 0.5. Handle model names
  if (!model %in% c("eth", "meta", "amazon")) cli::cli_abort("model argument is not valid. Please, use either <eth> or <meta>.")

  # 1. Get data based on model
  if (model == "eth") {
    fd_canopy_height_eth(x = x, lon = lon, lat = lat, layer = layer, crop = crop, mask = mask, merge = merge, quiet = quiet)
  } else if (model == "meta") {
    fd_canopy_height_meta(x = x, lon = lon, lat = lat, crop = crop, mask = mask, merge = merge, quiet = quiet)
  } else if (model == "amazon") {
    fd_canopy_height_amazon(x = x, lon = lon, lat = lat, crop = crop, mask = mask, merge = merge, quiet = quiet)
  }


}
