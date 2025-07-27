

#' European Maps of Forest Biomass 
#'
#' Download maps of Forest Biomass and Growing Stock Volume in Europe.
#'
#' @param x a \code{sf} or \code{SpatVector} object. It will retrieve the
#' necessary tiles to cover the area (if \code{lat} and \code{lon} are
#' specified, this argument is ignored). See details.
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param layer a string for the layer to download. See details.
#' @param year a number specifying the year of the data. Possible options are 2017, and
#' 2020-2023.
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param mask when \code{x} is specified, whether to mask the tile(s) to the object
#' @param merge if \code{TRUE}, it will merge the tiles into one raster.
#' If \code{FALSE} a SpatRasterCollection will be returned.
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @include utils-not-exported.R
#' @return A \code{SpatRaster} or \code{SpatRasterCollection}
#' @export
#'
#' @details
#' The data is distributed in tiles according to the Sentinel-2 tiles. Additionally, there's
#' a full map of Europe that can be accessed using the special option \code{x = "europe"}.
#' 
#' The available layers to download are:
#' 
#' * __AGB__: Aboveground Biomass
#' 
#' * __BGB__: Belowground Biomass
#' 
#' * __GSV__: Growing Stock Volume
#'
#' @references <https://www.forestcarbonplatform.org/>
#'
#' @examples
#' # download a tile in Galicia (Spain)
#' galicia_tile_rast <- fd_forest_eu_biomass(lon = -7, lat = 42)
fd_forest_eu_biomass <- function(x = NULL, 
                                  lon = NULL, 
                                  lat = NULL, 
                                  layer = "AGB", 
                                  year = 2023, 
                                  crop = FALSE,
                                  mask = FALSE,
                                  merge = FALSE,
                                  quiet = FALSE) {
  # 0. Manage errors
  if (!year %in% c(2017, 2020:2023)) cli::cli_abort("`year` must be 2017, 2020, 2021, 2022, or 2023")
  if (!layer %in% c("AGB", "BGB", "GSV")) cli::cli_abort("`layer` must be AGB, BGB, or GSV. See function details.")

  # 1. If user specify lat and lon
  if (!is.null(lat) & !is.null(lon)) {
    ## 1.1. Create a point
    point_sfc <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = "EPSG:4326")
    ## 1.2. Filter tile(s)
    tiles_sf <- sf::st_filter(sentinel2_tiles_sf, point_sfc)
    tiles_vec <- tiles_sf$Name
  } else {
    # 1.3. Manage Europe map
    if (is.character(x)) {
      if (x == "europe") {
        ## 1.3.1. Variable
        variable <- switch(layer,
          "AGB" = paste0("Above_ground_biomass/FCM_Europe_demo_", year, "_AGB.tif"),
          "BGB" = paste0("Below_ground_biomass/FCM_Europe_demo_", year, "_BGB.tif"),
          "GSV" = paste0("Growing_stock_volume/FCM_Europe_demo_", year, "_GSV.tif"),
        )
        ## 1.3.2. Generate URL
        base_url <- "https://portal.forestcarbonplatform.org/data/download/ESA_Forest_Carbon_Monitoring/"
        url <- paste0(base_url, variable)
        ## 1.3.3. Download raster
        final_rast <- fdi_download_raster(url)
        names(final_rast) <- paste0(layer, "_", year)
        ## 1.3.4. Return
        if (!quiet) cli::cli_alert_success("Cite this dataset using {.url https://doi.org/10.1016/j.dib.2025.111613}")
        return(final_rast)
      }
    }
    ## 1.4. Get tiles for x
    ## transform to same CRS as sentinel2_tiles_sf
    xwgs84 <- sf::st_transform(x, sf::st_crs(sentinel2_tiles_sf))
    ## 1.5. Filter tiles
    tiles_sf <- sf::st_filter(sentinel2_tiles_sf, xwgs84)
    tiles_vec <- tiles_sf$Name
  }
  if (length(tiles_vec) == 0) cli::cli_abort("No tiles were found.")

  # 2. URL(s)
  ## 2.1. Variable
  variable <- switch(layer,
    "AGB" = paste0("Above_ground_biomass/", tiles_vec, "_", year, "_AGB.tif"),
    "BGB" = paste0("Below_ground_biomass/", tiles_vec, "_", year, "_BGB.tif"),
    "GSV" = paste0("Growing_stock_volume/", tiles_vec, "_",  year, "_GSV.tif"),
  )
  ## 2.2. Generate URL
  base_url <- "https://portal.forestcarbonplatform.org/data/download/ESA_Forest_Carbon_Monitoring/"
  url <- paste0(base_url, variable)

  # 3. Manage different tiles
  ## 3.1. Get the combined rasters per year
  ## feedback
  if (!quiet) cli::cli_alert_info("Downloading {length(url)} tile{?s}...")
  tiles_list <- list()
  if (!quiet) download_pb <- cli::cli_progress_bar(
    "Downloaded tiles",
    total       = length(url),
    type        = "tasks",
    format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## actual download
  for (i in 1:length(url)) {
    tiles_list[[i]] <- fdi_download_raster(url[i])
    if (!quiet) cli::cli_progress_update(id = download_pb)
  }
  if (!quiet) cli::cli_process_done(id = download_pb)
  ## eliminate NULLs (some sentinel-2 tiles are not in the dataset)
  tiles_list <- Filter(Negate(is.null), tiles_list)
  ## 3.2. Crop layers
  ### eliminate terra bars
  user.opts <- terra::terraOptions(print = FALSE)
  terra::terraOptions(progress = 0)
  on.exit(terra::terraOptions(progressbar = user.opts$progress))
  ### crop
  if (crop & !is.null(x)) tiles_list <- crop_with_feedback(tiles_list, xwgs84, quiet)
  ## 3.4. Mask layers
  if (mask & !is.null(x)) tiles_list <- mask_with_feedback(tiles_list, xwgs84, quiet)

  # 4. Merging
  ## 4.1. Manage merging
  if (merge & length(tiles_list) > 1) {
    ## check all CRS (they are not always the same)
    crs_freq <- lapply(
      tiles_list,
      \(x) terra::crs(x, describe = TRUE)$code
    ) |> 
      as.numeric() |> 
      table()
    ## if there are more than 1 CRS, project to the most common one
    if (length(crs_freq) > 1) {
      tiles_list <- lapply(
        tiles_list,
        \(x) terra::project(x, paste0("EPSG:", names(crs_freq)[which.max(crs_freq)]))
      )
    }
    ## merge tiles
    ## user feedback
    if (!quiet) cli::cli_alert_info("Merging {length(tiles_vec)} tile{?s}...")
    final_rast <- do.call(terra::merge, tiles_list)
    names(final_rast) <- paste0(layer, "_", year)
    
    ## 4.2. Manage not merging multiple tiles
  } else if (!merge & length(tiles_list) > 1) {
    final_rast <- terra::sprc(tiles_list)

    ## 4.3. Manage single tiles
  } else {
    final_rast <- tiles_list[[1]]
    names(final_rast) <- paste0(layer, "_", year)
  }

  # 5. Return
  if (!quiet) cli::cli_alert_success("Visit {.url https://www.forestcarbonplatform.org/} for more information.")
  return(final_rast)
}
