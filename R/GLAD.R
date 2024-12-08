


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
#' @param merge if \code{FALSE} (default), it will merge the tiles into one raster.
#' If \code{FALSE} a SpatRasterCollection will be returned.
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
                           merge = FALSE,
                           quiet = FALSE) {

  # 0. Handle errors
  if (year != "all" & !year %in% seq(2000, 2020, 5)) cli::cli_abort("Invalid year. Please, use year 2000, 2020 or 'all'")
  if (is.na(sf::st_crs(x)) & !is.null(x)) cli::cli_abort("The object x is not georreferenced.")
  if (!class(x)[1] %in% c("sf", "SpatVector") & is.null(lat) & is.null(lon)) cli::cli_abort("Invalid x format, or lat&lon not specified.")
  ## 0.1. Handle formats
  if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  ## 0.2. Handle incompatible arguments (crop = TRUE & coords)
  if (crop & !is.null(lon) & !is.null(lat)) {
    cli::cli_abort("`crop = TRUE` is only available when `x` is specified.")
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
    cli::cli_abort("Invalid model. Please, use <extent>, <height> or <landcover>")
  )

  # 3. Manage different tiles
  if (model %in% c("extent", "height") & !quiet) {
    cli::cli_alert_info("{length(tiles_vec)} tile(s) were found. A total of {length(tiles_vec)*1.5} GB of data will be read into R. This may take a while.")
  }
  ## 3.1. Read rasters
  ### eliminate terra bars
  user.opts <- terra::terraOptions(print = FALSE)
  terra::terraOptions(progress = 0)
  on.exit(terra::terraOptions(progressbar = user.opts$progress))

  if (length(tiles_vec) > 1) {

    ### Read tiles
    if (!quiet) cli::cli_alert_info("Downloading data...")
    download_pb <- cli::cli_progress_bar(
      "Dowloaded tiles",
      total       = length(tiles_vec),
      type        = "tasks",
      format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
      clear       = FALSE
    )

    r <- list()
    for (i in (1:length(tiles_vec))) {
      r[[i]] <- try(terra::rast(tiles_vec[[i]]), silent = TRUE) |> suppressWarnings()
      if (!quiet) cli::cli_progress_update(id = download_pb)
    }
    if (!quiet) cli::cli_process_done(id = download_pb)
    ## stop if download failed
    if (length(r) == 0) return(cli::cli_alert_danger("`fd_forest_glad()` failed to retrieve the data. Service might be currently unavailable"))

    ### Crop
    if (crop) {
      ## user feedback
      if (!quiet) cli::cli_alert_info("Cropping tiles...")
      crop_pb <- cli::cli_progress_bar(
        "Crop tiles",
        total       = length(tiles_vec),
        type        = "tasks",
        format_done = "{.alert-success Crop completed {.timestamp {cli::pb_elapsed}}}",
        clear       = FALSE
      )
      ## crop
      for (i in (1:length(tiles_vec))) {
        r[[i]] <- terra::crop(r[[i]], xwgs84)
        if (!quiet) cli::cli_progress_update(id = crop_pb)
      }
      if (!quiet) cli::cli_process_done(id = crop_pb)
    }
    ## Mask
    if (mask) {
      ## user feedback
      if (!quiet) cli::cli_alert_info("Masking tiles...")
      mask_pb <- cli::cli_progress_bar(
        "Masked tiles",
        total       = length(tiles_vec),
        type        = "tasks",
        format_done = "{.alert-success Mask completed {.timestamp {cli::pb_elapsed}}}",
        clear       = FALSE
      )
      ## mask
      for (i in (1:length(tiles_vec))) {
        r[[i]] <- terra::mask(r[[i]], xwgs84)
        if (!quiet) cli::cli_progress_update(id = mask_pb)
      }
      if (!quiet) cli::cli_process_done(id = mask_pb)
    }
    ### Merge
    if (merge) {
      ## user feedback
      if (!quiet) cli::cli_alert_info("Merging tiles...")
      if (!quiet) merge_pb <- cli::cli_progress_bar(
        "Merging tiles",
        total       = length(tiles_vec) - 1, ,
        type        = "tasks",
        format_done = "{.alert-success Merge completed {.timestamp {cli::pb_elapsed}}}",
        clear       = FALSE
      )
      ## merge tiles iteratively
      r_merge <- r[[1]]
      for (i in 2:length(r)) {
        r_merge <- terra::merge(r_merge, r[[i]])
        if (!quiet) cli::cli_progress_update(id = merge_pb)
      }
      ## finnish user feedback
      cli::cli_progress_done(id = merge_pb)
      r <- r_merge
    } else {
      r <- terra::sprc(r)
    }

  } else {
    ## Read tile
    try({r <- terra::rast(tiles_vec)}, silent = TRUE)
    if (!exists("r")) return(cli::cli_alert_danger("`fd_forest_glad()` failed to retrieve the data. Service might be currently unavailable"))
    ### Crop
    if (crop) r <- terra::crop(r, xwgs84)
    if (mask) r <- terra::mask(r, xwgs84)
  }
  ## 3.2. Rename layers
  names(r) <- paste0(model, "_", year)

  # 4. Return
  if (!quiet) {
    switch(model,
           "extent"           = cli::cli_alert_success("Cite this dataset using {cli::col_br_cyan('https://doi.org/10.1016/j.rse.2020.112165')}"),
           "height"           = cli::cli_alert_success("Cite this dataset using {cli::col_br_cyan('https://doi.org/10.1016/j.rse.2020.112165')}"),
           "landcover"        = cli::cli_alert_success("Cite this dataset using {cli::col_br_cyan('https://doi.org/10.3389/frsen.2022.856903')}"),
           "landcover-change" = cli::cli_alert_success("Cite this dataset using {cli::col_br_cyan('https://doi.org/10.3389/frsen.2022.856903')}")
    )
  }

  return(r)

}
















