



# fd_landcover_copernicus

#' Global Land Cover
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param x an \code{sf} or \code{SpatVector} object. It will retrieve the
#'          necessary tiles to cover the area (if \code{lat} and \code{lon} are
#'          specified, this argument is ignored)
#' @param lat a number specifying the latitude of the area where we want the tile
#' @param lon a number specifying the longitude of the area where we want the tile
#' @param year year of the land cover data. One of 2015:2019 or 'all'
#' @param layer a character vector of the layer(s) to use from the Global
#'              Land Cover. See details
#' @param crop when \code{x} is specified, whether to crop the tile(s) to the object
#' @param ... additional arguments passed to the \link[terra]{crop} function
#' @param quiet if \code{TRUE}, suppress any message or progress bar
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
fd_landcover_copernicus <- function(x,
                                    lon   = NULL,
                                    lat   = NULL,
                                    year  = 2019,
                                    layer = "forest",
                                    crop  = FALSE, ...,
                                    quiet = FALSE) {

  lifecycle::deprecate_stop(
    when = "0.4.0", what = "fd_landcover_copernicus()", with = NULL,
    details = "Global Land Cover now requires authentication for accessing the data."
  )

  # # 0. Handle errors
  # if (!year %in% 2015:2019 & year != "all") cli::cli_abort("Invalid year")
  # if (!is.null(lon) & !is.null(lat)) {
  #   if (lon > 180 | lon < -180) cli::cli_abort("Invalid longitude coordinate value")
  #   if (lat > 80 | lat < -60) cli::cli_abort("Invalid latitude coordinate value")
  # } else {
  #   if (inherits(x, "SpatVector")) x <- sf::st_as_sf(x)
  # }
  # sel_year <- year
  # if (!all(layer %in% unique(glc_tbl$layer_shrt))) cli::cli_abort("Invalid layer name(s)")

  # # 1. If user specify lat and lon
  # if (!is.null(lat) & !is.null(lon)) {
  #   ## 1.1. Get tile coordinates
  #   new_lat <- ceiling(lat / 20) * 20
  #   new_lon <- floor(lon / 20) * 20
  #   ## 1.2. Filter file
  #   tile_tbl <- glc_tbl |>
  #     dplyr::filter(lat == new_lat & lon == new_lon)
  # } else {
  #   ## 1.3. Get tiles for x
  #   ### 1.3.1. Transform to lat/lon and get bbox
  #   xwgs84 <- sf::st_transform(x, crs = "epsg:4326")
  #   xbbox  <- sf::st_bbox(xwgs84)
  #   ### 1.3.2 Get tile coordinates
  #   new_lon <- floor(xbbox[c(1,3)]/20) * 20
  #   new_lat <- ceiling(xbbox[c(2,4)]/20) * 20
  #   ### 1.3.3. Filter file
  #   tile_tbl <- glc_tbl |>
  #     dplyr::filter(lat %in% new_lat & lon %in% new_lon)
  # }

  # # 2. Rest of the filters
  # ## 2.1. Filter years
  # if (year != "all") tile_tbl <- dplyr::filter(tile_tbl, year %in% sel_year)
  # ## 2.2. Filter layer
  # tile_tbl <- dplyr::filter(tile_tbl, layer_shrt %in% tolower(layer))

  # # 3. Manage different tiles
  # ## 3.1. Get one element per year and layer
  # id_year <- unique(tile_tbl$year)
  # id_lyr  <- unique(tile_tbl$layer_shrt)
  # ids <- expand.grid(year = id_year, layer = id_lyr)
  # ids$layer_names <- paste0(ids$layer, "_", ids$year)
  # ## 3.2. Get the combined rasters per year
  # ## user feedback
  # if (!quiet) cli::cli_alert_info("{nrow(tile_tbl)} tile(s) were found. {nrow(tile_tbl) / length(id_year)} tile(s) per year.")
  # download_pb <- cli::cli_progress_bar(
  #   "Dowloaded years",
  #   total       = nrow(ids),
  #   type        = "tasks",
  #   format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
  #   clear       = FALSE
  # )
  # ## get data
  # combined_sr <- list()
  # for (i in 1:nrow(ids)) {
  #   combined_sr[[i]] <- get_combined_raster_2l(
  #     year_i    = ids$year[i],
  #     layer_i   = ids$layer[i],
  #     url_table = tile_tbl
  #   )
  #   ## check for success
  #   if (is.null(combined_sr[[i]])) {
  #     cli::cli_process_failed()
  #     return(cli::cli_alert_danger("`fd_landcover_copernicus()` failed to retrieve the data. Service might be currently unavailable"))
  #   }

  #   if (!quiet) cli::cli_progress_update(id = download_pb)
  # }
  # if (!quiet) cli::cli_process_done(id = download_pb)
  # ## 3.3. Convert to SpatRaster if it's a list
  # glad_sr <- terra::rast(combined_sr)
  # ## 3.4. Rename layers
  # names(glad_sr) <- ids$layer_names

  # # 4. Manage crop
  # if (crop) glad_sr <- terra::crop(glad_sr, xwgs84, ...)

  # # 5. Return
  # return(glad_sr)

}






## fd_landcover_esri

#' Download data from the ESRI Land Cover Explorer
#'
#' Download an UTM tile of the ESRI Land Cover Explorer for a specified year
#'
#' @param utm_code a character string of length 1 with an UTM code (e.g. "29N")
#' @param year an integer or vector of integers corresponding to the base year
#'             of the land cover tile. The option \code{year = 'all'} downloads all
#'             the available images (2017:2024)
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @return A \code{SpatRaster}
#' @export
#'
#'
#' @references \url{https://livingatlas.arcgis.com/en/home/}
#'
#' @examples
#' \donttest{
#' # Download Land Cover for UTM tile 29N year 2023
#' lc <- fd_landcover_esri("29N", year = 2023)
#'
#' # Download Land Cover for UTM time 29N for all years
#' lc <- fd_landcover_esri("29N", year = "all")
#' }
fd_landcover_esri <- function(utm_code,
                              year,
                              quiet = FALSE) {

  # 0. Handle year error
  if (!(year %in% seq(2017, 2024, 1)) & year != "all") cli::cli_abort("The indicated year is not valid. Please, use a year between 2017 and 2024, or the 'all' function to retrieve all.")

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
  if (length(download_url) == 0) cli::cli_abort("The UTM Code doesn't exist, is incorrect or fell into the sea. Please, use two numbers and one letter (e.g. 05T)")

  # 4. Download
  ## 4.1. Tiff file
  tif_path <- paste0(tempdir(), "/", basename(download_url))
  ## User feedback
  if (!quiet) cli::cli_alert_info("Downloading data...")
  if (!quiet) download_pb <- cli::cli_progress_bar(
    "Dowloaded tiles",
    total       = length(tif_path),
    type        = "tasks",
    format_done = "{.alert-success Download completed {.timestamp {cli::pb_elapsed}}}",
    clear       = FALSE
  )
  ## Download tiles
  for (i in 1:length(tif_path)) {
    dwld <- fdi_download(
      download_url = download_url[i],
      destfile     = tif_path[i]
    )
    ## check for success
    if (!dwld) {
      cli::cli_process_failed()
      return(cli::cli_alert_danger("`fd_landcover_esri()` failed to retrieve the data. Service might be currently unavailable"))
    }

    if (!quiet) cli::cli_progress_update(id = download_pb)
  }
  if (!quiet) cli::cli_process_done(id = download_pb)
  # 5. Read into R
  ## 5.1. Read file
  lc_sr <- terra::rast(tif_path)
  ## 5.2. Layer names
  lc_names <- stringr::str_sub(basename(download_url), 1, 8)
  names(lc_sr) <- lc_names
  ## 5.3. Return object
  return(lc_sr)

}

