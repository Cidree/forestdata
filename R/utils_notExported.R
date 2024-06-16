# fdi_fix_names ----

#' (Internal) Fix non Latin-ASCII names
#'
#' A function that fixes names with strange characters, spaces, and
#' also convert to title
#'
#' @param name String to fix
#'
#' @return A character vector of same length as name
#' @keywords internal
#'
#' @importFrom stringi stri_trans_general
#' @examples
#' \dontrun{
#' fdi_fix_names("CórDoBa   ")
#' }
fdi_fix_names <- function(name) {
  name %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_to_title() %>%
    stringr::str_trim()
}

# fdi_download_unzip ----

#'  (Internal) Downloads data to tempdir
#'  Download data to tempdir
#'
#' @param download_url Url of data to download
#' @param dir_zip Path of the zipped downloaded data
#' @param dir_unzip Path of the unzipped downloaded data
#' @param timeout Time to stop downloading
#' @param quiet If \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
#'
#' @return Unzipped file
#' @keywords internal
#' @examples
#' \dontrun{
#' fdi_download_unzip()
#' }
fdi_download_unzip <- function(download_url, dir_unzip, dir_zip,
                               timeout = 1000, quiet = TRUE) {

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- download_url
  dir_unzip    <- dir_unzip
  dir_zip      <- dir_zip
  ## 1.2. Download to tempdir
  if (!file.exists(dir_unzip)) {
    options(timeout = max(timeout, getOption("timeout")))
    download.file(
      url      = download_url,
      destfile = dir_zip,
      quiet    = quiet,
      mode     = "wb"
    )
    ## 1.3. Unzip the file
    unzip(
      zipfile = dir_zip,
      exdir   = dir_unzip
    )
    ## 1.4. Remove zip to release space
    file.remove(dir_zip)
  }
}

# fdi_download_7zip ----

#'  (Internal) Downloads data to tempdir
#'  Download data to tempdir
#'
#' @param download_url Url of data to download
#' @param dir_zip Path of the zipped downloaded data
#' @param dir_unzip Path of the unzipped downloaded data
#' @param timeout Time to stop downloading
#' @param quiet If \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
#'
#' @return Unzipped file
#' @keywords internal
#' @examples
#' \dontrun{
#' fdi_download_7zip()
#' }
fdi_download_7zip <- function(download_url, dir_unzip, dir_zip,
                               timeout = 5000, quiet = TRUE) {

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- download_url
  dir_unzip    <- dir_unzip
  dir_zip      <- dir_zip
  ## 1.2. Download to tempdir
  if (!file.exists(dir_unzip)) {
    options(timeout = max(timeout, getOption("timeout")))
    download.file(
      url      = download_url,
      destfile = dir_zip,
      quiet    = quiet,
      mode     = "wb"
    )

    ## 1.3. Extract it
    archive::archive_extract(dir_zip, dir_unzip)

    ## 1.4. Remove zip to release space
    file.remove(dir_zip)
  }
}

# get_combined_raster

#'  Combines different raster tiles
#'  (Internal) Helper to combine rasters from forest extent GLAD
#'
#' @return A \code{SpatRaster}
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_combined_raster(2000, url_table = urls)
#' }

get_combined_raster <- function(year_i, url_table) {

  ## Filter urls within the year
  filtered_url <- dplyr::filter(url_table, year == year_i) %>%
    dplyr::pull(url) %>%
    as.character()

  ## Download all the rasters
  rast_lst <- lapply(filtered_url, terra::rast)

  ## Combine all the raster
  if (length(rast_lst) == 1) {
    r_combined <- rast_lst[[1]]
  } else {
    r_combined <- do.call(terra::merge, rast_lst)
  }

  return(r_combined)
}

# get_combined_raster_2l

#'  Combines different raster tiles
#'  (Internal) Helper to combine rasters from Copernicus Global Land Cover.
#'
#' @return A \code{SpatRaster}
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_combined_raster(2000, "forest", url_table = urls)
#' }

get_combined_raster_2l <- function(year_i, layer_i, url_table) {

  ## Filter urls within the year
  filtered_url <- dplyr::filter(url_table, year == year_i, layer_shrt == layer_i) %>%
    dplyr::pull(url) %>%
    as.character()

  ## Download all the rasters
  rast_lst <- lapply(filtered_url, terra::rast)

  ## Combine all the raster
  if (length(rast_lst) == 1) {
    r_combined <- rast_lst[[1]]
  } else {
    r_combined <- do.call(terra::merge, rast_lst)
  }

  return(r_combined)
}


# fdi_download_raster

#'  Donwload a read a raster
#'  (Internal) Helper to download and read a raster from an URL
#'
#' @return A \code{SpatRaster}
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fdi_download_raster(url)
#' }

fdi_download_raster <- function(url, start = NULL, end = NULL, timeout = 5000) {

  ## 1. File name
  if (is.null(start) & is.null(end)) {
    url_path <- stringr::str_glue("{tempdir()}/{basename(url)}")
  } else {
    url_path <- stringr::str_glue("{tempdir()}/{basename(url) %>% stringr::str_sub(start, end)}")
  }


  ## Filter urls within the year
  if (!file.exists(url_path)) {
    options(timeout = max(timeout, getOption("timeout")))
    download.file(
      url      = url,
      destfile = url_path,
      mode     = "wb"
    )
  }
  ## Read raster into R
  r <- terra::rast(url_path)

  return(r)

}

