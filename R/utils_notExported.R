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


