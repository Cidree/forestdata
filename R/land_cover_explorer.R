
## get_landcoverexplorer_tbl ----

#' (Internal) Get codes table of Land Cover Explorer
#'
#' Get a table with the Land Cover explorer codes and urls
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils_notExported.R
#'
#' @examples
#' \dontrun{
#' get_landcoverexplorer_tbl()
#' }
get_landcoverexplorer_tbl <- function() {

  # 1. Possible options
  years   <- 2017:2022
  letters <- LETTERS[-c(1, 2, 9, 15, 25:26)]
  nmbrs   <- 1:60
  nmbrs   <- sprintf("%02d", nmbrs)

  # 2. Table with options
  expand.grid(Year = years, Number = nmbrs, Letter = letters) %>%
    dplyr::mutate(download_url =
                    stringr::str_glue("https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc{Year}/{Number}{Letter}_{Year}0101-{Year + 1}0101.tif")
    )
}


## fd_esri_landcover ----

#' Download a Land Cover SpatRaster for a UTM tile
#'
#' Download a UTM tile of the ESRI Land Cover Explorer for a specified year
#'
#' @param utm_code A character string of length 1 with the name of a
#'                 spanish province
#' @param year An integer or vector of integers corresponding to the base year
#'             of the Land Cover image. The option year = 'all' download all
#'             the available images.
#' @param quiet If \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
#'
#' @return A \code{SpatRaster}
#' @export
#'
#' @details
#' The Land Cover Explorer ...
#'
#' @references <https://livingatlas.arcgis.com/en/home/>
#'
#' @examples
#' \dontrun{
#' # Download Land Cover for UTM tile 29N year 2022
#' lc <- fd_landcover_esri("29N", 2017:2020)
#' }
fd_landcover_esri <- function(utm_code,
                              year,
                              quiet = TRUE) {

  # 0. Handle year error
  if (!(year %in% seq(2017, 2022, 1)) & year != "all") stop("The indicated year is incorrect. Please, use a year between 2017 and 2022, or the 'all' function to retrieve all.")

  # 1. Get number and letter
  nmbr <- stringr::str_sub(utm_code, 1, 2)
  lttr <- stringr::str_sub(utm_code, 3, 3)

  # 2. Get url
  if (year == "all") {
    download_url <- landcover_explorer_tbl %>%
      dplyr::filter(Number == nmbr & Letter == lttr) %>%
      dplyr::pull(download_url)
  } else {
    download_url <- landcover_explorer_tbl %>%
      dplyr::filter(Year %in% year & Number == nmbr & Letter == lttr) %>%
      dplyr::pull(download_url)
  }

  # 3. Handle error
  if (length(download_url) == 0) stop("The UTM Code doesn't exist, is incorrect or fell into the sea. Please, use two numbers and one letter (e.g. 05T)")

  # 4. Download
  ## 4.1. Tiff file
  tif_path <- paste0(tempdir(), "/", basename(download_url))
  options(timeout = max(1000, getOption("timeout")))
  purrr::map2(
    download_url,
    tif_path,
    \(x, y) download.file(
      url      = x,
      destfile = y,
      mode     = "wb",
      quiet    = quiet
    )
  )

  # 5. Read into R
  ## 5.1. Read file
  lc_sr <- terra::rast(tif_path)
  ## 5.2. Layer names
  lc_names <- stringr::str_sub(basename(download_url), 1, 8)
  names(lc_sr) <- lc_names
  ## 5.3. Return object
  return(lc_sr)

}













