

get_landcoverexplorer_tbl <- function() {

  # 1. Posible options
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


fd_landcover_esri <- function(utm_code, year) {

  # 1. Get number and letter
  nmbr <- str_sub(utm_code, 1, 2)
  lttr <- str_sub(utm_code, 3, 3)

  # 2. Get url
  download_url <- landcover_explorer_tbl %>%
    dplyr::filter(Year == year & Number == nmbr & Letter == lttr) %>%
    dplyr::pull(download_url)

  # 3. Handle error
  if (length(download_url) == 0) stop("The UTM Code doesn't exist, is incorrect or fell into the sea. Please, use two numbers and one letter (e.g. 05T)")

  # 4. Download
  ## 4.1. Tiff file
  # tif_path <- paste0(tempdir(), "/", basename(download_url))
  # options(timeout = max(1000, getOption("timeout")))
  # download.file(
  #   url      = download_url,
  #   destfile = tif_path,
  #   mode     = "wb",
  #   quiet    = quiet
  # )
  ## 4.2. Read
  terra::rast(download_url)

}
















