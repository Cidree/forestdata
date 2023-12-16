
get_mfe_ccaa_tbl <- function() {
  # 1. Read url
  url <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mfe50_descargas_ccaa.html"
  url_html <- rvest::read_html(url)

  # 2. Get CCAA table
  ## 2.1. Get CCAA names
  ccaa_vec <- url_html %>%
    rvest::html_element(".image-mapping") %>%
    rvest::html_elements("area") %>%
    rvest::html_attr("title")
  ## 2.2. Get CCAA url
  ccaa_url_vec <- url_html %>%
    rvest::html_element(".image-mapping") %>%
    rvest::html_elements("area") %>%
    rvest::html_attr("href")
  ## 2.3. Create table and remove NA (Ceuta and Melilla)
  ccaa_tbl <- data.frame(
    ccaa = ccaa_vec,
    url  = ccaa_url_vec
  ) %>% na.omit()
  ## 2.4. Return
  return(ccaa_tbl)
}

create_mfe_table <- function(url) {

  # 1. Get provinces vector
  provinces_vec <- read_html(url) %>%
    html_elements(".data-table") %>%
    html_elements("tr") %>%
    html_element("td") %>%
    html_text() %>%
    .[-c(1,2)]

  # 2. Get url for provinces
  url_provinces_vec <- read_html(url) %>%
    html_elements(".data-table") %>%
    html_elements("tr") %>%
    html_elements("td") %>%
    html_elements("a") %>%
    html_attr("href")

  # 3. Create table
  data.frame(
    province = provinces_vec,
    url      = url_provinces_vec
  )
}

get_mfe_provinces_tbl <- function() {

  # 1. Get CCAA table
  ccaa_tbl <- get_mfe_ccaa_tbl()

  # 2. Get urls for each province
  provinces_lst <- purrr::map(ccaa_tbl$url, create_mfe_table)

  # 3. Convert list to tibble
  provinces_tbl <- provinces_lst %>%
    purrr::list_rbind() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      province = fdi_fix_names(province)
    )

  # 4. Return
  return(provinces_tbl)

}

fd_forest_spain_mfe <- function(province, path_metadata = NULL, quiet = TRUE) {

  # 1. Get url
  ## 1.1. Fix province
  province_fix <- province %>%
    fdi_fix_names()
  ## 1.2. Get url for province
  province_url <- mfe_provinces %>%
    dplyr::filter(str_detect(province, province_fix)) %>%
    dplyr::pull(url)
  ## 1.3. Fix URL (incomplete)
  province_url <- paste0("https://www.miteco.gob.es", province_url)

  # 2. Download file
  ## 2.1. Url and paths
  download_url <- province_url
  dir_unzip    <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_zip      <- stringr::str_glue("{dir_unzip}.zip")
  ## 2.1. Download and unzip
  fdi_download_unzip(download_url, dir_unzip, dir_zip, quiet = quiet)

  # 3. Read vectorial data
  ## 3.1. Get file name
  dir_shp <- list.files(
    path       = dir_unzip,
    pattern    = ".shp$",
    full.names = TRUE
  )
  ## 3.2. Read it into R
  province_shp <- sf::read_sf(dir_shp)

  # 4. Return depending on path_metadata
  ## 4.1. If path_metadata is supplied, download it
  if (!is.null(path_metadata)) {
    url_metadata <- "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/MFE50_DD_tcm30-154309.xls"
    download.file(
      url      = url_metadata,
      destfile = stringr::str_glue("{path_metadata}/{basename(url_metadata)}"),
      mode     = "wb"
    )
  }
  ## 4.2. Return data
  return(province_shp)

}





