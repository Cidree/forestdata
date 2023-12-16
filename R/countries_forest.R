# get_mfe50_ccaa_tbl ----

#' (Internal) Get table CCAA/MFE50
#'
#' Get a table with the Autonomous Communities of Spain, and the urls
#' to each of them.
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils_notExported.R
#'
#' @examples
#' \dontrun{
#' get_mfe50_ccaa_tbl()
#' }
get_mfe50_ccaa_tbl <- function() {
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

# create_mfe50_table ----

#' (Internal) Create MFE50 provinces table for one CCAA
#'
#' Creates a table with the province name and the url of the
#' vectorial data.
#'
#' @param url The url to the spatial data
#'
#' @return A \code{data.frame}
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' ccaa_tbl <- get_mfe50_ccaa_tbl()
#' create_mfe50_table(ccaa_tbl[1,2])
#' }
create_mfe50_table <- function(url) {

  # 1. Get provinces vector
  provinces_vec <- rvest::read_html(url) %>%
    rvest::html_elements(".data-table") %>%
    rvest::html_elements("tr") %>%
    rvest::html_element("td") %>%
    rvest::html_text() %>%
    .[-c(1,2)]

  # 2. Get url for provinces
  url_provinces_vec <- rvest::read_html(url) %>%
    rvest::html_elements(".data-table") %>%
    rvest::html_elements("tr") %>%
    rvest::html_elements("td") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href")

  # 3. Create table
  data.frame(
    province = provinces_vec,
    url      = url_provinces_vec
  )
}

# get_mfe50_provinces_tbl ----

#' (Internal) Creates the MFE50 provinces table
#'
#' Creates a table with the province names, and the url to each province's
#' vectorial data (full table with all provinces)
#'
#' @return A \code{tibble}
#' @keywords internal
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_mfe50_provinces_tbl()
#' }
get_mfe50_provinces_tbl <- function() {

  # 1. Get CCAA table
  ccaa_tbl <- get_mfe50_ccaa_tbl()

  # 2. Get urls for each province
  provinces_lst <- purrr::map(ccaa_tbl$url, create_mfe50_table)

  # 3. Convert list to tibble
  provinces_tbl <- provinces_lst %>%
    purrr::list_rbind() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      province = fdi_fix_names(name = province)
    )

  # 4. Return
  return(provinces_tbl)

}


# fd_forest_spain_mfe50 ----

#' Download MFE50 for a province
#'
#' Download the MFE50 (Spanish Forestry Map 1:50,000) for a province. The
#' MFE50 was built during 1997-2006.
#'
#' @param province A character string of length 1 with the name of a
#'                 spanish province
#' @param path_metadata A character string of length 1 with the path
#'                      to store the metadata of the MFE50. The default
#'                      \code{path_metadata = NULL} does not download the
#'                      metadata
#' @param quiet If \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
#'
#' @return A \code{sf} object with \code{POLYGON} geometry
#' @export
#'
#' @details
#' The Spanish Forestry Map at scale 1:50,000 is a project that was undertaken
#' during the years 1997-2006. The data contains the cartography of forest
#' stands in Spain. The definition of the variables is contained in an excel
#' file that can be downloaded by using the argument \code{path_metadata}.
#'
#' @references <https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mfe50.html>
#'
#' @examples
#' \dontrun{
#' # Download MFE50 for the province of Lugo
#' lugo_mfe50_sf <- fd_forest_spain_mfe50(province = "Lugo") # works
#' lugo_mfe50_sf <- fd_forest_spain_mfe50(province = "luGo") # also works
#'
#' # Download MFE50 for the province of Córdoba with metadata
#' cordoba_mfe50_sf <- fd_forest_spain_mfe50(province = "Cordoba", metadata = getwd())
#' }
fd_forest_spain_mfe50 <- function(province,
                                  path_metadata = NULL,
                                  quiet = TRUE) {

  # 1. Get url
  ## 1.1. Fix province
  province_fix <- province %>%
    fdi_fix_names()
  ## 1.2. Get url for province
  province_url <- mfe50_provinces %>%
    dplyr::filter(stringr::str_detect(province, province_fix)) %>%
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



