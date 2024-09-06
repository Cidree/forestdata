
## Spain Inventario Forestal Nacional ---------------------------



## get_spain_ifn_metadata_tbl ----

#' (Internal) Get table with URLs of the IFN metadata
#'
#' Get a table with the IFN and database codes, and their metadata download URL
#'
#' @return A \code{tibble}
#' @keywords internal
get_spain_ifn_metadata_tbl <- function() {

  tibble::tibble(
    ifnx = c(2, 2, 3, 3, 4, 4),
    databasex = rep(c("field", "gis"), 3),
    url = c(
      "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls",
      "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013528a_tcm30-278472.xls",
      "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdcampo_ifn3_tcm30-282240.pdf",
      "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/documentador_bdsig_ifn3_tcm30-293905.pdf",
      "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/ifn/ifn4/documentador_ifn4_campo_tcm30-536595.pdf",
      "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/documentador_sig_tcm30-536622.pdf"
    )
  ) |>
    dplyr::mutate(
      filename = paste0("metadata_ifn", ifn, "_", database, c(".xls", ".xls", rep(".pdf", 4)))
    )

}

## get_spain_ifn2_tbl ----

#' (Internal) Get table with Provinces and Urls for the Spanish IFN2
#'
#' Get a table with the Provinces of Spain, and the Urls to download data
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils_notExported.R
get_spain_ifn2_tbl <- function() {

  # 1. Read urls (there is one per each 25 provinces)
  url_al <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_1_25.html"
  url_lz <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_parcelas_26_50.html"
  url_al_html <- rvest::read_html(url_al)
  url_lz_html <- rvest::read_html(url_lz)

  # 2. Read province names
  provinces_al <- url_al_html |>
    rvest::html_elements(".anchors-list__item") |>
    rvest::html_elements("a") |>
    rvest::html_attr("title")

  provinces_lz <- url_lz_html |>
    rvest::html_elements(".anchors-list__item") |>
    rvest::html_elements("a") |>
    rvest::html_attr("title")

  # 3. Read field URLs
  field_urls <- purrr::map(
    list(url_al_html, url_lz_html),
    \(x) x |>
      rvest::html_elements(".paragraph-item") |>
      rvest::html_elements(".cmp-text") |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
  ) |>
    unlist()

  # 4. Read SIG URLs
  ## 4.1. Read URLs
  url_sig_al <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_cartografia_1_25.html"
  url_sig_lz <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn2_cartografia_26_50.html"
  url_al_sig_html <- rvest::read_html(url_sig_al)
  url_lz_sig_html <- rvest::read_html(url_sig_lz)
  ## 4.2. Read SIG
  sig_urls <- purrr::map(
    list(url_al_sig_html, url_lz_sig_html),
    \(x) x |>
      rvest::html_elements(".paragraph-item") |>
      rvest::html_elements(".cmp-text") |>
      rvest::html_elements("li") |>
      rvest::html_elements(xpath = "//li[contains(text(), 'Parcelas de campo')]") |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")
  ) |>
    unlist()
  ## 4.3. Add NULL to empty spots (Asturias, Cantabria, Navarra)
  sig_urls <- append(sig_urls, NA, after = 30)
  sig_urls <- append(sig_urls, NA, after = 32)
  sig_urls <- append(sig_urls, NA, after = 38)

  # 3. Create tibble with provinces and URLs
  tibble::tibble(
    province     = c(provinces_al, provinces_lz),
    url_field_db = field_urls,
    url_sig_db   = sig_urls
  ) |>
    dplyr::mutate(
      province     = stringr::str_sub(province, start = 4),
      url_field_db = paste0("https://www.miteco.gob.es", url_field_db),
      url_sig_db   = paste0("https://www.miteco.gob.es", url_sig_db),
    )

}



## get_spain_ifn3_tbl ----

#' (Internal) Get table with Provinces and Urls for the Spanish IFN3
#'
#' Get a table with the Provinces of Spain, and the Urls to download data
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils_notExported.R
get_spain_ifn3_tbl <- function() {

  # 1. Read urls (there is one per each 25 provinces)
  url_al <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_1_25.html"
  url_lz <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/ifn3_base_datos_26_50.html"
  url_al_html <- rvest::read_html(url_al)
  url_lz_html <- rvest::read_html(url_lz)

  # 2. Read province names
  provinces_al <- url_al_html |>
    rvest::html_elements(".anchors-list__item") |>
    rvest::html_elements("a") |>
    rvest::html_attr("title")

  provinces_lz <- url_lz_html |>
    rvest::html_elements(".anchors-list__item") |>
    rvest::html_elements("a") |>
    rvest::html_attr("title")

  provinces_df <- data.frame(
    province = c(provinces_al, provinces_lz)
  )

  # 3. Separate id and province in 2 columns, and add the urls
  provinces_df |>
    tidyr::separate_wider_delim(
      cols     = province,
      delim    = " ",
      names    = c("id", "province"),
      too_many = "merge"
    ) |>
    dplyr::mutate(
      url_field_db = paste0(
        "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/Ifn3p",
        id, ".zip"
      ),
      url_sig_db   = paste0(
        "https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/servicios/banco-datos-naturaleza/Sig_",
        id, ".zip"
      )
    )

}

## get_spain_ifn4_tbl ----

#' (Internal) Get table Provinces and Urls
#'
#' Get a table with the Provinces of Spain, and the Urls to download data
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils_notExported.R
get_spain_ifn4_tbl <- function() {
  # 1. Read url
  url <- "https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional/cuarto_inventario.html"
  url_html <- rvest::read_html(url)
  sf_menus <- url_html |>
    rvest::html_elements(".sf-menu")

  # 2. Get provinces table table
  ## 2.1. Get provinces names
  provinces <- sf_menus[1] |>
    rvest::html_elements("a") |>
    rvest::html_text2() |>
    fdi_fix_names()
  ## 2.2. Get field Database url
  field_db <- sf_menus[1] |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  ## 2.3. Get SIG database url
  sig_db <- sf_menus[2] |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")
  ## 2.4. Create table with provinces and urls
  ifn4_tbl <- tibble::tibble(
    province     = provinces,
    url_field_db = paste0("https://www.miteco.gob.es", field_db),
    url_sig_db   = paste0("https://www.miteco.gob.es", sig_db)
  )
  ## 2.4. Return
  return(ifn4_tbl)
}



#' Download Spanish Forest Inventory
#'
#' Download the tables and SIG data from the Spanish Forest Inventory
#'
#' @param province A character string of length 1 with the name of a
#'                 Spanish province
#' @param ifn Number of Spanish Forest Inventory (from 2 to 4)
#' @param database The name of the database (either 'field' or 'gis')
#' @param path_metadata A character string of length 1 with the path to store the
#' metadata of the selected database. The default \code{path_metadata = NULL}
#' does not download the metadata
#' @param quiet If \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
#'
#' @return A \code{list} with the tables
#' @export
#'
#' @seealso [metadata_forestdata] for a list of possible species
#'
#' @details
#' The IFN2 doesn't have 'gis' data for Asturias, Cantabria and Navarra.
#'
#' @references \url{https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html}
#'
#' @examples
#' \donttest{
#' # Download MFE50 for the province of Lugo
#' lugo_ifn4_lst <- fd_inventory_spain("Lugo")
#'
#' lugo_ifn3_gis_lst <- fd_inventory_spain("Lugo", ifn = 3, database = "gis")
#' }
fd_inventory_spain <- function(province,
                               ifn = 4,
                               database = "field",
                               path_metadata = NULL,
                               quiet = TRUE) {

  # 1. Filter province
  ## 1.1. Fix province
  province_fix <- province %>%
    fdi_fix_names()
  ## 1.2. Filter selected province
  ifn_data <- switch(as.character(ifn),
    "2" = ifn2_tbl,
    "3" = ifn3_tbl,
    "4" = ifn4_tbl,
    stop("Invalid IFN number. Please, choose a number from 1 to 4")
  )

  selected_province <- ifn_data %>%
    dplyr::filter(stringr::str_detect(fdi_fix_names(province), province_fix))

  # 2. Download data
  ## 2.1. Url and paths
  download_url <- if (database == "field") selected_province$url_field_db else selected_province$url_sig_db
  dir_zip      <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_unzip    <- stringr::str_remove(dir_zip, ".zip")
  ## 2.1. Download and unzip
  fdi_download_unzip(download_url, dir_unzip, dir_zip, quiet = quiet)

  # 3. Download metadata?
  if (!is.null(path_metadata)) {
    metadata_file <- ifn_metadata_tbl |>
      dplyr::filter(
        ifnx      == ifn,
        databasex == database
      )
    download.file(
      metadata_file$url,
      destfile = metadata_file$filename,
      mode     = "wb",
      quiet    = quiet
    )
  }

  # 4. Read data
  if (ifn == 2) {
    if (database == "gis") {
      ## 4.1. File name
      filename <- list.files(dir_unzip, full.names = TRUE, pattern = ".shp$")
      ## 4.2. Read file
      data_sf <- sf::read_sf(filename)
      ## 4.3. Return results
      return(data_sf)
    } else {
      ## 4.2. File name
      filename  <- list.files(dir_unzip, full.names = TRUE)
      ## 4.3. Read files
      data_lst <- purrr::map(
        filename,
        \(x) foreign::read.dbf(x) |> tibble::as_tibble()
      )
      ## 4.4. Rename tables
      names(data_lst) <- sub(".DBF", "", list.files(dir_unzip))
      ## 4.5. Return results
      return(data_lst)
    }
  } else {
    ## 4.1. File name
    filename <- list.files(dir_unzip, full.names = TRUE)
    ## 4.2. Connect to DB
    conn <- RODBC::odbcConnectAccess2007(filename)
    ## 4.3. Table names
    tables_vec <- RODBC::sqlTables(conn) |>
      dplyr::filter(TABLE_TYPE != "SYSTEM TABLE") |>
      dplyr::pull(TABLE_NAME)
    ## 4.4. Read data into a list
    data_lst <- purrr::map(
      .x = tables_vec,
      .f = \(x) RODBC::sqlFetch(conn, x) |>
        tibble::as_tibble()
    )
    ## 4.5. Disconnect from DB
    RODBC::odbcClose(conn)
    ## 4.6. Rename list
    names(data_lst) <- tables_vec
    ## 4.7. Convert IFN4 PCDatosMap to projected SF (only in field database)
    ## -> IFN3 doesn't have huso column
    if (ifn == 4 & database == "field") {
      data_lst$PCDatosMap_sf <- sf::st_as_sf(
        x      = data_lst$PCDatosMap,
        coords = c("CoorX", "CoorY"),
        crs    = paste0("EPSG:", 258, data_lst$PCDatosMap$Huso[1])
      )
    }
    ## 4.7. Return results
    return(data_lst)
  }


}













