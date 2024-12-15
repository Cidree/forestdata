


#' Spanish Forest Inventory
#'
#' Download the tables and SIG data from the Spanish Forest Inventory
#'
#' @param province a character string of length 1 with the name of a
#'                 Spanish province
#' @param ifn number of Spanish Forest Inventory (from 2 to 4)
#' @param database the name of the database (either 'field' or 'gis')
#' @param process_level integer. Used when \code{database = 'field'}. Level
#' of process of raw data.
#' @param path_metadata a character string of length 1 with the path to store the
#' metadata of the selected database. The default \code{path_metadata = NULL}
#' does not download the metadata
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @return A \code{list} with the tables
#' @export
#'
#' @seealso \link{metadata_forestdata} for a list of possible species
#'
#' @details
#' The IFN2 doesn't have 'gis' data for Asturias, Cantabria and Navarra.
#'
#'
#' @references \url{https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html}
#'
#' @examples
#' \dontrun{
#' library(odbc)
#' if ("Microsoft Access Driver (*.mdb, *.accdb)" %in% odbc::odbcListDrivers()$name) {
#'   # Download MFE50 for Canary Islands
#'   canarias_ifn4_lst <- fd_inventory_spain("Canarias")
#' } else {
#'   message("Skipping example as <Microsoft Access Driver (*.mdb, *.accdb)> is not available.")
#' }
#' }
fd_inventory_spain <- function(province,
                               ifn           = 4,
                               database      = "field",
                               process_level = 0,
                               path_metadata = NULL,
                               quiet         = FALSE) {
  # 0. Handle errors
  if (!requireNamespace("RODBC", quietly = TRUE)) cli::cli_abort("Package `RODBC` is required to access the inventory data. Please, install it.")
  if (!"Microsoft Access Driver (*.mdb, *.accdb)" %in% odbc::odbcListDrivers()$name) {
    cli::cli_abort("<Microsoft Access Driver (*.mdb, *.accdb)> is not available. Please, install it to use this function.")
  }
  # 1. Filter province
  ## 1.1. Fix province
  province_fix <- province |>
    fdi_fix_names()
  ## 1.2. Filter selected province
  ifn_data <- switch(as.character(ifn),
                     "2" = ifn2_tbl,
                     "3" = ifn3_tbl,
                     "4" = ifn4_tbl,
                     cli::cli_abort("Invalid IFN number. Please, choose a number from 2 to 4")
  )

  selected_province <- ifn_data |>
    dplyr::filter(stringr::str_detect(fdi_fix_names(province), province_fix))

  # 2. Download data
  ## 2.1. Url and paths
  download_url <- if (database == "field") selected_province$url_field_db else selected_province$url_sig_db
  dir_zip      <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_unzip    <- stringr::str_remove(dir_zip, ".zip")
  ## 2.1. Download and unzip
  if (!quiet) cli::cli_progress_step("Downloading data...", "Downloaded", "Download failed")
  dwld <- fdi_download_unzip(download_url, dir_unzip, dir_zip)
  if (!dwld) {
    cli::cli_process_failed()
    return(cli::cli_alert_danger("`fd_inventory_spain()` failed to retrieve the data. Service might be currently unavailable"))
  }
  if (!quiet) cli::cli_progress_step("Preparing data...", "Prepared")

  # 3. Download metadata?
  if (!is.null(path_metadata)) {
    metadata_file <- ifn_metadata_tbl |>
      dplyr::filter(
        ifnx      == ifn,
        databasex == database
      )
    ## download only if it doesn't exist
    if (!file.exists(metadata_file$filename)) {
      meta_dwld <- fdi_download(
        metadata_file$url,
        destfile = paste0(path_metadata, "/", metadata_file$filename)
      )
      if (!meta_dwld & !quiet) cli::cli_alert_danger("`fd_inventory_spain()` failed to retrieve the metadata. Service might be currently unavailable, or province data is not published yet.")
    }
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
    filename <- list.files(dir_unzip, full.names = TRUE, pattern = "\\.accdb$|\\.mdb$")
    ## 4.2. Connect to DB
    conn <- RODBC::odbcConnectAccess2007(filename)
    on.exit(RODBC::odbcClose(conn))
    ## 4.3. Table names
    tables_vec <- RODBC::sqlTables(conn) |>
      dplyr::filter(TABLE_TYPE != "SYSTEM TABLE") |>
      dplyr::pull(TABLE_NAME)
    ## 4.5. Read data into a list
    data_lst <- purrr::map(
      .x = tables_vec,
      .f = \(x) RODBC::sqlFetch(conn, x) |>
        tibble::as_tibble()
    )
    ## 4.6. Rename list
    names(data_lst) <- tables_vec
    ## 4.7. Convert IFN4 PCDatosMap to projected SF (only in field database)
    ## -> IFN3 doesn't have huso column
    if (database == "field") {
      if (ifn == 4) {
        ## get datum code based on province/ccaa
        datum <- dplyr::case_when(
          province_fix %in% c("Navarra", "Lugo", "A Coruna", "Lugo", "Pontevedra",
                              "Ourense", "Asturias", "Cantabria", "Murcia", "Baleares",
                              "Pais Vasco", "La Rioja", "Madrid", "Cataluna") ~ 230,
          province_fix %in% c("Canarias") ~ 326,
          .default = 258
        )
        ## convert to spatial
        na_data <- data_lst$PCDatosMap |> dplyr::filter(is.na(CoorX))
        if (nrow(na_data) > 0) cli::cli_alert_warning("Plot(s) {paste0(na_data$Estadillo, collapse = ',')} do not have coordinates, and are eliminate from `PCDatosMap_sf` table")
        data_lst$PCDatosMap_sf <- sf::st_as_sf(
          x      = data_lst$PCDatosMap |> dplyr::filter(!is.na(CoorX)),
          coords = c("CoorX", "CoorY"),
          crs    = paste0("EPSG:", datum, data_lst$PCDatosMap$Huso[1])
        )

        ## IFN 3 - SF column
      } else {
        data_lst$PCDatosMap <- data_lst$PCDatosMap |>
          dplyr::mutate(
            Huso = dplyr::case_when(
              ## Huso 29
              province_fix %in% c("Lugo", "Ourense", "A Coruna", "Pontevedra") ~ 29,
              province_fix %in% c("Sevilla", "Badajoz", "Caceres", "Salamanca", "Cadiz",
                                  "Zamora", "Leon", "Asturias", "Huelva") & CoorX > 5e5 ~ 29,
              ## Huso 28
              province_fix %in% c("Santa Cruz De Tenerife", "Las Palmas") ~ 28,
              ## Huso 31
              province_fix %in% c("Islas Baleares", "Barcelona", "Girona", "Lleida", "Tarragona") ~ 31,
              province_fix %in% c("Castellon", "Huesca", "Zaragoza",
                                  "Teruel", "Alicante") & CoorX < 5e5 ~ 31,
              .default = 30
            )
          )
        ## convert to spatial
        ## some provinces have 2 different CRS (they use topographic map sheets)
        husos <- unique(data_lst$PCDatosMap$Huso)
        if (length(husos) > 1) {
          ## they are always 2 different CRS
          data_huso_1 <- sf::st_as_sf(
            x      = data_lst$PCDatosMap[data_lst$PCDatosMap$Huso == husos[1], ],
            coords = c("CoorX", "CoorY"),
            crs    = paste0("EPSG:230", husos[1])
          )

          data_huso_2 <- sf::st_as_sf(
            x      = data_lst$PCDatosMap[data_lst$PCDatosMap$Huso == husos[2], ],
            coords = c("CoorX", "CoorY"),
            crs    = paste0("EPSG:230", husos[2])
          ) |> sf::st_transform(paste0("EPSG:230", husos[1]))

          data_lst$PCDatosMap_sf <- rbind(data_huso_1, data_huso_2)

        } else {
          ## for only 1 CRS
          data_lst$PCDatosMap_sf <- sf::st_as_sf(
            x      = data_lst$PCDatosMap,
            coords = c("CoorX", "CoorY"),
            crs    = paste0("EPSG:230", data_lst$PCDatosMap$Huso[1])
          )
        }

      }

    }
    ## 4.7. Process level
    if (database == "field" & ifn %in% c(3, 4) & process_level > 0) {
      final_data <- process_ifn(data_lst, process_level = process_level, ifn = ifn, province_fix)

    } else {
      final_data <- data_lst
    }
    if (!quiet) cli::cli_process_done()
    return(final_data)

  }


}













