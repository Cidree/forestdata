
# SPAIN -------------------------------------------------------------

## get_mfe50_ccaa_tbl

#' (Internal) Get table CCAA/MFE50
#'
#' Get a table with the Autonomous Communities of Spain, and the urls
#' to each of them.
#'
#' @return A \code{tibble}
#' @keywords internal
#' @include utils-not-exported.R
get_mfe50_ccaa_tbl <- function() {
  # 1. Read url
  url <- "https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mfe50_descargas_ccaa.html"
  url_html <- rvest::read_html(url)

  # 2. Get CCAA table
  ## 2.1. Get CCAA names
  ccaa_vec <- url_html |>
    rvest::html_element(".image-mapping") |>
    rvest::html_elements("area") |>
    rvest::html_attr("title")
  ## 2.2. Get CCAA url
  ccaa_url_vec <- url_html |>
    rvest::html_element(".image-mapping") |>
    rvest::html_elements("area") |>
    rvest::html_attr("href")
  ## 2.3. Create table and remove NA (Ceuta and Melilla)
  ccaa_tbl <- data.frame(
    ccaa = ccaa_vec,
    url  = ccaa_url_vec
  ) |> na.omit()
  ## 2.4. Return
  return(ccaa_tbl)
}

## create_mfe50_table

#' (Internal) Create MFE50 provinces table for one CCAA
#'
#' Creates a table with the province name and the url of the
#' vectorial data.
#'
#' @param url The url to the spatial data
#'
#' @return A \code{data.frame}
#' @keywords internal
create_mfe50_table <- function(url) {

  # 1. Get provinces vector
  provinces_vec <- rvest::read_html(url) |>
    rvest::html_elements(".data-table") |>
    rvest::html_elements("tr") |>
    rvest::html_element("td") |>
    rvest::html_text()
    provinces_vec <- provinces_vec[-c(1,2)]

  # 2. Get url for provinces
  url_provinces_vec <- rvest::read_html(url) |>
    rvest::html_elements(".data-table") |>
    rvest::html_elements("tr") |>
    rvest::html_elements("td") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  # 3. Create table
  data.frame(
    province = provinces_vec,
    url      = url_provinces_vec
  )
}

## get_mfe50_provinces_tbl

#' (Internal) Creates the MFE50 provinces table
#'
#' Creates a table with the province names, and the url to each province's
#' vectorial data (full table with all provinces)
#'
#' @return A \code{tibble}
#' @keywords internal
#' @importFrom rlang .data
get_mfe50_provinces_tbl <- function() {

  # 1. Get CCAA table
  ccaa_tbl <- get_mfe50_ccaa_tbl()

  # 2. Get urls for each province
  provinces_lst <- purrr::map(ccaa_tbl$url, create_mfe50_table)

  # 3. Convert list to tibble
  provinces_tbl <- provinces_lst |>
    purrr::list_rbind() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      province = fdi_fix_names(name = province)
    )

  # 4. Return
  return(provinces_tbl)

}


## fd_forest_spain_mfe50

#' Forest Cover of Spain
#'
#' Download the MFE50 (Spanish Forestry Map 1:50,000) for a province. The
#' MFE50 was built during 1997-2006.
#'
#' @param province a character string of length 1 with the name of a Spanish province
#' @param path_metadata a character string of length 1 with the path to store the
#' metadata of the MFE50. The default \code{path_metadata = NULL} does not download
#' the metadata
#' @param quiet if \code{TRUE}, suppress any message or progress bar
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
#' @seealso \link{metadata_forestdata} for a list of possible species
#'
#' @references \url{https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mfe50.html}
#'
#' @examples
#' \donttest{
#' # Download MFE50 for the province of Lugo
#' lugo_mfe50_sf <- fd_forest_spain_mfe50(province = "Lugo")
#' }
fd_forest_spain_mfe50 <- function(province,
                                  path_metadata = NULL,
                                  quiet = FALSE) {

  # 1. Get url
  ## 1.1. Fix province
  province_fix <- province |>
    fdi_fix_names()
  ## 1.2. Get url for province
  province_url <- mfe_provinces_tbl |>
    dplyr::filter(stringr::str_detect(province, province_fix)) |>
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
      mode     = "wb",
      quiet    = quiet
    )
  }
  ## 4.2. Return data
  if (!quiet) message(crayon::cyan("Visit <https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/mfe50.html> for more information on the dataset"))
  return(province_shp)

}


# FRANCE ---------------------------------------------------

#' (Internal) Creates the BD Forêt table
#'
#' Creates a table with the France department names, the url to download
#' the data, and the version
#'
#' @return A \code{tibble}
#' @keywords internal
#' @importFrom rlang .data
get_bdforet_tbl <- function() {
  # 1. Read url
  url <- "https://geoservices.ign.fr/bdforet"
  url_html <- rvest::read_html(url)

  # 2. Get the departments vector
  ## -> as for 2024-07-17 there's an error on Department 67.
  ## -> Instead of being <p>, it's <a>
  departments <- url_html |>
    rvest::html_elements(".field--item") |>
    rvest::html_elements("p") |>
    rvest::html_elements(xpath = "//p[contains(text(), 'partement ')]") |>
    rvest::html_text2() |>
    ## Clean names
    stringr::str_split(" - ") |>
    purrr::map(\(x) purrr::pluck(x, 2)) |>
    as.character() |>
    stringr::str_remove(" :") |>
    fdi_fix_names()
  ## -> Temporary fix
  departments <- append(departments, "Bas-Rhin", after = 67)

  # 3. Get url vector
  ## -> Eliminate first element: a GPKG in the begining
  download_url <- url_html |>
    rvest::html_elements(".field .field--name-field-texte") |>
    rvest::html_elements(".field--item") |>
    rvest::html_elements("ul") |>
    rvest::html_elements("li") |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  # 4. Create data frame
  tibble::tibble(
    Department   = departments,
    url          = download_url[-1],
    Version      = c(rep(2, length(departments)/2), rep(1, length(departments)/2))
  )
}


## fd_forest_france

#' BD Forêt
#'
#' Download the BD Forêt data for a French Department. This function downloads
#' the polygons of forest vegetation in France.
#'
#' @param department a character string of length 1 with the name of a
#' French department (see examples)
#' @param path_metadata a character string of length 1 with the path to store
#' the metadata of the BD Forêt database. The default \code{path_metadata = NULL}
#' does not download the metadata
#' @param version the version number of the BD Forêt data. Either 1 or 2 (see details)
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @return A \code{sf} object with \code{POLYGON} geometry
#' @export
#'
#' @details
#'
#' The BD Forêt is a database where the forest cover of France is stored by
#' department, with the spatial distribution of tree species in the country.
#'
#' The BD Forêt version 1 was developed by photointerpretation of infrared color
#' aerial images with a minimum mapped area of 2.25 hectares. The year of reference
#' for each department varies between 1987 and 2002. The version 1 contains the
#' following variables:
#'
#' - ID: surface object identifier
#'
#' - CODE_TFV: alphanumeric code of the vegetation formation
#'
#' - TFV: vegetation formation type
#'
#' - TFV_G11: type of coverage and predominant composition of the vegetation
#'            in 11 groups
#'
#' - ESSENCE: description of tree species according to the unique basic
#'            nomenclature for all departments
#'
#' The BD Forêt version 2 was developed between 2007 and 2018 by photointerpretation
#' of color infrared images from the BD ORTHO. It assigns a vegetation formation
#' type to each mapped area larger than 5,000\eqn{m^2}. This version contains the variables:
#'
#' - DEP: department name
#'
#' - CYCLE: order number of the departmental revision
#'
#' - ANREF: year of reference of the data
#'
#' - TFIFN: code of the departalmental type of vegetation cover. The nomenclature is
#'   specific to each department
#'
#' - LIBELLE: departamental type of vegetation cover. The nomenclature is
#'   specific to each department
#'
#' - LIBELLE2: departamental type of vegetation cover in capital letters.
#'   The nomenclature is specific to each department
#'
#' - TYPN: code of the national type of vegetation cover
#'
#' - NOMB_TYPN: national type of vegetation cover
#'
#' For more information, download the metadata using the argument
#' \code{path_metadata} (information in French).
#'
#'
#' @seealso \link{metadata_forestdata} for a list of possible Department names
#'
#' @references \url{https://geoservices.ign.fr/bdforet}
#'
#' @examples
#' \donttest{
#' # Download BD Foret V2 for the department of Ardèche
#' ardeche_bdforet1_sf <- fd_forest_france(department = "Ardeche", version = 1)
#' }
fd_forest_france <- function(department,
                             path_metadata = NULL,
                             version       = 2,
                             quiet         = FALSE) {

  # 0. Check for errors
  ## 0.1. Fix name
  department_fix <- fdi_fix_names(department)
  ## 0.2. Department name valid?
  if (!department_fix %in% bdforet_tbl$Department) stop("The department name is not valid. Check <metadata_forestdata$bdforet_tbl_departments> for the department names")
  if (!version %in% c(1 , 2)) stop("The valid versions are 1 or 2")

  # 1. Get download url
  download_url <- bdforet_tbl |>
    ## Filter version number (1 or 2)
    dplyr::filter(Version == version) |>
    ## Filter department name
    dplyr::filter(stringr::str_detect(Department, department_fix
    )) |>
    dplyr::pull(url)

  # 2. Download file
  ## 2.1. Url and paths
  dir_zip   <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_unzip <- stringr::str_remove(dir_zip, ".7z$")
  ## 2.2. Download and unzip (download.file very slow for .7z, find alternative)
  fdi_download_7zip(download_url, dir_unzip, dir_zip, quiet = quiet)

  # 3. Get metadata?
  if (!is.null(path_metadata)) {
    if (version == 1) {
      url_metadata <- "https://geoservices.ign.fr/sites/default/files/2021-06/DC_BDForet_1-0.pdf"
      download.file(
        url      = url_metadata,
        destfile = stringr::str_glue("{path_metadata}/{basename(url_metadata)}"),
        mode     = "wb",
        quiet    = quiet
      )
    } else if (version == 2) {
      url_metadata <- "https://geoservices.ign.fr/sites/default/files/2021-06/DC_BDForet_2-0.pdf"
      download.file(
        url      = url_metadata,
        destfile = stringr::str_glue("{path_metadata}/{basename(url_metadata)}"),
        mode     = "wb",
        quiet    = quiet
      )
    }
  }

  # 4. Get file to R
  ##  4.1.Find the shapefile
  vegetation_file <- list.files(
    path       = dir_unzip,
    pattern    = ".shp$",
    recursive  = TRUE,
    full.names = TRUE
  )
  ## 4.2. Read into R
  if (!quiet) message(crayon::cyan("Visit <https://geoservices.ign.fr/bdforet> for more information on the dataset"))
  sf::read_sf(vegetation_file)

}








