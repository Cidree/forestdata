# get_chorological_tbl

#'  Chorological Maps table
#'  Get a table with the names and references of each species
#'
#' @return \code{tibble}
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' get_chorological_tbl()
#' }
get_chorological_tbl <- function() {

  ## Get website html
  cm_html <- rvest::read_html("https://forest.jrc.ec.europa.eu/en/european-atlas/atlas-data-and-metadata/")

  ## Get the species list
  species_vec <- cm_html %>%
    rvest::html_elements("div") %>%
    rvest::html_elements(".row") %>%
    rvest::html_elements(".chp-latin") %>%
    rvest::html_text2() %>%
    stringr::str_trim() %>%
    stringr::str_remove_all("\\u2514\\u2500 ")

  ## Get the species links
  urls_vec <- cm_html %>%
    rvest::html_elements("div") %>%
    rvest::html_elements(".row") %>%
    rvest::html_elements(".chp-cho") %>%
    rvest::html_element("a") %>%
    rvest::html_attr("href")

  ## Create a dataset
  choro_tbl <- tibble::tibble(
    Species = species_vec,
    DOI     = urls_vec
  ) %>%
    tidyr::drop_na()

  ## Add codes
  choro_tbl <- choro_tbl %>%
    dplyr::mutate(code = DOI %>%
                    stringr::str_split("\\.")%>%
                    purrr::map(purrr::pluck, 5)) %>%
    dplyr::mutate(code = as.character(code))

  ## Return table
  return(choro_tbl)
}


# get_eutrees4f_tbl

#'  EU-Trees4F species table
#'  Get a table with the names and references of each species
#'
#' @return \code{tibble}
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' get_chorological_tbl()
#' }
get_eutrees4f_tbl

