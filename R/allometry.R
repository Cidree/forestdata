
## TALLO database -------------

#' TALLO database
#'
#' Downloads the TALLO database, a global tree allometry and crown architecture
#' database. Over 500,000 data points of individual trees with several measurements
#'
#' @param country a character vector with either ISO2 codes, ISO3 codes or
#' full country names (not mixed) to filter out the data
#' @param spatial logical. Whether to retrieve a `tibble` or a `sf` object
#' @param metadata_path a character string of length 1 with the path to store the
#' metadata and bibliography. The default \code{path_metadata = NULL}
#' does not download the metadata
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @return a \code{tibble} or a \code{sf} object
#' @export
#'
#' @importFrom utils read.csv write.csv
#'
#' @references Tallo: A global tree allometry and crown architecture database.
#' \url{https://doi.org/10.1111/gcb.16302}
#'
#' @examples
#' \donttest{
#' ## Download full database as tibble
#' tallo_tbl <- fd_allometry_tallo()
#'
#' ## Download full database as sf
#' tallo_sf <- fd_allometry_tallo(spatial = TRUE)
#'
#' ## Download data as sf for Czechia and Germany
#' tallo_cz_ge_sf <- fd_allometry_tallo(country = c("Czechia", "Germany"))
#' }
fd_allometry_tallo <- function(country = NULL,
                               spatial = FALSE,
                               metadata_path = NULL,
                               quiet = FALSE) {

  # 0. Handle errors


  # 1. Read data
  ## 1.1. Tallo database
  data_tbl <- read.csv("https://zenodo.org/records/6637599/files/Tallo.csv?download=1") |>
    tibble::as_tibble()
  ## 1.2. Metadata and bibliography
  if (!is.null(metadata_path)) {

    ## temporary file names
    metadata_path   <- stringr::str_glue("{metadata_path}/tallo_metadata.csv")
    references_path <- stringr::str_glue("{metadata_path}/tallo_references.csv")
    ## remove files if they exist
    if (file.exists(metadata_path)) file.remove(metadata_path)
    if (file.exists(references_path)) file.remove(references_path)
    ## save into selected path
    write.csv("https://zenodo.org/records/6637599/files/Tallo_metadata.csv?download=1", metadata_path)
    write.csv("https://zenodo.org/records/6637599/files/Tallo_references.csv?download=1", references_path)
  }

  # 2. Filters
  ## 2.1. Convert to spatial if selected
  if (spatial | !is.null(country)) {
    ## message if spatial = FALSE, and country are selected
    if (!spatial & !is.null(country)) message("You selected a country, so the function will use `spatial = TRUE` automatically")
    data_tbl <- sf::st_as_sf(
      data_tbl,
      coords = c("longitude", "latitude"),
      crs    = "EPSG:4326"
    )
    ## If spatial filter applied
    if (!is.null(country)) {
      ## check if giscoR is installed
      if (!requireNamespace("giscoR", quietly = TRUE)) stop("Package `giscoR` is needed for filtering by country/continent. Please, install it.")
      ## filter countries depending on ISO2, ISO3 or full name
      countries_sf <-
        switch(as.character(nchar(country[1])),

                "2" = giscoR::gisco_get_countries() |>
                  dplyr::filter(CNTR_ID %in% toupper(country)),

                "3" = giscoR::gisco_get_countries() |>
                  dplyr::filter(ISO3_CODE %in% toupper(country)),

                giscoR::gisco_get_countries() |>
                  dplyr::filter(NAME_ENGL %in% stringr::str_to_title(country))
        )
      ## filter points belonging to the selected countries
      data_tbl <- sf::st_filter(
        x = data_tbl,
        y = countries_sf
      )
    }

  }

  # 3. Return final object
  if (!quiet) message(crayon::cyan("Cite this dataset using <https://doi.org/10.1111/gcb.16302>"))
  return(data_tbl)

}




