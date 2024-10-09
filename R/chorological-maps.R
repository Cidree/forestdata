# get_chorological_tbl

#'  Chorological Maps table
#'  Get a table with the names and references of each species
#'
#' @return A \code{tibble}
#' @keywords internal
#'
get_chorological_tbl <- function() {

  ## Get website html
  cm_html <- rvest::read_html("https://forest.jrc.ec.europa.eu/en/european-atlas/atlas-data-and-metadata/")

  ## Get the species list
  species_vec <- cm_html |>
    rvest::html_elements("div") |>
    rvest::html_elements(".row") |>
    rvest::html_elements(".chp-latin") |>
    rvest::html_text2() |>
    stringr::str_trim() |>
    stringr::str_remove_all("\\u2514\\u2500 ")

  ## Get the species links
  urls_vec <- cm_html |>
    rvest::html_elements("div") |>
    rvest::html_elements(".row") |>
    rvest::html_elements(".chp-cho") |>
    rvest::html_element("a") |>
    rvest::html_attr("href")

  ## Create a dataset
  ## - Fix typo error in website of umilis -> humilis
  ## - Eliminate species with no polygon data
  choro_tbl <- tibble::tibble(
    Species = species_vec,
    DOI     = urls_vec
  ) |>
    tidyr::drop_na() |>
    dplyr::mutate(Species = ifelse(Species == "Chamaerops umilis", "Chamaerops humilis", Species)) |>
    dplyr::filter(
      !Species %in% c("Cedrus atlantica", "Phoenix theophrasti")
    )

  ## Add codes
  choro_tbl <- choro_tbl |>
    dplyr::mutate(code = DOI |>
                    stringr::str_split("\\.")|>
                    purrr::map(purrr::pluck, 5)) |>
    dplyr::mutate(code = as.character(code))

  ## Return table
  return(choro_tbl)
}


# fd_forest_chorological

#' Download the Chorological Maps
#'
#' Download the Chorological Maps for the main European Woody Species.
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @param species a character vector with the Latin name of a tree species
#' contained in the Chorological Maps database (see details)
#' @param range the default "\code{nat}" downloads the probable native range
#' of the species, while "\code{syn}" downloads the synanthropic range
#' (i.e. the introduced and naturalized area and isolated population since Neolithic)
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @return \code{sf} object
#' @importFrom utils download.file unzip
#' @export
#'
#' @details
#' The chorological maps provide a general overview of the distribution of
#' the main European woody species. The geodatabase was formed by the
#' combination of numerous and heterogeneous data for a continental-scale
#' overview of the species' distribution range. There are a total of 4 versions
#' available, and the function will get the most recent version for each
#' of the species. This means for instance that some species may be on
#' version 2, and therefore, the data from that version will be retrieved.
#'
#' @references Caudullo, G., Welk, E., San-Miguel-Ayanz, J., 2017.
#' Chorological maps for the main European woody species. Data in Brief 12,
#' 666. DOI: doi.org/10.1016/j.dib.2017.05.007
#'
#' @seealso \link{metadata_forestdata} for a list of possible species
#'
#' @examples
#' \donttest{
#'  # Download data for sweet chestnut
#'  chestnut_nat_sf <- fd_forest_chorological(species = "Castanea sativa", range = "nat")
#'
#'  # Plot the data
#'  plot(chestnut_nat_sf$geometry)
#'  }
fd_forest_chorological <- function(species, range = "nat", quiet = FALSE) {

  # 1. Get species table
  choro_tbl <- get_chorological_tbl()

  # 2. Filter user species name
  ## 2.1. Stop if species is incorrect
  if (!species %in% choro_tbl$Species) stop("The species name is not supported.")
  ## 2.2. Filter species
  user_species_tbl <- choro_tbl |>
    dplyr::filter(Species == species)

  # 3. Download the species to tempdir
  ## 3.1. Create download links
  download_url       <- stringr::str_glue("https://figshare.com/ndownloader/articles/{user_species_tbl$code}/versions")
  user_species_zip   <- stringr::str_glue("{tempdir()}/{user_species_tbl$code}.zip")
  user_species_unzip <- stringr::str_glue("{tempdir()}/{user_species_tbl$code}")
  if (file.exists(user_species_zip)) file.remove(user_species_zip)
  if (file.exists(user_species_unzip)) unlink(user_species_unzip, recursive = TRUE, force = TRUE)
  ## 3.2. Download version 5
  try(
    suppressWarnings(
      file.d <- download.file(
        url      = stringr::str_glue("{download_url}/5"),
        destfile = user_species_zip,
        quiet    = quiet,
        mode     = "wb"
      )
    ),
    silent = TRUE
  )
  ## 3.3. Handle when version 5 is not available
  ## Download version 4
  if (!exists("file.d", mode = "integer")) {
    try(
      suppressWarnings(
        file.d <- download.file(
          url      = stringr::str_glue("{download_url}/4"),
          destfile = user_species_zip,
          quiet    = quiet,
          mode     = "wb"
        )
      ),
      silent = TRUE
    )
  }
  ### Version 3
  if (!exists("file.d", mode = "integer")) {
    try(
      suppressWarnings(
        file.d <- download.file(
          url      = stringr::str_glue("{download_url}/3"),
          destfile = user_species_zip,
          quiet    = quiet,
          mode     = "wb"
        )
      ),
      silent = TRUE
    )
  }
  ### Version 2
  if (!exists("file.d", mode = "integer")) {
    try(
      suppressWarnings(
        file.d <- download.file(
          url      = stringr::str_glue("{download_url}/2"),
          destfile = user_species_zip,
          quiet    = quiet,
          mode     = "wb"
        )
      ),
      silent = TRUE
    )
  }
  ### Version 1
  if (!exists("file.d", mode = "integer")) {
    suppressWarnings(
      file.d <- download.file(
        url      = stringr::str_glue("{download_url}/1"),
        destfile = user_species_zip,
        quiet    = quiet,
        mode     = "wb"
      )
    )
  }
  ## 3.4. Unzip
  unzip(
    zipfile = user_species_zip,
    exdir   = user_species_unzip
  )

  # 4. Get the shapefile
  ## 4.1. Create paths
  species_shp <- stringr::str_glue("{user_species_tbl$Species |> stringr::str_replace_all(' ', '_')}_shp") |>
    stringr::str_replace_all("-", "")
  user_species_shp_zip   <- stringr::str_glue("{user_species_unzip}/{species_shp}.zip")
  user_species_shp_unzip <- stringr::str_glue("{user_species_unzip}/{species_shp}")
  ## 4.2. Unzip the shapefile
  unzip(
    zipfile = user_species_shp_zip,
    exdir   = user_species_shp_unzip
  )
  ## 4.2. Check for range
  if (range == "nat") {
    ## check files which contain "plg" and end with ".shp
    files <- list.files(user_species_shp_unzip, pattern = "plg.*\\.shp$")
    ## eliminate files containing "syn
    files_filtered <- files[!grepl("syn|pnt", files)]
    ## sort decreasing and take first one (the one with clip if exists)
    selected_file <- sort(files_filtered, decreasing = TRUE)[1]
    if (is.na(selected_file)) stop(stringr::str_glue("There's no range = `{range}` for {species}"))
    path_shp <- stringr::str_glue("{user_species_shp_unzip}/{selected_file}")
  } else if (range == "syn") {
    ## check files which contain "syn" and end with ".shp
    files <- list.files(user_species_shp_unzip, pattern = "syn.*\\.shp$")
    ## eliminate files containing "syn
    files_filtered <- files[!grepl("pnt", files)]
    ## sort decreasing and take first one (the one with clip if exists)
    selected_file <- sort(files_filtered, decreasing = TRUE)[1]
    if (is.na(selected_file)) stop(stringr::str_glue("There's no range = `{range}` for {species}"))
    path_shp <- stringr::str_glue("{user_species_shp_unzip}/{selected_file}")
  }
  ## 4.3. Read file
  if (!quiet) message(crayon::cyan("Cite this dataset using <https://doi.org/10.1016/j.dib.2017.05.007>"))
  sf::read_sf(path_shp)

}










