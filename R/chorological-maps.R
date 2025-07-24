


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

  # 1. Request url
  ## 1.1. Species code
  species_code <- chorological_tbl$code[chorological_tbl$Species == species]
  ## 1.2. Url
  url <- paste0("https://api.figshare.com/v2/articles/", species_code)
  ## 1.3. Get article metadata
  req <- httr2::request(url) |>
    httr2::req_user_agent("R httr2 client") |>
    httr2::req_perform()
  ## 1.4. Get metadata
  resp_json <- httr2::resp_body_json(req)

  # 2. Get download url
  ## 2.1. Table with downloadable files
  files_info <- data.frame(
    name = sapply(resp_json$files, function(x) x$name),
    download_url = sapply(resp_json$files, function(x) x$download_url),
    type = sapply(resp_json$files, function(x) x$mimetype)
  )
  ## 2.2. Filter the zip folder
  zip_info <- files_info[files_info$type == "application/zip", ]
  zip_info <- files_info[grep(".zip", files_info$name), ]

  # 3. Download to tempdir
  ## 3.1. Download
  dir_zip   <- paste0(tempdir(), "/", zip_info$name)
  dir_unzip <- paste0(tempdir(), "/", basename(zip_info$download_url))
  if (!quiet) cli::cli_progress_step("Downloading data...", msg_done = "Downloaded", "Download failed")
  file.d <- fdi_download(zip_info$download_url, destfile = dir_zip)
  ## 3.2. Check for success
  if (!file.d) {
    cli::cli_process_failed()
    return(cli::cli_alert_danger("`fd_forest_chorological()` failed to retrieve the data. Service might be currently unavailable"))
  }
  # 4. Get the shapefile
  ## 4.1. Unzip
  unzip(dir_zip, exdir = dir_unzip, overwrite = TRUE)
  ## 4.2. Check for range
  if (range == "nat") {
    ## check files which contain "plg" and end with ".shp
    files <- list.files(dir_unzip, pattern = "plg.*\\.shp$")
    ## eliminate files containing "syn
    files_filtered <- files[!grepl("syn|pnt", files)]
    ## sort decreasing and take first one (the one with clip if exists)
    selected_file <- sort(files_filtered, decreasing = TRUE)[1]
    if (is.na(selected_file)) cli::cli_abort("There's no range = `{range}` for {species}")
    path_shp <- paste0(dir_unzip, "/", selected_file)
  } else if (range == "syn") {
    ## check files which contain "syn" and end with ".shp
    files <- list.files(dir_unzip, pattern = "syn.*\\.shp$")
    ## eliminate files containing pnt
    files_filtered <- files[!grepl("pnt", files)]
    ## sort decreasing and take first one (the one with clip if exists)
    selected_file <- sort(files_filtered, decreasing = TRUE)[1]
    if (is.na(selected_file)) cli::cli_abort("There's no range = `{range}` for {species}")
    path_shp <- paste0(dir_unzip, "/", selected_file)
  }
  ## 4.3. Read file
  if (!quiet) cli::cli_process_done()
  if (!quiet) cli::cli_alert_success("Cite this dataset using {.url https://doi.org/10.1016/j.dib.2017.05.007}")
  sf::read_sf(path_shp)

}

