
# fd_forest_eutrees4f

#' EU-Trees4F Database
#'
#' Download data for tree species distribution in Europe for current (2005)
#' distribution, and future distribution (2035, 2065, 2095).
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @param species a character vector of length 1 with the Latin name of the
#'                tree species (genus and species)
#' @param model a character vector of length 1 with the name of the ensemble
#'              projection. One of 'clim' or 'sdms' (see details)
#' @param period a numeric or character vector of length 1 with the center of
#'               the 30-year time period used for the model. One of '2005',
#'               '2035', '2065', '2095', or 'all' (see details)
#' @param scenario a character vector of length 1 with the climate change
#'                scenario used. One of 'rcp45' or 'rcp85' (see details)
#' @param type a character vector of length 1 with the type of output layer.
#'             One of 'bin', 'prob' or 'std' (see details)
#' @param distrib a character vector of length 1 with the type of distribution.
#'                One of 'nat', 'pot', 'disp' or 'disp_lu' (see details)
#' @param quiet if \code{TRUE}, suppress any message or progress bar
#'
#' @return A single-band or multi-band \code{SpatRaster}
#' @export
#'
#' @details
#' The data of EU-Trees4F database represent the distribution of the main woody
#' species in Europe at 5 arc-minutes (~ 10 km) spatial resolution, in the
#' Lambert Azimuthal Equal Area (EPSG:3035) CRS. The possible models to download
#' are the following:
#'
#' \strong{Model}: type of model used
#'
#' - clim: climatic ensemble. A ensemble mean model that projects a consensus model
#' from \code{biomod2} into future conditions using the average of 11 Regional
#' Climate Models (RCM).
#'
#' - sdms: Species Distribution Model (SDM) ensemble. A model that projects the
#' consensus model for every single RCM, and then it averages the output of
#' then 11 SDMs.
#'
#' \strong{Period}: 30-year time period
#'
#' - 2005: for current projections. This option ignores the scenario argument.
#' They are not available for \code{model = 'sdms'} with \code{type = 'std'}.
#'
#' - 2035: average of 2020-2050
#'
#' - 2065: average of 2050-2080
#'
#' - 2095: average of 2080-2110
#'
#' - all: get the four periods (or three for std type). Note that for some species
#' or configurations this might fail, because the raster extent might not match
#' in different periods
#'
#' \strong{Scenario}: climate change scenario
#'
#' - rcp45: a climate change scenario that assumes moderate emissions reductions
#'
#' - rcp85: a climate change scenario with high greenhouse gas emissions
#' and limited mitigation efforts
#'
#' \strong{Type}: type of output layer
#'
#' - bin: binary distribution map, where 1 represents presence of the tree species,
#' while 0 represents absence of the tree species, derived from the prob map
#'
#' - prob: probability distribution map (0-1000). Represents the probability
#' of being the potential distribution of the species
#'
#' - std: standard deviation of prob map. Only available for \code{model = 'sdms'}.
#'
#' \strong{Distrib}: type of species distribution
#'
#' - nat: realized distribution (masked with native range). Only available
#' with \code{type = 'bin'}
#'
#' - pot: potential distribution
#'
#' - disp: natural dispersal model (migclim). Only available
#' with \code{type = 'bin'}
#'
#' - disp_lu: natural dispersal model clipped by forest areas. Only available
#' with \code{type = 'bin'}
#'
#' @seealso \link{metadata_forestdata} for a list of possible species
#'
#'
#' @references Mauri, Achille; Cescatti, Alessandro; GIRARDELLO, MARCO; Strona,
#' Giovanni; Beck, Pieter; Caudullo, Giovanni; et al. (2022). EU-Trees4F. A
#' dataset on the future distribution of European tree species.. figshare.
#' Collection. https://doi.org/10.6084/m9.figshare.c.5525688.v2
#'
#' @examples
#' \donttest{
#' # Download data for Betula pendula
#' betula_pendula_sr <- fd_forest_eutrees4f(species = "Betula pendula")
#' }
fd_forest_eutrees4f <- function(species,
                                model    = "clim",
                                period   = "all",
                                scenario = "rcp45",
                                type     = "bin",
                                distrib  = "pot",
                                quiet    = FALSE) {

  # 0. Errors if...
  if (model == "clim" & type == "std") cli::cli_abort("There's no std type for model clim.")
  if (type == "prob" & distrib != "pot") cli::cli_abort("You must use distrib = 'pot' for type = 'prob'.")
  if (type == "std" & distrib != "pot") cli::cli_abort("You must use distrib = 'pot' for type = 'std'.")
  if (type == "std" & period == 2005) cli::cli_abort("There's no current map (2005) for type = 'std'. Please, choose 2035, 2065 or 2095.")
  if (distrib %in% c("nat", "disp", "dip_lu") & type != "bin") cli::cli_abort("The distribution chosen is only available in binary output. Please use `type = 'bin'`")
  if (distrib == "nat" & period != 2005) cli::cli_abort("Natural distribution is only available for 2005")

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- "https://springernature.figshare.com/ndownloader/files/36704304"
  dir_unzip    <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_zip      <- stringr::str_glue("{dir_unzip}.zip")
  ## 1.2. Download
  if (!file.exists(dir_unzip) & !quiet) cli::cli_progress_step("Downloading data...", "Downloaded", "Download failed")
  dwld <- fdi_download_unzip(download_url, dir_unzip, dir_zip)
  if (!dwld) {
    cli::cli_process_failed()
    return(cli::cli_alert_danger("`fd_forest_eutrees4f()` failed to retrieve the data. Service might be currently unavailable"))
  }
    if (!quiet) cli::cli_progress_step("Preparing data...", msg_done = "Prepared")
  ## 1.3. Get the tree species
  if (!species %in% eutrees4f_tbl) cli::cli_abort("The chosen species is not supported. Please, check `forestdata::eutrees4f_trees` for a list of available species")

  # 2. Create file name
  ## 2.1. Fix species name
  tmp.species <- stringr::str_replace_all(species, ' ', '_')
  ## 2.2. Create file name
  if (model %in% c("clim", "sdms") & period %in% c(2035, 2065, 2095)) {
    rast.name <- stringr::str_glue("{tmp.species}_ens-{model}_{scenario}_fut{period}_{type}_{distrib}.tif")
  } else if (model %in% c("clim", "sdms") & period == 2005) {
    rast.name <- stringr::str_glue("{tmp.species}_ens-{model}_cur{period}_{type}_{distrib}.tif")
  } else if (model %in% c("clim", "sdms") & period == "all") {
    rast.current <- stringr::str_glue("{tmp.species}_ens-{model}_cur2005_{type}_{distrib}.tif")
    yrs <- c(2035, 2065, 2095)
    rast.future <- stringr::str_glue("{tmp.species}_ens-{model}_{scenario}_fut{yrs}_{type}_{distrib}.tif")
    ### Manage std (does not have 2005)
    if (type == "std") {
      rast.name <- rast.future
    } else {
      rast.name <- c(rast.current, rast.future)
    }

  } else {
    cli::cli_abort("Incorrect model or period. The valid models are 'clim' or 'sdms', and periods 2005, 2035, 2065 or 2095.")
  }
  ## 2.3. Full path to file
  if (period == "all") {
    ## File paths
    rast.path <- lapply(rast.name, function(x) list.files(
      path       = stringr::str_glue("{dir_unzip}/ens_{model}/{type}"),
      pattern    = x,
      full.names = TRUE
    )) |> unlist()

    ## Read into R and rename
    rst <- terra::rast(rast.path)
    names(rst) <- c("cur2005", "fut2035", "fut2065", "fut2095")
  } else {
    rast.path <- list.files(
      path       = stringr::str_glue("{dir_unzip}/ens_{model}/{type}"),
      pattern    = rast.name,
      full.names = TRUE
    )
    rst <- terra::rast(rast.path)
  }

  # 3. Return the raster
  if (!quiet) cli::cli_process_done()
  if (!quiet) cli::cli_alert_success("Cite this dataset using {cli::col_br_cyan('https://doi.org/10.6084/m9.figshare.c.5525688.v2')}")
  return(rst)

}







