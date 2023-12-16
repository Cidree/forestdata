# get_eutrees4f_tbl

#'  (Internal) Gets the tree species
#'  Get tree species for EU-Trees4F database
#'
#' @return a character vector
#' @examples
#' \dontrun{
#' get_eutrees4f_tbl()
#' }
get_eutrees4f_tbl <- function() {
  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- "https://springernature.figshare.com/ndownloader/files/36704304"
  dir_unzip    <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_zip      <- stringr::str_glue("{dir_unzip}.zip")
  ## 1.2. Download
  fdi_download_unzip(download_url, dir_unzip, dir_zip, quiet = TRUE)

  # 2. Get tree species names
  ## 2.1. List files in a folder
  eutrees_files <- list.files(path = stringr::str_glue("{dir_unzip}/ens_clim/bin"))
  ## 2.2. Extract species
  eutrees_trees <- eutrees_files %>%
    stringr::str_split("_") %>%
    purrr::map(\(x) paste(x[1:2], collapse = " ")) %>%
    as.character() %>%
    unique()
  ## 2.3. Return results
  return(eutrees_trees)
}


# fd_forest_eutrees4f

#' Download data from EU-Trees4F Database
#'
#' Download data for tree species distribution in Europe for current (2005)
#' distribution, and future distribution (2035, 2065, 2095).
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropiately (see references below).
#'
#' @param species A character vector of length 1 with the latin name of the
#'                tree species (genus and species)
#' @param model A character vector of length 1 with the name of the ensemble
#'              projection. One of 'clim' or 'sdms' (see details)
#' @param period A numeric or character vector of length 1 with the center of
#'               the 30-year time period used for the model. One of '2005',
#'               '2035', '2065', '2095', or 'all' (see details)
#' @param scenario A character vector of length 1 with the climate change
#'                scenario used. One of 'rcp45' or 'rcp85' (see details)
#' @param type A character vector of length 1 with the type of output layer.
#'             One of 'bin', 'prob' or 'std' (see details)
#' @param distrib A character vector of length 1 with the type of distribution.
#'                One of 'nat', 'pot', 'disp' or 'disp_lu' (see details)
#' @param quiet If \code{TRUE} (the default), suppress status messages, and
#'              the progress bar
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
#' __Model__: type of model used
#'
#' - _clim_: climatic ensemble. A ensemble mean model that projects a consensus model
#' from \code{biomod2} into future conditions using the average of 11 Regional
#' Climate Models (RCM).
#'
#' - _sdms_: Species Distribution Model (SDM) ensemble. A model that projects the
#' consensus model for every single RCM, and then it averages the output of
#' then 11 SDMs.
#'
#' __Period__: 30-year time period
#'
#' - _2005_: for current projections. This option ignores the scenario argument.
#' They are not available for \code{model = 'sdms'} with \code{type = 'std'}.
#'
#' - _2035_: average of 2020-2050
#'
#' - _2065_: average of 2050-2080
#'
#' - _2095_: average of 2080-2110
#'
#' - _all_: get the four periods (or three for std type)
#'
#' __Scenario__: climate change scenario
#'
#' - _rcp45_: a climate change scenario that assumes moderate emissions reductions
#'
#' - _rcp85_: a climate change scenario with high greenhouse gas emissions
#' and limited mitigation efforts
#'
#' __Type__: type of output layer
#'
#' _bin_: binary distribution map, where 1 represents presence of the tree species,
#' while 0 represents absence of the tree species, derived from the prob map
#'
#' _prob_: probability disitribution map (0-1000). Represents the probability
#' of being the potential distribution of the species
#'
#' _std_: standard deviation of prob map. Only available for \code{model = 'sdms'}.
#'
#' __Distrib__: type of species distribution
#'
#' - _nat_: realized distribution (masked with native range). Only available
#' with \code{type = 'bin'}
#'
#' - _pot_: potential distribution
#'
#' - _disp_: natural dispersal model (migclim). Only available
#' with \code{type = 'bin'}
#'
#' - *disp_lu*: natural dispersal model clipped by forest areas. Only available
#' with \code{type = 'bin'}
#'
#'
#' @references Mauri, Achille; Cescatti, Alessandro; GIRARDELLO, MARCO; Strona,
#' Giovanni; Beck, Pieter; Caudullo, Giovanni; et al. (2022). EU-Trees4F. A
#' dataset on the future distribution of European tree species.. figshare.
#' Collection. https://doi.org/10.6084/m9.figshare.c.5525688.v2
#'
#' @examples
#' \dontrun{
#' # Download data for Betula pendula
#' betula_pendula_sr <- fd_forest_eutrees4f(species = "Betula pendula")
#'
#' }
fd_forest_eutrees4f <- function(species,
                                model    = "clim",
                                period   = "all",
                                scenario = "rcp45",
                                type     = "bin",
                                distrib  = "pot",
                                quiet    = TRUE) {

  # 0. Errors if...
  if (model == "clim" & type == "std") stop("There's no std type for model clim.")
  if (type == "prob" & distrib != "pot") stop("You must use distrib = 'pot' for type = 'prob'.")
  if (type == "std" & distrib != "pot") stop("You must use distrib = 'pot' for type = 'std'.")
  if (type == "std" & period == 2005) stop("There's no current map (2005) for type = 'std'. Please, choose 2035, 2065 or 2095.")
  if (distrib %in% c("nat", "disp", "dip_lu") & type != "bin") stop("The distribution chosen is only available in binary output. Please use `type = 'bin'`")

  # 1. Download file
  ## 1.1. Url and file destination
  download_url <- "https://springernature.figshare.com/ndownloader/files/36704304"
  dir_unzip    <- stringr::str_glue("{tempdir()}/{basename(download_url)}")
  dir_zip      <- stringr::str_glue("{dir_unzip}.zip")
  ## 1.2. Download
  fdi_download_unzip(download_url, dir_unzip, dir_zip, quiet)
  ## 1.3. Get the tree species
  tree_species <- get_eutrees4f_tbl()
  if (!species %in% tree_species) stop("The chosen species is not supported. Please, check `forestdata::eutrees4f_trees` for a list of available species")

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
    stop("Incorrect model or period. The valid models are 'clim' or 'sdms', and periods 2005, 2035, 2065 or 2095.")
  }
  ## 2.3. Full path to file
  if (period == "all") {
    ## File paths
    rast.path <- purrr::map(rast.name, \(x) list.files(
      path       = stringr::str_glue("{dir_unzip}/ens_{model}/{type}"),
      pattern    = x,
      full.names = TRUE
    )) %>% as.character()
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
  return(rst)

}







