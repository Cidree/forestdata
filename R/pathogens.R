# fd_pathogens_defid2

#' Download the DEFID2 database
#'
#' Download the Database of European Forest Insect and Disease Disturbances.
#'
#' Data may be freely used for research, study, or teaching, but be cited
#' appropriately (see references below).
#'
#' @param agent a character vector with the desired forest insect(s) and/or
#'              disease(s). The default '\code{all}' retrieves every agent
#' @param host a character vector with the desired host tree(s) species. The
#'              default '\code{all}' retrieves every tree
#' @param symptoms a character vector with the desired symptom(s). The default
#'              '\code{all}' retrieves every symptom
#' @param country a character vector with the desired country(ies). The default
#'              '\code{all}' retrieves every country
#' @param geometry a string with '\code{polygon}' to retrieve polygon data, or
#'               '\code{point}' to retrieve point data
#'
#' @return \code{sf} object with \code{MULTIPOLYGON} or \code{POINT} geometry
#' @importFrom stats na.omit
#' @export
#'
#' @details
#' This function will download the DEFID2 database to the temporary directory
#' once per session. After it's downloaded, the queries to the database are
#' faster than the first time.
#'
#' The data comprises over 650,000 georeferenced records, which can be retrieved
#' as points or polygons, representing insects and diseases that occurred between
#' 1963 and 2021 in European Forests.
#'
#' Please, cite the data with the reference below.
#'
#' @references Forzieri G, Dutrieux LP, Elia A, Eckhardt B, Caudullo G, Taboada FÁ,
#'  Andriolo A, Bălacenoiu F, Bastos A, Buzatu A, Castedo Dorado F, Dobrovolný L,
#'  Duduman M, Fernandez-Carillo A, Hernández-Clemente R, Hornero A, Ionuț S,
#'  Lombardero MJ, Junttila S, Lukeš P, Marianelli L, Mas H, Mlčoušek M, Mugnai F,
#'  Nețoiu C, Nikolov C, Olenici N, Olsson P, Paoli F, Paraschiv M, Patočka Z,
#'  Pérez-Laorga E, Quero JL, Rüetschi M, Stroheker S, Nardi D, Ferenčík J,
#'  Battisti A, Hartmann H, Nistor C, Cescatti A, Beck PSA (2023).
#'  The Database of European Forest Insect and Disease Disturbances: DEFID2.
#'  Global Change Biology
#'
#' @examples
#' \donttest{
#' # Get the entire database (takes some seconds/minutes)
#' defid2_sf <- fd_pathogens_defid2()
#'
#' # Get data for Spain and Portugal
#' defid2_iberia_sf <- fd_pathogens_defid2(country = c("Spain", "Portugal"))
#'
#' }
fd_pathogens_defid2 <- function(agent = "all",
                                host = "all",
                                symptoms = "all",
                                country = "all",
                                geometry = "polygon") {

  # 1. Download file
  ## 1.1. File url
  url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/FOREST/DISTURBANCES/DEFID2/VER1-0/defid2.gpkg"
  file_path <- stringr::str_glue("{tempdir()}/{basename(url)}")
  ## 1.2. Download file if doesn't exist
  if (!file.exists(file_path)) {
    ## Check for user's timeout
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout))
    ## Download file
    options(timeout = 10000)
    download.file(
      url      = url,
      destfile = stringr::str_glue("{tempdir()}/{basename(url)}"),
      mode     = "wb"
    )
  }
  # 2. Prepare query
  ## 2.1. Agents
  if (any(agent != "all")) {
    tmp.agent <- paste0("('", paste0(agent, collapse = "', '"), "')")
    agent_qr <- stringr::str_glue("agents IN {tmp.agent}")
  } else {
    agent_qr <- NA
  }
  ## 2.2. Hosts
  if (any(host != "all")) {
    tmp.host <- paste0("('", paste0(host, collapse = "', '"), "')")
    host_qr <- stringr::str_glue("hosts IN {tmp.host}")
  } else {
    host_qr <- NA
  }
  ## 2.3. Symptoms
  if (any(symptoms != "all")) {
    tmp.symptoms <- paste0("('", paste0(symptoms, collapse = "', '"), "')")
    symptoms_qr <- stringr::str_glue("symptoms IN {tmp.symptoms}")
  } else {
    symptoms_qr <- NA
  }
  ## 2.4. Countries
  if (any(country != "all")) {
    tmp.country <- paste0("('", paste0(country, collapse = "', '"), "')")
    country_qr <- stringr::str_glue("country IN {tmp.country}", sep = ",")
  } else {
    country_qr <- NA
  }
  ## 2.5. Build Query
  ### Data frame with semi-queries
  query_df <- data.frame(
    clause = c("agent", "host", "country", "symptoms"),
    query  = c(agent_qr, host_qr, country_qr, symptoms_qr)
  ) |> na.omit()
  ### Build final clauses query
  tmp.query <- ""
  if (nrow(query_df) == 1) {
    tmp.query <- stringr::str_glue("WHERE {query_df[1,2]}")
  } else if (nrow(query_df) > 1) {
    tmp.query <- stringr::str_glue("WHERE {query_df[1,2]}")
    for (i in 2:nrow(query_df)) {
      tmp.query <- stringr::str_glue("{tmp.query} AND {query_df[i,2]}")
    }
    tmp.query <- stringr::str_glue("{tmp.query}")
  }

  # 3. Read into R
  if (geometry == "polygon") {
    sf::read_sf(
      dsn   = file_path,
      query = stringr::str_glue("SELECT * FROM exact_polygons {tmp.query};")
    )
  } else if (geometry == "point") {
    sf::read_sf(
      dsn   = file_path,
      query = stringr::str_glue("SELECT * FROM exact_points {tmp.query};")
    )
  } else {
    stop("Invalid geometry. Please select polygon or point.")
  }

}

