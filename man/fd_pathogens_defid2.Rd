% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pathogens.R
\name{fd_pathogens_defid2}
\alias{fd_pathogens_defid2}
\title{Download the DEFID2 database}
\usage{
fd_pathogens_defid2(
  agent = "all",
  host = "all",
  symptoms = "all",
  country = "all",
  geometry = "polygon",
  quiet = FALSE
)
}
\arguments{
\item{agent}{a character vector with the desired forest insect(s) and/or
disease(s). The default '\code{all}' retrieves every agent}

\item{host}{a character vector with the desired host tree(s) species. The
default '\code{all}' retrieves every tree}

\item{symptoms}{a character vector with the desired symptom(s). The default
'\code{all}' retrieves every symptom}

\item{country}{a character vector with the desired country(ies). The default
'\code{all}' retrieves every country}

\item{geometry}{a string with '\code{polygon}' to retrieve polygon data, or
'\code{point}' to retrieve point data}

\item{quiet}{if \code{TRUE}, suppress any message or progress bar}
}
\value{
\code{sf} object with \code{MULTIPOLYGON} or \code{POINT} geometry
}
\description{
Download the Database of European Forest Insect and Disease Disturbances.
}
\details{
Data may be freely used for research, study, or teaching, but be cited
appropriately (see references below).

This function will download the DEFID2 database to the temporary directory
once per session. After it's downloaded, the queries to the database are
faster than the first time.

Note that 99.6\% of the observations correspond to \emph{Picea abies}.
Also, 99.3\% of the observations are in Czechia.

The data comprises over 650,000 georeferenced records, which can be retrieved
as points or polygons, representing insects and diseases that occurred between
1963 and 2021 in European Forests.

Please, cite the data with the reference below.
}
\examples{
\donttest{
# Get the entire database (takes some seconds/minutes)
defid2_sf <- fd_pathogens_defid2()

# Get data for Spain and Portugal
defid2_iberia_sf <- fd_pathogens_defid2(country = c("Spain", "Portugal"))

}
}
\references{
Forzieri G, Dutrieux LP, Elia A, Eckhardt B, Caudullo G, Taboada FÁ,
Andriolo A, Bălacenoiu F, Bastos A, Buzatu A, Castedo Dorado F, Dobrovolný L,
Duduman M, Fernandez-Carillo A, Hernández-Clemente R, Hornero A, Ionuț S,
Lombardero MJ, Junttila S, Lukeš P, Marianelli L, Mas H, Mlčoušek M, Mugnai F,
Nețoiu C, Nikolov C, Olenici N, Olsson P, Paoli F, Paraschiv M, Patočka Z,
Pérez-Laorga E, Quero JL, Rüetschi M, Stroheker S, Nardi D, Ferenčík J,
Battisti A, Hartmann H, Nistor C, Cescatti A, Beck PSA (2023).
The Database of European Forest Insect and Disease Disturbances: DEFID2.
Global Change Biology
}
