% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allometry.R
\name{fd_allometry_tallo}
\alias{fd_allometry_tallo}
\title{TALLO database}
\usage{
fd_allometry_tallo(
  country = NULL,
  spatial = FALSE,
  metadata_path = NULL,
  quiet = FALSE
)
}
\arguments{
\item{country}{a character vector with either ISO2 codes, ISO3 codes or
full country names (not mixed) to filter out the data}

\item{spatial}{logical. Whether to retrieve a \code{tibble} or a \code{sf} object}

\item{metadata_path}{a character string of length 1 with the path to store the
metadata and bibliography. The default \code{path_metadata = NULL}
does not download the metadata}

\item{quiet}{if \code{TRUE}, suppress any message or progress bar}
}
\value{
a \code{tibble} or a \code{sf} object
}
\description{
Downloads the TALLO database, a global tree allometry and crown architecture
database. Over 500,000 data points of individual trees with several measurements
}
\examples{
\donttest{
## Download full database as tibble
tallo_tbl <- fd_allometry_tallo()

## Download full database as sf
tallo_sf <- fd_allometry_tallo(spatial = TRUE)

## Download data as sf for Czechia and Germany
tallo_cz_ge_sf <- fd_allometry_tallo(country = c("Czechia", "Germany"))
}
}
\references{
Tallo: A global tree allometry and crown architecture database.
\doi{10.1111/gcb.16302}
}
