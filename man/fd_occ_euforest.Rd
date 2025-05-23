% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eutrees4f.R
\name{fd_occ_euforest}
\alias{fd_occ_euforest}
\title{EU Forest Species database}
\usage{
fd_occ_euforest(species = NULL, country = NULL, spatial = FALSE, quiet = FALSE)
}
\arguments{
\item{species}{a character vector with the name of one or more tree species}

\item{country}{a character vector with either ISO2 codes, ISO3 codes or
full country names (not mixed) to filter out the data}

\item{spatial}{logical. Whether to retrieve a \code{tibble} or a \code{sf} object}

\item{quiet}{if \code{TRUE}, suppress any message or progress bar}
}
\value{
a \code{tibble} or a \code{sf} object
}
\description{
Downloads the EU Forest Species database, an European database of more than
500,000 forest tree species occurrences
}
\examples{
\donttest{
## Download full database as tibble
euforest_tbl <- fd_occ_euforest()

## Download full database as spatial
euforest_sf <- fd_occ_euforest(spatial = TRUE)

## Download data for Abies alba for Czechia and Germany
euforest_cz_ge_sf <- fd_occ_euforest(species = "Abies alba", country = c("Czechia", "Germany"))
}
}
\references{
A high resolution pan-European tree occurrence dataset
\doi{10.6084/m9.figshare.c.3288407.v1}
}
\seealso{
\link{metadata_forestdata} eutrees4f_species for a list of possible species
}
