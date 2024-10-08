% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eutrees4f.R
\name{fd_forest_eutrees4f}
\alias{fd_forest_eutrees4f}
\title{EU-Trees4F Database}
\usage{
fd_forest_eutrees4f(
  species,
  model = "clim",
  period = "all",
  scenario = "rcp45",
  type = "bin",
  distrib = "pot",
  quiet = FALSE
)
}
\arguments{
\item{species}{a character vector of length 1 with the Latin name of the
tree species (genus and species)}

\item{model}{a character vector of length 1 with the name of the ensemble
projection. One of 'clim' or 'sdms' (see details)}

\item{period}{a numeric or character vector of length 1 with the center of
the 30-year time period used for the model. One of '2005',
'2035', '2065', '2095', or 'all' (see details)}

\item{scenario}{a character vector of length 1 with the climate change
scenario used. One of 'rcp45' or 'rcp85' (see details)}

\item{type}{a character vector of length 1 with the type of output layer.
One of 'bin', 'prob' or 'std' (see details)}

\item{distrib}{a character vector of length 1 with the type of distribution.
One of 'nat', 'pot', 'disp' or 'disp_lu' (see details)}

\item{quiet}{if \code{TRUE}, suppress any message or progress bar}
}
\value{
A single-band or multi-band \code{SpatRaster}
}
\description{
Download data for tree species distribution in Europe for current (2005)
distribution, and future distribution (2035, 2065, 2095).
}
\details{
Data may be freely used for research, study, or teaching, but be cited
appropriately (see references below).

The data of EU-Trees4F database represent the distribution of the main woody
species in Europe at 5 arc-minutes (~ 10 km) spatial resolution, in the
Lambert Azimuthal Equal Area (EPSG:3035) CRS. The possible models to download
are the following:

\strong{Model}: type of model used
\itemize{
\item clim: climatic ensemble. A ensemble mean model that projects a consensus model
from \code{biomod2} into future conditions using the average of 11 Regional
Climate Models (RCM).
\item sdms: Species Distribution Model (SDM) ensemble. A model that projects the
consensus model for every single RCM, and then it averages the output of
then 11 SDMs.
}

\strong{Period}: 30-year time period
\itemize{
\item 2005: for current projections. This option ignores the scenario argument.
They are not available for \code{model = 'sdms'} with \code{type = 'std'}.
\item 2035: average of 2020-2050
\item 2065: average of 2050-2080
\item 2095: average of 2080-2110
\item all: get the four periods (or three for std type). Note that for some species
or configurations this might fail, because the raster extent might not match
in different periods
}

\strong{Scenario}: climate change scenario
\itemize{
\item rcp45: a climate change scenario that assumes moderate emissions reductions
\item rcp85: a climate change scenario with high greenhouse gas emissions
and limited mitigation efforts
}

\strong{Type}: type of output layer
\itemize{
\item bin: binary distribution map, where 1 represents presence of the tree species,
while 0 represents absence of the tree species, derived from the prob map
\item prob: probability distribution map (0-1000). Represents the probability
of being the potential distribution of the species
\item std: standard deviation of prob map. Only available for \code{model = 'sdms'}.
}

\strong{Distrib}: type of species distribution
\itemize{
\item nat: realized distribution (masked with native range). Only available
with \code{type = 'bin'}
\item pot: potential distribution
\item disp: natural dispersal model (migclim). Only available
with \code{type = 'bin'}
\item disp_lu: natural dispersal model clipped by forest areas. Only available
with \code{type = 'bin'}
}
}
\examples{
\donttest{
# Download data for Betula pendula
betula_pendula_sr <- fd_forest_eutrees4f(species = "Betula pendula")
}
}
\references{
Mauri, Achille; Cescatti, Alessandro; GIRARDELLO, MARCO; Strona,
Giovanni; Beck, Pieter; Caudullo, Giovanni; et al. (2022). EU-Trees4F. A
dataset on the future distribution of European tree species.. figshare.
Collection. https://doi.org/10.6084/m9.figshare.c.5525688.v2
}
\seealso{
\link{metadata_forestdata} for a list of possible species
}
