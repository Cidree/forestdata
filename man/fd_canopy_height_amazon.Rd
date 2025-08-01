% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canopy-height.R
\name{fd_canopy_height_amazon}
\alias{fd_canopy_height_amazon}
\title{Forest Canopy Height}
\usage{
fd_canopy_height_amazon(
  x = NULL,
  lon = NULL,
  lat = NULL,
  crop = FALSE,
  mask = FALSE,
  merge = FALSE,
  quiet = FALSE
)
}
\arguments{
\item{x}{a \code{sf} or \code{SpatVector} object. It will retrieve the
necessary tiles to cover the area (if \code{lat} and \code{lon} are
specified, this argument is ignored)}

\item{lon}{a number specifying the longitude of the area where we want the tile}

\item{lat}{a number specifying the latitude of the area where we want the tile}

\item{crop}{when \code{x} is specified, whether to crop the tile(s) to the object}

\item{mask}{when \code{x} is specified, whether to mask the tile(s) to the object}

\item{merge}{if \code{FALSE} (default), it will merge the tiles into one raster.
If \code{FALSE} a SpatRasterCollection will be returned.}

\item{quiet}{if \code{TRUE}, suppress any message or progress bar}
}
\value{
A \code{SpatRaster} or \code{SpatRasterCollection}
}
\description{
Download the High Resolution 5m for the Amazon Forest
}
\details{
Data may be freely used for research, study, or teaching, but be cited
appropriately (see references below).
}
\references{
\url{https://arxiv.org/abs/2501.10600}
}
\keyword{internal}
