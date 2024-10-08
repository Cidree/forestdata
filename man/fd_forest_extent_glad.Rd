% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLAD.R
\name{fd_forest_extent_glad}
\alias{fd_forest_extent_glad}
\title{Forest Extent}
\usage{
fd_forest_extent_glad(
  x = NULL,
  lon = NULL,
  lat = NULL,
  year = 2020,
  crop = FALSE,
  quiet = FALSE,
  ...
)
}
\arguments{
\item{x}{a \code{sf} or \code{SpatVector} object. It will retrieve the
necessary tiles to cover the area (if \code{lat} and \code{lon} are
specified, this argument is ignored)}

\item{lon}{a number specifying the longitude of the area where we want the tile}

\item{lat}{a number specifying the latitude of the area where we want the tile}

\item{year}{year of the forest extent data. One of 2000, 2020 or 'all'}

\item{crop}{when \code{x} is specified, whether to crop the tiles(s) to the object}

\item{quiet}{if \code{TRUE}, suppress any message or progress bar}

\item{...}{additional arguments passed to the \link[terra]{crop} function}
}
\value{
\code{SpatRaster} object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
This function is deprecated in favour of \link{fd_forest_glad}. Download the Forest
Extent raster from the Global Land Analysis & Discovery by
using a vectorial object or a pair of coordinates (latitude, longitude).
}
\details{
The Forest Extent Map is a product offered by the Global Land Analysis &
Discovery organization. The spatial resolution of the product is 0.00025º
(approximately 30 meters at the Equator), and it's distributed in tiles of
10ºx10º. Pixels with forest height > 5 meters are classified as the forest class.

Note that each tile is stored as a raster file of 1.5 GB, so for
big extensions the function might take some time to retrieve the data.
}
\examples{
\donttest{
 # Get tile for Galicia (Spain)
 galicia_forest_extent <- fd_forest_extent_glad(lon = -7.8, lat = 42.7, year = 2020)
}
}
\references{
Potapov P., Hansen M.C., Pickens A., Hernandez-Serna A., Tyukavina A.,
Turubanova S., Zalles V., Li X., Khan A., Stolle F., Harris N., Song X.-P.,
Baggett A., Kommareddy I., Kommareddy A. (2022) The global 2000-2020 land cover
and land use change dataset derived from the Landsat archive: first results.
Frontiers in Remote Sensing \url{https://doi.org/10.3389/frsen.2022.856903}
}
