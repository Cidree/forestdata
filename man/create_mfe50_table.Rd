% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_country.R
\name{create_mfe50_table}
\alias{create_mfe50_table}
\title{(Internal) Create MFE50 provinces table for one CCAA}
\usage{
create_mfe50_table(url)
}
\arguments{
\item{url}{The url to the spatial data}
}
\value{
A \code{data.frame}
}
\description{
Creates a table with the province name and the url of the
vectorial data.
}
\examples{
\dontrun{
ccaa_tbl <- get_mfe50_ccaa_tbl()
create_mfe50_table(ccaa_tbl[1,2])
}
}
\keyword{internal}
