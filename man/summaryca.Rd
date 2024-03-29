% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defineclassesandfunctions.R
\name{summaryca}
\alias{summaryca}
\title{Prints brief 2-d results, with standard deviations}
\usage{
summaryca(x, datasetname = NULL, mcaprintindividuals = FALSE)
}
\arguments{
\item{x}{An object of class \code{\linkS4class{cabootcrsresults}}}

\item{datasetname}{The name (in "") of the data set, to be used in the output, defaults to that in cabootcrs object}

\item{mcaprintindividuals}{If TRUE then print individual (row) point results in multiple
correspondence analysis when using indicator or doubled matrix}
}
\value{
Printed results, no plots or objects produced
}
\description{
\code{summaryca} prints correspondence analysis results for the first two dimensions,
giving inertias, coordinates,
representations, contributions and standard deviations
}
\examples{
results <- cabootcrs(DreamData, showresults=FALSE)
summaryca(results, datasetname="Dreams")

}
\seealso{
\code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\link{printca}}, \code{\link{plotca}},
\code{\linkS4class{cabootcrsresults}}
}
