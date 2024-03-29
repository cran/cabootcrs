% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defineclassesandfunctions.R
\name{reflectaxes}
\alias{reflectaxes}
\title{Reflect coordinates for chosen axes}
\usage{
reflectaxes(x, axes = c(1, 2))
}
\arguments{
\item{x}{An object of class \code{\linkS4class{cabootcrsresults}}}

\item{axes}{A list or vector containing the numbers of the axes to be reflected}
}
\value{
An object of class \code{\linkS4class{cabootcrsresults}}
}
\description{
\code{reflectaxes} reflects the principal and standard coordinates
of the axes chosen, and the appropriate covariances where needed
}
\details{
This may be useful when comparing results between different data sets,
or from different packages
}
\examples{
results <- cabootcrs(DreamData)
resultsreflectfirstaxis <- reflectaxes(results, 1)
summaryca(resultsreflectfirstaxis)
plotca(resultsreflectfirstaxis)

\dontrun{

# Often needed when comparing results between different packages,
# or same package on different machines,
# or to allow ellipses from this package to be added to plots from other packages

library(ca)
cad3 <- mjca(DreamData223by3)
bd3 <- cabootcrs(DreamData223by3, catype="mca")
summary(cad3)
bd3reflect1 <- reflectaxes(bd3,1)
summaryca(bd3reflect1)

}

}
\seealso{
\code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\link{reordercategories}}, \code{\linkS4class{cabootcrsresults}}
}
