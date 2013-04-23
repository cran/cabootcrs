\name{allvarscovs}
\alias{allvarscovs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract variances and covariances
}
\description{
Extract a data frame containing all variances and covariances of either the row or the column points.
}
\usage{
allvarscovs(x, thing = "rows")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class cabootcrsresults.
}
  \item{thing}{
"rows" - all rows. \cr
"columns" - all columns.
}
}
\details{
Shows all variances and covariances as a data frame for ease of viewing. 
To obtain a covariance matrix itself use \code{\link{covmat}} instead. 
}
\value{
Data frame.
}
\author{
T.J. Ringrose
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{covmat}} ,
\code{\linkS4class{cabootcrsresults}} 
}
\examples{
dreamdata <- t(matrix(c(7,4,3,7,10,15,11,13,23,9,11,7,28,9,12,10,32,5,4,3),4,5))
bd <- cabootcrs(dreamdata)
allvarscovs(bd)
allvarscovs(bd, thing="columns")


## The function is currently defined as
function (x, thing = "rows") 
{
    getcovs <- function(allC, n, ncovs) {
        V <- matrix(0, n, ncovs)
        for (i in 1:n) {
            y <- allC[i, , ]
            V[i, ] <- y[upper.tri(y)]
        }
        invisible(V)
    }
    if (!(class(x) == "cabootcrsresults")) 
        stop(paste("Must be of type cabootcrsresults\n\n"))
    if (!any(thing == c("rows", "columns"))) 
        stop(paste("Must be rows or columns\n\n"))
    ncovs <- x@axisvariances * (x@axisvariances - 1)/2
    vcnames <- character(length = x@axisvariances + ncovs)
    k <- 1
    for (i in 1:x@axisvariances) {
        vcnames[i] <- paste(" Var Axis", i)
        if (i < x@axisvariances) {
            for (j in (i + 1):x@axisvariances) {
                vcnames[x@axisvariances + k] <- paste(" Cov axes", 
                  i, j)
                k <- k + 1
            }
        }
    }
    if (thing == "rows") {
        Covs <- getcovs(x@RowCov, x@rows, ncovs)
        allV <- data.frame(cbind(x@RowVar, Covs), row.names = x@rowlabels)
    }
    else {
        Covs <- getcovs(x@ColCov, x@columns, ncovs)
        allV <- data.frame(cbind(x@ColVar, Covs), row.names = x@collabels)
    }
    names(allV) <- vcnames
    allV
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
