\name{covmat}
\alias{covmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract covariance matrix for a single category point 
}
\description{
Extract the 2 by 2 covariance matrix, for one row or column point, for one pair of axes.
}
\usage{
covmat(x, i, thing = "row", axis1 = 1, axis2 = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class cabootcrsresults.
}
  \item{i}{
row or column number.
}
  \item{thing}{
"row" - i-th row. \cr
"column" - i-th column.
}
  \item{axis1}{
first axis of pair.
}
  \item{axis2}{
second axis of pair, must be <= axisvariances value for x
}
}
\details{
Extracts a covariance matrix for use in further calculations. To inspect variances
and covariances use \code{\link{allvarscovs}} for more reader-friendly output. 
}
\value{
2 by 2 covariance matrix. 
}
\author{
T.J. Ringrose
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{allvarscovs}} ,
\code{\linkS4class{cabootcrsresults}} 
}
\examples{
dreamdata <- t(matrix(c(7,4,3,7,10,15,11,13,23,9,11,7,28,9,12,10,32,5,4,3),4,5))
bd <- cabootcrs(dreamdata, lastaxis=3)
covrow3axes1and2 <- covmat(bd, i=3)
covcol2axes2and3 <- covmat(bd, i=2, thing="column", axis1=2, axis2=3)



## The function is currently defined as
function (x, i, thing = "row", axis1 = 1, axis2 = 2) 
{
    printwithaxes <- function(res, thenames) {
        names(res) <- thenames
        print(res, digits = 4)
    }
    if (!(class(x) == "cabootcrsresults")) 
        stop(paste("Must be of type cabootcrsresults\n\n"))
    if (!any(thing == c("row", "column"))) 
        stop(paste("Must be row or column\n\n"))
    if (axis1 == axis2) 
        stop(paste("What are you playing at?\n\n"))
    if (!any(axis1 == seq(1, x@axisvariances))) 
        stop(paste("Covariance not available for these axes\n\n"))
    if (!any(axis2 == seq(1, x@axisvariances))) 
        stop(paste("Covariance not available for these axes\n\n"))
    if ((thing == "row") & !any(i == seq(1, x@rows))) 
        stop(paste("Invalid row number\n\n"))
    if ((thing == "column") & !any(i == seq(1, x@columns))) 
        stop(paste("Invalid column number\n\n"))
    a1 <- min(axis1, axis2)
    a2 <- max(axis1, axis2)
    tname <- ""
    if (thing == "row") {
        V <- matrix(c(x@RowVar[i, axis1], x@RowCov[i, a1, a2], 
            x@RowCov[i, a1, a2], x@RowVar[i, axis2]), 2, 2)
        if (!is.null(x@rowlabels)) {
            tname <- paste("(", x@rowlabels[[i]], ")")
        }
    }
    else {
        V <- matrix(c(x@ColVar[i, axis1], x@ColCov[i, a1, a2], 
            x@ColCov[i, a1, a2], x@ColVar[i, axis2]), 2, 2)
        if (!is.null(x@collabels)) {
            tname <- paste("(", x@collabels[[i]], ")")
        }
    }
    cat(paste("Covariance matrix of", switch(thing, row = "row", 
        column = "column"), i, tname, "for axes", axis1, axis2, 
        "\n\n"))
    rcnames <- c(paste("Axis", axis1), paste("Axis", axis2))
    printwithaxes(data.frame(V, row.names = rcnames), rcnames)
    invisible(V)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
