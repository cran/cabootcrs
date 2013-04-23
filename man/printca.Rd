\name{printca}
\alias{printca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print full results
}
\description{
Prints full results from Correspondence Analysis, including variances, but no plots. 
}
\usage{
printca(x, datasetname = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class cabootcrsresults.
}
  \item{datasetname}{
name of data set, to appear in output.
}
}
\details{
Prints the usual Correspondence Analysis output plus the variances and covariances calculated by cabootcrs. 

Firstly, the principal inertias for all dimensions.
 
Secondly, for the number of dimensions specified in the printdims slot, 
defined by the original call to cabootcrs: 

Principal coordinates for row points \cr
Contributions (per mil) for row points \cr
Representations a.k.a. correlations (per mil) for row points \cr
Principal coordinates for column points \cr
Contributions (per mil) for column points \cr
Representations a.k.a. correlations (per mil) for column points

Thirdly, for the number of dimensions defined by the axisvariances slot, 
which was defined by the lastaxis parameter in the original call to cabootcrs:

Estimated variances and covariances for row points. \cr
Estimated variances and covariances for column points.

}
\value{
Printed output.
}
\author{
T.J. Ringrose
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{plotca}} ,
\code{\link{summaryca}} ,
\code{\linkS4class{cabootcrsresults}} 
}
\examples{
dreamdata <- t(matrix(c(7,4,3,7,10,15,11,13,23,9,11,7,28,9,12,10,32,5,4,3),4,5))
bd <- cabootcrs(dreamdata)
printca(bd, datasetname="Dreams")


## The function is currently defined as
function (x, datasetname = "") 
{
    printwithaxes <- function(res, thenames) {
        names(res) <- thenames
        print(res, digits = 4)
    }
    d <- min(x@printdims, x@br@r)
    axnames <- character(length = d)
    for (i in 1:d) {
        axnames[i] <- paste(" Axis", i)
    }
    cat("\n    RESULTS for Correspondence Analysis:", datasetname, 
        "\n\n")
    cat("Total inertia ", x@inertiasum, "\n\n")
    cat("Inertias, percent inertias and cumulative percent inertias \n\n")
    ins <- data.frame(x@inertias)
    names(ins) <- c("Inertia", "\%  ", "Cum. \%")
    print(ins, digits = 6)
    cat("\nRows in principal coordinates\n\n")
    printwithaxes(data.frame(x@Rowprinccoord[, 1:d], row.names = x@rowlabels), 
        axnames)
    cat("\nRow contributions (per mil)\n\n")
    printwithaxes(data.frame(round(x@RowCTR[, 1:d] * 1000), row.names = x@rowlabels), 
        axnames)
    cat("\nRow representations (per mil)\n\n")
    printwithaxes(data.frame(round(x@RowREP[, 1:d] * 1000), row.names = x@rowlabels), 
        axnames)
    cat("\nColumns in principal coordinates\n\n")
    printwithaxes(data.frame(x@Colprinccoord[, 1:d], row.names = x@collabels), 
        axnames)
    cat("\nColumn contributions (per mil)\n\n")
    printwithaxes(data.frame(round(x@ColCTR[, 1:d] * 1000), row.names = x@collabels), 
        axnames)
    cat("\nColumn representations (per mil)\n\n")
    printwithaxes(data.frame(round(x@ColREP[, 1:d] * 1000), row.names = x@collabels), 
        axnames)
    if (x@nboots > 0) {
        cat("\n\n  Results for Bootstrapping\n\n")
        cat(x@nboots, "bootstrap replications with", x@resampledistn, 
            "resampling\n")
        if (x@resampledistn == "multinomial" & x@multinomialtype != 
            "whole") 
            cat(paste("  ", switch(x@multinomialtype, rowsfixed = "with row sums constant", 
                columnsfixed = "with column sums constant"), 
                "\n"))
        cat("\nEstimated variances and covariances\n\n")
        cat("Rows\n\n")
        print(allvarscovs(x, "rows"), digits = 4)
        cat("\nColumns\n\n")
        print(allvarscovs(x, "columns"), digits = 4)
        cat("\n\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }


