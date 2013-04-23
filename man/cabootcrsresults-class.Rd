\name{cabootcrsresults-class}
\Rdversion{1.1}
\docType{class}
\alias{cabootcrsresults-class}

\title{Class \code{"cabootcrsresults"}}
\description{
Contains an object of type cabasicresults, 
plus contributions and correlations etc, plus the variances and covariances calculated
by bootstrapping, plus records the various settings used in doing this.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cabootcrsresults", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{br}:}{Object of class \code{"cabasicresults"} From the sample data matrix. }
    \item{\code{DataMatrix}:}{Object of class \code{"matrix"} The sample data matrix. }
    \item{\code{rows}:}{Object of class \code{"numeric"} Number of rows. }
    \item{\code{columns}:}{Object of class \code{"numeric"} Number of columns. }
    \item{\code{rowlabels}:}{Object of class \code{"character"} Row category labels. }
    \item{\code{collabels}:}{Object of class \code{"character"} Column category labels. }
    \item{\code{Rowprinccoord}:}{Object of class \code{"matrix"} Principal coordinates for row points. }
    \item{\code{Colprinccoord}:}{Object of class \code{"matrix"} Principal coordinates for column points. }
    \item{\code{Rowstdcoord}:}{Object of class \code{"matrix"} Standard coordinates for row points. }
    \item{\code{Colstdcoord}:}{Object of class \code{"matrix"} Standard coordinates for column points. }
    \item{\code{RowCTR}:}{Object of class \code{"matrix"} Contributions for row points. }
    \item{\code{RowREP}:}{Object of class \code{"matrix"} Representations for row points. }
    \item{\code{ColCTR}:}{Object of class \code{"matrix"} Contributions for column points. }
    \item{\code{ColREP}:}{Object of class \code{"matrix"} Representations for column points. }
    \item{\code{RowVar}:}{Object of class \code{"matrix"} Variances for row points. }
    \item{\code{RowCov}:}{Object of class \code{"array"} Covariances for row points. }
    \item{\code{ColVar}:}{Object of class \code{"matrix"} Variances for column points. }
    \item{\code{ColCov}:}{Object of class \code{"array"} Covariances for column points. }
    \item{\code{inertiasum}:}{Object of class \code{"numeric"} Total inertia. }
    \item{\code{inertias}:}{Object of class \code{"matrix"} Axis inertias.  }
    \item{\code{nboots}:}{Object of class \code{"numeric"} Number of bootstrap replicates used to calculate the (co)variances.  }
    \item{\code{resampledistn}:}{Object of class \code{"character"} Distribution used for resampling.  }
    \item{\code{multinomialtype}:}{Object of class \code{"character"} Form of multinomial resampling used. }
    \item{\code{sameaxisorder}:}{Object of class \code{"numeric"} Number of resamples with no reordering in first six bootstrap axes. }
    \item{\code{poissonzeronewmean}:}{Object of class \code{"numeric"} Mean used for resampling zero cells. }
    \item{\code{newzeroreset}:}{Object of class \code{"numeric"} Option to reset resample zero cells. }
    \item{\code{printdims}:}{Object of class \code{"numeric"} Number of dimensions to print, though note that all are stored. }
    \item{\code{axisvariances}:}{Object of class \code{"numeric"} Number of axes for which variances were calculated and are stored. }
  }
}
\section{Methods}{
No methods defined with class "cabootcrsresults" in the signature.
}
\author{
T.J. Ringrose
}
\note{
This object class contains all the output from the cabootcrs routine.
The generic routines print, plot and summary can be applied to produce
different versions of the output without requiring further calculations. 

The value in the axisvariances slot, giving the number of axes for which variances were calculated, 
is either the lastaxis parameter in the call to cabootcrs or the number of the dimensions in the solution, 
whichever is smaller. 

Note that variances above the fifth axis may be inaccurate, 
see \code{\link{rearrange}} for further details. 

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\linkS4class{cabasicresults}} ,
\code{\link{rearrange}} 
}
\examples{
showClass("cabootcrsresults")
}
\keyword{classes}
