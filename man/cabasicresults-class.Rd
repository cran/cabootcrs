\name{cabasicresults-class}
\Rdversion{1.1}
\docType{class}
\alias{cabasicresults-class}

\title{Class \code{"cabasicresults"}}
\description{
Contains all the fundamental output from the singular value decomposition
at the heart of correspondence analysis, but nothing more.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cabasicresults", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{Rprofile}:}{Object of class \code{"matrix"} Row profile matrix  }
    \item{\code{Cprofile}:}{Object of class \code{"matrix"} Column profile matrix }
    \item{\code{Rweights}:}{Object of class \code{"matrix"} Matrix of weights for row points: square roots of inverse column sums }
    \item{\code{Cweights}:}{Object of class \code{"matrix"} Matrix of weights for column points: square roots of inverse row sums }
    \item{\code{Raxes}:}{Object of class \code{"matrix"} Matrix of axes for row points: right singular vectors of weighted, centred data matrix }
    \item{\code{Caxes}:}{Object of class \code{"matrix"} Matrix of axes for column points: left singular vectors of weighted, centred data matrix }
    \item{\code{r}:}{Object of class \code{"numeric"} Rank of weighted, centred data matrix }
    \item{\code{mu}:}{Object of class \code{"numeric"} Singular values of weighted, centred data matrix }
  }
}
\section{Methods}{
No methods defined with class "cabasicresults" in the signature.
}
\author{
T.J. Ringrose
}
\note{
For internal use, not intended for direct user access. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\linkS4class{cabootcrsresults}} 
}
\examples{
showClass("cabasicresults")
}
\keyword{classes}
