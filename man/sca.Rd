\name{sca}
\alias{sca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correspondence analysis on a contingency table or similar 
}
\description{
Performs the usual simple (2-d) correspondence analysis calculations on
an input data matrix, producing an object that contains the fundamental
results but nothing more. 
}
\usage{
sca(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ 
A matrix of positive values, usually a contingency table. 
}
}
\details{
Assumes that input matrix has rows >= columns.
}
\value{
An object of class cabasicresults. 
}
\author{
T.J. Ringrose
}
\note{
Internal routine, not intended for direct call by users, although it can be.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\linkS4class{cabasicresults}} 
}
\examples{
dreamdata <- t(matrix(c(7,4,3,7,10,15,11,13,23,9,11,7,28,9,12,10,32,5,4,3),4,5))
cad <- sca(dreamdata)


## The function is currently defined as
function (X) 
{
    X <- X/sum(X)
    rmax <- min(dim(X)) - 1
    rsums <- as.vector(rowSums(X))
    csums <- as.vector(colSums(X))
    drm1 <- diag(1/(rsums + (rsums == 0)) * (1 - (rsums == 0)))
    dcm1 <- diag(1/(csums + (csums == 0)) * (1 - (csums == 0)))
    drmh <- sqrt(drm1)
    dcmh <- sqrt(dcm1)
    Z <- drmh \%*\% (X - rsums \%*\% t(csums)) \%*\% dcmh
    Y <- svd(Z)
    mu <- Y$d[1:rmax]
    r <- sum(mu > 1e-15)
    R <- drm1 \%*\% X
    Rweights <- dcmh
    Raxes = Y$v[, 1:rmax]
    C <- dcm1 \%*\% t(X)
    Cweights <- drmh
    Caxes = Y$u[, 1:rmax]
    if (r < rmax) {
        mu[(r + 1):rmax] <- 0
        Raxes[, (r + 1):rmax] <- 0
        Caxes[, (r + 1):rmax] <- 0
    }
    sca <- new("cabasicresults", Rprofile = R, Cprofile = C, 
        Rweights = Rweights, Cweights = Cweights, Raxes = Raxes, 
        Caxes = Caxes, r = r, mu = mu)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
