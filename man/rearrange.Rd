\name{rearrange}
\alias{rearrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rearrange bootstrap axes by comparing to sample axes. 
}
\description{
Compares one set of axes for row points and column points (from the bootstrap
data matrix) to another (from the sample data matrix) by looking at all possible
reorderings and reflections (only) of the bootstrap axes and picking the one which 
best matches the sample axes. 
}
\usage{
rearrange(RS, RB, CS, CB, r)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RS}{
Sample axes for row points. 
}
  \item{RB}{
Bootstrap axes for row points.
}
  \item{CS}{
Sample axes for column points.
}
  \item{CB}{
Bootstrap axes for column points.
}
  \item{r}{
Rank of bootstrap data matrix. 
}
}
\details{
Used to find the ordering of the bootstrap axes which best matches the sample axes under reordering
and reflection, but not rotation. 

Only the first six axes at most of the sample and bootstrap solutions are considered, for speed and simplicity.
It is assumed that users are usually only interested in the first 2-4 axes of the sample solution
and that hence the only reorderings of axes between sample and resample that are of interest
are among the first six. Hence variances for the 6th axis may be inaccurate because reordering has
not been fully allowed for, while those for the 7th axis and above will be very inaccurate. 

Note that the routine is very literal and unsubtle and considers every possible ordering.
The for loop calculating the match values is usually the main computational burden in the 
whole program, and a better algorithm for finding the best permutation for the bootstrap
eigenvectors to match the sample eigenvectors would speed the program substantially. 
}
\value{
A list of items used in rearranging.
\item{T}{Matrix to postmultiply the bootstrap axes to match them to the sample axes.}
\item{numrearranged}{Number of axes potentially rearranged = min(input rank,6).}
\item{match}{Vector of values of the matching coefficient for each possible ordering.}
\item{same}{TRUE if no reordering needed, FALSE otherwise.}
}
\author{
T.J. Ringrose
}
\note{
Internal routine, not intended for direct call by users. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{sca}} ,
\code{\link{cabootcrs}}
}
\examples{
## Not intended for direct call by users.

## The function is currently defined as
function (RS, RB, CS, CB, r) 
{
    if (r >= 1) {
        maxrearrange <- 6
        numrearranged <- min(r, maxrearrange)
        switch(numrearranged, per <- matrix(1, 1, 1), per <- rbind(c(1, 
            2), c(2, 1)), per <- cbind(rep(1:3, each = 2), c(2, 
            3, 1, 3, 1, 2), c(3, 2, 3, 1, 2, 1)), {
            p <- cbind(rep(1:3, each = 2), c(2, 3, 1, 3, 1, 2), 
                c(3, 2, 3, 1, 2, 1), 4)
            per <- rbind(p, p + 3 * (p == 1) - 3 * (p == 4), 
                p + 2 * (p == 2) - 2 * (p == 4), p + (p == 3) - 
                  (p == 4))
        }, {
            p <- cbind(rep(1:3, each = 2), c(2, 3, 1, 3, 1, 2), 
                c(3, 2, 3, 1, 2, 1), 4)
            p <- rbind(p, p + 3 * (p == 1) - 3 * (p == 4), p + 
                2 * (p == 2) - 2 * (p == 4), p + (p == 3) - (p == 
                4))
            p <- cbind(p, 5)
            per <- rbind(p, p + 4 * (p == 1) - 4 * (p == 5), 
                p + 3 * (p == 2) - 3 * (p == 5), p + 2 * (p == 
                  3) - 2 * (p == 5), p + (p == 4) - (p == 5))
        }, {
            p <- cbind(rep(1:3, each = 2), c(2, 3, 1, 3, 1, 2), 
                c(3, 2, 3, 1, 2, 1), 4)
            p <- rbind(p, p + 3 * (p == 1) - 3 * (p == 4), p + 
                2 * (p == 2) - 2 * (p == 4), p + (p == 3) - (p == 
                4))
            p <- cbind(p, 5)
            p <- rbind(p, p + 4 * (p == 1) - 4 * (p == 5), p + 
                3 * (p == 2) - 3 * (p == 5), p + 2 * (p == 3) - 
                2 * (p == 5), p + (p == 4) - (p == 5))
            p <- cbind(p, 6)
            per <- rbind(p, p + 5 * (p == 1) - 5 * (p == 6), 
                p + 4 * (p == 2) - 4 * (p == 6), p + 3 * (p == 
                  3) - 3 * (p == 6), p + 2 * (p == 4) - 2 * (p == 
                  6), p + (p == 5) - (p == 6))
        })
        nper <- dim(per)[1]
        match <- matrix(0, nper, 1)
        for (i in 1:nper) {
            match[i] = sum(diag(abs(t(RS[, 1:numrearranged]) \%*\% 
                RB[, per[i, ]] + t(CS[, 1:numrearranged]) \%*\% 
                CB[, per[i, ]])))
        }
        posn <- which.max(match)
        same <- posn == 1
        I <- diag(rep(1, numrearranged))
        T <- I[, per[posn, ]]
        t <- diag(t(RS[, 1:numrearranged]) \%*\% RB[, per[posn, 
            ]] + t(CS[, 1:numrearranged]) \%*\% CB[, per[posn, 
            ]])
        T <- T \%*\% diag((t >= 0) - (t < 0), nrow = numrearranged, 
            ncol = numrearranged)
    }
    else {
        T <- matrix(1, 1, 1)
        numrearranged <- 1
        match <- 0
        same <- 0
    }
    list(T = T, numrearranged = numrearranged, match = match, 
        same = same)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
