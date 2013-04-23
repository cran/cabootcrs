\name{cabootcrs}
\alias{cabootcrs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate category point variances using bootstrapping
}
\description{
Performs correspondence analysis and then uses bootstrap resampling to
construct confidence ellipses for each row and column category point,
printing and plotting the results. 

For description of the package itself see \code{\link{cabootcrs-package}}.
}
\usage{
cabootcrs(xobject = NULL, datafile = NULL, 
     groupings = NULL, grouplabels = NULL, 
     plotsymbolscolours=c(19,"alldifferent",18,"alldifferent"), 
     othersmonochrome = "black", crpercent = 95, nboots = 1000, 
     resampledistn = "Poisson", multinomialtype = "whole", 
     poissonzeronewmean = 0, newzeroreset = 0, 
     printdims = 4, lastaxis = 2, 
     catype = "sca", bootstdcoords = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xobject}{
name of data object (data frame, matrix or similar class that can be coerced to data frame) 
containing the data in contingency table format. \cr
recommended that rows >= columns as matrix will be transposed if columns > rows.
}
  \item{datafile}{
name of a text file (in " ") containing the data in contingency table format, 
with or without row and column labels. \cr
ignored if xobject is non-null. \cr
recommended that rows >= columns as matrix will be transposed if columns > rows.
}
  \item{groupings}{
to be passed to plot, see \code{\link{plotca}} for details. 
}
  \item{grouplabels}{
to be passed to plot, see \code{\link{plotca}} for details. 
}
  \item{plotsymbolscolours}{
to be passed to plot, see \code{\link{plotca}} for details. 
}
  \item{othersmonochrome}{
to be passed to plot, see \code{\link{plotca}} for details. 
}
  \item{crpercent}{
to be passed to plot, see \code{\link{plotca}} for details. 
}
  \item{nboots}{
number of boostrap replicate matrices used, default and recommended minimum is 1000, 
but 10000 is recommended if machine and data set size allows; 
the calculated variances will sometimes differ 
around 3rd decimal place, but pictures should look the same. \cr
if nboots=0 then correspondence analysis is performed as usual with no variances calculated. 
}
  \item{resampledistn}{
"Poisson" - resampled matrices constructed using Poisson resampling on each cell separately.\cr
"multinomial" - resampled matrices constructed using multinomial resampling, treating the 
cells as defining one or more multinomial distributions.
}
  \item{multinomialtype}{
only relevant for multinomial sampling, otherwise ignored: \cr
"whole" - all cells define a single multinomial distribution. \cr
"rowsfixed" - each row defines a separate multinomial distribution. \cr
"columnsfixed" - each column defines a separate multinomial distribution.
}
  \item{poissonzeronewmean}{
0 - no effect, method as described in paper. \cr
> 0 - cells which are zero in the data are instead resampled from a 
Poisson distribution with this mean, which should be very small (say 0.1); 
this is for situations where rare cases did not occur but could have done, 
so that it might be appropriate for zero cells in the sample to be 
occasionally non-zero in resamples.
}
  \item{newzeroreset}{
0 - no effect, method as described in paper. \cr
1 - if a cell value is non-zero in the sample but zero in the resample
then it is reset to 1 in the resample, so that the sparsity structure
of the sample is maintained in the resample. This can be useful with
sparse data sets and, in effect, conditions on the sample sparsity structure.
}
  \item{printdims}{
print full correspondence analysis coordinates, contributions, correlations etc for all output 
dimensions up to and including this one.
}
  \item{lastaxis}{
calculate variances and covariances for all output axes (dimensions) up to this one
(or the number of dimensions in the solution if smaller). \cr
recommended maximum is 5 as variances for those above the fifth axis may be inaccurate.
}
  \item{catype}{
"sca" - use simple (classical) correspondence analysis. \cr
"mca", "omca" - not yet implemented
}
  \item{bootstdcoords}{
FALSE - produce variances and confidence regions using principal coordinates, as in the paper. \cr
TRUE - produce variances etc for standard coordinates.
}
}
\details{
Performs all of the usual Correspondence Analysis calculations while also using bootstrapping
to estimate, for each row and column category on each dimension of the solution, 
the variance of the difference between the sample and population point when both are projected
onto the sample axes in principal coordinates, allowing for sampling variation in both the points and the axes.  

Constructs confidence ellipses for each row and column category point and plots the results 
by a call to \code{\link{plotca}}. Also prints the usual Correspondence Analysis summary
output and the calculated variances and covariances through calls to 
\code{\link{summaryca}} and \code{\link{allvarscovs}} respectively. 
Use \code{\link{printca}} for more detailed numerical results. 

For further examples see \code{\link{cabootcrs-package}}.
}
\value{
Object of class cabootcrsresults, plus output from calls to the summaryca, allvarscovs and plotca functions.
}
\references{
Ringrose, T.J. (2012).
Bootstrap confidence regions for correspondence analysis. \cr
Journal of Statistical Computation and Simulation.
Vol 83, No. 10, October 2012, 1397-1413. 
}
\author{
T.J. Ringrose
}
\note{
In R itself both versions of the call to the routine, either with a data frame or matrix as input:

data(DreamData) \cr
bd <- cabootcrs(DreamData)

or with a data file as input:

bd <- cabootcrs(datafile="DreamData.txt")

will work, assuming that setwd() has been used appropriately. 

However, only the former worked in R CMD check when the package was being checked, 
so only that version appears in the examples. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{cabootcrs-package}} ,
\code{\link{printca}} ,
\code{\link{plotca}} ,
\code{\link{summaryca}} ,
\code{\link{covmat}} ,
\code{\link{allvarscovs}} ,
\code{\linkS4class{cabootcrsresults}} 
}
\examples{
# Produce CRs for the Dream data set, supplied from a data frame. 
# Use all defaults: 1000 bootstraps, Poisson resampling, calculate variances 
# only for first two axes, but give usual output for up to the first 4 axes. 
# Show a biplot with CRs for rows in principal coordinates and another with 
# CRs for columns in principal coordinates.

data(DreamData)
bd <- cabootcrs(DreamData)

# Calculate variances and covariances for axes 1-3,
# though by default only plots axis 1 versus axis 2. 

\dontrun{
bd3 <- cabootcrs(DreamData, lastaxis=3)

# Use 10000 bootstrap replicates, which is recommended if the machine is 
# fast enough but the difference is usually fairly small.

bd10k <- cabootcrs(DreamData, nboots=10000)

# Use multinomial resampling, with the matrix treated as a single
# multinomial distribution. 

bdm <- cabootcrs(DreamData, resampledistn="multinomial")

# Fix the row sums (i.e. keep sum of age group constant).

bdmrf <- cabootcrs(DreamData, resampledistn="multinomial", multinomialtype="rowsfixed")

# Just perform correspondence analysis, without bootstrapping.

bd0 <- cabootcrs(DreamData, nboots=0)
}


## The function is currently defined as
function (xobject = NULL, datafile = NULL, groupings = NULL, 
    grouplabels = NULL, plotsymbolscolours=c(19,"alldifferent",18,"alldifferent"), 
    othersmonochrome = "black", crpercent = 95, 
    nboots = 1000, resampledistn = "Poisson", multinomialtype = "whole", 
    poissonzeronewmean = 0, newzeroreset = 0, printdims = 4, 
    lastaxis = 2, catype = "sca", bootstdcoords = FALSE) 
{
    if (nboots == 0) {
        cat(paste(
      "\n Standard Correspondence Analysis results only, no confidence regions calculated\n\n"))
    }
    else {
        if (nboots < 999) 
            cat(paste("\n WARNING", nboots, 
                      "is too few bootstrap replicates for reliable results\n\n"))
        if (!any(resampledistn == c("Poisson", "multinomial", 
            "nonparametric", "balanced"))) 
            stop(paste("Resampling must be Poisson, multinomial, nonparametric or balanced\n\n"))
        if (!any(multinomialtype == c("whole", "rowsfixed", "columnsfixed"))) 
            stop(paste("Multinomial resampling is either whole, rowsfixed or columnsfixed\n\n"))
        if ((resampledistn == "Poisson") & any(multinomialtype == 
            c("rowsfixed", "columnsfixed"))) 
            cat(paste("\n WARNING can't fix rows or columns with Poisson resampling\n\n"))
        if (poissonzeronewmean < 0) 
            stop(paste("Poisson mean for zero entries must be non-negative\n\n"))
        if (!any(newzeroreset == c(0, 1))) 
            stop(paste("Zeros in sample can only be set to zero or one in bootstrap\n\n"))
    }
    if (printdims < 2) 
        stop(paste("number of dims for output must be at least 2\n\n"))
    if (lastaxis < 2) 
        stop(paste("last axis must be at least 2\n\n"))
    if (!any(catype == c("sca", "mca", "omca"))) 
        stop(paste("Must be sca, mca or omca"))
    maxrearrange <- 6
    if (is.null(xobject)) {
        Xtable <- read.table(file = datafile)
        if ((row.names(Xtable)[[1]] == "1") & (names(Xtable)[[1]] == 
            "V1")) {
            for (i in 1:dim(Xtable)[1]) rownames(Xtable)[i] <- paste("r", 
                i, sep = "")
            for (i in 1:dim(Xtable)[2]) colnames(Xtable)[i] <- paste("c", 
                i, sep = "")
        }
    }
    else {
        Xtable <- as.data.frame(xobject)
        if ((is.null(rownames(xobject))) | (row.names(Xtable)[[1]] == 
            "1")) {
            for (i in 1:dim(Xtable)[1]) rownames(Xtable)[i] <- paste("r", 
                i, sep = "")
        }
        if ((is.null(colnames(xobject))) | (names(Xtable)[[1]] == 
            "V1")) {
            for (i in 1:dim(Xtable)[2]) colnames(Xtable)[i] <- paste("c", 
                i, sep = "")
        }
    }
    X <- as.matrix(Xtable)
    if ((dim(X)[2] > dim(X)[1]) & (catype == "sca")) {
        X <- t(X)
        rowlabels <- colnames(Xtable)
        collabels <- rownames(Xtable)
    }
    else {
        rowlabels <- rownames(Xtable)
        collabels <- colnames(Xtable)
    }
    rows <- dim(X)[1]
    cols <- dim(X)[2]
    n <- sum(X)
    axisvariances <- min(lastaxis, rows - 1, cols - 1)
    if ((lastaxis >= maxrearrange) & (cols >= maxrearrange + 
        2)) 
        cat(paste("\n WARNING variances for the ", maxrearrange, 
            "th axis and above may be unreliable\n\n"))
    if (lastaxis>axisvariances) 
        cat(paste("\n WARNING there are only", axisvariances, 
            "axes in the solution\n\n"))
    S <- switch(catype, sca = sca(X), mca = sca(X), omca = sca(X))
    zerorowS <- rowSums(X > 0) == 0
    zerocolS <- colSums(X > 0) == 0
    onerowS <- rowSums(X > 0) == 1
    onecolS <- colSums(X > 0) == 1
    tworowS <- rowSums(X > 0) == 2
    twocolS <- colSums(X > 0) == 2
    RSBsum <- matrix(0, rows, axisvariances)
    RSBsumsq <- matrix(0, rows, axisvariances)
    RSBsumcp <- array(0, c(rows, axisvariances, axisvariances))
    CSBsum <- matrix(0, cols, axisvariances)
    CSBsumsq <- matrix(0, cols, axisvariances)
    CSBsumcp <- array(0, c(cols, axisvariances, axisvariances))
    rownB <- rep(nboots, rows)
    colnB <- rep(nboots, cols)
    RSBvar <- RSBsum
    CSBvar <- CSBsum
    RSBcov <- RSBsumcp
    CSBcov <- CSBsumcp
    sameaxisorder <- 0
    if (resampledistn == "nonparametric") {
        bno <- matrix(sample(rep(1:rows, nboots), replace = TRUE), 
            rows, nboots)
    }
    else {
        if (resampledistn == "balanced") {
            bno <- matrix(sample(rep(1:rows, nboots)), rows, 
                nboots)
        }
    }
    for (b in 1:nboots) {
        if (resampledistn == "multinomial") {
            Xr <- vector("numeric", rows * cols)
            if (multinomialtype == "whole") {
                Xr <- rmultinom(1, n, X)
            }
            if (multinomialtype == "rowsfixed") {
                for (i in 1:rows) {
                  Xr[seq(i, (cols - 1) * rows + i, by = rows)] <- rmultinom(1, 
                    sum(X[i, ]), X[i, ])
                }
            }
            if (multinomialtype == "columnsfixed") {
                for (i in 1:cols) {
                  Xr[((i - 1) * rows + 1):(i * rows)] <- rmultinom(1, 
                    sum(X[, i]), X[, i])
                }
            }
        }
        else {
            if (resampledistn == "Poisson") {
                Xr <- rpois(rows * cols, X + poissonzeronewmean * 
                  (X == 0))
            }
            else {
                Xr <- X[bno[, b], ]
            }
        }
        XB <- matrix(data = Xr, nrow = rows, ncol = cols)
        if (newzeroreset == 1) {
            XB <- (XB == 0 & X > 0) + XB
        }
        B <- switch(catype, sca = sca(XB), mca = sca(XB), omca = sca(XB))
        zerorowB <- rowSums(XB > 0) == 0
        zerocolB <- colSums(XB > 0) == 0
        rownB <- rownB - zerorowB
        colnB <- colnB - zerocolB
        Re <- rearrange(S@Raxes, B@Raxes, S@Caxes, B@Caxes, B@r)
        RaxesBRe <- B@Raxes
        CaxesBRe <- B@Caxes
        RaxesBRe[, 1:Re$numrearranged] <- RaxesBRe[, 1:Re$numrearranged] \%*\% 
            Re$T
        CaxesBRe[, 1:Re$numrearranged] <- CaxesBRe[, 1:Re$numrearranged] \%*\% 
            Re$T
        RSB <- (B@Rprofile - S@Rprofile) \%*\% B@Rweights \%*\% RaxesBRe[, 
            1:axisvariances]
        CSB <- (B@Cprofile - S@Cprofile) \%*\% B@Cweights \%*\% CaxesBRe[, 
            1:axisvariances]
        RSB <- RSB * (1 - zerorowS) * (1 - zerorowB)
        CSB <- CSB * (1 - zerocolS) * (1 - zerocolB)
        sameaxisorder <- sameaxisorder + Re$same
        if (bootstdcoords == TRUE) {
            dmum1 <- diag(1/(B@mu + (B@mu == 0)) * (1 - (B@mu == 
                0)))
            dmum1[1:Re$numrearranged, 1:Re$numrearranged] <- dmum1[1:Re$numrearranged, 
                1:Re$numrearranged] \%*\% Re$T
            dmum1 <- dmum1[1:axisvariances, 1:axisvariances]
            RSB <- RSB \%*\% dmum1
            CSB <- CSB \%*\% dmum1
        }
        RSBsum <- RSBsum + RSB
        RSBsumsq <- RSBsumsq + RSB * RSB
        CSBsum <- CSBsum + CSB
        CSBsumsq <- CSBsumsq + CSB * CSB
        if (axisvariances > 1) {
            for (a1 in 1:(axisvariances - 1)) {
                for (a2 in (a1 + 1):axisvariances) {
                  RSBsumcp[, a1, a2] <- RSBsumcp[, a1, a2] + 
                    RSB[, a1] * RSB[, a2]
                  CSBsumcp[, a1, a2] <- CSBsumcp[, a1, a2] + 
                    CSB[, a1] * CSB[, a2]
                }
            }
        }
    }
    RSBmean <- diag((1/(rownB + (rownB == 0))) * (1 - (rownB == 
        0))) \%*\% RSBsum
    CSBmean <- diag((1/(colnB + (colnB == 0))) * (1 - (colnB == 
        0))) \%*\% CSBsum
    rbm1 <- diag((1/(rownB - 1 + 2 * (rownB <= 1))) * (1 - (rownB <= 
        1)))
    cbm1 <- diag((1/(colnB - 1 + 2 * (colnB <= 1))) * (1 - (colnB <= 
        1)))
    RSBvar <- rbm1 \%*\% (RSBsumsq - diag(rownB) \%*\% RSBmean * 
        RSBmean)
    CSBvar <- cbm1 \%*\% (CSBsumsq - diag(colnB) \%*\% CSBmean * 
        CSBmean)
    if (axisvariances > 1) {
        for (a1 in 1:(axisvariances - 1)) {
            for (a2 in (a1 + 1):axisvariances) {
                RSBcov[, a1, a2] <- rbm1 \%*\% (RSBsumcp[, a1, 
                  a2] - diag(rownB) \%*\% RSBmean[, a1] * RSBmean[, 
                  a2])
                CSBcov[, a1, a2] <- cbm1 \%*\% (CSBsumcp[, a1, 
                  a2] - diag(colnB) \%*\% CSBmean[, a1] * CSBmean[, 
                  a2])
            }
        }
    }
    Fmat <- S@Rprofile \%*\% S@Rweights \%*\% S@Raxes
    Gmat <- S@Cprofile \%*\% S@Cweights \%*\% S@Caxes
    dmum1 <- diag(1/(S@mu + (S@mu == 0)) * (1 - (S@mu == 0)))
    Gbi <- Gmat \%*\% dmum1
    Fbi <- Fmat \%*\% dmum1
    inertia <- S@mu * S@mu
    dmum2 <- diag(1/(inertia + (S@mu == 0)) * (1 - (S@mu == 0)))
    inertiasum <- sum(inertia)
    inertiapc <- 100 * inertia/inertiasum
    cuminertiapc <- cumsum(inertiapc)
    inertiapc <- round(100 * inertiapc)/100
    cuminertiapc <- round(100 * cuminertiapc)/100
    inertias <- cbind(inertia, inertiapc, cuminertiapc)
    Xstd <- X/sum(X)
    dr <- diag(as.vector(rowSums(Xstd)))
    dc <- diag(as.vector(colSums(Xstd)))
    Fsq <- Fmat * Fmat
    RowCTR <- dr \%*\% Fsq \%*\% dmum2
    Frs <- diag(1/rowSums(Fsq))
    RowREP <- Frs \%*\% Fsq
    Gsq <- Gmat * Gmat
    ColCTR <- dc \%*\% Gsq \%*\% dmum2
    Grs <- diag(1/rowSums(Gsq))
    ColREP <- Grs \%*\% Gsq
    bootca <- new("cabootcrsresults", br = S, DataMatrix = X, 
        rows = rows, columns = cols, rowlabels = rowlabels, collabels = collabels, 
        Rowprinccoord = Fmat, Colprinccoord = Gmat, Rowstdcoord = Fbi, 
        Colstdcoord = Gbi, RowCTR = RowCTR, RowREP = RowREP, 
        ColCTR = ColCTR, ColREP = ColREP, RowVar = RSBvar, RowCov = RSBcov, 
        ColVar = CSBvar, ColCov = CSBcov, inertiasum = inertiasum, 
        inertias = inertias, nboots = nboots, resampledistn = resampledistn, 
        multinomialtype = multinomialtype, sameaxisorder = sameaxisorder, 
        poissonzeronewmean = poissonzeronewmean, newzeroreset = newzeroreset, 
        printdims = printdims, axisvariances = axisvariances)
    summaryca(bootca, datasetname = as.character(datafile))
    plotca(bootca, datasetname = as.character(datafile), groupings = groupings, 
        grouplabels = grouplabels, plotsymbolscolours = plotsymbolscolours, 
        othersmonochrome = othersmonochrome, crpercent = crpercent)
    bootca
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
