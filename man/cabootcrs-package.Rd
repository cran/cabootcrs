\name{cabootcrs-package}
\alias{cabootcrs-package}
\docType{package}
\title{
Bootstrap Confidence Regions for Correspondence Analysis 
}
\description{
Performs simple (classical) correspondence analysis
on a two-way contingency table and produces bootstrap-based confidence regions
around the projected coordinates for the category points.
Includes additional routines for summarising the output and for
plotting the results in a variety of ways, including both french and biplot styles. 
}
\details{
\tabular{ll}{
Package: \tab cabootcrs\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-04-23\cr
License: \tab GPL-2\cr
Depends: \tab Base only\cr
}
Correspondence Analysis plots usually only show the coordinates for each of
the row and column category points projected onto the new axes, with no indication of 
the degree of sampling variation. This package produces bootstrap-based confidence
ellipses for each of the row and column points with respect to the axes shown. 

These confidence regions are based on the sampling variation of the difference between
sample and population points when both are projected onto the sample axes, allowing for
variation in both points and axes and the correlation between them. Hence the coverage
percentage is the chance of drawing a sample such that the confidence ellipse contains 
the population point when it is projected onto the samples axes as a supplementary point.
See the reference below for further details.

There are options for different ways of generating the bootstrap resamples,
notably based on either the Poisson or the multinomial distribution, with
the latter allowing the option of fixed row or column sums. 

Correspondence analysis results can be plotted in two main ways. 
The default option here is to produce a biplot where the row category
points are plotted in principal coordinates 
(i.e. coordinates which allow for the different inertias of the axes) 
and the confidence ellipses are shown for these row category points. 
The column category points are shown in standard coordinates on this plot
and drawn as directions in the common biplot style. 
A second biplot is also produced where the roles of the rows and columns are reversed. 

The other main plotting option is to produce a "french-style" plot where both 
row categories and column categories are plotted as points in principal coordinates.
However, again two plots are produced, one with confidence ellipses 
for the row category points and one with confidence ellipses for the
column category points. This is a deliberate restriction, partly to reduce
plot clutter but mostly to emphasize that the row and column points
are in different spaces and that their relative positions should not be over-interpreted. 

There are several options for different ways of plotting the data,  
with simple options to vary the colour schemes, or to suppress point labels, 
or to show only a few of the ellipses, intended particularly to reduce 
the clutter in pictures with large data matrices. 
There is also an option for fuller control over the graphics, by supplying
either files or data frames to define groups of points which can be plotted
in common colours and symbols, or to suppress their point labels and ellipses.

The package can also be used just to perform Correspondence Analysis as usual,
but with the above plotting options available. 

The package does not use any routines from any of the many
other Correspondence Analysis packages, only base R routines. This was deliberate,
in order to maintain control over the precise details. 
}
\author{
T.J. Ringrose <t.j.ringrose@cranfield.ac.uk>
}
\references{
Ringrose, T.J. (2012).
Bootstrap confidence regions for correspondence analysis. \cr
Journal of Statistical Computation and Simulation.
Vol 83, No. 10, October 2012, 1397-1413. 
}
\keyword{ package }
\keyword{ correspondence analysis }
\keyword{ confidence ellipse }
\keyword{ resampling }

\seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
\code{\link[cabootcrs]{cabootcrs}} ,
\code{\link[cabootcrs]{printca}} ,
%% \code{\link{plot.cabootcrsresults}} ,
\code{\link[cabootcrs]{plotca}} ,
%% \code{\link{summary.cabootcrsresults}} ,
\code{\link[cabootcrs]{summaryca}} ,
\code{\link{covmat}} ,
\code{\link{allvarscovs}} ,
\code{\linkS4class{cabootcrsresults}}
}
\examples{
# Data frame, with row and column labels, from file

data(DreamData)

# Matrix with no labels

dreamdata <- t(matrix(c(7,4,3,7,10,15,11,13,23,9,11,7,28,9,12,10,32,5,4,3),4,5))

# Calculate variances and produce confidence ellipses for the Dream data set, 
# with labels taken from those in the data file and default symbols and colours. 
# Use all defaults: 1000 bootstraps, Poisson resampling, calculate variances 
# only for first two axes, but give usual output for up to the first 4 axes. 
# Show one biplot with confidence ellipses for row points in principal coordinates,
# another biplot with confidence ellipses for column points in principal coordinates.
# In each case the other set of points are in standard coordinates, but note that the 
# lines are cropped to fit the plot by default, as it is the directions that matter most. 

bd <- cabootcrs(DreamData)

# Same thing, but input data matrix rather than read from file,
# rows and columns by default just labelled by their number.

\dontrun{
bd2 <- cabootcrs(dreamdata)

# Plot in "french" style where both rows and columns are in principal coordinates, 
# not as a biplot, but still produce two plots, with row ellipses in one plot 
# and column ellipses in the other. 

plotca(bd, plottype="french")

# Calculate variances and covariances for axes 1-3, though only plots axis 1 versus 2. 
# Then plot axis 1 against axis 3.

bd3 <- cabootcrs(DreamData, lastaxis=3)
plotca(bd3, firstaxis=1, lastaxis=3)

# See the stored results, an object of type cabootcrsresults.

bd
}

# Prettier printed output, no plots.  

printca(bd)

# Brief summary output, similar style to ca package, no plots.  

summaryca(bd,datasetname="Dreams")

# Extract the covariance matrix of: 
# row 4 for axes 1 and 2; 
# column 1 for axes 1 and 2.

vmr4 <- covmat(bd,4,"row",1,2)
vmc1 <- covmat(bd,1,"column",1,2)

# Display all variances and covariances for each row and column, axes 1-2.

allvarscovs(bd, "rows")
allvarscovs(bd, "columns")

}
