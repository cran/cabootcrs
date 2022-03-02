\name{cabootcrs-package}
\alias{cabootcrs-package}
\docType{package}
\title{
Bootstrap Confidence Regions for Simple and Multiple Correspondence Analysis
}
\description{
Performs simple (classical) correspondence analysis
on a two-way contingency table and produces bootstrap-based confidence regions
around the projected coordinates for the category points.
Includes additional routines for summarising the output and for
plotting the results in a variety of ways, including both french and biplot styles.

Performs multiple correspondence analysis (homogeneity analysis)
of a Burt matrix (a matrix of two-way contingency tables for p variables)
and produces bootstrap-based confidence regions
around the projected coordinates for the variable category points.
This includes a new method to correct the confidence regions
for the well-known distortion of the results
caused by the diagonal of the Burt matrix.
Also contains a highly experimental method to produce confidence regions
when analysing an indicator matrix.
}
\details{
\tabular{ll}{
Package: \tab cabootcrs\cr
Type: \tab Package\cr
Version: \tab 2.1.0\cr
Date: \tab 2022-02-03\cr
License: \tab GPL-3\cr
Depends: \tab lpSolve, colorspace \cr
}
\strong{Simple Correspondence Analysis:}

Correspondence Analysis plots usually only show the coordinates for each of
the row and column category points projected onto the new axes, with no indication of
the degree of sampling variation. This package produces bootstrap-based confidence
ellipses for each of the row and column points with respect to the axes shown.

These confidence regions are based on the sampling variation of the difference between
sample and population points when both are projected onto the sample axes, allowing for
variation in both the points and the axes and the correlation between them. Hence the coverage
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
with simple choices to vary the colour schemes, or to suppress point labels,
or to show only a few of the ellipses, intended particularly to reduce
the clutter in pictures with large data matrices.
There is also an option for fuller control over the graphics, by supplying
either files or data frames to define groups of points which can be plotted
in common colours and symbols, or to suppress their point labels and ellipses.

The package can also be used just to perform Correspondence Analysis as usual,
but with the above plotting options available.

The package can also be used just to produce the covariance matrices for each
of the category points, which for example can then be used with ellipse() to add
confidence ellipses to results from the ca() and mjca() (below) routines in the ca package.


\strong{Multiple Correspondence Analysis:}

The same principles as above carry over to the Multiple Correspondence Analysis
case with p categorical variables.

The recommended approach is to bootstrap the indicator matrix
and then apply MCA to the derived Burt matrix.
A new method is provided in the variance calculations to correct for the
well-known problems induced by the diagonal of the Burt matrix.

Standard options for correcting inertias and coordinates are also given.
In particular options exists such that MCA with p=2 gives the same
results as SCA, so that MCA can be regarded as a proper generalisation of SCA
(not all would agree with this, however).

The default plotting of the results is to produce p plots,
with each plot showing confidence ellipses for all of the categories of just one variable.
This is again done to reduce plot clutter, albeit at the expense of more plots.
Again, numerous options for controlling the plots are provided.

Bootstrapping can also be applied to the analysis of an indicator matrix or a doubled matrix,
but the procedure is highly experimental and very slow.

The data set can be input in numerous formats, and routines are provided
to convert between them. However note that the data cannot be inputted as
a Burt matrix because this loses some of the information in the data.

\strong{Changes from Version 1.0:}

Multiple correspondence analysis routines added.

Hungarian algorithm added for axis rearranging, to replace the embarrassingly poor
method used previously.

Bootstrap critical values added, and are now the default.

Numerous fairly minor changes made to existing simple correspondence analysis routines.

New routines to convert data between different formats.

There may be a little backwards incompatibility as some options to the routines
have changed a bit, but standard use with few options will still work.
Any changes should produce easily fixable failures to run.

\strong{Changes from Version 2.0:}

Debugging, including for plotca to call dev.new() when in R but not Rstudio,
and option to call it from Rstudio.

New routines to reflect axes and reorder categories,
e.g. for when using results with other packages.

New routine to add supplementary points to plots.

More options and examples for plotting.

\strong{Final Notes:}

The package does not use any routines from any of the
other Correspondence Analysis packages, only base R routines.
This was deliberate, in order to maintain control over the precise details.
The only external routines used are lp.assign, from the lpSolve package, for the Hungarian algorithm,
and some hcl functions from the colo(u)rspace package for plotting.

The results can be used as input to other packages, such as using ellipse()
to draw the ellipses on a plot from ca(), see \code{\link{covmat}}
and \code{\link{plotca}} for this.

}
\author{
T.J. Ringrose <t.j.ringrose@cranfield.ac.uk>
}
\references{
Ringrose, T.J. (2012).
Bootstrap confidence regions for correspondence analysis. \cr
Journal of Statistical Computation and Simulation.
Vol 83, No. 10, October 2012, 1397-1413.\cr\cr
A paper on the application to MCA is in preparation.
}
\keyword{ package }
\keyword{ correspondence analysis }
\keyword{ multiple correspondence analysis }
\keyword{ homogeneity analysis }
\keyword{ confidence ellipse }
\keyword{ resampling }

\seealso{
%% ~~ Optional links to other man pages, e.g. ~~
%% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
\code{\link[cabootcrs]{cabootcrs}} ,
\code{\link[cabootcrs]{plotca}} ,
\code{\link[cabootcrs]{summaryca}} ,
\code{\link[cabootcrs]{printca}} ,
\code{\link[cabootcrs]{convert}} ,
\code{\link[cabootcrs]{covmat}} ,
\code{\link[cabootcrs]{allvarscovs}} ,
\code{\link[cabootcrs]{reflectaxes}} ,
\code{\link[cabootcrs]{reordercategories}} ,
\code{\link[cabootcrs]{addsupplementary}} ,
\code{\link[cabootcrs]{sca}} ,
\code{\link[cabootcrs]{rearrange}} ,
\code{\linkS4class{cabootcrsresults}} ,
\code{\linkS4class{cabasicresults}}
}
\examples{

# Data frame of a contingency table, with row and column labels

data(DreamData)

# Perform (simple) correspondence analysis, calculate variances and show confidence ellipses.
# Use all defaults: 999 bootstrap replicates, Poisson resampling, calculate variances
# only for first two axes, but give usual output for up to the first 4 axes.
# Show one biplot with confidence ellipses for row points in principal coordinates,
# another biplot with confidence ellipses for column points in principal coordinates.
# In each case the other set of points are in standard coordinates, but note that the
# lines are cropped to fit the plot by default, as it is the directions that matter most.

bd <- cabootcrs(DreamData)

# Plot in "french" style where both rows and columns are in principal coordinates,
# not as a biplot, but still produce two plots, with row ellipses in one plot
# and column ellipses in the other.

plotca(bd, plottype="french")

\dontrun{

# See the stored results, an object of type cabootcrsresults

bd

# Prettier printed output, no plots.

printca(bd)

# Brief summary output, similar style to ca package, no plots

summaryca(bd,datasetname="Dreams")

# Extract the covariance matrix of:
# row 4 for axes 1 and 2;
# column 1 for axes 1 and 2.

vmr4 <- covmat(bd,4,"row",1,2)
vmc1 <- covmat(bd,1,"column",1,2)

# Display all variances and covariances for each row and column, axes 1-2

allvarscovs(bd, "rows")
allvarscovs(bd, "columns")

# Convert the data set into a 223 individuals by 2 variables
# matrix of category membership values

ddnbyp <- convert(DreamData,input="CT",output="nbyp")$result

# Perform multiple correspondence analysis with all defaults:
# non-parametric resampling, analyse Burt matrix, correct for
# the Burt diagonal in the inertias, coordinates and bootstrapping.
# Note that the coordinates, inertias etc are identical to those
# from simple CA above, while the standard deviations and hence
# the ellipses are very similar, but not quite the same.

bdmca <- cabootcrs(ddnbyp, catype="mca")

}

}

