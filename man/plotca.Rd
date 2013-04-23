\name{plotca}
\alias{plotca}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce correspondence analysis plots with confidence ellipses
}
\description{
Produces two plots of correspondence analysis results, one with confidence regions
for row categories and one with confidence regions for column categories. 

In the following, the categories for which confidence regions are being given are referred 
to as the primary points, the others as the secondary points. 
The primary points are always plotted in principal coordinates while the secondary
points can be in standard (biplot style) or principal (french style) coordinates. 

The default colour scheme is for the primary points and their confidence ellipses
to be plotted each in a different colour, as this makes it easier to see which 
ellipse goes with which point, while the secondary points are all plotted in black
to make it easier to distinguish between the two sets of points. This can all be changed. 

Note that the plots will look better if saved as .eps or .pdf, rather than viewed in R or as .jpg or .png.
}
\usage{
plotca(x, datasetname = "", 
   showrowlabels = TRUE, showcolumnlabels = TRUE, 
   groupings = NULL, grouplabels = NULL, 
   plotsymbolscolours = c(19, "alldifferent", 18, "alldifferent"), 
   othersmonochrome = "black", crpercent = 95, plottype = "biplot", 
   showrowcrs = TRUE, showcolumncrs = TRUE, 
   firstaxis = 1, lastaxis = 2, plotallpairs = FALSE, 
   picsize = c(-1, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object of class cabootcrsresults.
}
  \item{datasetname}{
name of data set (in " "), to appear on plots.
}
  \item{showrowlabels}{
TRUE - label row points as usual. \cr
FALSE - suppress labels of row points. 
}
  \item{showcolumnlabels}{
TRUE - label column points as usual. \cr
FALSE - suppress labels of column points. 
}
  \item{groupings}{
name of file (in " ") or data frame containing group structure of row and column points;
if the n rows are divided into m groups and the p columns divided into k groups, the
file or data frame is n+p by 2, with the first column just 1..n 1..p (purely to make the
file easier to read) and the second column containing the number of the group-of-rows (1..m)
or group-of-columns (1..k) that the row or column belongs to:  

1 < the number of the group-of-rows to which row 1 belongs > \cr
 \dots \cr
n < the number of the group-of-rows to which row n belongs > \cr
1 < the number of the group-of-columns to which column 1 belongs > \cr
 \dots \cr
p < the number of the group-of-columns to which column p belongs > \cr \cr
}
  \item{grouplabels}{
name of file (in " ") or data frame containing the colours and labels to be used, 
in association with the groupings option above, in a m+k by 5 array: 

1 < legend > < plot symbol > < plot colour > < draw ellipse? > \cr
 \dots \cr
m < legend > < plot symbol > < plot colour > < draw ellipse? > \cr
1 < legend > < plot symbol > < plot colour > < draw ellipse? > \cr
 \dots \cr
k < legend > < plot symbol > < plot colour > < draw ellipse? > 

legend (in " " in data frame but not in file) - for this group of rows/columns, to be shown on plot \cr
plot symbol - for all rows/columns in this group, either an R number code for a symbol 
or a character (the latter in " " if in a data frame but not if in a file) \cr
colour (in " ") - for symbols and ellipses for this group \cr
T - draw ellipses and label points for this group, F - suppress them
(both in " " in data frame but not in file)  

See notes section and examples below to make more sense of this. 

These options are particularly intended for large data sets,
to allow attention to be drawn to some points above others,
to emphasize any group structure within the data, or to show only the
most important ellipses in order to make the picture less cluttered. \cr \cr
}
  \item{plotsymbolscolours}{
4-vector in the form c(row symbol,"row colour", column symbol,"column colour") \cr
giving plot symbols and colours for row and column points and ellipses
when they are the primary points. \cr
First element: \cr
  any R number code - the plot symbol for row points. \cr
  character (in " ") - the plot symbol for row points. \cr
Second element: \cr
  a valid R colour (in " ") - colour for row points, their ellipses and labels. \cr
  "alldifferent" - each row is plotted with a different colour, going through the R colour list 
    with the first few rows as reds and the last few rows as blues. \cr
  "differentreds" - each row is plotted with a different shade of red to green. \cr
  "differentblues" - each row is plotted with a different shade of green to blue. \cr
Third element: \cr
  as first element but for column points. \cr
Fourth element: \cr
  as second element but for column points.
}
  \item{othersmonochrome}{
a valid R colour (in " ") - all secondary points are plotted in this colour. \cr
NULL - secondary points are plotted using the same colours as when they are the primary points. 
}
  \item{crpercent}{
notional coverage percentage of the confidence ellipses.
}
  \item{plottype}{
"biplot" - one plot with confidence regions for rows in principal coordinates 
while columns are shown as directions in standard coordinates, another 
plot with confidence regions for columns in principal coordinates while rows
are shown as directions in standard coordinates. \cr
"french" - two plots each with both rows and columns in principal coordinates,
one plot shows confidence regions for the rows and the other shows
confidence regions for the columns.
}
  \item{showrowcrs}{
TRUE - plot all confidence ellipses for row points as usual. \cr
A row number or vector of row numbers - plot confidence ellipses only for these row points,
which can be specified in any standard R format such as 7 or c(1,4,9) or 5:11.  \cr
FALSE - suppress plotting of all confidence ellipses for row points.
}
  \item{showcolumncrs}{
TRUE - plot all confidence ellipses for column points as usual. \cr
A column number or vector of column numbers - plot confidence ellipses only for these column points,
which can be specified in any standard R format such as 7 or c(1,4,9) or 5:11.  \cr
FALSE - suppress plotting of all confidence ellipses for column points.
}
  \item{firstaxis}{
number of first (i.e. highest inertia) axis to be plotted.
}
  \item{lastaxis}{
number of last (i.e. lowest inertia) axis to be plotted, must be <= axisvariances value for x.
}
  \item{plotallpairs}{
FALSE - plot firstaxis v lastaxis only. \cr
TRUE - plot all pairs of axes between firstaxis and lastaxis.
}
  \item{picsize}{
if 2-vector, minimum and maximum of both x and y axes on each plot. \cr
if 4-vector, min and max of x axis followed by min and max of y axis. \cr
note that the same scales are used for both plots, so in biplot case it
might occasionally be preferred to run plotca twice with different
picsize values, one being better for rows in principal coordinates
and the other better for columns in principal coordinates. \cr
note also that if picsize is used to focus in on a particular area
of the plot then biplot labels might not appear properly.
}
}
\details{
For large matrices the plots from exploratory multivariate methods
are often so busy that the whole point of the method, to clarify
the structure of the data, is nullified. This is even more of a
problem when confidence regions are shown on the plots. 

The showrowcrs, showcolumncrs, showrowlabels, showcolumnlabels
and othersmonochrome options are available as ways of reducing plot
clutter in large data sets, for example by showing the column points
unlabelled and monochrome as a way of drawing the eye to the 
multicoloured row points and ellipses. 

Note that french-style plots
are often less cluttered because they omit the biplot lines,  
while they also show the two sets of points on similar scales 
so that it is easier to fit all the points on one picture 
without cropping or excessive empty space.  

The groupings and grouplabels options are chosen via separate text files
or data frames to define the groups of points, as it is usually easier 
to edit these than to edit lists of command options in R. 
}
\value{
Two plots with confidence regions shown. 
}
\author{
T.J. Ringrose
}
\note{
There are two ways of defining groupings and group labels. The first of these is by
defining a pair of data frames within R and supplying them as parameters either to
cabootcrs initially or to plotca. This method works in R CMD check and hence is the
one used in the examples, but as you can see is rather hard to follow: 

data(DreamData) \cr
bd <- cabootcrs(DreamData) 

groupingsframe <- cbind(c(1:5,1:4),c(1,1,2,2,3,1,1,2,2)) \cr
grouplabframe <- cbind( c(1,2,3,1,2), c("AB","CD","E","ab","cd"), c(19,20,21,"+","*"),  \cr
                        c("green","blue","yellow","red","orange"), "T" ) \cr
plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe) 


A version which produces identical results, but does not work in R CMD check, is 
usually much easier for the user. The groupings and group labels are defined in files,
present in the directory specified in setwd(). To obtain identical results to the above,
create two text files: 

DreamGroupings.txt contains

1 1\cr
2 1\cr
3 2\cr
4 2\cr
5 3\cr
1 1\cr
2 1\cr
3 2\cr
4 2

e.g. the first two lines show that rows 1,2 belong to group-of-rows 1, 
while the last two lines show that columns 3,4 belong to group-of-columns 2. 

DreamGroupLabels.txt contains

1 AB 19  "green"  T\cr
2 CD 20  "blue"   T\cr
3  E 21  "yellow" T\cr
1 ab  +  "red"    T\cr
2 cd  *  "orange"  T

e.g. group-of-rows 1 will be shown in green and plotted with symbol 19,
with the legend AB. 

These files can be edited outside R, which is usually much easier than
doing things within R, and used in plotting with: 

plotca(bd, groupings="DreamGroupings.txt", grouplabels="DreamGroupLabels.txt") 


Note that plotca, summaryca and printca are all defined as new functions, 
rather than as overloaded versions of plot, summary and print, 
simply in order to avoid complication and unintended consequences within R. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{printca}} ,
\code{\link{summaryca}} ,
\code{\linkS4class{cabootcrsresults}} 
}
\examples{

# the main function call plots with the default options

data(DreamData)
bd <- cabootcrs(DreamData)

\dontrun{
# Plot with specified size to fit the whole of the arrows in without cropping 
 
plotca(bd, picsize=c(-2.5,2.5))

# or smaller, note the warning

plotca(bd, picsize=c(-0.5,0.5))

# 90% regions with different colour scheme

plotca(bd, plotsymbolscolours=c(3,"differentreds","*","blue"), crpercent=90)

# suppress labels for column points, to de-clutter row points picture,
# this mostly useful for larger data sets than this one

plotca(bd, showcolumnlabels=FALSE, datasetname="Dream data")

# only show ellipses for rows 1, 1-2 and 1-3 respectively 

plotca(bd, showrowcrs=1)
plotca(bd, showrowcrs=c(1,2))
plotca(bd, showrowcrs=1:3)


# both plots almost the same as the plot from the ca() package

plotca(bd,plottype="french",showrowcrs=FALSE,showcolumncrs=FALSE,othersmonochrome=NULL,
        plotsymbolscolours=c(19,"blue",17,"red"),picsize=c(-0.5,0.6))

# plot axes 1 v 2, 1 v 3 and 2 v 3

bd3 <- cabootcrs(DreamData, lastaxis=3)
plotca(bd3, firstaxis=1, lastaxis=3, plotallpairs=TRUE)
}

# more complicated plotting, define group structure in data frames

groupingsframe <- cbind(c(1:5,1:4),c(1,1,2,2,3,1,1,2,2))
grouplabframe <- cbind( c(1,2,3,1,2), c("AB","CD","E","ab","cd"), c(19,20,21,"+","*"), 
                        c("green","blue","yellow","red","orange"), "T" )
plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe)

\dontrun{
plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe, plottype="french")
}


## The function is currently defined as
function (x, datasetname = "", showrowlabels = TRUE, showcolumnlabels = TRUE, 
    groupings = NULL, grouplabels = NULL, plotsymbolscolours = c(19, 
        "alldifferent", 18, "alldifferent"), othersmonochrome = "black", 
    crpercent = 95, plottype = "biplot", showrowcrs = TRUE, showcolumncrs = TRUE, 
    firstaxis = 1, lastaxis = 2, plotallpairs = FALSE, picsize = c(-1, 
        1)) 
{
    plotonepic <- function(a1, a2, plottype, things, nthings, 
        nvars, Thingcoord, Varcoord, SBvar, SBcov, twoS, inertiapc, 
        resampledistn, multinomialtype, thinggroup, thinggrlab, 
        vargroup, vargrlab, thinglabels, varlabels, showcrs, 
        picsizex, picsizey) {
        eps <- 1e-15
        critchisq2 <- qchisq(0.01 * crpercent, 2)
        critchisq1 <- qchisq(0.01 * crpercent, 1)
        theta <- seq(0, 2 * pi, 0.001)
        ellipsecoords <- rbind(sin(theta), cos(theta))
        thinggrlab3 <- as.list(thinggrlab[[3]])
        thinggrlab3int <- !is.na(as.integer(thinggrlab3))
        for (i in 1:max(thinggroup[, 2])) {
            if (thinggrlab3int[[i]]) {
                thinggrlab3[[i]] <- as.integer(thinggrlab3[[i]])
            }
        }
        if (plottype == "french") {
            vargrlab3 <- as.list(vargrlab[[3]])
            vargrlab3int <- !is.na(as.integer(vargrlab3))
            for (i in 1:max(vargroup[, 2])) {
                if (vargrlab3int[[i]]) {
                  vargrlab3[[i]] <- as.integer(vargrlab3[[i]])
                }
            }
        }
        dev.new()
        plot(Thingcoord[1, a1], Thingcoord[1, a2], xlim = picsizex, 
            ylim = picsizey, xlab = paste("Axis ", a1, "    ", 
                inertiapc[a1], "\%", sep = ""), ylab = paste("Axis ", 
                a2, "    ", inertiapc[a2], "\%", sep = ""), asp = 1, 
            pch = thinggrlab3[[thinggroup[1, 2]]], col = thinggrlab[[4]][thinggroup[1, 
                2]])
        for (i in 2:nthings) {
            points(Thingcoord[i, a1], Thingcoord[i, a2], asp = 1, 
                pch = thinggrlab3[[thinggroup[i, 2]]], col = thinggrlab[[4]][thinggroup[i, 
                  2]])
        }
        abline(h = 0, v = 0)
        if (!all(thinggrlab[[2]] == "")) {
            labnum <- as.integer(thinggrlab3)
            labchar <- as.character(thinggrlab3)
            legend("topleft", thinggrlab[[2]], pch = labnum, 
                col = thinggrlab[[4]], text.col = thinggrlab[[4]])
            for (i in 1:max(thinggroup[, 2])) {
                if (is.na(labnum[[i]])) {
                  labchar[[i]] <- thinggrlab3[[i]]
                }
                else {
                  labchar[[i]] <- NA
                }
            }
            legend("topleft", thinggrlab[[2]], pch = labchar, 
                col = thinggrlab[[4]], text.col = thinggrlab[[4]])
        }
        if (plottype == "biplot") {
            if ((x@nboots > 0) & (any(showcrs == TRUE))) {
                title(paste("Confidence regions for biplot of", 
                  things, "\n \n", datasetname))
                title(paste("\n", resampledistn, "resampling,", 
                  switch(multinomialtype, whole = "", rowsfixed = "row sums fixed,", 
                    columnsfixed = "column sums fixed,"), x@nboots, 
                  "resamples \n"), font.main = 1)
            }
            else {
                title(paste("Biplot of", things, "\n", datasetname))
            }
            for (i in 1:nvars) {
                lines(c(0, Varcoord[i, a1]), c(0, Varcoord[i, 
                  a2]), col = vargrlab[[4]][vargroup[[2]][i]])
            }
            grat <- cbind(Varcoord[, a1]/picsizex[1], Varcoord[, 
                a1]/picsizex[2], Varcoord[, a2]/picsizey[1], 
                Varcoord[, a2]/picsizey[2], 0.95)/0.95
            cl <- 1.05/apply(grat, 1, max)
            text(cl * Varcoord[, a1], cl * Varcoord[, a2], labels = varlabels, 
                col = vargrlab[[4]][vargroup[[2]]], pos = 4, 
                cex = 0.75)
        }
        else {
            if ((x@nboots > 0) & (any(showcrs == TRUE))) {
                title(paste("Confidence regions for", things, 
                  "\n \n", datasetname))
                title(paste("\n", resampledistn, "resampling,", 
                  switch(multinomialtype, whole = "", rowsfixed = "row sums fixed,", 
                    columnsfixed = "column sums fixed,"), x@nboots, 
                  "resamples \n"), font.main = 1)
            }
            else {
                title(paste("Correspondence plot \n", datasetname))
            }
            for (i in 1:nvars) {
                points(Varcoord[i, a1], Varcoord[i, a2], asp = 1, 
                  pch = vargrlab3[[vargroup[i, 2]]], col = vargrlab[[4]][vargroup[i, 
                    2]])
            }
            text(Varcoord[, a1], Varcoord[, a2], labels = varlabels, 
                col = vargrlab[[4]][vargroup[[2]]], pos = 4, 
                cex = 0.75)
            if (!all(vargrlab[[2]] == "")) {
                labnum <- as.integer(vargrlab3)
                labchar <- as.character(vargrlab3)
                legend("topright", vargrlab[[2]], pch = labnum, 
                  col = vargrlab[[4]], text.col = vargrlab[[4]])
                for (i in 1:max(vargroup[, 2])) {
                  if (is.na(labnum[[i]])) {
                    labchar[[i]] <- vargrlab3[[i]]
                  }
                  else {
                    labchar[[i]] <- NA
                  }
                }
                legend("topright", vargrlab[[2]], pch = labchar, 
                  col = vargrlab[[4]], text.col = vargrlab[[4]])
            }
        }
        for (i in 1:nthings) {
            if (thinggrlab[[5]][thinggroup[i, 2]]) {
                text(Thingcoord[i, a1], Thingcoord[i, a2], labels = thinglabels[i], 
                  pos = 4, cex = 0.75, col = thinggrlab[[4]][thinggroup[i, 
                    2]])
                if (showcrs[i]) {
                  xbar <- Thingcoord[i, cbind(a1, a2)]
                  V <- matrix(c(SBvar[i, a1], SBcov[i, min(a1, 
                    a2), max(a1, a2)], SBcov[i, min(a1, a2), 
                    max(a1, a2)], SBvar[i, a2]), 2, 2)
                  E <- eigen(V, symmetric = TRUE)
                  usec2 <- (1 - twoS[i]) * (E$values[1] > eps)
                  critchisq <- critchisq2 * usec2 + critchisq1 * 
                    (1 - usec2)
                  coords <- E$vectors \%*\% (critchisq * diag(E$values))^(1/2) \%*\% 
                    ellipsecoords
                  lines(xbar[1] + coords[1, ], xbar[2] + coords[2, 
                    ], pch = ".", col = thinggrlab[[4]][thinggroup[i, 
                    2]])
                }
            }
        }
        if (any(Thingcoord[, a1] < picsizex[1])) {
            cat(paste("Warning: point outside plot limits, lowest x-value is ", 
                min(Thingcoord[, a1]), "\n"))
        }
        if (any(Thingcoord[, a1] > picsizex[2])) {
            cat(paste("Warning: point outside plot limits, largest x-value is ", 
                max(Thingcoord[, a1]), "\n"))
        }
        if (any(Thingcoord[, a2] < picsizey[1])) {
            cat(paste("Warning: point outside plot limits, lowest y-value is ", 
                min(Thingcoord[, a2]), "\n"))
        }
        if (any(Thingcoord[, a2] > picsizey[2])) {
            cat(paste("Warning: point outside plot limits, largest y-value is ", 
                max(Thingcoord[, a2]), "\n"))
        }
    }
    if (!is.null(plotsymbolscolours)) {
        if (!dim(array(plotsymbolscolours)) == 4) 
            stop(paste("plotsymbolscolours must contain row symbol and colour, 
                        column symbol and colour\n\n"))
    }
    if (!any(plotsymbolscolours[c(2, 4)] == c(colours(), "alldifferent", 
        "differentblues", "differentreds"))) 
        stop(paste("colours must be alldifferent, differentblues, differentreds 
                    or R colour (type colours() for full list) \n\n"))
    if ((crpercent <= 0) | (crpercent >= 100)) 
        stop(paste("coverage percentage must be between 0 and 100 exclusive\n\n"))
    if (!any(plottype == c("biplot", "french"))) 
        stop(paste("plotting must be biplot or french style\n\n"))
    if (!any(class(showrowcrs) == c("integer", "numeric", "logical"))) 
        stop(paste("showrowcrs must be logical or a vector of row numbers\n\n"))
    if (!any(class(showcolumncrs) == c("integer", "numeric", 
        "logical"))) 
        stop(paste("showcolumncrs must be logical or a vector of row numbers\n\n"))
    if ((firstaxis < 1) | (firstaxis > x@axisvariances - 1)) 
        stop(paste("incorrect first axis =", firstaxis, "\n\n"))
    if (lastaxis > x@axisvariances) 
        stop(paste("don't have variances for last axis =", lastaxis, 
            "\n\n"))
    if (firstaxis >= lastaxis) 
        stop(paste("last axis must be greater than first axis\n\n"))
    if (!any(dim(array(picsize)) == c(2, 4))) 
        stop(paste("picsize bounds are  lower,upper OR x lower,x upper,y lower,y upper \n\n"))
    if (picsize[1] >= picsize[2]) 
        stop(paste("incorrect axis scale picsize =", picsize[1], 
            picsize[2], "\n\n"))
    if (dim(array(picsize)) == 4) {
        if (picsize[3] >= picsize[4]) 
            stop(paste("incorrect y axis scale picsize =", picsize[3], 
                picsize[4], "\n\n"))
        if (abs((picsize[4] - picsize[3]) - (picsize[2] - picsize[1])) > 
            1e-10) 
            stop(paste("x and y axes must be same length\n\n"))
    }
    options(warn = -1)
    picsizey <- picsizex <- picsize[1:2]
    if (dim(array(picsize)) == 4) 
        picsizey <- picsize[3:4]
    tworowS <- rowSums(x@DataMatrix > 0) == 2
    twocolS <- colSums(x@DataMatrix > 0) == 2
    if (is.null(groupings)) {
        if (any(plotsymbolscolours[2] == c("alldifferent", "differentreds", 
            "differentblues"))) {
            rowgroup <- as.data.frame(cbind(1:x@rows, 1:x@rows))
            hv <- switch(plotsymbolscolours[2], alldifferent = c(0, 
                0.85), differentreds = c(0, 0.45), differentblues = c(0.5, 
                0.85))
            rowgrlab <- as.data.frame(cbind(1:x@rows, "", plotsymbolscolours[1], 
                rainbow(n = x@rows, start = hv[1], end = hv[2]), 
                "T"), stringsAsFactors = FALSE)
        }
        else {
            rowgroup <- as.data.frame(cbind(1:x@rows, rep(1, 
                x@rows)))
            rowgrlab <- as.data.frame(cbind(1, "", plotsymbolscolours[1], 
                plotsymbolscolours[2], "T"), stringsAsFactors = FALSE)
        }
        class(rowgrlab[, 1]) <- "integer"
        class(rowgrlab[, 5]) <- "logical"
        if (any(plotsymbolscolours[4] == c("alldifferent", "differentreds", 
            "differentblues"))) {
            colgroup <- as.data.frame(cbind(1:x@columns, 1:x@columns))
            hv <- switch(plotsymbolscolours[4], alldifferent = c(0, 
                0.85), differentreds = c(0, 0.45), differentblues = c(0.5, 
                0.85))
            colgrlab <- as.data.frame(cbind(1:x@columns, "", 
                plotsymbolscolours[3], rainbow(n = x@columns, 
                  start = hv[1], end = hv[2]), "T"), stringsAsFactors = FALSE)
        }
        else {
            colgroup <- as.data.frame(cbind(1:x@columns, rep(1, 
                x@columns)))
            colgrlab <- as.data.frame(cbind(1, "", plotsymbolscolours[3], 
                plotsymbolscolours[4], "T"), stringsAsFactors = FALSE)
        }
        class(colgrlab[, 1]) <- "integer"
        class(colgrlab[, 5]) <- "logical"
    }
    else {
        if (class(groupings) == "character") {
            rcgroup <- read.table(file = groupings, colClasses = c("integer", 
                "integer"))
        }
        else {
            rcgroup <- as.data.frame(groupings)
        }
        rowgroup <- rcgroup[1:x@rows, ]
        colgroup <- rcgroup[(x@rows + 1):(x@rows + x@columns), 
            ]
        nrowgroups <- max(rowgroup[, 2])
        ncolgroups <- max(colgroup[, 2])
        if (class(grouplabels) == "character") {
            rcgrlab <- read.table(file = grouplabels, colClasses = c("integer", 
                "character", "character", "character", "logical"))
        }
        else {
            rcgrlab <- as.data.frame(grouplabels, stringsAsFactors = FALSE)
            class(rcgrlab[, 1]) <- "integer"
            class(rcgrlab[, 5]) <- "logical"
        }
        rowgrlab <- rcgrlab[1:nrowgroups, ]
        colgrlab <- rcgrlab[(nrowgroups + 1):(nrowgroups + ncolgroups), 
            ]
    }
    rowcrs <- logical(length = x@rows)
    columncrs <- logical(length = x@columns)
    if (any(class(showrowcrs) == c("numeric", "integer"))) {
        for (i in 1:length(showrowcrs)) {
            rowcrs[showrowcrs[i]] <- TRUE
        }
    }
    else {
        rowcrs <- rowcrs | showrowcrs
    }
    if (any(class(showcolumncrs) == c("numeric", "integer"))) {
        for (i in 1:length(showcolumncrs)) {
            columncrs[showcolumncrs[i]] <- TRUE
        }
    }
    else {
        columncrs <- columncrs | showcolumncrs
    }
    vrowgrlab <- rowgrlab
    vcolgrlab <- colgrlab
    if (any(othersmonochrome == colours())) {
        vrowgrlab[[4]] <- othersmonochrome
        vcolgrlab[[4]] <- othersmonochrome
    }
    if (showrowlabels == TRUE) {
        rowptlabels <- x@rowlabels
    }
    else {
        rowptlabels <- NULL
    }
    if (showcolumnlabels == TRUE) {
        colptlabels <- x@collabels
    }
    else {
        colptlabels <- NULL
    }
    for (a1 in firstaxis:(lastaxis - 1)) {
        for (a2 in (a1 + 1):lastaxis) {
            if ((plotallpairs == TRUE) | ((a1 == firstaxis) & 
                (a2 == lastaxis))) {
                if (plottype == "biplot") {
                  plotonepic(a1, a2, plottype, "rows", x@rows, 
                    x@columns, x@Rowprinccoord, x@Colstdcoord, 
                    x@RowVar, x@RowCov, tworowS, x@inertias[, 
                      2], x@resampledistn, x@multinomialtype, 
                    rowgroup, rowgrlab, colgroup, vcolgrlab, 
                    rowptlabels, colptlabels, rowcrs, picsizex, 
                    picsizey)
                  plotonepic(a1, a2, plottype, "columns", x@columns, 
                    x@rows, x@Colprinccoord, x@Rowstdcoord, x@ColVar, 
                    x@ColCov, twocolS, x@inertias[, 2], x@resampledistn, 
                    x@multinomialtype, colgroup, colgrlab, rowgroup, 
                    vrowgrlab, colptlabels, rowptlabels, columncrs, 
                    picsizex, picsizey)
                }
                else {
                  plotonepic(a1, a2, plottype, "rows", x@rows, 
                    x@columns, x@Rowprinccoord, x@Colprinccoord, 
                    x@RowVar, x@RowCov, tworowS, x@inertias[, 
                      2], x@resampledistn, x@multinomialtype, 
                    rowgroup, rowgrlab, colgroup, vcolgrlab, 
                    rowptlabels, colptlabels, rowcrs, picsizex, 
                    picsizey)
                  plotonepic(a1, a2, plottype, "columns", x@columns, 
                    x@rows, x@Colprinccoord, x@Rowprinccoord, 
                    x@ColVar, x@ColCov, twocolS, x@inertias[, 
                      2], x@resampledistn, x@multinomialtype, 
                    colgroup, colgrlab, rowgroup, vrowgrlab, 
                    colptlabels, rowptlabels, columncrs, picsizex, 
                    picsizey)
                }
            }
        }
    }
    options(warn = 0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
 

