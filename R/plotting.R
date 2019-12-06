
#' Plotting results with confidence regions
#'
#' \code{plotca} produces one or more scatterplots of the results of simple or multiple
#' correspondence analysis, with elliptical confidence regions around chosen points.
#'
#' In the following, the categories for which confidence regions are being shown are referred
#' to as the primary points, the others as the secondary points.
#' The primary points are always plotted in principal coordinates while the secondary
#' points can be in standard (biplot style in simple CA) or principal
#' (french style in simple CA, always in multiple CA) coordinates.
#'
#' The default colour scheme is for the primary points and their confidence ellipses
#' to be plotted each in a different colour, as this makes it easier to see which
#' ellipse goes with which point, while the secondary points are all plotted in monochrome
#' to make it easier to distinguish between the two sets of points.
#' This can all be controlled by the user.
#' Note that a point will still be treated as a primary point and plotted with its own
#' colour even when the plotting of its ellipse is suppressed with the showrowcrs or showcolumncrs options.
#'
#' Note that the plots will look better if saved as .eps or .pdf rather than as .jpg or .png.
#'
#' \strong{(1) Simple CA}
#'
#' Two plots are produced, in each plot one set of points (rows or columns) is regarded as
#' the primary set and is plotted in principal coordinates with confidence regions shown:\cr
#'   - one plot shows confidence regions for rows in principal coordinates\cr
#'   - one plot shows confidence regions for columns in principal coordinates\cr
#' The other set of points (columns or rows) is regarded as the secondary set and the plotting
#' depends on the choice of biplot or french-style plot:\cr
#'  biplot - secondary points shown as directions in standard coordinates\cr
#'  french - secondary points shown in principal coordinates
#'
#' \strong{(2) Multiple CA}
#'
#' All points are plotted in principal coordinates ("french")
#'
#' Burt matrix (mcatype="Burt"):
#'
#'   a) only plot the columns of the Burt matrix (the rows are the same)\cr
#'   b) plot all variable categories, i.e. columns\cr
#'   c) if mcaoneploteach=TRUE then produce p plots, each with CRs for all categories of one of the variables,\cr
#'   otherwise produce one plot showing CRs for all variables (busy)\cr
#'   d) Columns are ordered by variable then category
#'   (e.g. for p=3 with 5 categories each, columns 6:10 are variable 2)
#'
#' Indicator matrix (mcatype="indicator"):
#'
#'   a) if mcashowindividuals=TRUE then plot individual (row) points, without CRs\cr
#'   b) as Burt for variables
#'
#' Indicator matrix (mcatype="indicator") with experimental likert resampling (cabootcrs had mcaindividualboot=TRUE):
#'
#'   a) if mcashowindividuals=TRUE then plot individual (row) points, with CRs (busy)\cr
#'   b) as Burt for variables
#'
#' \strong{(3) Critical values}
#'
#' Critical values for the ellipses default to those specified in cabootcrs,
#' which default to bootstrap critical values
#'
#' \strong{(4) Choosing colours and which ellipses to show}
#'
#' The showrowcrs, showcolumncrs, showrowlabels, showcolumnlabels
#' and othersmonochrome options are available as ways of reducing plot
#' clutter in large data sets, for example by showing the column points
#' unlabelled and monochrome as a way of drawing the eye to the
#' multicoloured row points and ellipses.
#'
#' The default is for each primary point to be in a different colour,
#' with secondary points in the colour defined by othersmonochrome (default grey).
#' If othersmonochrome=NULL then secondary points are also plotted with different colours.
#'
#' Note that french-style plots in simple CA
#' are often less cluttered because they omit the biplot lines,
#' while they also show the two sets of points on similar scales
#' so that it is easier to fit all the points on one picture
#' without cropping or excessive empty space.
#'
#' \strong{(5) Specifying colours for (groups of) points and ellipses}
#'
#' For large matrices the plots from exploratory multivariate methods
#' are often so busy that the whole point of the method, to clarify
#' the structure of the data, is nullified. This is even more of a
#' problem when confidence regions are shown on the plots.
#'
#' Hence points can be defined in groups as below, so you can divide them into groups
#' in one or more ways, e.g. rows 1-3 in red and rows 4-8 in blue,
#' or rows 1-5 in green and rows 6-8 in orange etc.
#'
#' The groupings and grouplabels options are chosen via separate text files
#' or data frames to define the groups of points.
#' If groupings is left null then plotsymbolscolours is used instead.
#'
#' There are two ways of defining groupings and group labels. The first of these is by
#' defining a pair of data frames within R and supplying them as parameters either to
#' cabootcrs initially or to plotca. This method works in R CMD check and hence is the
#' one used in the examples, but as you can see is rather hard to follow.
#'
#' To plot with colours defined using groups-of-points:
#'
#' bd <- cabootcrs(DreamData)
#'
#' Then define the groups using data frames in R or text files:
#'
#' \strong{(5a) Using data frames}
#'
#' These data frames define the same groupings and colours as the files below,
#' see the files for a clearer explanation:
#'
#' \preformatted{
#' groupingsframe <- cbind(c(1:5,1:4),c(1,1,2,2,3,1,1,2,2))
#' grouplabframe <- cbind( c(1,2,3,1,2), c("AB","CD","E","ab","cd"), c(19,20,21,"+","*"), c("green","blue","yellow","red","orange"), "T" )
#' plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe)
#' }
#'
#' \strong{(5b) Using text files}
#'
#' A version which produces identical results, but does not work in R CMD check, is
#' usually much easier for the user as they can be edited outside R.
#' The groupings and group labels are defined in files,
#' present in the directory specified in setwd().
#' To obtain identical results to the above, create two text files as below:
#'
#' DreamGroupings.txt contains
#'
#' \preformatted{
#' 1 1
#' 2 1
#' 3 2
#' 4 2
#' 5 3
#' 1 1
#' 2 1
#' 3 2
#' 4 2
#' }
#'
#' e.g. the first two lines show that rows 1,2 belong to group-of-rows 1,
#' while the last two lines show that columns 3,4 belong to group-of-columns 2.
#'
#' DreamGroupLabels.txt contains
#'
#' \preformatted{
#' 1 AB 19  "green"  T
#' 2 CD 20  "blue"   T
#' 3  E 21  "yellow" T
#' 1 ab  +  "red"    T
#' 2 cd  *  "orange"  T
#' }
#'
#' e.g. group-of-rows 1 will be shown in green and plotted with symbol 19, with the legend AB.
#'
#' plotca(bd, groupings="DreamGroupings.txt", grouplabels="DreamGroupLabels.txt")
#'
#' \strong{(5c) General use}
#'
#' Even without groupings this can be used to specify all colours, simply by specifying each point as its own group,
#' in this case rows 1-5 and columns 1-4 define row groups 1-5 and column groups 1-4, no legend is required so repeat "",
#' choose 9 plot symbols and 9 colours.\cr
#' Hence to plot each point with its own colour and symbol:
#'
#' \preformatted{
#' groupingsframe <- cbind(c(1:5,1:4),c(1:5,1:4))
#' grouplabframe <- cbind( c(1:5,1:4), rep("",9), 11:19, c("green","blue","yellow","red","orange","grey1","grey22","grey44","grey66"), "T" )
#' plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe)
#' }
#'
#' \strong{(5d) MCA use}
#'
#' As before, but need to specify for both row and column categories even though only row categories will be plotted,
#' so just duplicate the data frames (yes I know it's a bodge). \cr
#' Hence to plot each point with your own choice of colour and symbol:
#'
#' \preformatted{
#' bd3 <- cabootcrs(DreamData223by3, catype="mca", varandcat=FALSE)
#' groupingsframe <- cbind(1:12,1:12)
#' groupingsframe <- rbind(groupingsframe,groupingsframe)
#' grouplabframe <- cbind( 1:12, rep("",12), 11:22, c("green","blue","yellow","red","orange","grey1","grey22","grey44","grey66","coral1","coral2","coral3"), "T" )
#' grouplabframe <- rbind(grouplabframe,grouplabframe)
#' plotca(bd3, groupings=groupingsframe, grouplabels=grouplabframe)
#' }
#'
#' This can also be used to plot the individual points with colours to denote groups, for example
#'
#' \preformatted{
#' bd3indnoboot <- cabootcrs(DreamData223by3,catype="mca",mcatype="indicator",varandcat=FALSE,nboots=0)
#' rowgroups <- cbind( 1:223, c(rep(1,100),rep(2,123)) )
#' colgroups <- cbind(1:12,1:12)
#' groupingsframe <- rbind( rowgroups, colgroups )
#' rowlabs <- cbind( 1:2, c("1-100","101-223"), c("+","+"), c("black","grey"), "T")
#' collabs <- cbind( 1:12, rep("",12), "*", c("green","cyan","yellow","red","orange","blue","blue1","blue2","blue3","coral1","coral2","coral3"), "T" )
#' grouplabframe <- rbind(rowlabs,collabs)
#' plotca(bd3indnoboot, groupings=groupingsframe, grouplabels=grouplabframe, mcashowindividuals=TRUE, mcaoneploteach=FALSE, showrowlabels=FALSE, othersmonochrome=NULL, picsize=c(-.36,.36))
#' }
#'
#' \strong{(6) Plotting results from cabootcrs() using ellipse() or ca()}
#'
#' This can be used with the ellipse() package to add the confidence ellipse to a picture from another package
#'
#' Example: confidence ellipse for row or column i on axes 1,2 from cabootcrs output Results is:
#'
#' \preformatted{
#' lines(ellipse(x=covmat(Results,i,"row",1,2,FALSE), centre=Results@Rowprinccoord[i,cbind(1,2)], npoints=1000), cex=1, pch=".", col="blue")
#' lines(ellipse(x=covmat(Results,i,"column",1,2,FALSE), centre=Results@Colprinccoord[i,cbind(1,2)], npoints=1000), cex=1, pch=".", col="blue")
#' }
#'
#' Example: to add row CRs to a plot from the ca() package to data set TheData
#'
#' \preformatted{
#' Results <- cabootcrs(TheData, showresults=FALSE)
#' caResults <- ca(TheData)
#' plot(caResults)
#' for (i in 1:dim(TheData)[1]) \{
#'   lines(ellipse(x=covmat(Results,i,"row",1,2,FALSE), centre=Results@Rowprinccoord[i,cbind(1,2)], npoints=1000), cex=1, pch=".", col="blue")
#' \}
#' }
#'
#' However note that reflectaxes() may also be needed.
#'
#' \strong{(7) Note}
#'
#' Note that plotca, summaryca and printca are all defined as new functions,
#' rather than as overloaded versions of plot, summary and print,
#' simply in order to avoid complication and unintended consequences within R
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @param datasetname A string to use as the name of the data set in the plots
#'
#' @param showrowlabels If TRUE then label row points as usual, otherwise
#' suppress labels of row points.
#'
#' Note: when analysing a Burt matrix the columns points are used
#'
#' @param showcolumnlabels If TRUE then label column points as usual, otherwise
#' suppress labels of column points.
#'
#' Note: when analysing a Burt matrix the columns points are plotted
#'
#' @param plotsymbolscolours A vector/list of length 1, 2 or 4.
#'
#' If length 4 then it takes the form:
#'
#' c(row symbol,"row colour", column symbol,"column colour")
#'
#' giving plot symbols and colours for row and column points and ellipses
#' when they are the primary points.
#' \describe{
#' \item{First element:}{The plot symbol for row points - either an R number code (usually 0-25), or a character (in " ")}
#' \item{Second element:}{The colour for row points, their ellipses and labels - either a valid R colour (in " ")  or \cr
#' "differentreds" - each row is plotted with a different shade of red to green \cr
#' "differentblues" - each row is plotted with a different shade of green to blue \cr
#' "alldifferent" - each row is plotted with a different colour, going through the R colour list
#'  with the first few rows as reds and the last few rows as blues}
#' \item{Third element:}{As first element but for column points}
#' \item{Fourth element:}{As second element but for column points}
#' }
#' If length 2 then these are assumed to be the symbol and colour(s) for columns only.
#'
#' If length 1 then this is assumed to be the colour(s) for columns only.
#'
#' Note: colour can also be specified as colours()[i] which picks out the i-th colour
#'
#' The idea behind "alldifferent" etc is that the colours change gradually, so that if the order of the rows/columns is
#' meaningful then the colour change tracks this.
#'
#' Note: groupings and grouplabels below override all this.
#'
#' @param othersmonochrome Either:
#' \describe{
#' \item{NULL}{secondary points plotted using same colours as when they are primary points}
#' \item{a valid R colour (in " ")}{all secondary points are plotted in this colour}
#' }
#'
#' @param crpercent The nominal coverage percentage of the confidence ellipses
#' (90, 95 or 99 only if using bootstrap critical values)
#'
#' @param usebootcrits Whether to use bootstrap critical values for the ellipses:
#' \describe{
#' \item{TRUE}{use bootstrap critical values}
#' \item{FALSE}{use \eqn{\chi^2} critical values}
#' \item{NULL}{inherit this choice from the chosen object of class \code{\linkS4class{cabootcrsresults}} }
#' }
#' Note: only 90\%, 95\% and 99\% bootstrap critical values are available
#'
#' @param plottype This is only relevant for simple CA, in MCA the choice is automatic.
#' \describe{
#' \item{"biplot"}{One plot with confidence regions for rows in principal coordinates
#' while columns are shown as directions in standard coordinates, another
#' plot with confidence regions for columns in principal coordinates while rows
#' are shown as directions in standard coordinates}
#' \item{"french"}{Two plots each with both rows and columns in principal coordinates,
#' one plot shows confidence regions for the rows and the other shows
#' confidence regions for the columns}
#' }
#' @param showrowcrs Whether to plot confidence ellipses for row points:
#' \describe{
#' \item{TRUE}{plot all confidence ellipses for row points as usual}
#' \item{A row number or vector of row numbers}{plot confidence ellipses only for these row points,
#' which can be specified in any standard R format such as 7 or c(1,4,9) or 5:11}
#' \item{FALSE}{suppress plotting of all confidence ellipses for row points}
#' }
#' Note: when analysing a Burt matrix the columns points are plotted
#'
#' @param showcolumncrs Whether to plot confidence ellipses for column points:
#' \describe{
#' \item{TRUE}{plot all confidence ellipses for column points as usual}
#' \item{A column number or vector of column numbers}{plot confidence ellipses only for these column points,
#' which can be specified in any standard R format such as 7 or c(1,4,9) or 5:11}
#' \item{FALSE}{suppress plotting of all confidence ellipses for column points}
#' }
#' Note: when analysing a Burt or indicator matrix the column points are plotted, the columns
#' being all of the variable categories, ordered by variable number,
#' e.g. for p=3 variables each with 5 categories then columns 6:10 are variable 2.
#'
#' Note: in MCA this is overridden by mcaoneploteach=TRUE, in which case there is one plot
#' per variable, automatically giving ellipses for each of its categories.
#'
#' @param likertarrows If TRUE then, for MCA on likert-type ordered categorical data,
#' draw arrows connecting the category points for each variable,
#' with the arrows drawn from a category point to the next higher category point.\cr
#' Note: will only be drawn from category points with ellipses shown.
#'
#' @param firstaxis Number of the first (i.e. highest inertia) axis to be plotted
#'
#' @param lastaxis Number of the last (i.e. lowest inertia) axis to be plotted,
#' which must be <= axisvariances value for x.
#'
#' @param plotallpairs Whether to plot all pairs of axes against each other:
#' \describe{
#' \item{"onlythese"}{plot firstaxis v lastaxis only}
#' \item{"successive"}{plot all successive pairs of axes between firstaxis and lastaxis}
#' \item{"all"}{plot all possible pairs of axes between firstaxis and lastaxis}
#' }
#'
#' @param picsize A 2-vector/list or 4-vector/list specifying the plot size:
#' \describe{
#' \item{2-vector}{minimum and maximum of both x and y axes on each plot}
#' \item{4-vector}{min and max of x axis followed by min and max of y axis, where the difference
#' between max and min must be the same in both cases}
#' }
#' All plots have an aspect ratio of 1.
#'
#' The same scales are used for all plots, so in the biplot case it
#' might occasionally be preferred to run plotca twice with different
#' picsize values, one being better for rows in principal coordinates
#' and the other better for columns in principal coordinates.
#'
#' If picsize is used to focus in on a particular area
#' of the plot then biplot labels might not appear properly.
#'
#' If using Rstudio then it may override this somewhat, especially if you resize the plot window
#' after plotting.
#'
#' @param mcaoneploteach For MCA only, a flag saying whether to produce one plot for each variable,
#' where confidence ellipses are shown for that variable but not others:
#' \describe{
#' \item{TRUE}{p plots are produced, each showing confidence regions for all of the category points
#' for just one variable, overriding showcolumncrs}
#' \item{FALSE}{only one plot is produced, with confidence regions shown for each category
#' point of each variable, although this can be controlled by showcolumncrs}
#' }
#' Note that mcaoneploteach=FALSE can be used to produce just one plot with ellipses for just one variable,
#' with showcolumncrs used to specify the column numbers of the required variable categories
#' (e.g. 1:5 for the first variable if it has 5 categories)
#'
#' @param mcashowindividuals For MCA on an indicator matrix only, a flag saying whether to
#' plot the individuals on the plot(s):
#' \describe{
#' \item{TRUE}{plot the individuals, which could be very "busy"}
#' \item{FALSE}{don't plot the individuals}
#' }
#'
#' @param mcavariablecolours
#' \describe{
#' \item{TRUE}{In MCA each variable has its own colour, and all category
#' points and ellipses for that variable have the same colour}
#' \item{FALSE}{Colours chosen in the default way}
#' }
#' If TRUE then the only colour options for plotsymbolscolours are "alldifferent" (default), "differentreds" or "differentblues" as above. \cr
#' Hence if TRUE then colours vary from variable 1 to variable p with each variable having all its categories
#' plotted with the same colour, while if FALSE then colours vary from variable 1 category 1 to variable p category J_p.
#'
#' @param mcacategorycolours
#' \describe{
#' \item{TRUE}{In MCA each category number has its own colour,
#' and all points and ellipses for that category number have the same colour,
#' for all variables (intended for Likert type data so that all category 1 points are
#' the same colour etc)}
#' \item{FALSE}{Colours chosen in the default way}
#' }
#' If TRUE then the only colour options for plotsymbolscolours are "alldifferent" (default), "differentreds" or "differentblues" as above. \cr
#' Hence if TRUE then colours vary from category 1 to category J_k with category i being the same colour for each variable,
#' while if FALSE then colours vary from variable 1 category 1 to variable p category J_p.
#'
#' @param groupings The name of a file (in " ") or data frame containing group structure of row and column points:
#'
#' the n rows are divided into m groups and the p columns divided into k groups
#'
#' the file or data frame is n+p by 2, where:
#'
#' first column is 1..n 1..p (to make the file easier to read) \cr
#' second column contains the number of the group-of-rows (1..m)
#' or group-of-columns (1..k) that the row or column belongs to.
#'
#' Hence the file or data frame is:
#'
#' 1 <the number of the group-of-rows to which row 1 belongs> \cr
#' \dots \cr
#' n <the number of the group-of-rows to which row n belongs> \cr
#' 1 <the number of the group-of-columns to which column 1 belongs> \cr
#' \dots \cr
#' p <the number of the group-of-columns to which column p belongs>
#'
#' @param grouplabels The name of the file (in " ") or data frame containing the colours and labels to be used,
#' in association with the groupings option above, in a m+k by 5 array:
#'
#'  1 <legend> <plot symbol> <plot colour> <draw ellipse?> \cr
#' \dots \cr
#' m <legend> <plot symbol> <plot colour> <draw ellipse?> \cr
#' 1 <legend> <plot symbol> <plot colour> <draw ellipse?> \cr
#' \dots \cr
#' k <legend> <plot symbol> <plot colour> <draw ellipse?>
#'
#' The first column contains the number of the group-of-rows or group-of-columns, the others are:
#' \describe{
#' \item{legend (in " " if in data frame but not in file)}{for this group of rows/columns, to be shown on plot}
#' \item{symbol}{for all rows/columns in this group, either an R number code for a symbol
#' or a character (the latter in " " if in a data frame but not if in a file)\cr
#' Note: both rows and columns need to be either all numbers or all characters, or the legend will
#' not come out right}
#' \item{colour (in " ")}{for symbols and ellipses for this group}
#' \item{draw?}{T or F to draw or suppress ellipses and label points for this group
#' (both in " " in data frame but not in file)}
#' }
#'
#' See Details section and examples below to make more sense of this.
#'
#' This can also be used for multiple CA, remembering that only column points are shown (usually)
#' and that columns are ordered by variable and then by category, so that three variables each with
#' 5 categories will be columns 1:5, 6:10 and 11:15 respectively.
#' It should not be used when mcaoneploteach=TRUE, however, as that already takes care of this sort of grouping.
#'
#' These options are particularly intended for large data sets,
#' to allow attention to be drawn to some points above others,
#' to emphasize any group structure within the data, or to show only the
#' most important ellipses in order to make the picture less cluttered.
#'
#' @param eps Any value less than this is treated as zero in some calculations and comparisons
#'
#' @return One or more plots are produced but no output object is created
#'
#' @examples
#' # the main function also calls plotca with the default options
#'
#' bd <- cabootcrs(DreamData,showresults=FALSE)
#' plotca(bd)
#'
#' \dontrun{
#'
#' # Plot options for SCA:
#'
#' # Note that Rstudio changes plots depending on the size of your plot window,
#' # so the picsize parameter (used for xlim, ylim in the plot command) is partially
#' # overridden, so warnings that a point is outside the plot limits may not be correct
#'
#' # Plot with specified size to fit the whole of the arrows in without cropping
#'
#' plotca(bd, picsize=c(-2.5,2.5))
#'
#' # or smaller, note the warning
#'
#' plotca(bd, picsize=c(-0.5,0.5))
#'
#' # All points in colour
#'
#' plotca(bd,othersmonochrome=NULL)
#'
#' # 90% regions with different colour schemes
#'
#' plotca(bd, plotsymbolscolours=c(3,"differentreds","*","blue"), crpercent=90)
#' plotca(bd, plotsymbolscolours=c(3,"differentreds"), crpercent=90)
#' plotca(bd, plotsymbolscolours="differentreds", crpercent=90)
#' plotca(bd, plotsymbolscolours=colours()[641], crpercent=90)
#'
#' # suppress labels for column points, to de-clutter row points picture,
#' # this is mostly useful for larger data sets than this one
#'
#' plotca(bd, showcolumnlabels=FALSE, datasetname="Dream data")
#'
#' # only show ellipses for rows 1, 1-2 and 1-3 respectively
#'
#' plotca(bd, showrowcrs=1)
#' plotca(bd, showrowcrs=c(1,2))
#' plotca(bd, showrowcrs=1:3)
#'
#' # plot axes 1 v 2, 1 v 3 and 2 v 3
#'
#' plotca(bd, firstaxis=1, lastaxis=3, plotallpairs="all")
#'
#' # If the cell values were all 10 times larger
#'
#' bdx10 <- cabootcrs(10*DreamData)
#' plotca(bdx10,plottype = "french",picsize=c(-0.4,0.4))
#'
#' # Various plots for a larger data set, note that the default colour scheme picks out
#' # males, females and ages because of the ordering of the rows
#'
#' bs <- cabootcrs(SuicideData)
#' plotca(bs, picsize=c(-0.7,0.8))
#' plotca(bs, plottype="french", picsize=c(-0.7,0.8))
#' plotca(bs, plottype="french", picsize=c(-0.7,0.8),
#'        plotsymbolscolours=c(".","differentreds","+","black"))
#'
#' # Note that the ellipses follow the horseshoe
#'
#' bas <- cabootcrs(AsbestosData)
#'
#' # more complicated plotting, define group structure in data frames
#'
#' groupingsframe <- cbind( c(1:5,1:4), c(1,1,2,2,3,1,1,2,2) )
#' grouplabframe <- cbind( c(1,2,3,1,2), c("AB","CD","E","ab","cd"), c(19,20,21,"+","*"),
#'                         c("green","blue","yellow","red","orange"), "T" )
#' plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe)
#'
#'
#' plotca(bd, groupings=groupingsframe, grouplabels=grouplabframe, plottype="french")
#'
#' # This can also be used for custom colour schemes other than "differentreds" etc as
#' # defined in the plotsymbolscolours option, though note that R colours are not ordered in the way
#' # you might expect, so the colour scheme below is purely illustrative and not very sensible
#'
#' customframe <- cbind( c(1:5,1:4), c(1:5,1:4) )
#' customlabframe <- cbind( c(1:5,1:4), rep("",9), c(rep(18,5),rep(19,4)),
#'                          colours()[c(seq(10,130,30),seq(440,590,50))], "T" )
#'
#' plotca(bd, groupings=customframe, grouplabels=customlabframe)
#'
#'
#' # Plot options for MCA:
#'
#' # Use one of the below, labelling row A as R:A or just A (etc) as preferred
#'
#' bd3 <- cabootcrs(DreamData223by3, catype="mca")
#' bd3 <- cabootcrs(DreamData223by3, catype="mca", varandcat=FALSE)
#'
#' # one plot showing CRs for all variable categories (busy)
#'
#' plotca(bd3,mcaoneploteach=FALSE,picsize=c(-0.35,0.35))
#'
#' # each variable has its own colour
#'
#' plotca(bd3,mcavariablecolours=TRUE,picsize=c(-0.35,0.35))
#'
#' # each category number has its own colour
#'
#' plotca(bd3,mcacategorycolours=TRUE,picsize=c(-0.35,0.35))
#'
#' # draw arrows between successive ordered categories
#'
#' plotca(bd3,likertarrows=TRUE,picsize=c(-0.35,0.35))
#'
#' # secondary points black rather than grey
#'
#' plotca(bd3,othersmonochrome="black",picsize=c(-0.35,0.35))
#'
#' # 99% CRs
#'
#' plotca(bd3,crpercent=99)
#'
#' # Plot together CRs for the first category of each variable
#'
#' plotca(bd3,showcolumncrs=c(1,6,10),mcaoneploteach=FALSE,picsize=c(-0.35,0.35))
#'
#' # Plot together CRs for the second category of each variable
#'
#' plotca(bd3,showcolumncrs=c(2,7,11),mcaoneploteach=FALSE,picsize=c(-0.35,0.35))
#'
#' # One plot with CRs only for variable 3
#'
#' plotca(bd3,showcolumncrs=10:12,mcaoneploteach=FALSE,picsize=c(-0.35,0.35))
#'
#' # Three plots, various colour schemes
#'
#' plotca(bd3,othersmonochrome="black",picsize=c(-0.35,0.35))
#' plotca(bd3,othersmonochrome="black",picsize=c(-0.35,0.35),mcacategorycolours=TRUE)
#' plotca(bd3,picsize=c(-0.35,0.35),mcavariablecolours=TRUE,likertarrows=TRUE)
#'
#' # All on one plot, various colour schemes
#'
#' plotca(bd3,mcaoneploteach=FALSE,showcolumncrs=1:5,othersmonochrome="black",picsize=c(-0.35,0.35))
#' plotca(bd3,mcaoneploteach=FALSE,showcolumncrs=1:5,likertarrows=TRUE,picsize=c(-0.35,0.35))
#' plotca(bd3,mcaoneploteach=FALSE,likertarrows=TRUE,mcacategorycolours=TRUE,picsize=c(-0.35,0.35))
#' plotca(bd3,mcaoneploteach=FALSE,likertarrows=TRUE,mcavariablecolours=TRUE,picsize=c(-0.35,0.35))
#'
#' # Plots with more complicated colour and grouping structure, as above but now in MCA case.
#' # Note the need to duplicate both data frames as groupings must be specified for both rows
#' # and columns, though only columns are used.
#' # Note also that symbol types need to be defined either all as numbers or all as symbols
#'
#' groupingsframe <- cbind(1:12,c(1,1,2,2,3,4,4,5,5,6,7,7))
#' groupingsframe <- rbind(groupingsframe,groupingsframe)
#' grouplabframe <- cbind( 1:7, c("AB","CD","E","ab","cd","v1","v23"), 19:25,
#'                         c("cyan","deepskyblue","blue","red","tomato","chartreuse","yellowgreen"),
#'                         "T" )
#' grouplabframe <- rbind(grouplabframe,grouplabframe)
#' plotca(bd3, groupings=groupingsframe, grouplabels=grouplabframe,
#'        mcaoneploteach=FALSE, picsize=c(-0.35,0.35))
#'
#'
#' # Comparing plots to those from ca()
#'
#' bd <- cabootcrs(DreamData, showresults=FALSE)
#'
#' # both plots almost the same as the plot from the ca() package
#'
#' plotca(bd, plottype="french", showrowcrs=FALSE, showcolumncrs=FALSE, othersmonochrome=NULL,
#'        plotsymbolscolours=c(19,"blue",17,"red"), picsize=c(-0.4,0.4) )
#'
#' # plot almost the same as the ca() plot, but with ellipses
#'
#' plotca(bd, plottype="french", othersmonochrome=NULL, plotsymbolscolours=c(19,"blue",17,"red"),
#'        picsize=c(-0.4,0.4))
#'
#' # Adding confidence ellipses for row points to plots from ca() using ellipse().
#' # Note: reflectaxes() is needed if cabootcrs() and ca() axes are reflected wrt each other
#'
#' library(ca)
#' library(ellipse)
#' cad <- ca(DreamData)
#' plot(cad)
#' for (i in 1:dim(DreamData)[1]) {
#'   lines( ellipse(x=covmat(bd,i,"row",1,2,FALSE), centre=bd@Rowprinccoord[i,cbind(1,2)],
#'                  npoints=1000),
#'          cex=1, pch=".", col="blue")
#' }
#'
#' }
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}},
#' \code{\link{printca}}, \code{\link{summaryca}}, \code{\linkS4class{cabootcrsresults}}
#'
#' @export
plotca <- function(x, datasetname="", showrowlabels=TRUE, showcolumnlabels=TRUE,
                   plotsymbolscolours=c(19,"alldifferent",18,"alldifferent"),
                   othersmonochrome="grey", crpercent=95, usebootcrits=NULL,
                   plottype="biplot", showrowcrs=TRUE, showcolumncrs=TRUE, likertarrows=FALSE,
                   firstaxis=1, lastaxis=2, plotallpairs="successive", picsize=NULL,
                   mcaoneploteach=TRUE, mcashowindividuals=FALSE, mcavariablecolours=FALSE, mcacategorycolours=FALSE,
                   groupings=NULL, grouplabels=NULL, eps=1e-15 ) {

  #setGeneric("plot", function(x,...) standardGeneric("plot") )
  #setMethod("plot", signature(x="cabootcrsresults"),
  # function(x, datasetname="", showrowlabels=TRUE, showcolumnlabels=TRUE,
  #   groupings=NULL, grouplabels=NULL, plotsymbolscolours=c(19,"alldifferent",18,"alldifferent"),
  #   othersmonochrome="black", crpercent=95, plottype="biplot", showrowcrs=TRUE, showcolumncrs=TRUE,
  #   firstaxis=1, lastaxis=2, plotallpairs=FALSE, picsize=c(-1,1)) {


  ## internal function to plot a  single picture

  plotonepic <- function(a1,a2,plottype,things,nthings,nvars,Thingcoord,Varcoord,SBvar,SBcov,twoS,bootcritThing,
                         inertiapc,resampledistn,multinomialtype,
                         thinggroup,thinggrlab,vargroup,vargrlab,thinglabels,varlabels,
                         showcrs,drawlinks,picsizex,picsizey,plotboth=TRUE,eps=1e-15) {

    critchisq2 <- qchisq(0.01*crpercent,2)
    critchisq1 <- qchisq(0.01*crpercent,1)
    if ( !(is.null(bootcritThing)) ) {
      if (crpercent==90) {
        cvn <- 1
      } else {
        if (crpercent==99) {
          cvn <- 3
        } else {
          cvn <- 2
          if ( !(crpercent==95) ) { cat(paste("\n WARNING only 90%, 95%, 99% supported, using 95% instead \n\n")) }
        } } }
    theta <- seq(0,2*pi,0.001)
    ellipsecoords <- rbind(sin(theta),cos(theta))

    # horrible bodge to convert thinggrlab[[3]] to lists, mixing numbers and chars, also for vargrlab[[3]] in french plot
    thinggrlab3 <- as.list(thinggrlab[[3]])
    thinggrlab3int <- !is.na(as.integer(thinggrlab3))
    for (i in  1:max(thinggroup[,2])) { if (thinggrlab3int[[i]]) { thinggrlab3[[i]] <- as.integer(thinggrlab3[[i]]) } }
    if (plottype=="french") {
      vargrlab3 <- as.list(vargrlab[[3]])
      vargrlab3int <- !is.na(as.integer(vargrlab3))
      for (i in  1:max(vargroup[,2])) { if (vargrlab3int[[i]]) { vargrlab3[[i]] <- as.integer(vargrlab3[[i]]) } }
    }

    #dev.new()
    plot(Thingcoord[1,a1], Thingcoord[1,a2], xlim=picsizex, ylim=picsizey,
         xlab=paste("Axis ", a1, "    ", inertiapc[a1], "%", sep=""),
         ylab=paste("Axis ", a2, "    ", inertiapc[a2], "%", sep=""),
         asp=1, pch=thinggrlab3[[thinggroup[1,2]]], col=thinggrlab[[4]][thinggroup[1,2]] )
    for (i in 2:nthings) { points(Thingcoord[i,a1], Thingcoord[i,a2],
                                  asp=1, pch=thinggrlab3[[thinggroup[i,2]]], col=thinggrlab[[4]][thinggroup[i,2]] ) }

    abline(h=0,v=0)

    if (!all(thinggrlab[[2]]=="")) {
      labnum <- as.integer(thinggrlab3)
      labchar <- as.character(thinggrlab3)
      #legend("topleft",thinggrlab[[2]],pch=labnum,col=thinggrlab[[4]],text.col=thinggrlab[[4]])
      #for (i in 1:max(thinggroup[,2])) { if (is.na(labnum[[i]])) { labchar[[i]]<-thinggrlab3[[i]] } else { labchar[[i]]<-NA } }
      #legend("topleft",thinggrlab[[2]],pch=labchar,col=thinggrlab[[4]],text.col=thinggrlab[[4]])
      if (any(is.na(labnum))) {
        legend("topleft",thinggrlab[[2]],pch=labchar,col=thinggrlab[[4]],text.col=thinggrlab[[4]])
      } else {
        legend("topleft",thinggrlab[[2]],pch=labnum,col=thinggrlab[[4]],text.col=thinggrlab[[4]])
      }
    }

    if (plottype=="biplot") {
      if ((x@nboots>0)&(any(showcrs==TRUE))) {
        title(paste(crpercent, "% Confidence regions for biplot of", things, "\n \n", datasetname ))
        title(paste("\n", resampledistn, "resampling,",
                    switch(multinomialtype, whole="", rowsfixed="row sums fixed,", columnsfixed="column sums fixed,"),
                    x@nboots, "resamples \n"), font.main=1 )
      } else {
        title(paste("Biplot of", things, "\n", datasetname ))
      }
      if (plotboth) {
        for (i in 1:nvars) { lines(c(0,Varcoord[i,a1]), c(0,Varcoord[i,a2]), col=vargrlab[[4]][vargroup[[2]][i]]) }
        grat <- cbind(Varcoord[,a1]/picsizex[1],Varcoord[,a1]/picsizex[2],Varcoord[,a2]/picsizey[1],Varcoord[,a2]/picsizey[2],0.95)/0.95
        cl <- 1.05/apply(grat,1,max)
        text(cl*Varcoord[ ,a1], cl*Varcoord[ ,a2], labels=varlabels, col=vargrlab[[4]][vargroup[[2]]], pos=4, cex=0.75 )
      }
    } else { # french
      if ((x@nboots>0)&(any(showcrs==TRUE))) {
        title(paste(crpercent, "% Confidence regions for", things, "\n \n", datasetname ) )
        title(paste("\n", resampledistn, "resampling,",
                    switch(multinomialtype, whole="", rowsfixed="row sums fixed,", columnsfixed="column sums fixed,"),
                    x@nboots, "resamples \n"), font.main=1 )
      } else {
        title(paste("Correspondence plot \n", datasetname ))
      }
      if (plotboth) {
        for (i in 1:nvars) { points(Varcoord[i,a1], Varcoord[i,a2], asp=1,
                                    pch=vargrlab3[[vargroup[i,2]]], col=vargrlab[[4]][vargroup[i,2]] ) }
        text(Varcoord[ ,a1], Varcoord[ ,a2], labels=varlabels, col=vargrlab[[4]][vargroup[[2]]], pos=4, cex=0.75 )
      }
      if ( (!all(vargrlab[[2]]=="")) & (x@catype=="sca") ) {
        labnum <- as.integer(vargrlab3)
        labchar <- as.character(vargrlab3)
        #legend("topright",vargrlab[[2]],pch=labnum,col=vargrlab[[4]],text.col=vargrlab[[4]])
        #for (i in 1:max(vargroup[,2])) { if (is.na(labnum[[i]])) { labchar[[i]]<-vargrlab3[[i]] } else { labchar[[i]]<-NA } }
        #legend("topright",vargrlab[[2]],pch=labchar,col=vargrlab[[4]],text.col=vargrlab[[4]])
        if (any(is.na(labnum))) {
          legend("topright",vargrlab[[2]],pch=labchar,col=vargrlab[[4]],text.col=vargrlab[[4]])
        } else {
          legend("topright",vargrlab[[2]],pch=labnum,col=vargrlab[[4]],text.col=vargrlab[[4]])
        }
      }
    }

    for (i in 1:nthings) {
      if (thinggrlab[[5]][thinggroup[i,2]]) {
        text(Thingcoord[i,a1], Thingcoord[i,a2], labels=thinglabels[i], pos=4, cex=0.75, col=thinggrlab[[4]][thinggroup[i,2]] )
        if (showcrs[i]) {
          xbar <- Thingcoord[i,cbind(a1,a2)]
          V <- matrix(c(SBvar[i,a1],SBcov[i,min(a1,a2),max(a1,a2)],SBcov[i,min(a1,a2),max(a1,a2)],SBvar[i,a2]),2,2)
          E <- eigen(V, symmetric=TRUE)
          usec2 <- (1-twoS[i]) * (E$values[1]>eps)
          if ( !(is.null(bootcritThing)) & !(is.null(SBvar)) ) {
            critval <- bootcritThing[i,a1,a2,cvn]
          } else {
            critval <- critchisq2 * usec2 + critchisq1 * (1-usec2)
          }
          coords <- E$vectors %*% (critval*diag(E$values))^(1/2) %*% ellipsecoords
          lines(xbar[1]+coords[1, ], xbar[2]+coords[2, ], pch=".", col=thinggrlab[[4]][thinggroup[i,2]] )
          # equivalent if using the ellipse package and chi-squared with 2 df:
          # lines(ellipse(x=V, centre=xbar, npoints=1000), cex=1, pch=".", col=thinggrlab[[4]][thinggroup[i,2]] )
        }
        if (drawlinks[i]) {
          arrows(Thingcoord[i,a1], Thingcoord[i,a2], Thingcoord[(i+1),a1], Thingcoord[(i+1),a2], col=thinggrlab[[4]][thinggroup[i,2]])
        }
      }
    }

    if (any(Thingcoord[,a1]<picsizex[1])) {
      cat(paste("Warning: point may be outside plot limits, lowest x-value is ", min(Thingcoord[,a1]), "\n")) }
    if (any(Thingcoord[,a1]>picsizex[2])) {
      cat(paste("Warning: point may be outside plot limits, largest x-value is ", max(Thingcoord[,a1]), "\n")) }
    if (any(Thingcoord[,a2]<picsizey[1])) {
      cat(paste("Warning: point may be outside plot limits, lowest y-value is ", min(Thingcoord[,a2]), "\n")) }
    if (any(Thingcoord[,a2]>picsizey[2])) {
      cat(paste("Warning: point may be outside plot limits, largest y-value is ", max(Thingcoord[,a2]), "\n")) }

  } # plot one pic

  ## main body of plotca

  if (is.null(usebootcrits)) usebootcrits <- x@usebootcrits

  if (!is.null(plotsymbolscolours)) {
    if(length(plotsymbolscolours)==1) { # switch doesn't allow otherwise for numeric input
      if (!any(plotsymbolscolours==c(colours(),"alldifferent","differentblues","differentreds")))
        stop(paste("colour must be alldifferent, differentblues, differentreds or R colour (type colours() for full list) \n\n"))
    } else {
      if(length(plotsymbolscolours)==2) {
        if (!any(plotsymbolscolours[2]==c(colours(),"alldifferent","differentblues","differentreds")))
          stop(paste("colour must be alldifferent, differentblues, differentreds or R colour (type colours() for full list) \n\n"))
      } else {
        if(length(plotsymbolscolours)==4) {
          if ( (!any(plotsymbolscolours[2]==c(colours(),"alldifferent","differentblues","differentreds"))) |
               (!any(plotsymbolscolours[4]==c(colours(),"alldifferent","differentblues","differentreds"))) )
            stop(paste("colour must be alldifferent, differentblues, differentreds or R colour (type colours() for full list) \n\n"))
        } else { # not length 1,2 or 4
          stop(paste("plotsymbolscolours must contain row symbol and colour, column symbol and colour\n\n OR column symbol and colour\n\n OR column colour\n\n"))
        }
      }
    }
  }

  if ( ((mcavariablecolours==TRUE)|(mcacategorycolours==TRUE)) & (!any(plotsymbolscolours[length(plotsymbolscolours)]==c("alldifferent","differentblues","differentreds"))) )
    stop(paste("if using mcavariablecolours or mcacategorycolours then colours must be alldifferent, differentblues or differentreds\n\n"))
  if ((crpercent<=0)|(crpercent>=100)) stop(paste("coverage percentage must be between 0 and 100 exclusive\n\n"))
  if (!any(plottype==c("biplot","french"))) stop(paste("plotting must be biplot or french style\n\n"))
  if (!any( is(showrowcrs,"integer"), is(showrowcrs,"numeric"), is(showrowcrs,"logical") ))
    stop(paste("showrowcrs must be logical or a vector of row numbers\n\n"))
  if (!any( is(showcolumncrs,"integer"), is(showcolumncrs,"numeric"), is(showcolumncrs,"logical") ))
    stop(paste("showcolumncrs must be logical or a vector of row numbers\n\n"))
  if (!any(plotallpairs==c("all","onlythese","successive"))) stop(paste("plotallpairs must be all, onlythese or successive\n\n"))
  if (lastaxis>x@br@r) {
    cat(paste("\n WARNING there is(are) only", x@br@r, "meaningful axis(axes) to plot\n\n"))
    lastaxis <- max(2,x@br@r)
  }
  if ((firstaxis<1)|(firstaxis>x@axisvariances-1)) stop(paste("incorrect first axis =", firstaxis, "\n\n"))
  if (firstaxis>=lastaxis) stop(paste("last axis must be greater than first axis\n\n"))
  if (lastaxis>x@axisvariances) cat(paste("\n WARNING there are only variances for", x@axisvariances, "axes\n\n"))

  if (is.null(picsize)) {
    if (plottype=="biplot") { # sca biplot
      picsize <- round(150*c(range(rbind(x@Rowprinccoord[,c(firstaxis:lastaxis)],x@Colprinccoord[,c(firstaxis:lastaxis)]))))/100
    } else {
      if ((x@catype=="mca")&(any(x@mcatype==c("indicator","doubled")))&(mcashowindividuals==FALSE)) { # mca indicator-type but not plotting rows
        picsize <- round(120*c(range(x@Colprinccoord[,c(firstaxis:lastaxis)])))/100
      } else { # usual mca or sca french
        picsize <- round(120*c(range(rbind(x@Rowprinccoord[,c(firstaxis:lastaxis)],x@Colprinccoord[,c(firstaxis:lastaxis)]))))/100
      }
    }
  }
  if (!any(dim(array(picsize))==c(2,4))) stop(paste("picsize bounds are  lower,upper OR x lower,x upper,y lower,y upper \n\n"))
  if (picsize[1]>=picsize[2]) {
    picsize[1] <- -0.1
    picsize[2] <- 0.1
  }
  if (dim(array(picsize))==4) {
    if (picsize[3]>=picsize[4]) {
      picsize[3] <- -0.1
      picsize[4] <- 0.1
    }
    if (abs((picsize[4]-picsize[3])-(picsize[2]-picsize[1]))>eps) stop(paste("x and y axes must be same length\n\n"))
  }

  options(warn=-1)
  picsizey <- picsizex <- picsize[1:2]
  if (dim(array(picsize))==4) picsizey <- picsize[3:4]

  tworowS <- rowSums(x@DataMatrix>0)==2
  twocolS <- colSums(x@DataMatrix>0)==2

  # Groupings file or data frame contains
  #  row no.  row group
  #  col no.  col group
  # Group label file or data frame contains
  #  row group number   group name   symbol   colour   plot ellipse?
  #  col group number   group name   symbol   colour   plot ellipse?
  # NOTE: in the data frame for MCA the last field is used for whether to label the point, while ellipse plotting is a separate variable

  # if mcavariablecolours=TRUE mcacategorycolours=TRUE then allocate colours below by var or cat, not by all cats, need p and Jk

  if (is.null(groupings)) { # if no groupings of points supplied
    if (length(plotsymbolscolours)==2) { # assume only columns specified
      plotsymbolscolours <- c(19,"alldifferent",plotsymbolscolours)
    } else {
      if (length(plotsymbolscolours)==1) { # assume only column colours specified
        plotsymbolscolours <- c(19,"alldifferent",18,plotsymbolscolours)
      }
    }
    if (any(plotsymbolscolours[2]==c("alldifferent","differentreds","differentblues"))) { # row points different colours
      rowgroup <- as.data.frame(cbind(1:x@rows,1:x@rows))
      hv <- switch(plotsymbolscolours[2], "alldifferent"=c(0,0.85), "differentreds"=c(0,0.45), "differentblues"=c(0.5,0.85))
      rowgrlab <- as.data.frame(cbind(1:x@rows,"",plotsymbolscolours[1],rainbow(n=x@rows,start=hv[1],end=hv[2]),"T"),stringsAsFactors=FALSE)
    } else { # all same colour
      rowgroup <- as.data.frame(cbind(1:x@rows,rep(1,x@rows)))
      rowgrlab <- as.data.frame(cbind(1,"",plotsymbolscolours[1],plotsymbolscolours[2],"T"),stringsAsFactors=FALSE)
    }
    class(rowgrlab[,1]) <- "integer"
    class(rowgrlab[,5]) <- "logical"
    plotdiff <- any(plotsymbolscolours[4]==c("alldifferent","differentreds","differentblues"))
    if (plotdiff) { # column points different colours
      hv <- switch(plotsymbolscolours[4], "alldifferent"=c(0,0.85), "differentreds"=c(0,0.45), "differentblues"=c(0.5,0.85))
    }
    if ( (x@catype=="mca") & ((mcavariablecolours==TRUE)|(mcacategorycolours==TRUE)) ) {
      if (mcavariablecolours==TRUE) { # each variable has its own colour
        if (x@mcatype=="doubled") { ntimes <- rep(2,x@p) } else { ntimes <- x@Jk }
        colgroup <- as.data.frame(cbind(1:x@columns,rep(1:x@p,times=ntimes)))
        colgrlab <- as.data.frame(cbind(1:x@p,"",plotsymbolscolours[3],rainbow(n=x@p,start=hv[1],end=hv[2]),"T"),stringsAsFactors=FALSE)
      } else { # each category value has its own colour
        if (x@mcatype=="doubled") {
          Jmax <- 2
          cg <- rep(1:2,times=x@p)
        } else {
          Jmax <- max(x@Jk)
          cg <- vector(mode="numeric",length=x@columns)
          cg[1:x@Jk[1]] <- 1:x@Jk[1]
          for (k in 2:x@p) {
            cg[(sum(x@Jk[1:(k-1)])+1):sum(x@Jk[1:k])] <- 1:x@Jk[k]
          }
        }
        colgroup <- as.data.frame(cbind(1:x@columns,cg))
        colgrlab <- as.data.frame(cbind(1:Jmax,"",plotsymbolscolours[3],rainbow(n=Jmax,start=hv[1],end=hv[2]),"T"),stringsAsFactors=FALSE)
      }
    } else { # same as rows
      if (plotdiff==TRUE) { # column points different colours
        colgroup <- as.data.frame(cbind(1:x@columns,1:x@columns))
        colgrlab <- as.data.frame(cbind(1:x@columns,"",plotsymbolscolours[3],rainbow(n=x@columns,start=hv[1],end=hv[2]),"T"),stringsAsFactors=FALSE)
      } else { # all same colour
        colgroup <- as.data.frame(cbind(1:x@columns,rep(1,x@columns)))
        colgrlab <- as.data.frame(cbind(1,"",plotsymbolscolours[3],plotsymbolscolours[4],"T"),stringsAsFactors=FALSE)
      }
    }
    class(colgrlab[,1]) <- "integer"
    class(colgrlab[,5]) <- "logical"
  } else { # groupings specified
    if (is(groupings,"character")) { # groupings in file
      rcgroup <- read.table(file=groupings,colClasses=c("integer","integer"))
    } else { # groupings in data frame
      rcgroup <- as.data.frame(groupings)
    }
    rowgroup <- rcgroup[1:x@rows,]
    colgroup <- rcgroup[(x@rows+1):(x@rows+x@columns),]
    nrowgroups <- max(rowgroup[,2])
    ncolgroups <- max(colgroup[,2])
    if (is(grouplabels,"character")) { # group labels in file
      rcgrlab <- read.table(file=grouplabels,
                            colClasses=c("integer","character","character","character","logical"))
    } else { # group labels in data frame
      rcgrlab <- as.data.frame(grouplabels,stringsAsFactors=FALSE)
      class(rcgrlab[,1]) <- "integer"
      class(rcgrlab[,5]) <- "logical"
    }
    rowgrlab <- rcgrlab[1:nrowgroups,]
    colgrlab <- rcgrlab[(nrowgroups+1):(nrowgroups+ncolgroups),]
  }

  # quick option to plot only a few regions, overrides other options
  rowcrs <- logical(length=x@rows)
  columncrs <- logical(length=x@columns)
  if (any( is(showrowcrs,"integer"), is(showrowcrs,"numeric") )) { for (i in 1:length(showrowcrs)) { rowcrs[showrowcrs[i]]<-TRUE }
  } else { rowcrs <- rowcrs | showrowcrs }
  if (any( is(showcolumncrs,"integer"), is(showcolumncrs,"numeric") )) { for (i in 1:length(showcolumncrs)) { columncrs[showcolumncrs[i]]<-TRUE }
  } else { columncrs <- columncrs | showcolumncrs }

  # option for secondary set of points, with CRs not shown, to be monochrome
  vrowgrlab <- rowgrlab
  vcolgrlab <- colgrlab
  if (any(othersmonochrome==colours())) {
    vrowgrlab[[4]] <- othersmonochrome
    vcolgrlab[[4]] <- othersmonochrome
  }

  # Plot row and col pictures for pairs of axes

  if (showrowlabels==TRUE) { rowptlabels <- x@rowlabels } else { rowptlabels <- NULL }
  if (showcolumnlabels==TRUE) { colptlabels <- x@collabels } else { colptlabels <- NULL }

  for (a1 in firstaxis:(lastaxis-1)) {
    for (a2 in (a1+1):lastaxis) {
      if ( (plotallpairs=="all") | ((plotallpairs=="onlythese")&(a1==firstaxis)&(a2==lastaxis)) | ((plotallpairs=="successive")&(a2==a1+1)) ) {

        if ( (x@catype=="sca") ) {

          if (plottype=="biplot") {
            plotonepic(a1, a2, plottype, x@varnames[[1]], x@rows, x@columns, x@Rowprinccoord, x@Colstdcoord, x@RowVar, x@RowCov, tworowS,
                       if(usebootcrits) { x@bootcritR } else { NULL },
                       x@inertias[,2], x@resampledistn, x@multinomialtype,
                       rowgroup, rowgrlab, colgroup, vcolgrlab, rowptlabels, colptlabels,
                       (rowcrs & (a2<=x@axisvariances)), (rowcrs & FALSE), picsizex, picsizey, eps )
            plotonepic(a1, a2, plottype, x@varnames[[2]], x@columns, x@rows, x@Colprinccoord, x@Rowstdcoord, x@ColVar, x@ColCov, twocolS,
                       if(usebootcrits) { x@bootcritC } else { NULL },
                       x@inertias[,2], x@resampledistn, x@multinomialtype,
                       colgroup, colgrlab, rowgroup, vrowgrlab, colptlabels, rowptlabels,
                       (columncrs & (a2<=x@axisvariances)), (columncrs & FALSE), picsizex, picsizey, eps)
          } else { # french
            plotonepic(a1, a2, plottype, x@varnames[[1]], x@rows, x@columns, x@Rowprinccoord, x@Colprinccoord, x@RowVar, x@RowCov, tworowS,
                       if(usebootcrits) { x@bootcritR } else { NULL },
                       x@inertias[,2], x@resampledistn, x@multinomialtype,
                       rowgroup, rowgrlab, colgroup, vcolgrlab, rowptlabels, colptlabels,
                       (rowcrs & (a2<=x@axisvariances)), (rowcrs & FALSE), picsizex, picsizey, eps)
            plotonepic(a1, a2, plottype, x@varnames[[2]], x@columns, x@rows, x@Colprinccoord, x@Rowprinccoord, x@ColVar, x@ColCov, twocolS,
                       if(usebootcrits) { x@bootcritC } else { NULL },
                       x@inertias[,2], x@resampledistn, x@multinomialtype,
                       colgroup, colgrlab, rowgroup, vrowgrlab, colptlabels, rowptlabels,
                       (columncrs & (a2<=x@axisvariances)), (columncrs & FALSE), picsizex, picsizey, eps)
          } # biplot or french

        } else { # mca or omca

          if (x@mcatype=="indicator") { # print rows/individuals plot only if indicator (not doubled) matrix used, french style
            if (x@mcaindividualboot) { # likert, with individual CRs, always uses bootstrap critical values
              plotonepic(a1, a2, "french", "individuals", x@rows, x@columns, x@Rowprinccoord, x@Colprinccoord, x@RowVar, x@RowCov, tworowS, x@bootcritR,
                         x@inertias[,2], x@resampledistn, x@multinomialtype,
                         rowgroup, rowgrlab, colgroup, vcolgrlab, rowptlabels, colptlabels,
                         (rowcrs & (a2<=x@axisvariances)), (rowcrs & FALSE), picsizex, picsizey, eps)
            } else { # usual, no CRs
              plotonepic(a1, a2, "french", "individuals", x@rows, x@columns, x@Rowprinccoord, x@Colprinccoord, NULL, NULL, tworowS, NULL,
                         x@inertias[,2], x@resampledistn, x@multinomialtype,
                         rowgroup, rowgrlab, colgroup, vcolgrlab, rowptlabels, colptlabels, (rowcrs&FALSE), (rowcrs&FALSE), picsizex, picsizey, eps)
            }
          }

          # if mcaoneploteach then ignore any showcrs commands, just one plot per variable with crs
          # note that likert arrows can only be drawn on variables with CRs
          if ((mcaoneploteach==TRUE)&((x@mcatype=="Burt")|(length(x@Jk)>1))) { # one plot for each variable, with CRs just for that one
            if (any(x@mcatype==c("Burt","indicator"))) {
              catf <- c(1,cumsum(x@Jk)[1:(x@p-1)]+1)
              catl <- cumsum(x@Jk)
            } else {
              if (x@mcatype=="doubled") {
                catf <- seq(1,2*x@p,2)
                catl <- seq(2,2*x@p,2)
              }
            }
            for (i in 1:x@p) {
              thisvar <- columncrs & FALSE
              thisvar[catf[i]:catl[i]] <- (a2<=x@axisvariances)
              drawlinks <- thisvar & likertarrows
              drawlinks[catl[i]] <- FALSE

              onecolgroup <- colgroup
              onecolgrlab <- colgrlab
              if (any(othersmonochrome==colours())) { # horrible bodge to grey out other variables
                monogroup <- length(onecolgrlab[[1]])+1
                onecolgrlab[monogroup,1] <- monogroup
                onecolgrlab[monogroup,2] <- ""
                onecolgrlab[monogroup,3] <- plotsymbolscolours[3]
                onecolgrlab[monogroup,4] <- othersmonochrome
                onecolgrlab[monogroup,5] <- TRUE
                onecolgroup[,2] <- monogroup
                onecolgroup[(catf[i]:catl[i]),2] <- colgroup[(catf[i]:catl[i]),2]
              }
              plotonepic(a1, a2, "french", x@varnames[[i]], x@columns, x@rows, x@Colprinccoord, x@Rowprinccoord, x@ColVar, x@ColCov, twocolS,
                         if(usebootcrits) { x@bootcritC } else { NULL },
                         x@inertias[,2], x@resampledistn, x@multinomialtype,
                         onecolgroup, onecolgrlab, rowgroup, vrowgrlab, colptlabels, rowptlabels, thisvar, drawlinks, picsizex, picsizey,
                         plotboth=(mcashowindividuals&(x@mcatype=="indicator")), eps )
            }
          } else { # just one plot with all variables
            thesevars <- columncrs & (a2<=x@axisvariances)
            drawlinks <- thesevars & likertarrows
            if (x@mcatype=="doubled") {
              drawlinks[seq(2,2*x@p,2)] <- FALSE
            } else {
              drawlinks[cumsum(x@Jk)] <- FALSE
            }
            plotonepic(a1, a2, "french", "variables", x@columns, x@rows, x@Colprinccoord, x@Rowprinccoord, x@ColVar, x@ColCov, twocolS,
                       if(usebootcrits) { x@bootcritC } else { NULL },
                       x@inertias[,2], x@resampledistn, x@multinomialtype,
                       colgroup, colgrlab, rowgroup, vrowgrlab, colptlabels, rowptlabels,
                       thesevars, drawlinks, picsizex, picsizey,
                       plotboth=(mcashowindividuals&(x@mcatype=="indicator")), eps )
          }

        } # mca

      } } } # pairs of pictures

  options(warn=0)

}

