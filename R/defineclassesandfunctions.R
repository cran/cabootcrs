
###### CLASSES

#' A class containing the basic results from CA
#'
#' This is intended for internal use within \code{\link{cabootcrs}} and only contains the
#' data structures required for each bootstrap replicate
#'
#' @slot Rprofile Row profile matrix, class \code{"matrix"}
#'
#' @slot Cprofile Column profile matrix, class \code{"matrix"}
#'
#' @slot Rweights Matrix of weights for row points: square roots of inverse column sums, class \code{"matrix"}
#'
#' @slot Cweights Matrix of weights for column points: square roots of inverse row sums, class \code{"matrix"}
#'
#' @slot Raxes Matrix of axes for row points: right singular vectors of weighted, centred data matrix, class \code{"matrix"}
#'
#' @slot Caxes Matrix of axes for column points: left singular vectors of weighted, centred data matrix, class \code{"matrix"}
#'
#' @slot r Rank of weighted, centred data matrix, class \code{"numeric"}
#'
#' @slot realr In multiple CA, the number of singular values (Burt matrix) or squared singular values
#' (indicator matrix) exceeding \eqn{1/p} where \eqn{p} is the number of variables,
#' class \code{"numeric"}
#'
#' @slot mu Singular values of weighted, centred data matrix, class \code{"numeric"}
#'
#' @seealso \code{\linkS4class{cabootcrsresults}}
#'
#' @export
setClass("cabasicresults",
representation(
  Rprofile="matrix", Cprofile="matrix", Rweights="matrix", Cweights="matrix",
  Raxes="matrix", Caxes="matrix", r="numeric", realr="numeric", mu="numeric") )

# Full, with variances etc for CRs
#  covariances are arrays with dim1=row/col, dim2/3=matrix (only upper triangle is non-zero)
#  axisvariances is number of axes for which vars/covs were calculated

#' A class containing the results from CA with bootstrapping
#'
#' This contains all of the usual output from simple or multiple CA,
#' plus the results of the bootstrap analysis and the various settings used for this.
#'
#' The meanings and possible values for the settings are described in \code{\link{cabootcrs}}
#'
#' @slot br The basic results from CA, class \code{\linkS4class{cabasicresults}}
#'
#' @slot DataMatrix The sample data matrix, class \code{"matrix"}
#'
#' @slot rows Number of rows, class \code{"numeric"}
#'
#' @slot columns Number of columns, class \code{"numeric"}
#'
#' @slot rowlabels Row category labels, class \code{"character"}
#'
#' @slot collabels Column category labels, class \code{"character"}
#'
#' @slot varnames Names of the variables, class \code{"character"}
#'
#' @slot Rowprinccoord Principal coordinates for row points, class \code{"matrix"}
#'
#' @slot Colprinccoord Principal coordinates for column points, class \code{"matrix"}
#'
#' @slot Rowstdcoord Standard coordinates for row points, class \code{"matrix"}
#'
#' @slot Colstdcoord Standard coordinates for column points, class \code{"matrix"}
#'
#' @slot RowCTR Contributions for row points, class \code{"matrix"}
#'
#' @slot RowREP Representations for row points, class \code{"matrix"}
#'
#' @slot ColCTR Contributions for column points, class \code{"matrix"}
#'
#' @slot ColREP Representations for column points, class \code{"matrix"}
#'
#' @slot RowVar Variances for row points, class \code{"matrix"}
#'
#' @slot RowCov Covariances for row points, class \code{"array"}
#'
#' @slot ColVar Variances for column points, class \code{"matrix"}
#'
#' @slot ColCov Covariances for column points, class \code{"array"}
#'
#' @slot inertiasum Total inertia, class \code{"numeric"}
#'
#' @slot inertias Axis inertias, class \code{"matrix"}
#'
#' @slot nboots Number of bootstrap replicates used to calculate the (co)variances, class \code{"numeric"}. \cr
#' If nboots=0 then standard CA or MCA is performed with no confidence regions produced.
#'
#' @slot resampledistn Distribution used for resampling, class \code{"character"}
#'
#' @slot multinomialtype Form of multinomial resampling used, class \code{"character"}
#'
#' @slot sameaxisorder Number of resamples with no reordering in first six bootstrap axes, class \code{"numeric"}
#'
#' @slot poissonzeronewmean Mean used for resampling zero cells, class \code{"numeric"}
#'
#' @slot newzeroreset Option to reset resample zero cells, class \code{"numeric"}
#'
#' @slot printdims Number of dimensions to print, though note that all are stored, class \code{"numeric"}
#'
#' @slot axisvariances Number of axes for which variances were calculated and are stored, class \code{"numeric"}
#'
#' @slot bootcritR Bootstrap critical values for row points, class \code{"array"}
#'
#' @slot bootcritC Bootstrap critical values for column points, class \code{"array"}
#'
#' @slot usebootcrits Whether to use bootstrap critical values for confidence ellipses, class \code{"logical"}
#'
#' @slot catype Type of correspondence analysis performed, class \code{"character"}
#'
#' @slot mcatype Type of multiple correspondence analysis performed, class \code{"character"}
#'
#' @slot mcaindividualboot Whether the experimental method to bootstrap an indicator or doubled matrix was used, class \code{"logical"}
#'
#' @slot IndicatorMatrix The indicator matrix dervied from the data matrix, class \code{"matrix"}
#'
#' @slot Jk The number of classes for each variable, class \code{"numeric"}
#'
#' @slot p The number of variables, class \code{"numeric"}
#'
#' @slot mcalikertnoise The noise value used in the experimental method to bootstrap an indicator or doubled matrix, class \code{"numeric"}
#'
#' @slot mcaadjustinertias Whether MCA inertias were adjusted, class \code{"logical"}
#'
#' @slot mcauseadjustinertiasum Whether the adjusted MCA inertia sum was used, class \code{"logical"}
#'
#' @slot mcaadjustcoords Whether the MCA coordinates were adjusted, class \code{"logical"}
#'
#' @slot mcaadjustmassctr Whether the MCA masses and contributions were adjusted, class \code{"logical"}
#'
#' @slot mcasupplementary How supplementary points were calculated when bootstrapping a Burt matrix, class \code{"character"}
#'
#' @seealso \code{\linkS4class{cabasicresults}}
#'
#'
#' @export
setClass("cabootcrsresults",
representation(
  br="cabasicresults",
  DataMatrix="matrix", rows="numeric", columns="numeric",
  rowlabels="character", collabels="character", varnames="character",
  Rowprinccoord="matrix", Colprinccoord="matrix", Rowstdcoord="matrix", Colstdcoord="matrix",
  RowCTR="matrix", RowREP="matrix", ColCTR="matrix", ColREP="matrix",
  RowVar="matrix", RowCov="array", ColVar="matrix", ColCov="array",
  inertiasum="numeric", inertias="matrix",
  nboots="numeric", resampledistn="character", multinomialtype="character",
  sameaxisorder="numeric",
  poissonzeronewmean="numeric", newzeroreset="numeric",
  printdims="numeric", axisvariances="numeric",
  bootcritR="array", bootcritC="array", usebootcrits="logical",
  catype="character", mcatype="character", mcaindividualboot="logical", IndicatorMatrix="matrix",
  Jk="numeric", p="numeric", mcalikertnoise="numeric",
  mcaadjustinertias="logical", mcauseadjustinertiasum="logical",
  mcaadjustcoords="logical", mcaadjustmassctr="logical", mcasupplementary="character" ) )

###### FUNCTIONS

#### Purely internal to printca and summaryca, no need to document

#' Internal function to be used by printca and summaryca
#'
#' \code{settingsinertias} prints the settings and the inertias
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @return printed output only
#'
#' @examples
#' # Purely internal, not intended for use by users
#'
#' @seealso \code{\link{summaryca}}, \code{\link{printca}}
#'
settingsinertias <- function(x) {

if (x@nboots>0) {
  if (!(any(x@resampledistn==c("multinomial","nonparametric")))) {
    cat(paste(switch(x@resampledistn, Poisson="Poisson", balanced="Balanced", myresample="Custom"), "resampling \n\n")) }
  if ((x@catype=="sca")&(x@resampledistn=="multinomial")&(!(x@multinomialtype=="whole"))) {
    cat(paste("Multinomial resampling with", switch(x@multinomialtype, rowsfixed="row sums fixed", columnsfixed="column sums fixed"),"\n\n" ))
  }
}
if (x@catype=="mca") {
  cat(paste( switch(x@mcatype, Burt="Burt", indicator="Indicator", doubled="Doubled"), "matrix \n") )
  if ( (x@mcaadjustinertias==TRUE) & (!(x@mcatype=="doubled")) ) {
    cat("Adjusted inertias, compared to ")
    if (x@mcauseadjustinertiasum==TRUE) { cat("their sum \n") } else { cat("average off-diagonal inertia \n") }
    if (x@mcaadjustcoords==TRUE) { cat("Adjusted coordinates \n") }
    if ((x@nboots>0)&(x@mcatype=="Burt")) {
      if (x@mcasupplementary=="offdiag") {
        cat("Bootstrap coordinates calculated without Burt diagonal \n")
      } else {
        cat("Bootstrap coordinates calculated with Burt diagonal \n")
      }
    }
    if (x@mcaadjustmassctr==TRUE) {
      cat("Mass and Ctr sum to p, so MCA with p=2 gives same as CA \n")
    }
  }
  cat(" \n")
}
cat("Total inertia ", x@inertiasum, "\n\n")
cat("Inertias, percent inertias and cumulative percent inertias \n\n")
ins <- data.frame(x@inertias)
names(ins) <- c("Inertia","%  ","Cum. %")
print(ins, digits=4)
cat("\n")

}


#' Prints reasonably full results, including variances
#'
#' \code{printca} prints full correspondence analysis results, including inertias, coordinates,
#' representations, contributions, variances, covariances and critical values
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @param datasetname The name (in "") of the data set, to be used in the output
#'
#' @return Printed results, no plots or objects produced
#'
#' @examples
#' results <- cabootcrs(DreamData, showresults=FALSE)
#' printca(results, datasetname="Dreams")
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\link{summaryca}}, \code{\link{plotca}},
#' \code{\linkS4class{cabootcrsresults}}
#'
#' @export
printca <- function(x, datasetname="") {

#setGeneric("print", function(x,...) standardGeneric("print") )
#setMethod("print", signature(x="cabootcrsresults"),
# function(x, datasetname="") {

## Printing macro
printwithaxes <- function(res, thenames) {
  names(res) <- thenames
  print(res, digits=4)
}
##

d <- min(x@printdims, x@br@realr)
axnames <- character(length=d)
for (i in 1:d) { axnames[i] <- paste(" Axis",i) }

cat("\n    RESULTS for Correspondence Analysis:", datasetname, "\n\n")

settingsinertias(x)

if ( (x@catype=="sca") | ((x@catype=="mca")&(x@mcatype=="indicator")) ) {
if (x@catype=="sca") { Things <- "Rows" } else { Things <- "Individuals" }
if (x@catype=="sca") { Thing <- "Row" } else { Thing <- "Individual" }
cat("\n", Things, "in principal coordinates\n\n")
printwithaxes(data.frame(x@Rowprinccoord[ ,1:d], row.names=x@rowlabels), axnames)
cat("\n", Thing, "contributions (per mil)\n\n")
printwithaxes(data.frame(round(x@RowCTR[ ,1:d]*1000), row.names=x@rowlabels), axnames)
cat("\n", Thing, "representations (per mil)\n\n")
printwithaxes(data.frame(round(x@RowREP[ ,1:d]*1000), row.names=x@rowlabels), axnames)
}

if (x@catype=="sca") { Things <- "Columns" } else { Things <- "Variable categories" }
if (x@catype=="sca") { Thing <- "Column" } else { Thing <- "Variable category" }
cat("\n", Things, "in principal coordinates\n\n")
printwithaxes(data.frame(x@Colprinccoord[ ,1:d], row.names=x@collabels), axnames)
cat("\n", Thing, "contributions (per mil)\n\n")
printwithaxes(data.frame(round(x@ColCTR[ ,1:d]*1000), row.names=x@collabels), axnames)
cat("\n", Thing, "representations (per mil)\n\n")
printwithaxes(data.frame(round(x@ColREP[ ,1:d]*1000), row.names=x@collabels), axnames)

if (x@nboots>0) {
cat("\n\n  Results for Bootstrapping\n\n")
cat(x@nboots, "bootstrap replications with", x@resampledistn, "resampling\n")
if (x@resampledistn=="multinomial" & x@multinomialtype!="whole")
 cat(paste("  ",
  switch(x@multinomialtype,rowsfixed="with row sums constant",columnsfixed="with column sums constant"),
  "\n") )

cat("\nBootstrap critical values for axis 1,2 plot\n\nDefault chi^2 is ")
print(qchisq(c(0.9,0.95,0.99),2),digits=4)
if (x@catype=="sca") {
cat("\nRows\n\n")
temp <- data.frame(x@bootcritR[,1,2,],row.names=x@rowlabels)
names(temp) <- c(90,95,99)
print(temp,digits=4)
}
cat("\nColumns\n\n")
temp <- data.frame(x@bootcritC[,1,2,],row.names=x@collabels)
names(temp) <- c(90,95,99)
print(temp,digits=4)
cat("\n")

cat("\nEstimated variances and covariances\n\n")
if (x@catype=="sca") {
cat("Rows\n\n")
print(allvarscovs(x,"rows"),digits=4)
}
cat("\nColumns\n\n")
print(allvarscovs(x,"columns"),digits=4)
cat("\n\n")
} # nboots>0
} # printca


#' Prints brief 2-d results, with standard deviations
#'
#' \code{summaryca} prints correspondence analysis results for the first two dimensions,
#' giving inertias, coordinates,
#' representations, contributions and standard deviations
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @param datasetname The name (in "") of the data set, to be used in the output
#'
#' @param mcaprintindividuals If TRUE then print individual (row) point results in multiple
#' correspondence analysis when using indicator or doubled matrix
#'
#' @return Printed results, no plots or objects produced
#'
#' @examples
#' results <- cabootcrs(DreamData, showresults=FALSE)
#' summaryca(results, datasetname="Dreams")
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\link{printca}}, \code{\link{plotca}},
#' \code{\linkS4class{cabootcrsresults}}
#'
#' @export
summaryca <- function(x, datasetname="", mcaprintindividuals=FALSE) {

#setGeneric("summary", function(x,...) standardGeneric("summary") )
#setMethod("summary", signature(x="cabootcrsresults"),
# function(x, datasetname="") {

colnames <- character(length=9)
colnames <- c("  Axis 1","StDev","Rep","Ctr","  Axis 2","StDev","Rep","Ctr","  Mass","Quality")
colnamesnosd <- character(length=7)
colnamesnosd <- c("  Axis 1","Rep","Ctr","  Axis 2","Rep","Ctr","  Mass","Quality")

cat("\n    SUMMARY RESULTS for Correspondence Analysis:", datasetname, "\n\n")

settingsinertias(x)

if (x@nboots>0) {
cat("Principal coords, std devs; rep and ctr (per mil); mass (per mil); 2-d rep (per mil)\n\n")
} else {
cat("Principal coords; rep and ctr (per mil); mass (per mil); 2-d rep (per mil)\n\n")
}

if ( (x@catype=="mca")&(x@mcaadjustmassctr==TRUE) ) { massmult<-x@p } else { massmult<-1 }

if ( (x@catype=="sca") | ((x@catype=="mca")&(x@mcatype=="indicator")&(mcaprintindividuals==TRUE)) ) {
if (x@catype=="sca") { cat("Rows: \n") } else { cat("Individuals: \n") }
rop <- data.frame(
 round(x@Rowprinccoord[,1]*1000)/1000,
 round(sqrt(x@RowVar[,1])*1000)/1000,
 round(x@RowREP[ ,1]*1000),
 round(x@RowCTR[ ,1]*1000),
 round(x@Rowprinccoord[,2]*1000)/1000,
 round(sqrt(x@RowVar[,2])*1000)/1000,
 round(x@RowREP[ ,2]*1000),
 round(x@RowCTR[ ,2]*1000),
 round(massmult*1000*rowSums(x@DataMatrix)/sum(x@DataMatrix)),
 round(rowSums(x@RowREP[ ,1:2]*1000)), row.names=x@rowlabels )
if (x@nboots==0) {
  rop <- rop[,c(1,3,4,5,7,8,9,10)]
  names(rop) <- colnamesnosd
} else {
  names(rop) <- colnames
}
print(rop,digits=3)
cat("\n")
} # print row output unless Burt

if (x@catype=="sca") { cat("Columns: \n") } else { cat("Variables: \n") }
cop <- data.frame(
 round(x@Colprinccoord[,1]*1000)/1000,
 round(sqrt(x@ColVar[,1])*1000)/1000,
 round(x@ColREP[ ,1]*1000),
 round(x@ColCTR[ ,1]*1000),
 round(x@Colprinccoord[,2]*1000)/1000,
 round(sqrt(x@ColVar[,2])*1000)/1000,
 round(x@ColREP[ ,2]*1000),
 round(x@ColCTR[ ,2]*1000),
 round(massmult*1000*colSums(x@DataMatrix)/sum(x@DataMatrix)),
 round(rowSums(x@ColREP[ ,1:2]*1000)), row.names=x@collabels )
if (x@nboots==0) {
  cop <- cop[,c(1,3,4,5,7,8,9,10)]
  names(cop) <- colnamesnosd
} else {
  names(cop) <- colnames
}
print(cop,digits=3)
cat("\n")

} # summaryca


#' Extract a single 2 by 2 covariance matrix
#'
#' \code{covmat} extracts a 2 by 2 covariance matrix for one data point on two dimensions,
#' allowing the confidence ellipse to be plotted
#'
#' This can be used with the ellipse() package to add the confidence ellipse to a picture from another package\cr
#'
#' Example: confidence ellipse for row or column i on axes 1,2 from cabootcrs() output Results is:\cr
#'
#' lines( ellipse(x=covmat(Results,i,"row",1,2,FALSE),
#'                centre=Results@Rowprinccoord[i,cbind(1,2)], npoints=1000),
#'        cex=1, pch=".", col="blue")\cr
#' lines( ellipse(x=covmat(Results,i,"column",1,2,FALSE),
#'                centre=Results@Colprinccoord[i,cbind(1,2)], npoints=1000),
#'       cex=1, pch=".", col="blue")
#'
#' Note that \code{\link{reflectaxes}} will be needed if cabootcrs() and ca() axes
#' are reflected with respect to each other
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @param i The number of the row or column, note that in MCA this will be the number of the variable category
#' (e.g. for p=3 variables with 5 categories each, column 8 is the 3rd category of the 2nd variable)
#'
#' @param thing Whether to extract the covariance matrix for the i-th
#' \describe{
#' \item{"row"}{row, or}
#' \item{"column"}{column}
#' }
#' Note that default is "column" as this is more convenient for MCA
#'
#' @param axis1 First axis for which (co)variances are required
#'
#' @param axis2 Second axis for which (co)variances are required
#'
#' @param show If TRUE then print the extracted covariance matrix
#'
#' @return An object of class \code{"matrix"} (square symmetric, 2 by 2)
#'
#' @examples
#' results <- cabootcrs(DreamData, showresults=FALSE)
#' row2covmataxes12 <- covmat(results,2,"row")
#' col3covmataxes23 <- covmat(results,3,"column",2,3)
#'
#' \dontrun{
#'
#' # There are now 3 variables with 5,4,3 categories, hence 12 columns
#' resultsmca <- cabootcrs(DreamData223by3, catype="mca", showresults=FALSE)
#' row2covmataxes12mca <- covmat(resultsmca,2,"column")
#' col3covmataxes23mca <- covmat(resultsmca,8,"column",2,3)
#' newvarcat2covmataxes12mca <- covmat(resultsmca,11,"column")
#'
#'
#' # Use ellipse() to put confidence regions around row points on a plot produced by ca().
#' # Note that reflectaxes() will be needed if cabootcrs() and ca() axes
#' # are reflected with respect to each other
#'
#' library(ca)
#' library(ellipse)
#' TheData <- DreamData
#' Results <- cabootcrs(TheData, showresults=FALSE)
#' caResults <- ca(TheData)
#' plot(caResults)
#' for (i in 1:dim(TheData)[1]) {
#'   lines( ellipse(x=covmat(Results,i,"row",1,2,FALSE),
#'                  centre=Results@Rowprinccoord[i,cbind(1,2)], npoints=1000),
#'         cex=1, pch=".", col="blue")
#' }
#' }
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\link{allvarscovs}},
#' \code{\linkS4class{cabootcrsresults}}
#'
#' @export
covmat <- function(x, i, thing="column", axis1=1, axis2=2, show=TRUE) {

## Printing macro
printwithaxes <- function(res, thenames) {
names(res) <- thenames
print(res, digits=4)
}
##

if (!(is(x,"cabootcrsresults"))) stop(paste("Must be of type cabootcrsresults\n\n"))
if (!any(thing==c("row","column"))) stop(paste("Must be row or column\n\n"))
if (axis1==axis2) stop(paste("What are you playing at?\n\n"))
if (!any(axis1==seq(1,x@axisvariances))) stop(paste("Covariance not available for these axes\n\n"))
if (!any(axis2==seq(1,x@axisvariances))) stop(paste("Covariance not available for these axes\n\n"))
if ((thing=="row") & !any(i==seq(1,x@rows))) stop(paste("Invalid row number\n\n"))
if ((thing=="column") & !any(i==seq(1,x@columns))) stop(paste("Invalid column number\n\n"))

a1 <- min(axis1,axis2)
a2 <- max(axis1,axis2)
tname <- ""
if (thing=="row") {
V <- matrix(c(x@RowVar[i,axis1],x@RowCov[i,a1,a2],x@RowCov[i,a1,a2],x@RowVar[i,axis2]),2,2)
if (!is.null(x@rowlabels)) { tname <- paste("(",x@rowlabels[[i]],")") }
} else { # column
V <- matrix(c(x@ColVar[i,axis1],x@ColCov[i,a1,a2],x@ColCov[i,a1,a2],x@ColVar[i,axis2]),2,2)
if (!is.null(x@collabels)) { tname <- paste("(",x@collabels[[i]],")") }
}

if (show==TRUE) {
cat(paste("Covariance matrix of", switch(thing,"row"="row","column"="column"), i, tname, "for axes", axis1,axis2,"\n\n"))
rcnames <- c(paste("Axis",axis1),paste("Axis",axis2))
printwithaxes(data.frame(V,row.names=rcnames),rcnames)
}

invisible(V)

# Can use this with ellipse package to add the confidence ellipse to a picture from another package
# For example, confidence ellipse for row or column i on axes 1,2 from cabootcrs output Results is:
#
# lines(ellipse(x=covmat(Results,i,"row",1,2,FALSE),
#               centre=Results@Rowprinccoord[i,cbind(1,2)], npoints=1000), cex=1, pch=".", col="blue")
# lines(ellipse(x=covmat(Results,i,"column",1,2,FALSE),
#               centre=Results@Colprinccoord[i,cbind(1,2)], npoints=1000), cex=1, pch=".", col="blue")
#
# Example: to add row CRs to a plot from package ca to data set TheData
# Results <- cabootcrs(TheData, showresults=FALSE)
# caResults <- ca(TheData)
# plot(caResults)
# for (i in 1:dim(TheData)[1]) { lines(ellipse(x=covmat(Results,i,"row",1,2,FALSE),
#       centre=Results@Rowprinccoord[i,cbind(1,2)], npoints=1000), cex=1, pch=".", col="blue") }

}


#' Extract all variances and covariances in readable form as a data frame
#'
#' \code{allvarscovs} extracts all variances and covariances for either rows or columns
#' and puts them in a data frame
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @param thing Whether to extract the variances for
#' \describe{
#' \item{"rows"}{rows, or}
#' \item{"columns"}{columns}
#' }
#' Note that default is "columns" as this is more convenient for MCA
#'
#' @return A data frame with one row for each row or column category
#'
#' @examples
#' results <- cabootcrs(DreamData, showresults=FALSE)
#' rowvars <- allvarscovs(results,"rows")
#' colvars <- allvarscovs(results,"columns")
#'
#' \dontrun{
#'
#' resultsmca <- cabootcrs(DreamData223by3, catype="mca", showresults=FALSE)
#' allvars <- allvarscovs(resultsmca)
#'
#' }
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}},
#' \code{\link{covmat}}, \code{\linkS4class{cabootcrsresults}}
#'
#' @export
allvarscovs <- function(x, thing="columns") {

## function to extract upper triangle

getcovs <- function(allC,n,ncovs) {
V <- matrix(0,n,ncovs)
for (i in 1:n) {
y <- allC[i,,]
V[i,] <- y[upper.tri(y)]
}
invisible(V)
} # getcovs

##

if (!(is(x,"cabootcrsresults"))) stop(paste("Must be of type cabootcrsresults\n\n"))
if (!any(thing==c("rows","columns"))) stop(paste("Must be rows or columns\n\n"))

ncovs <- x@axisvariances*(x@axisvariances-1)/2
vcnames <- character(length=x@axisvariances+ncovs)
k <- 1
for (i in 1:x@axisvariances) {
  vcnames[i] <- paste(" Var Axis",i)
  if (i<x@axisvariances) {
    for (j in (i+1):x@axisvariances) {
      vcnames[x@axisvariances+k] <- paste(" Cov axes",i,j)
      k <- k+1
} } }

if (thing=="rows") {
Covs <- getcovs(x@RowCov,x@rows,ncovs)
allV <- data.frame(cbind(x@RowVar,Covs), row.names=x@rowlabels)
} else { # columns
Covs <- getcovs(x@ColCov,x@columns,ncovs)
allV <- data.frame(cbind(x@ColVar,Covs), row.names=x@collabels)
}

names(allV) <- vcnames

allV

}


#' Reflect coordinates for chosen axes
#'
#' \code{reflectaxes} reflects the principal and standard coordinates
#' of the axes chosen, and the appropriate covariances where needed
#'
#' This may be useful when comparing results between different data sets or from different packages
#'
#' @param x An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @param axes A list or vector containing the numbers of the axes to be reflected
#'
#' @return An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @examples
#' results <- cabootcrs(DreamData)
#' resultsreflectfirstaxis <- reflectaxes(results)
#' summaryca(resultsreflectfirstaxis)
#' plotca(resultsreflectfirstaxis)
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\linkS4class{cabootcrsresults}}
#'
#' @export
reflectaxes <- function(x, axes=c(1,2)) {

refx <- x
refmat <- diag(1,x@axisvariances,x@axisvariances)
for (i in 1:length(axes) ) { refmat[axes[i],axes[i]] <- -1 }

refx@Rowprinccoord <- refx@Rowprinccoord %*% refmat
refx@Colprinccoord <- refx@Colprinccoord %*% refmat
refx@Rowstdcoord <- refx@Rowstdcoord %*% refmat
refx@Colstdcoord <- refx@Colstdcoord %*% refmat

for (i in 1:x@axisvariances) {
  if (i<x@axisvariances) {
    for (j in (i+1):x@axisvariances) {
       if (xor( any(i==axes), any(j==axes) )) {
         refx@RowCov[,i,j] <- -1 * refx@RowCov[,i,j]
         refx@ColCov[,i,j] <- -1 * refx@ColCov[,i,j]
} } } }

refx

}

#' Old and rubbish algorithm to rearrange bootstrap axes by comparing to sample axes
#'
#' \code{rearrange_old} compares one set of axes for row points and column points (from the bootstrap
#'   data matrix) to another (from the sample data matrix) by looking at all possible
#'   reorderings and reflections (only) of the bootstrap axes and picking the one which
#'   best matches the sample axes.
#'
#' This is only intended for internal use by the \code{\link{cabootcrs}} function, and only for
#' simple CA if for some reason the lpSolve package is unavailable.
#'
#' It has not been used with MCA, and so will almost certainly not work properly in that case.
#'
#' Finds the rearrangement of columns of RB and CB to maximise match = tr( abs(RS'*RB + CS'*CB) )
#'
#' Goes through all possible orderings and so is painfully slow.
#'
#' @param RS Sample axes for row points (as columns)
#' @param RB Bootstrap axes for row points (as columns)
#' @param CS Sample axes for column points (as columns)
#' @param CB Bootstrap axes for column points (as columns)
#' @param r Rank of the bootstrap matrix
#'
#' @return list containing: \cr
#'   T = matrix to rearrange xB so it is equivalent to xS, i.e. xS <- xB * T \cr
#'   numre = number of axes checked for rearranging = min(r,maxrearrange)\cr
#'   match = assign$objval from the Hungarian algorithm\cr
#'   same = flag for whether there was no reordering of axes (but may have been reflection)
#'
#' @examples
#' # Not intended for direct call by users
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}},  \code{\link{rearrange}}
#'
rearrange_old <- function( RS, RB, CS, CB, r ) {

  # r = rank of bootstrap matrix, so if < sample rank
  #     will ignore last sample axis
  # RS = sample axes for row points (as columns)
  # RB = bootstrap axes for row points (as columns)
  # CS = sample axes for column points (as columns)
  # CB = bootstrap axes for column points (as columns)
  # T = matrix to rearrange xB so is equivalent to xS, i.e.
  # xS ~= xB * T
  # find rearrangement of columns of RB and CB to maximise
  # match = tr( abs(RS'*RB) + abs(CS'*CB) )
  # Literal, uses all axes of bootstrap matrix up to its rank,
  #  up to a maximum of maxrearrange=6

  if (r>=1) {
    maxrearrange <- 6
    numrearranged <- min(r,maxrearrange)
    switch(numrearranged,
           per <- matrix(1,1,1),
           per <- rbind( c(1,2), c(2,1) ),
           per <- cbind( rep(1:3,each=2), c(2,3,1,3,1,2), c(3,2,3,1,2,1) ),
           { p <- cbind( rep(1:3,each=2), c(2,3,1,3,1,2), c(3,2,3,1,2,1), 4 )
           per <- rbind(p,p+3*(p==1)-3*(p==4),p+2*(p==2)-2*(p==4),p+(p==3)-(p==4) ) },
           { p <- cbind( rep(1:3,each=2), c(2,3,1,3,1,2), c(3,2,3,1,2,1), 4 )
           p <- rbind(p,p+3*(p==1)-3*(p==4),p+2*(p==2)-2*(p==4),p+(p==3)-(p==4) )
           p <- cbind(p,5)
           per <- rbind(p,p+4*(p==1)-4*(p==5),p+3*(p==2)-3*(p==5),p+2*(p==3)-2*(p==5),p+(p==4)-(p==5) ) },
           { p <- cbind( rep(1:3,each=2), c(2,3,1,3,1,2), c(3,2,3,1,2,1), 4 )
           p <- rbind(p,p+3*(p==1)-3*(p==4),p+2*(p==2)-2*(p==4),p+(p==3)-(p==4) )
           p <- cbind(p,5)
           p <- rbind(p,p+4*(p==1)-4*(p==5),p+3*(p==2)-3*(p==5),p+2*(p==3)-2*(p==5),p+(p==4)-(p==5) )
           p <- cbind(p,6)
           per <- rbind(p,p+5*(p==1)-5*(p==6),p+4*(p==2)-4*(p==6),p+3*(p==3)-3*(p==6),p+2*(p==4)-2*(p==6),p+(p==5)-(p==6) ) }
    )

    nper <- dim(per)[1]
    match <- matrix(0,nper,1)
    for (i in 1:nper) {
      match[i] = sum( diag( abs( t(RS[ ,1:numrearranged]) %*% RB[ ,per[i, ] ] + t(CS[ ,1:numrearranged]) %*% CB[ ,per[i, ] ] ) ) )
    }

    posn <- which.max(match)
    same <- posn==1
    I <- diag( rep(1,numrearranged) )
    T <- I[ ,per[posn, ] ]
    t <- diag( t(RS[ ,1:numrearranged]) %*% RB[ ,per[posn, ] ] + t(CS[ ,1:numrearranged]) %*% CB[ ,per[posn, ] ] )
    T <- T %*% diag( (t>=0)-(t<0), nrow=numrearranged, ncol=numrearranged )
  } else {  # r=0, i.e. raw bootstrap matrix is rank 1
    T <- matrix(1,1,1)
    numrearranged <- 1
    match <- 0
    same <- 0
  }

  list(T=T,numrearranged=numrearranged,match=match,same=same)

}

