
#' Rearranges bootstrap axes by comparing to sample axes
#'
#' \code{rearrange} compares one set of axes for row points and column points (from the bootstrap
#'   data matrix) to another (from the sample data matrix) by looking at all possible
#'   reorderings and reflections (only) of the bootstrap axes and picking the one which
#'   best matches the sample axes.
#'
#' This is only intended for internal use by the \code{\link{cabootcrs}} function.
#'
#' Finds the rearrangement of columns of RB and CB to maximise match = tr( abs(RS'*RB + CS'*CB) )
#'
#' Uses the Hungarian algorithm via lp.assign in lpSolve up to a maximum of maxrearrange vectors.
#'
#' Algorithm assigns columns (B) to rows (S), hence transpose matrix so postmultiplication moves B to coincide with S.
#'
#' In effect this is Procrustes rotation of bootstrap axes to best match sample axes,
#' except that there is no rotation, only reflection and reordering of axes (aka rearranging).
#'
#' Note that this seeks the best fit to all axes, not best fit just to the ones
#'  whose variances are being calculated, and does not weight the reordering
#'  by eigenvalues or restrict how far a vector can be reordered by.
#'  Hence a fairly low maxrearrange may be preferable.
#'
#' Faster than full comparison when rank >= 4, for maxrearrange=6,
#'  but can take much longer if rearrange all axes
#'
#' Rearranging more axes means higher chance of finding a matching axis,
#'  so std dev can be decreased by average of 1-2\% if all axes are rearranged.
#'
#' Limited testing suggests that rearranging all axes tends to over-reorder
#'  and hence underestimate variances, due to ignoring eigenvalues,
#'  hence seems best to rearrange 6 as before, unless very large numbers
#'  of close eigenvalues.
#'
#' When mca bootstrap replicate has fewer "real" singular values (i.e. > 1/p)
#'  than the sample matrix then only the first B@realr axes will be compared,
#'  so that the last sample axis will get nothing from this replicate and the
#'  "real" bootstrap axes will be matched only with the same first few sample ones.
#'
#' r = rank of bootstrap matrix, so if < sample rank will ignore last sample axis
#'
#' @param RS Sample axes for row points (as columns)
#' @param RB Bootstrap axes for row points (as columns)
#' @param CS Sample axes for column points (as columns)
#' @param CB Bootstrap axes for column points (as columns)
#' @param r Rank of the bootstrap matrix
#' @param reflectonly TRUE to reflect the axes only, no reordering
#' @param catype Can be "sca" for simple or "mca" for multiple CA.\cr
#'   If "sca" then the rearranging will use both row axes and column axes,\cr
#'   if "mca" then this depends on the mcatype parameter
#' @param mcatype "Burt" if using Burt matrix rows and columns are the same, so only use column axes\cr
#'   "indicator" if using indicator matrix, only use row axes
#' @param mcaindividualboot TRUE to use highly experimental method
#' @param maxrearrange Maximum number of axes to rearrange
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
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}
#'
#' @export
rearrange <- function( RS, RB, CS, CB, r, reflectonly=FALSE, catype="sca", mcatype="Burt", mcaindividualboot=FALSE, maxrearrange=6) {

numre <- min(r,maxrearrange) # number to rearrange

# sca uses both
# mca with indicator only row axes make sense unless experimental likert boot used
# mca with burt only one needed
# row = sample axis number, column = bootstrap axis number
if (catype=="sca") { val <- t(RS) %*% RB + t(CS) %*% CB } else {
  if (mcatype=="Burt") { val <- t(CS) %*% CB } else {
    if (mcaindividualboot) { val <- t(RS) %*% RB + t(CS) %*% CB } else {
       val <- t(RS) %*% RB
} } }

val <- val[1:numre,1:numre]
reflect <- (val>=0) - (val<0)
val <- abs(val)

if ( (reflectonly==FALSE) & (r>1) ) {

assign <- lp.assign(val, direction="max")
soln <- assign$solution
match <- assign$objval
T <- t(soln * reflect)
same <- abs(sum(diag(soln))-numre)<1e-10

} else {

T <- as.matrix(reflect)
match <- 0
same <- 0

}

list(T=T,numre=numre,match=match,same=same)

}


