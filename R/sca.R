
#' Performs standard Correspondence Analysis calculations
#'
#' \code{sca} returns all the basic results from a CA of a matrix with rows >= cols,
#' in an object of class \code{\linkS4class{cabasicresults}}
#'
#' This is only intended for internal use by the \code{\link{cabootcrs}} function.
#'
#' @param X A data matrix with rows >= cols
#'
#' @param catype Can be "sca" for simple CA or "mca" for multiple CA
#'
#' @param mcatype If catype="mca" then this can be "Burt", "Indicator"
#'  or "doubled" depending on the analysis required.\cr
#'  This affects the number of meaningful singular values, as does p below
#'
#' @param p Number of variables, only needed if catype="mca"
#'
#' @return An object of class \code{\linkS4class{cabasicresults}}
#'
#' @examples
#' results <- sca(as.matrix(DreamData))
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}, \code{\linkS4class{cabasicresults}}
#'
#' @export
sca <- function(X,catype="sca",mcatype=NULL,p=2) {

# Performs simple CA of a matrix X with rows >= cols

# Output:
# R = row profile matrix
# C = column profile matrix
# Rweights = D_c^-1/2
# Cweights = D_r^-1/2
# Raxes = V
# Caxes = U
# r = rank
# mu = mu

X <- X/sum(X)

rmax <- min(dim(X))-1

rsums <- as.vector(rowSums(X))
csums <- as.vector(colSums(X))

drm1 <- diag( 1/( rsums + (rsums==0) ) * (1-(rsums==0)) )
dcm1 <- diag( 1/( csums + (csums==0) ) * (1-(csums==0)) )
drmh <- sqrt(drm1)
dcmh <- sqrt(dcm1)

Z <- drmh %*% ( X - rsums %*% t(csums) ) %*% dcmh

Y <- svd(Z)

mu <- Y$d[1:rmax]
r <- sum(mu>1e-15)

R <- drm1 %*% X
Rweights <- dcmh
Raxes = Y$v[ ,1:rmax]

C <- dcm1 %*% t(X);
Cweights <- drmh;
Caxes = Y$u[, 1:rmax]

if (r < rmax) {
  mu[ (r+1):rmax ] <- 0;
  Raxes[ , (r+1):rmax ] <- 0;
  Caxes[ , (r+1):rmax ] <- 0;
}

if (catype=="sca") {
  realr <- r
} else {
  if (mcatype=="doubled") {
    realr <- r
  } else {
    if (mcatype=="Burt") {
      realr <- length(mu[mu>(1/p)])
    } else { # indicator
      realr <- length(mu[mu^2>(1/p+1e-16)])
    }
  }
}

sca <- new("cabasicresults",
          Rprofile=R,Cprofile=C,Rweights=Rweights,Cweights=Cweights,
          Raxes=Raxes,Caxes=Caxes,r=r,realr=realr,mu=mu)

}


