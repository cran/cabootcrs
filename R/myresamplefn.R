
#' Example of a user-generated resampling routine.
#'
#' \code{myresamplefn} in this case assumes that each pair of cells represents 50 people
#' answering yes or no to a question, with undecideds not recorded
#'
#' This is only intended as an example of a user-generated resampling routine,
#' users should replace it with their own function of this name
#'
#' In this example we assume that rows groups of 50 people have each been
#' asked columns/2 questions, with possible answers yes/no/undecided.
#' It uses binomial bootstrapping for pairs of columns, assuming that
#' successive columns are "yes" and "no" answers, with others undecided,
#' with sums of pairs of columns having a maximum, in this case 50.
#'
#' @param X a data matrix, to be resampled from
#'
#' @return a resampled version of the input data matrix
#'
#' @examples
#' # Five groups of people answer two yes/no/undecided questions
#' # Note: this is just an example, and does not intend to claim that this
#' # is the correct analysis for such a data set
#'
#' x <- as.matrix( rbind( c(22,25,18,22), c(12,23,21,27),
#'                        c(31,12,28,22), c(29,14,35,11), c(7,31,12,21)))
#' xresampled <- myresamplefn(x)
#' bmr <- cabootcrs(x,"myresample",nboots=199)
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\link{cabootcrs}}
#'
#' @export
myresamplefn <- function(X) {

  rows <- dim(X)[1]
  cols <- dim(X)[2]
  pairs <- cols/2
  maxsum <- 50
  Xr <- matrix(0,rows,cols)

  for (j in 1:pairs) {
    for (i in 1:rows) {
      p <- numeric(3)
      p[1] <- X[i,(2*j-1)]
      p[2] <- X[i,(2*j)]
      p[3] <- maxsum-X[i,(2*j)]-X[i,(2*j-1)]
      p <- p/maxsum
      Xr[i,(2*j-1):(2*j)] <- rmultinom(1,maxsum,p)[1:2]
    }
  }

  Xr

}

