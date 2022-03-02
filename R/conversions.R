
#' Converting a data matrix from one format into another
#'
#' \code{convert} recodes a data matrix from one format, used by versions of correspondence analysis,
#' into another (n objects by p variables, counts for distinct combinations of p variables, indicator matrix, contingency table).
#'
#' @param Xinput A data matrix, in the form of a data frame or similar
#'
#' @param input The format of the input matrix:
#' \describe{
#'   \item{"nbyp"}{An n individuals/objects/data points by p categorical variables matrix,
#'   where each row is a different data point and each column contains the category
#'   for that data point on that variable, where these categories can be numbers, strings or factors}
#'   \item{"nbypcounts"}{Similar to the above, but each row represents all of the data points taking
#'   the same combination of categories, and the first column contains the count for this combination
#'   (hence the name used here is a bit of a misnomer, but it emphasises the similarities to an n by p)}
#'   \item{"indicator"}{An indicator matrix, similar to the n by p matrix except that a variable with J_k categories
#'   is represented by J_k columns and a data point taking the i-th category has 1 in the i-th
#'   of these columns and a zero in the others}
#'   \item{"CT"}{A contingency table of counts}
#'  }
#'
#' @param output The format of the output matrix:
#' \describe{
#'   \item{"nbyp"}{As above}
#'   \item{"nbypcounts"}{As above}
#'   \item{"indicator"}{As above}
#'   \item{"doubled"}{Similar to indicator but each variable is now represented by 2 columns, and a data point
#'   taking the i-th category for a variable with J_k categories is given the values
#'   J_k-i in the first (low) column and i-1 in the second (high) column}
#' }
#'
#' @param Jk A list containing the number of distinct categories for each variable.\cr
#'   Either Jk or maxcat must be specified if input is "indicator"
#'
#' @param maxcat The maximum category value, for use when all variables are Likert on a scale of 1 to maxcat.\cr
#'   Either Jk or maxcat must be specified if input is "indicator"
#'
#' @param varandcat Flag for how to construct column names in an indicator matrix:
#' \describe{
#' \item{TRUE}{if many variables have the same categories, e.g. Likert, column names will be varname:catname}
#' \item{FALSE}{when variables have distinct categories, column names will just be category names}
#' }
#'
#' @return A list containing:
#' \describe{
#'   \item{result}{the output data matrix formatted according to the output argument}
#'   \item{varnames}{a list of length p containing the names of each variable}
#'   \item{catnames}{a list/array (of length p) containing the lists (of length Jk[i]) of category names for each variable}
#'   \item{Jk}{a list of length p containing the number of distinct categories for each variable}
#'   \item{p}{the number of variables}
#' }
#'
#' @examples
#' dreamdataCT <- DreamData
#' dreamdatanbyplist <- convert(dreamdataCT,input="CT",output="nbyp")
#' dreamdatanbyp <- dreamdatanbyplist$result
#'
#' \dontrun{
#'
#' dreamdataCTb <- table(dreamdatanbyp)
#' dreamdatanbypcounts <- convert(dreamdatanbyp,input="nbyp",output="nbypcounts")$result
#' dreamdataindicatorlist <- convert(dreamdatanbypcounts,input="nbypcounts",output="indicator")
#' dreamdatanbypb <- convert(dreamdataindicatorlist$result,input="indicator",
#'                           output="nbyp",Jk=dreamdataindicatorlist$Jk)$result
#'
#' nishdatanbyp <- NishData
#' nishdataindicator <- convert(nishdatanbyp)$result
#' nishdataBurt <- t(nishdataindicator)%*%nishdataindicator
#'
#' }
#'
#' @family conversion functions
#' @seealso
#' \code{\link{getBurt}} to obtain a Burt matrix or a subset of an existing one\cr
#' \code{\link{getCT}} to obtain a contingency table (only if p=2)\cr
#' \code{\link{getindicator}} to obtain an indicator matrix\cr
#' \code{\link{getdoubled}} to obtain a doubled matrix if all variables are ordered categorical with numbered categories
#'
#' @export
convert <- function(Xinput,input="nbyp",output="indicator",Jk=NULL,maxcat=NULL,varandcat=TRUE) {

  # Convert input matrix (Indicator, n by p or contingency table) into list of
  # matrix (Indicator or n by p), variable list, category list, Jk, p
  #  list(result=Xoutput,varnames=varnames,catnames=catnames,Jk=Jk,p=p)

  # possible conversions are
  # CT to indicator, doubled or n by p (raw or counts)
  # n by p (raw or counts) to indicator or doubled
  # indicator to n by p (raw or counts) or doubled

  # If input="nbyp" or "nbypcounts" then
  # if Jk=NULL then construct from observed categories
  # if Jk specified then assume p lots of 1 to Jk[k]
  # if maxcat specified for indicator (only) input then assume p lots of 1 to maxcat
  # If input="CT" then expand out CT into n by p first
  # if output="nbyp" then just leave at that
  # if output="indicator" then do that
  # if output="doubled" then do that
  # If input="indicator" then convert to nbyp according to Jk or maxcat
  # varandcat = TRUE gives varname:catname
  # varandcat = FALSE uses just catname in Indicator matrix, only if all variables have different category names
  # if nbyp or nbypcounts then should be data frame to allow mixed numeric and factor/character varcats

  if (!any(input==c("indicator","nbyp","nbypcounts","CT"))) stop(paste("Input must be indicator, nbyp, nbypcounts or CT\n\n"))
  if (any(output==c("Burt","burt"))) stop(paste("Use getBurt function for this\n\n"))
  if (!any(output==c("indicator","doubled","nbyp","nbypcounts"))) stop(paste("Output must be indicator, doubled, nbyp or nbypcounts\n\n"))
  if (input==output) stop(paste("Don't be stupid\n\n"))
  if ((input=="indicator")&(is.null(Jk))&(is.null(maxcat))) stop(paste("Must specify Jk or maxcat\n\n"))
  if ((output=="doubled")&(!is.numeric(as.matrix(Xinput)))) stop(paste("Can only double numeric classes\n\n"))

  if (input=="CT") { # create n by p
    Xct <- as.data.frame.matrix(Xinput)
    Jk <- dim(Xct)
    p <- 2
    n <- sum(Xct)
    varnames <- c("R","C")
    catnames <- list( rownames(Xct), colnames(Xct) )
    Xnp <- matrix(0,n,p) # both have non-numeric categories
    rno <- 0
    for (i in 1:dim(Xct)[1]) { for (j in 1:dim(Xct)[2]) { if (Xct[i,j]>0) {
      Xnp[(rno+1):(rno+Xct[i,j]),1] <- matrix(rownames(Xct)[[i]],Xct[i,j],1)
      Xnp[(rno+1):(rno+Xct[i,j]),2] <- matrix(colnames(Xct)[[j]],Xct[i,j],1)
      rno <- rno+Xct[i,j]
    } } }
    colnames(Xnp) <- varnames

  } # input CT

  if (input=="nbypcounts") {

    Xnpc <- as.data.frame(Xinput)
    if (!is.numeric(Xnpc[,1])) stop(paste("First column should be the counts\n"))

    Xnpc <- Xnpc[Xnpc[,1]>0,]
    n <- sum(Xnpc[,1])
    p <- dim(Xnpc)[2]-1

    # Xnp <- as.data.frame(matrix(0,n,p)) can't declare properly as then factors get changed to numeric
    Xnp <- Xnpc[,2:(p+1)]
    rno <- n
    for (i in (dim(Xnpc)[1]):1) {
      if (Xnpc[i,1]>0) {
        Xnp[(rno-Xnpc[i,1]+1):rno,] <- Xnpc[i,2:(p+1)]
        rno <- rno - Xnpc[i,1]
      }
    }
    if (!is.null(colnames(Xinput))) {
      colnames(Xnp) <- colnames(Xinput)[2:(p+1)] # [] for subsets
    } else {
      for (j in 1:p) colnames(Xnp)[j] <- paste("v",j,sep="")
    }
    varnames <- colnames(Xnp)

  } # input n by p counts

  if (input=="nbyp") {

    Xnp <- as.matrix(Xinput)

    if (is.numeric(Xnp[,1])) {
      if (any(Xnp[,1]>9)) cat(paste("WARNING: should you be specifying \"nbypcounts\"?\n\n"))
    }
    varnames <- colnames(Xnp)
    n <- dim(Xnp)[1]
    p <- dim(Xnp)[2]
  } # input n by p

  if (input=="indicator") {

    # can't work out var names or cat names just from indicator
    Xind <- Xinput
    pvec <- apply(Xind,1,sum)
    n <- dim(Xind)[1]
    p <- mean(pvec)
    if (!((p-round(p))==0)) stop(paste("Not a valid indicator matrix, rows don't have same sum\n\n"))
    if (is.null(Jk)) { Jk <- rep(maxcat,p) }
    if (!((dim(Xind)[[2]])==sum(Jk))) stop(paste("Sum of category sizes and width of indicator matrix do not agree\n\n"))
    Xnp <- matrix(0,n,p)
    varnames <- character(p)
    catnames <- NULL

  } # input indicator

  if (!(is.null(Jk))) { if (!(length(Jk)==p)) { stop(paste("Jk is wrong length\n\n")) } }

  if (input=="indicator") { # create n by p
    csjk <- c(0,cumsum(Jk))
    for (k in 1:p) { for (j in 1:Jk[k]) {
      Xnp[ Xind[,(csjk[k]+j)]==1, k] <- j
    } }
  } # input indicator

  # get catnames from matrix if don't already have

  if (any(input==c("indicator","nbyp","nbypcounts"))) {
   # catnames <- tapply(Xnp,col(Xnp),unique)
    catnames <- apply(Xnp,2,unique)
    if (is(catnames,"matrix")) { # R creates matrix instead of list if all the same length
      listcatnames <- vector("list", length=p)
      for (k in 1:p) { listcatnames[[k]] <- catnames[,k] }
      catnames <- listcatnames
    }
  }

  # get Jk from matrix if don't already have

  if ( any(input==c("nbyp","nbypcounts")) ) { #
    if (is.null(Jk)) { Jk <- numeric(p) }
    for (j in 1:p) {
      catnames[[j]]<- sort(catnames[[j]])
      if (Jk[j]==0) { Jk[j] <- length(catnames[[j]]) }
    }
  }

  # Create new indicator matrix if needed

  if ( (!(input=="indicator")) & (any(output==c("indicator","doubled"))) ) {

    csjk <- c(0,cumsum(Jk))
    Xind <- matrix(0,dim(Xnp)[1],sum(Jk))
    for (k in 1:p) { for (j in 1:Jk[k]) {
      Xind[,csjk[k]+j] <- (Xnp[,k]==catnames[[k]][j])
    } }
    Xind <- as.data.frame(Xind)

    # If input table has proper column names and category numbers, Indicator names are Var Name Cat Number e.g. A3
    if (!is.null(colnames(Xnp))) {
      for (k in 1:p) {
        if (any(colnames(Xnp)[[k]]==c("",paste("c",k,sep="")))) colnames(Xnp)[[k]] <- paste("v",k,sep="")
        for (j in 1:Jk[k]) {
          # sepchar of colon is omitted if variable is alphabetical and category is numeric e.g. A1 not A:1
          suppressWarnings( if ( any(is.na(as.numeric(catnames[[k]]))) | any(colnames(Xnp)[[1]]==c("V1","v1","R1","C1","Var1","var1")) ) {  sepchar <- ":" } else { sepchar <- "" })
          if ( varandcat | (length(unique(do.call(c,catnames))) < sum(Jk)) ) {
            colnames(Xind)[csjk[k]+j] <- paste(colnames(Xnp)[[k]],catnames[[k]][j],sep=sepchar)
          } else { # only if all catnames are distinct
            colnames(Xind)[csjk[k]+j] <- catnames[[k]][j]
          }
        }
      }
    } else {
      if (varandcat) {
        for (k in 1:p) {
          for (j in 1:Jk[k]) {
            colnames(Xind)[csjk[k]+j] <- paste("v",k,":",catnames[[k]][j],sep="")
          }
        }
      } else {
        for (k in 1:p) {
          for (j in 1:Jk[k]) {
            colnames(Xind)[csjk[k]+j] <- catnames[[k]][j]
          }
        }
      }
    }
    Xind <- as.matrix(Xind)

  } # create indicator matrix


  # output indicator, doubled or n by p

  if (output=="indicator") {
    Xoutput <- as.matrix(Xind)
  } else {
    if (output=="doubled") {
      Xdoubled <- as.data.frame(matrix(0,dim(Xnp)[1],2*p))
      #Xnpmat <- matrix(as.numeric(Xnp),ncol=p)
      Xnpmat <- as.matrix(Xnp)
      for (k in 1:p) {
        Xdoubled[ ,(2*k)] <- Xnpmat[ ,k]-1
        Xdoubled[ ,(2*k-1)] <- Jk[k] - Xnpmat[ ,k]
        colnames(Xdoubled)[2*k-1] <- colnames(Xind)[csjk[k]+1]
        colnames(Xdoubled)[2*k] <- colnames(Xind)[csjk[k]+Jk[k]]
        if (varnames[k]=="") { varnames[k] <- paste(colnames(Xdoubled)[2*k-1],colnames(Xdoubled)[2*k]) }
      }
      Xoutput <- as.matrix(Xdoubled)
    } else { # n by p or n by p counts
      if (is.null(colnames(Xnp))) {
        Xnp <- as.data.frame(Xnp)
        for (k in 1:p) {
          colnames(Xnp)[k] <- paste("v",k,sep="")
        }
      }
      if (output=="nbyp") {
        Xoutput <- as.data.frame(Xnp)
      } else { # n by p counts
        # horribly slow and poor way to combine rows, but rarely used and can't use CT/Burt if p>2
        n <- dim(Xnp)[[1]]
        nunique <- 0
        Xnpcount <- data.frame(rep(1,n),Xnp)
        colnames(Xnpcount)[[1]] <- "count"
        for (i in 1:n) {
          if (Xnpcount[i,1]==1) {
            nunique <- nunique+1
            Xnpcount[nunique,] <- Xnpcount[i,]
            if (i<n) { for (j in (i+1):n) {
              if (Xnpcount[j,1]==1) {
                if (all(Xnp[i,]==Xnp[j,])) {
                  Xnpcount[nunique,1] <- Xnpcount[nunique,1]+1
                  Xnpcount[j,1] <- 0
                }
              }
            } }
          }
        }
        Xoutput <- Xnpcount[1:nunique,]
      }
    }
  }

  list(result=Xoutput,varnames=varnames,catnames=catnames,Jk=Jk,p=p)

}


#' Converting a data matrix into an indicator matrix
#'
#' \code{getindicator} recodes a data matrix from one format (n objects by p variables,
#' counts for distinct combinations of p variables, contingency table) into an indicator matrix
#'
#' @param Xinput A data matrix, in the form of a data frame or similar
#'
#' @param input See \code{\link{convert}}
#'
#' @param Jk See \code{\link{convert}}
#'
#' @param maxcat See \code{\link{convert}}
#'
#' @param varandcat See \code{\link{convert}}
#'
#' @return
#' An indicator matrix, where a variable with J_k categories is represented by J_k columns and a
#' data point taking the i-th category has 1 in the i-th of these columns and a zero in the others
#'
#' @examples
#' nishindicator <- getindicator(NishData)
#'
#' @family conversion functions
#'
#' @export
getindicator <- function(Xinput,input="nbyp",Jk=NULL,maxcat=NULL,varandcat=TRUE) {

  Xindlist <- convert(Xinput,input=input,output="indicator",Jk=Jk,maxcat=maxcat,varandcat=varandcat)
  Xind <- Xindlist$result

  Xind

}

#' Converting a data matrix into a doubled matrix
#'
#' \code{getdoubled} recodes a data matrix from one format (n objects by p variables,
#' counts for distinct combinations of p variables, contingency table) into a doubled matrix
#'
#' @param Xinput A data matrix, in the form of a data frame or similar,
#' all variables must be ordered categorical with numerical categories
#'
#' @param input See \code{\link{convert}}
#'
#' @param Jk See \code{\link{convert}}
#'
#' @param maxcat See \code{\link{convert}}
#'
#' @return
#' A doubled matrix, where each variable is represented by 2 columns, and a data point
#'   taking the i-th category for a variable with J_k categories is given the values
#'   J_k-i in the first (low) column and i-1 in the second (high) column
#'
#' @examples
#' nishdoubled <- getdoubled(NishData)
#'
#' @family conversion functions
#'
#' @export
getdoubled <- function(Xinput,input="nbyp",Jk=NULL,maxcat=NULL) {

  Xdlist <- convert(Xinput,input=input,output="doubled",Jk=Jk,maxcat=maxcat)
  Xd <- Xdlist$result

  Xd

}

#' Converting a data matrix into a Burt matrix
#'
#' \code{getBurt} recodes a data matrix from one format (n objects by p variables,
#' counts for distinct combinations of p variables, contingency table) into a Burt matrix,
#' or extracts a subset of a Burt matrix for selected variables
#'
#' @param Xinput A data matrix, in the form of a data frame or similar
#'
#' @param input The format of the input matrix:
#' \describe{
#'   \item{"nbyp"}{An n individuals/objects/data points by p categorical variables matrix,
#'   where each row is a different data point and each column contains the category
#'   for that data point on that variable, where these categories can be numbers, strings or factors}
#'   \item{"nbypcounts"}{Similar to the above, but each row represents all of the data points taking
#'   the same combination of categories, and the first column contains the count for this combination
#'   (hence the name used here is a bit of a misnomer, but it emphasises the similarities to an n by p)}
#'   \item{"indicator"}{An indicator matrix, similar to the n by p matrix except that a variable with J_k categories
#'   is represented by J_k columns and a data point taking the i-th category has 1 in the i-th
#'   of these columns and a zero in the others}
#'   \item{"Burt"}{A Burt matrix, symmetrical and block-diagonal with each block being a
#'   contingency table for a pair of variables}
#'  }
#'
#' @param Jk See \code{\link{convert}}
#'
#' @param maxcat See \code{\link{convert}}
#'
#' @param varandcat See \code{\link{convert}}
#'
#' @param vars A list of the variable numbers to be used in the Burt matrix, if only a subset is wanted.\cr
#' If not all variables are wanted and the input is not n by p then Jk must be specified.
#'
#' @return
#' A Burt matrix, symmetrical and block-diagonal with each block being a contingency table for a pair of variables
#'
#' @examples
#' nishburt <- getBurt(NishData)
#' nishburtvars1to3 <- getBurt(NishData,vars=1:3)
#' nishburtvars2and4 <- getBurt(nishburt,input="Burt",Jk=rep(3,4),vars=c(2,4))
#'
#' @family conversion functions
#'
#' @export
getBurt <- function(Xinput,input="nbyp",Jk=NULL,maxcat=NULL,varandcat=TRUE,vars=NULL) {

  if ( (!(input=="Burt")) & (dim(Xinput)[1]==dim(Xinput)[2]) & (!(is.null(vars))) ) {
    input <- "Burt"
    cat(paste("WARNING: assuming that this is a Burt matrix\n\n"))
  }
  if ( any(input==c("Burt","indicator")) & (!(is.null(vars))) & (is.null(Jk)) ) stop(paste("Must specify full Jk\n\n"))

  if (input=="Burt") {
    Xburt <- Xinput
  } else {
    if (input=="indicator") {
      Xind <- Xinput
    } else {
      Xindlist <- convert(Xinput,input=input,output="indicator",Jk=Jk,maxcat=maxcat,varandcat=varandcat)
      Xind <- Xindlist$result
      Jk <- Xindlist$Jk
    }
    Xburt <- t(Xind)%*%Xind
  }

  if ( !(is.null(vars)) ) {
    numvars <- length(vars)
    csjk <- c(0,cumsum(Jk))
    csjksub <- c(0,cumsum(Jk[vars]))
    subsize <- sum(Jk[vars])
    subset <- numeric(length=subsize)
    for (i in 1:numvars) {
      subset[ (csjksub[i]+1):(csjksub[i+1]) ] <- (csjk[vars[i]]+1):(csjk[vars[i]+1])
    }
    Xburt <- Xburt[subset,subset]
  }

  Xburt

}

#' Converting a data matrix into a contingency table
#'
#' \code{getCT} recodes a data matrix from one format (n objects by p variables,
#' counts for distinct combinations of p variables, indicator matrix or Burt matrix) into a contingency table,
#' for cases where table() doesn't work
#'
#' @param Xinput A data matrix, in the form of a data frame or similar
#'
#' @param input See \code{\link{getBurt}}
#'
#' @param Jk See \code{\link{convert}}
#'
#' @param maxcat See \code{\link{convert}}
#'
#' @param varandcat See \code{\link{convert}}
#'
#' @param vars A list of the variable numbers to be used in the contingency table when there are more than 2. \cr
#' If not all variables are wanted and the input is not n by p then Jk must be specified.
#'
#' @return
#' A contingency table, giving counts for the two cross-classified variables
#'
#' @examples
#' nishCTvars23 <- getCT(NishData,Jk=rep(3,4),vars=2:3)
#'
#' @family conversion functions
#'
#' @export
getCT <- function(Xinput,input="nbyp",Jk=NULL,maxcat=NULL,varandcat=TRUE,vars=NULL) {

  if ( !(is.null(vars)) & !(length(vars)==2) ) stop(paste("Only two variables in a CT\n\n"))

  Xburt <- getBurt(Xinput,input=input,Jk=Jk,maxcat=maxcat,vars=vars,varandcat=varandcat)

  k <- dim(Xburt)[[1]]
  diff <- 10
  i <- 1
  while (diff>0) {
    i <- i+1
    diff <- sum(Xburt[i,(i+1):k])-sum(Xburt[i,1:(i-1)])
  }

  Xct <- Xburt[1:(i-1),i:k]

  Xct

}

