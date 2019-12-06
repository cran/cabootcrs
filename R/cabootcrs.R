#'
#' Calculate category point variances using bootstrapping
#'
#' \code{cabootcrs} performs simple or multiple correspondence analysis
#' and uses bootstrap resampling to
#' construct confidence ellipses for each appropriate category point,
#' printing and plotting the results;
#' for help on the package see \code{\link{cabootcrs-package}}.
#'
#' This routine performs all of the usual Correspondence Analysis calculations while also using bootstrapping
#' to estimate the variance of the difference between the sample and population point when both are projected
#' onto the sample axes in principal coordinates.
#' This is done for each row and column category on each dimension of the solution,
#' allowing for sampling variation in both the points and the axes.
#'
#' It hence constructs confidence ellipses for each category point, plots the results
#' by a call to \code{\link{plotca}} and prints the usual Correspondence Analysis summary
#' output and the calculated standard deviations through a call to \code{\link{summaryca}}.
#' Use \code{\link{printca}} for more detailed numerical results.
#'
#' For further examples and help on the package as a whole see \code{\link{cabootcrs-package}}.
#'
#' \strong{(1) Corrections for Burt diagonal}
#'
#' It is well-known that in multiple CA (MCA) the results are distorted by the diagonal elements of
#' the Burt matrix. As well as the standard methods to correct for this, here we propose
#' and implement a new method to correct for this when bootstrapping.
#' If bootstrapping is applied in a naive way then, even when the standard corrections are used,
#' the estimated variances will be much too small because diagonal elements of the standardised Burt matrix are
#' the same in every bootstrap replicate, thus underestimating the true variation in the data.
#'
#' All bootstrapping is performed on the indicator matrix (or equivalently the n by p matrix)
#' and the resampled Burt matrix is then constructed from the resampled indicator matrix in the usual way.
#'
#' Included here are the usual corrections to the
#' inertias (mcaadjustinertias=TRUE, the default)
#' and the coordinates (mcaadjustcoords=TRUE, the default).
#' In addition you can choose to use, as the total inertia, either
#' the sum of these adjusted inertias (mcauseadjustinertiasum=TRUE) as proposed by Benzecri or
#' the average of the off-diagonal inertias (mcauseadjustinertiasum=FALSE, the default)
#' as proposed by Greenacre.
#' You can adjust (multiply by p) the Contribution figures in MCA so that they sum to p over all variables,
#' i.e. an average of 1 for each variable as in SCA (mcaadjustmassctr=TRUE, the default),
#' rather than a total of 1 over all variables, as usually in MCA (mcaadjustmassctr=FALSE).
#' This also adjusts (multiplies by p) the point masses so that they sum to
#' 1 for each variable, rather than over all variables.
#'
#' The fundamental problem with MCA is that in a Burt matrix each diagonal element is the
#' value of a variable category cross-classified with itself, so it is always equal
#' to the number of times that category appears.
#' Hence if a category appears k times then the row (or column) in the Burt matrix
#' consists of p blocks each of which sum to k, so its row (and column) sum is kp.
#'
#' Therefore when the row (or column) profile matrix
#' is calculated the diagonal elements of the Burt matrix are always all 1/p while
#' the elements for the categories in the offdiagonal blocks sum to 1/p in each block.
#' Hence when the projected difference between the sample and resample row (or column)
#' profile matrices is calculated this is artificially small because the
#' diagonals of the two matrices are always the same, no matter how different
#' the off-diagonal elements are.
#'
#' The new method to correct for the diagonal elements of the Burt matrix when
#' calculating variances therefore works by re-expressing the coordinates
#' purely in terms of the interesting and variable off-diagonal elements,
#' excluding the uninteresting and constant diagonal elements.
#'
#' First calculate the Burt principal coordinates (PC), but with the
#' diagonal elements of the profile matrix ignored (or set to zero).
#' It is easy to verify that the usual Burt principal coordinates can be re-expressed,
#' using the singular values (SV), as
#'
#' Burt PC = ( Burt SV / (Burt SV - (1/p)) ) x Burt PC without diagonal element
#'
#' Hence in the bootstrapping the sample and resample points are re-expressed in this way
#' and their differences when projected onto the bootstrap axes are calculated as usual.
#'
#' Similarly the adjusted principal coordinates are calculated as
#'
#' adj Burt PC = (p/(p-1)) x ( (Burt SV - (1/p)) / Burt SV ) x Burt PC
#'
#' So, when using the usual adjusted coordinates (and adjusted inertias),
#' both of the above will be used, hence ending up with just a correction of (p/(p-1)).
#' The unadjusted coordinates can be used, but this is not recommended.
#'
#' One consequence of this correction is that when mcaadjustinertias=TRUE, mcaadjustcoords=TRUE,
#' mcauseadjustinertiasum=TRUE, mcaadjustmassctr=TRUE and mcasupplementary="offdiag"
#' then when p=2 the bootstrap
#' variances for MCA are almost the same as those for SCA, while all other results
#' for MCA are the same as those for SCA. The package author regards this as a good thing,
#' making MCA more of a proper generalisation of MCA, but recognises that some people
#' regard MCA as a fundamentally different method to SCA, linked only by the common algebra.
#'
#' Note that if this adjustment is not made then in the p=2 case it is easy to see that the
#' projected differences are half those in the SCA case and the standard deviations
#' are a quarter the size.
#'
#' The new method will be written up for publication once this update to the package is finished.
#'
#' \strong{(2) Experimental method for bootstrapping indicator matrices}
#'
#' A highly experimental method is included for bootstrapping with an indicator or doubled matrix
#' in the case of ordered categorical (e.g. Likert scale) data.
#' This has not been studied or optimised extensively, and is currently very slow, so use is very
#' much at the user's discretion and at user's risk. To try it, choose:\cr\cr
#' mcatype="indicator" or mcatype="doubled" with nboots>0\cr\cr
#' If CRs are required for the individual points then also choose:\cr\cr
#' mcaindividualboot=TRUE
#'
#' The bootstrap methodology used here relies on the comparison of bootstrap to sample points when
#' both are projected onto bootstrap axes. In SCA this is fine because when looking at column points
#' the axes are given by the rows and vice versa, and row i and column j each represent the same
#' category in all bootstrap replicates. Similarly in MCA with a Burt matrix when looking at column
#' points the axes are given by the rows, which again each represent the same category in all bootstrap replicates.
#'
#' However, with an indicator or doubled matrix, row i of the matrix does not represent the same
#' individual in each replicate, it just represents the i-th individual drawn in the resampling
#' for that particular replicate.
#' In order for this type of bootstrapping to work with an indicator or doubled matrix we would
#' need a resampling method whereby the i-th row represents the i-th individual in all bootstrap replicates.
#' The resampled row would need to represent the answers of the same individual "on another day".
#' This might make sense with questionnaire data where an individual's answers have uncertainty
#' attached, in that if asked the same questions on multiple occasions they would give different answers
#' due to random variation rather than temporal change. If a believable model for the sampling,
#' and hence the resampling, could be derived (CUB models perhaps) then this could lead to CRs
#' for an individual point, representing the uncertainty in what that person actually thinks.
#'
#' The method here uses the same idea of treating the sample row as
#' representing an individual, and bootstrap replicates of that row as representing the variability
#' in how that individual might have answered the questionnaire (or similar).
#' However, instead of an explicit model to represent this variability, it is generated
#' by the data themselves. The bootstrap replicate indicator (or doubled) matrix is generated as usual,
#' by either non-parametric (multinomial) or balanced resampling, and then its rows are "matched"
#' to the rows of the sample indicator (or doubled) matrix. The rows of the resampled matrix
#' are reordered so that the rows of the resampled matrix are, overall, as similar as possible
#' to the same rows of the sample matrix. Hence the resampled rows can reasonably be viewed
#' as representing the same individual in each bootstrap replicate, and hence variances for
#' the column points (categories) and row points (individuals) can be produced.
#'
#' The matching again uses the Hungarian algorithm from lpSolve. This only makes sense if
#' all variables are ordered categorical. Applying this to (ordered) categorical data results
#' in a very large number of ties, so the mcalikertnoise parameter defines the standard deviation
#' of white noise added to each of the sample and resample category numbers for the purpose
#' of the matching (only).
#'
#' Note that this is very slow for all but small data sets, and if all of the CRs for individuals
#' are shown then the plot is impossibly busy. Hence it is recommended that this experimental
#' method only be used for fairly small data sets where CRs are wanted for only a few example individuals.
#' A better approach might be to average the CRs of all of the individuals who give the
#' same results in the sample, but this is not implemented yet.
#'
#' Note that supplementary row points in indicator matrix MCA are usually regarded as different people answering
#' the same questions, whereas in this case for CRs to make any sense we need to regard them
#' as the same people answering the same questions on different days.
#'
#' \strong{(3) Critical values}
#'
#' Bootstrap critical values are calculated by re-using the bootstrap replicates used to calculate
#' the variances, with a critical value calculated for each ellipse.
#' The projected differences between bootstrap and sample points are ordered and the appropriate
#' percentile value picked. These are usually slightly larger than the \eqn{\chi^2} critical values.
#' Only 90\%, 95\% (default) and 99\% critical values are calculated.
#'
#' Alternatively use \eqn{\chi^2} critical values, usually with df=2, but with df=1 if only 2 non-zero cells in row/col.
#'
#' The experimental method to construct ellipses for individuals in MCA always uses bootstrap critical values
#'
#' @param xobject Name of data object (data frame or similar class that can be coerced to data frame).\cr
#' For simple CA (SCA) the default is contingency table format,
#' recommended that rows >= columns as matrix will be transposed if columns > rows. \cr
#' For multiple CA (MCA) the default is an n individuals by p variables matrix of category values (numbers or text).
#'
#' @param datafile Name of a text file (in " ") containing the data, same defaults as xobject,
#' ignored if xobject is non-null
#'
#' @param nboots Number of boostrap replicate matrices used, default and recommended minimum is 999,
#' but 9999 is recommended if machine and data set size allows;
#' the calculated variances will sometimes differ
#' around the third decimal place, but the pictures should look the same. \cr
#' If nboots=0 then correspondence analysis is performed as usual with no variances calculated
#'
#' @param resampledistn
#' Poisson resampling is the default for SCA,\cr
#' nonparametric is the default for MCA (Poisson and multinomial will both default to nonparametric)
#' \describe{
#' \item{"Poisson"}{resampled matrices constructed using Poisson resampling on each cell separately (only SCA)}
#' \item{"multinomial"}{resampled matrices constructed using multinomial resampling, treating the
#' cells as defining one or more multinomial distributions (only SCA)}
#' \item{"nonparametric"}{non-parametric resampling of the rows of the n individuals by p variables matrix, equivalent to multinomial (only MCA)}
#' \item{"balanced"}{multinomial resampling balanced so that each data point occurs equally often over the resamples (only MCA)}
#' \item{"myresample"}{resampling algorithm is contained in a file called myresample.R}
#' }
#'
#' @param multinomialtype Only relevant for multinomial sampling in SCA, otherwise ignored:
#' \describe{
#' \item{"whole"}{all cells define a single multinomial distribution}
#' \item{"rowsfixed"}{row sums fixed, each row defines a separate multinomial distribution}
#' \item{"columnsfixed"}{column sums fixed, each column defines a separate multinomial distribution}
#' }
#'
#' @param printdims Print full correspondence analysis coordinates, contributions, correlations etc for all output
#' dimensions up to and including this one
#'
#' @param lastaxis Calculate variances and covariances for all output axes (dimensions) up to this one
#' (or the number of dimensions in the solution if smaller). \cr
#' Recommended maximum is maxrearrange-1 as variances for those above this axis may be inaccurate
#'
#' @param maxrearrange The maximum number of axes to consider when rearranging
#'
#' @param rearrangemethod The method used to rearrange the axes:
#' \describe{
#' \item{"lpassign"}{The Hungarian algorithm in the lpSolve package}
#' \item{anything else}{The embarrassingly slow direct comparison method used in version 1.0 - don't use it}
#' }
#' Option is only included in case something weird goes wrong with lpSolve.
#'
#' @param usebootcrits To be passed to the plot routine, see \code{\link{plotca}} for details
#'
#' @param groupings To be passed to the plot routine, see \code{\link{plotca}} for details
#'
#' @param grouplabels To be passed to the plot routine, see \code{\link{plotca}} for details
#'
#' @param plotsymbolscolours To be passed to the plot routine, see \code{\link{plotca}} for details
#'
#' @param othersmonochrome To be passed to the plot routine, see \code{\link{plotca}} for details
#'
#' @param crpercent To be passed to the plot routine, see \code{\link{plotca}} for details
#'
#' @param catype Type of correspondence analysis:
#' \describe{
#' \item{"sca"}{Simple (classical) correspondence analysis of a contingency table}
#' \item{"mca"}{Multiple correspondence analysis of a Burt, indicator or doubled matrix}
#' }
#'
#' @param scainput Format of input data, only applies for SCA:
#' \describe{
#' \item{"CT"}{Contingency table of counts, preferably with rows >= columns}
#' \item{"nbyp"}{An n individuals/objects/data points by p=2 categorical variables matrix,
#'   where each row is a different data point and each column contains the category
#'   for that data point on that variable, where these categories can be numbers, strings or factors}
#' \item{"nbypcounts"}{Similar to the above, but each row represents all of the data points taking
#'   the same combination of categories, and the first column contains the count for this combination
#'   (hence the name used here is a bit of a misnomer, but it emphasises the similarities to an n by p=2)}
#' }
#'
#' @param mcainput Format of input data, only applies for MCA:
#' \describe{
#' \item{"nbyp"}{An n individuals/objects/data points by p categorical variables matrix,
#'   where each row is a different data point and each column contains the category
#'   for that data point on that variable, where these categories can be numbers, strings or factors}
#' \item{"nbypcounts"}{Similar to the above, but each row represents all of the data points taking
#'   the same combination of categories, and the now first column contains the count for this combination,
#'   so that the input matrix is n by p+1
#'   (hence the name used here is a bit of a misnomer, but it emphasises the similarities to an n by p)}
#' \item{"indicator"}{An n by sum-of-distict-variable-categories (i.e. sum of elements of Jk) indicator matrix}
#' }
#'
#' @param mcatype Format of data matrix analysed, only applies for MCA:
#' \describe{
#' \item{"Burt"}{Analyse the Burt matrix. Output will be given for the column (variable category) points but
#'   not for the (identical) row points. \cr
#'   NOTE: it is highly recommended that this version is used}
#' \item{"indicator"}{Analyse the indicator matrix. Output will be given for the column (variable category)
#'   points but not usually for the row (individual) points. \cr
#'   NOTE: the bootstrap method used in this case is highly experimental and very slow, see Details section part (2)}
#' \item{"doubled"}{Analyse the doubled matrix. Output will be given for the column
#'   points (two points, high and low, for each variable) but not usually for the row (individual) points. \cr
#'   NOTE: the bootstrap method used in this case is highly experimental and very slow, see Details section part (2)}
#' }
#'
#' @param mcavariant Currently must be "mca", placeholder for future updates
#'
#' @param mcasupplementary How the sample points are projected as supplementary points onto the bootstrap axes
#' when calculating the variances in MCA of a Burt matrix, see Details section for full explanation
#' \describe{
#' \item{"offdiag"}{Only the off-diagonal parts of the Burt matrix are used in the projection}
#' \item{"all"}{Projection is calculated in the usual way}
#' }
#' If "offdiag" then when p=2 the variances will be very similar to those from SCA. \cr\cr
#' If "all" then the fact that the diagonal elements of the Burt matrix are by definition
#' the same for both the sample and bootstrap matrices
#' means that the projected differences between sample and population points,
#' and hence the variances, will be artificially small. \cr\cr
#' NOTE: if mcaadjustinertias is FALSE then mcasupplementary will be set to "all",
#' because adjusting the coordinates in the calculation of the variances but not adjusting
#' the inertias makes no sense
#'
#' @param mcaadjustinertias Whether to adjust inertias to allow for the meaningless inertia terms
#'   induced by the diagonal of the Burt matrix in MCA:
#' \describe{
#' \item{TRUE}{Analysing Burt matrix: subtract 1/p from all singular values, then positive singular values are
#'   multiplied by p/(p-1) while negative ones are ignored \cr
#'   Analysing indicator matrix: same applies but to the squared singular values }
#' \item{FALSE}{No adjustment}
#' }
#' If TRUE then when p=2 the inertias will agree with those from SCA. \cr\cr
#' NOTE: inertias are the square (Burt) or fourth power (indicator) of the (adjusted) singular values.\cr\cr
#' NOTE: if mcaadjustinertias is FALSE then mcaadjustcoords will also be set to FALSE and mcasupplementary
#' set to "all", as adjusting the coordinates but not the inertias makes no sense
#'
#' @param mcauseadjustinertiasum How to define the total inertia in MCA,
#'   whether to just use the sum of the adjusted inertias:
#' \describe{
#' \item{TRUE}{The inertias are expressed as a percentage of the sum of the adjusted inertias (Benzecri)}
#' \item{FALSE}{The inertias are expressed as a percentage of the average of off-diagonal inertias (Greenacre),
#'   note that this will be incorrect if Jk includes categories that are not observed in the data }
#' }
#' If TRUE then when p=2 the inertias will agree with those from SCA \cr
#' If TRUE then the percentage inertias will sum to 100\% \cr
#' If FALSE then the percentage inertias will usually sum to less than 100\%
#'
#' @param mcaadjustcoords Whether to adjust the principal coordinates in MCA using the adjusted
#' inertias above, as in Greenacre and Blasius, p68:
#' \describe{
#' \item{TRUE}{Adjust coordinates, but only for column points}
#' \item{FALSE}{No adjustment}
#' }
#' If TRUE then when p=2 the coordinates will agree with those from SCA. \cr\cr
#' NOTE: if mcaadjustinertias is FALSE then mcaadjustcoords will also be set to FALSE, as adjusting the coordinates
#' but not the inertias makes no sense
#'
#' @param mcaadjustmassctr Whether to adjust the point masses and column contributions in MCA so that the masses and contributions are
#'   with respect to each variable (as in SCA) rather than with respect to all variables together:
#' \describe{
#' \item{TRUE}{Multiply point masses and contributions by p so that they sum to p over all variables}
#' \item{FALSE}{No adjustment}
#' }
#' If TRUE then when p=2 the CTR will agree with those from SCA
#'
#' @param mcaoneploteach Parameter passed to \code{\link{plotca}} for MCA.\cr\cr
#' A flag saying whether to produce one plot for each variable,
#' where confidence ellipses are shown for that variable but not others:
#' \describe{
#' \item{TRUE}{p plots are produced, each showing confidence regions for the category points
#' for just one variable}
#' \item{FALSE}{only one plot produced, with confidence regions shown for each category
#' point of each variable, which could be very "busy"}
#' }
#'
#' @param mcashowindividuals Parameter passed to \code{\link{plotca}} for MCA.\cr\cr
#' For MCA on an indicator matrix only, a flag saying whether to plot the individuals:
#' \describe{
#' \item{TRUE}{plot the individuals, which could be very "busy". \cr
#' NOTE: if mcaindividualboot=TRUE this also plots the CRs constructed using the experimental method,
#' see Details section part (2)}
#' \item{FALSE}{don't plot the individuals}
#' }
#'
#' @param mcavariablecolours Parameter passed to \code{\link{plotca}} for MCA:
#' \describe{
#' \item{TRUE}{In MCA each variable has its own colour, and all category
#' points and ellipses for that variable have the same colour}
#' \item{FALSE}{Colours chosen in the default way}
#' }
#'
#' @param mcacategorycolours Parameter passed to \code{\link{plotca}} for MCA:
#' \describe{
#' \item{TRUE}{In MCA each category number has its own colour,
#' and all points and ellipses for that category number have the same colour,
#' for all variables (intended for Likert type data so that all category 1 points are
#' the same colour etc)}
#' \item{FALSE}{Colours chosen in the default way}
#' }
#'
#' @param Jk The number of classes for each variable in MCA, as a list or vector, which only needs
#' specifying when inputting an indicator matrix, as in other cases it can be derived from the input matrix
#'
#' @param varandcat Flag for how to construct variable category names:
#' \describe{
#' \item{TRUE}{names are varname:catname, to be used if many variables have the same categories, e.g. Likert}
#' \item{FALSE}{names are just category names, to be used if variables all have distinct categories}
#' }
#'
#' @param likertarrows Parameter passed to \code{\link{plotca}} for MCA.\cr\cr
#' If TRUE then, for likert-type ordered categorical data,
#' draw arrows connecting the category points for each variable,
#' with the arrow drawn from a category point to the next higher category point
#'
#' @param mcastoreindicator If TRUE then store the indicator matrix created for MCA
#'
#' @param mcaindividualboot If TRUE then use the experimental method to bootstrap an indicator or doubled matrix,
#' see Details section part (2) for full explanation
#'
#' @param mcalikertnoise The "noise" value to use in the experimental method (above)
#' to bootstrap an indicator or doubled matrix,
#' see Details section part (2) for full explanation
#'
#' @param poissonzeronewmean Experimental method for SCA to deal with contingency tables where zero cells
#' could have been non-zero, i.e. they are not structural zeros.\cr
#' Only relevant for Poisson sampling in SCA, otherwise ignored:\cr
#' 0 : no effect, method as described in paper \cr
#' 1 : cells which are zero in the data are instead resampled from a
#' Poisson distribution with this mean, which should be very small (say 0.1);
#' this is for situations where rare cases did not occur but could have done,
#' so that it might be appropriate for zero cells in the sample to be
#' occasionally non-zero in resamples
#'
#' @param newzeroreset Experimental method for SCA to deal with sparse contingency tables.\cr
#' Only relevant for SCA, otherwise ignored:\cr
#' 0 : no effect, method as described in paper\cr
#' 1 : if a cell value is non-zero in the sample but zero in the resample
#' then it is reset to 1 in the resample, so that the sparsity structure
#' of the sample is maintained in the resample. This can be useful with
#' sparse data sets and, in effect, conditions on the sample sparsity structure
#'
#' @param bootstdcoords If TRUE then produce bootstrap variances for points in standard coordinates
#' instead of principal coordinates\cr
#' Note: intended only for experiments with the methodology
#'
#' @param reflectonly If TRUE then just allow for axis reflections and not axis reorderings\cr
#' Note: intended only for experiments with the methodology
#'
#' @param showresults If TRUE then output the results using \code{\link{summaryca}}
#' and \code{\link{plotca}}, otherwise output suppressed
#'
#' @param eps Any value less than this is treated as zero in some calculations and comparisons
#'
#' @return An object of class \code{\linkS4class{cabootcrsresults}}
#'
#' @examples
#' # Simple CA (SCA) of a 5 by 4 contingency table, using all SCA defaults:
#' # 999 bootstraps, Poisson resampling, variances for up to first four axes,
#' # usual output for up to the first 4 axes,
#' # one biplot with CRs for rows in principal coordinates and another with
#' # CRs for columns in principal coordinates
#'
#' bd <- cabootcrs(DreamData)
#'
#' \dontrun{
#'
#' # Multiple CA (MCA) of 3 categorical variables with all defaults:
#' # non-parametric resampling, Burt matrix analysed,
#' # each variable has one plot with it in colour with CRs shown, other variables in monochrome.
#' # Same data set but now as 223 by 3 matrix, with random 3rd column (with 3 categories) added.
#'
#' bd3 <- cabootcrs(DreamData223by3, catype="mca")
#'
#' # Comparison of SCA to MCA with p=2, by converting contingency table to 223 by 2 matrix.
#' # Note that the coordinates and inertias etc are the same while the standard deviations
#' # and hence the ellipses are very similar but not identical.
#'
#' bd <- cabootcrs(DreamData)
#' DreamData223by2 <- convert(DreamData,input="CT",output="nbyp")$result
#' bdmca <- cabootcrs(DreamData223by2, catype="mca", varandcat=FALSE)
#'
#' # Not adjusting inertias, which means that coordinates will also not be adjusted and
#' # the bootstrapping will use the Burt diagonal.
#' # Note how the coordinates are larger but the inertias and ellipses are smaller.
#'
#' bdmcaunadj <- cabootcrs(DreamData223by2, catype="mca", varandcat=FALSE, mcaadjustinertias=FALSE)
#'
#' # Applying the standard adjustments to inertias and coordinates, but with
#' # the bootstrapping still using the Burt diagonal.
#' # Note how inertias and coordinates are now the same as SCA, but ellipses are smaller.
#'
#' bdmcaadjbutall <- cabootcrs(DreamData223by2, catype="mca", varandcat=FALSE, mcasupplementary="all")
#'
#'
#' # Effect of sample size in SCA:
#'
#' bdx4 <- cabootcrs(4*DreamData)
#' bdx9 <- cabootcrs(9*DreamData)
#'
#' ba <- cabootcrs(AttachmentData)
#'
#' bs <- cabootcrs(SuicideData)
#'
#' bas <- cabootcrs(AsbestosData)
#'
#'
#' # Options for SCA:
#'
#' # SCA with multinomial resampling, with the matrix treated as a single multinomial distribution
#'
#' bdm <- cabootcrs(DreamData, resampledistn="multinomial")
#'
#' # Fix the row sums, i.e. keep sum of age group constant
#'
#' bdmrf <- cabootcrs(DreamData, resampledistn="multinomial", multinomialtype="rowsfixed")
#'
#' # Use chi-squared critical values for the CRs
#'
#' bdchisq <- cabootcrs(DreamData, usebootcrits=FALSE)
#'
#' # Just perform correspondence analysis, without bootstrapping
#'
#' bdnb0 <- cabootcrs(DreamData, nboots=0)
#'
#'
#' # Effect of sample size in MCA:
#'
#' bn <- cabootcrs(NishData, catype="mca")
#'
#'
#' # Options for MCA
#'
#' # Using default settings the SCA and MCA standard results are the same when p=2,
#' # bootstrap standard deviations (multinomial/nonparametric) are similar but not identical
#'
#' bdsca <- cabootcrs(DreamData,resampledistn="multinomial")
#' bdmca <- cabootcrs(convert(DreamData,input="CT",output="nbyp")$result, catype="mca")
#'
#' # Row A can be labelled A rather than R:A
#' # because the three variables have all different category names
#'
#' bd3l <- cabootcrs(DreamData223by3, catype="mca", varandcat=FALSE)
#'
#' # Balanced resampling, each of the 223 rows occurs 999 times in the 999 resamples
#'
#' bd3b <- cabootcrs(DreamData223by3, catype="mca", resampledistn="balanced")
#'
#' # Do not adjust inertias, coordinates or contributions
#' # (if inertias are not adjusted then coordinates are also not adjusted)
#'
#' bd3unadj <- cabootcrs(DreamData223by3, catype="mca",
#'                       mcaadjustinertias=FALSE, mcaadjustmassctr=FALSE)
#' }
#'
#' @seealso \code{\link{cabootcrs-package}}, \code{\linkS4class{cabootcrsresults}},
#' \code{\link{plotca}}, \code{\link{printca}}, \code{\link{summaryca}},
#' \code{\link{covmat}}, \code{\link{allvarscovs}}
#'
#' @import lpSolve
#'
#' @import methods
#'
#' @import grDevices
#'
#' @import graphics
#'
#' @import stats
#'
#' @import utils
#'
#' @export
cabootcrs <- function(
  xobject=NULL, datafile=NULL,
  nboots=999, resampledistn="Poisson", multinomialtype="whole",
  printdims=4, lastaxis=4,
  maxrearrange=6, rearrangemethod="lpassign", usebootcrits=TRUE,
  groupings=NULL, grouplabels=NULL,
  plotsymbolscolours=c(19,"alldifferent",18,"alldifferent"),
  othersmonochrome="grey", crpercent=95,
  catype="sca", scainput="CT", mcainput="nbyp", mcatype="Burt",  mcavariant="mca",
  mcasupplementary="offdiag", mcaadjustinertias=TRUE, mcauseadjustinertiasum=FALSE,
  mcaadjustcoords=TRUE, mcaadjustmassctr=TRUE,
  mcaoneploteach=TRUE, mcashowindividuals=FALSE,
  mcavariablecolours=FALSE, mcacategorycolours=FALSE,
  Jk=NULL, varandcat=TRUE, likertarrows=FALSE,
  mcastoreindicator=TRUE, mcaindividualboot=FALSE, mcalikertnoise=0.1,
  poissonzeronewmean=0, newzeroreset=0, bootstdcoords=FALSE, reflectonly=FALSE,
  showresults=TRUE, eps=1e-15 ) {

if (!(is.null(xobject))) { if ( !(is.matrix(xobject)) & !(is.data.frame(xobject)) ) stop(paste("Input must be matrix or data frame\n\n")) }
if (nboots==0) {
  cat(paste("\n Standard Correspondence Analysis results only, no confidence regions\n\n"))
} else {
if (nboots<999) cat(paste("\n WARNING", nboots, "is too few bootstrap replicates for reliable results\n\n"))
if ( (catype=="sca") & !any(resampledistn==c("Poisson","multinomial","myresample")) ) stop(paste("Resampling must be Poisson or multinomial"))
if (catype=="mca") {
  # in mca multinomial is same as nonparametric
  if ( (any(resampledistn==c("Poisson","multinomial"))) ) { resampledistn <- "nonparametric" }
  if (mcaindividualboot) {
    if (!(resampledistn=="balanced")) {
      cat(paste("\n WARNING balanced resampling used \n\n"))
      resampledistn <- "balanced"
    }
  } else {
    if (!any(resampledistn==c("nonparametric","balanced","myresample"))) stop(paste("Resampling must be nonparametric or balanced \n\n"))
  }
} # mca
if (!any(multinomialtype==c("whole","rowsfixed","columnsfixed")))
    stop(paste("Multinomial resampling is either whole, rowsfixed or columnsfixed\n\n"))
if ( !(resampledistn=="multinomial") & any(multinomialtype==c("rowsfixed","columnsfixed")) )
    cat(paste("\n WARNING can only fix rows or columns with multinomial resampling\n\n"))
if (poissonzeronewmean<0) stop(paste("Poisson mean for zero entries must be non-negative\n\n"))
if (!any(newzeroreset==c(0,1))) stop(paste("Zeros in sample can only be set to zero or one in bootstrap\n\n"))
} # if bootstrapping
if (printdims<2) stop(paste("number of dims for output must be at least 2\n\n"))
if (lastaxis<2) stop(paste("last axis must be at least 2\n\n"))
if (!any(catype==c("sca","mca"))) stop(paste("Must be sca or mca"))
if (!any(scainput==c("nbyp","nbypcounts","CT"))) stop(paste("Must be CT, nbyp or nbypcounts"))
if (!any(mcatype==c("Burt","indicator","doubled"))) stop(paste("Must be Burt, indicator or doubled"))
if ((mcainput=="doubled")) stop(paste("For doubled analysis please supply data as indicator, nbyp or nbypcounts"))
if (!any(mcainput==c("nbyp","nbypcounts","indicator"))) stop(paste("Must be nbyp, nbypcounts or indicator, can't be Burt as this loses the\n multi-way associations used in the bootstrapping,\n where resampling is performed on the indicator matrix"))
if (!any(mcavariant==c("mca"))) stop(paste("Must be mca")) # eventually add omca
if (!any(mcasupplementary==c("offdiag","all"))) stop(paste("Must be all or offdiag"))
if (!is.null(Jk)) { if ( !(any( is(Jk,"numeric"), is(Jk,"integer") )) ) stop(paste("Must be rep(maximum category,number of variables) or similar")) }
if ((is.null(Jk))&(mcainput=="indicator")) stop(paste("Must specify Jk when input is indicator matrix"))
if ((mcaadjustinertias==FALSE)&(mcaadjustcoords==TRUE)) {
  mcaadjustcoords <- FALSE
  cat(paste("\n WARNING Not adjusting inertias so mcaadjustcoords set to FALSE\n"))
}
if ((mcaadjustinertias==FALSE)&(mcasupplementary=="offdiag")) {
  mcasupplementary <- "all"
  cat(paste("\n WARNING Not adjusting inertias so mcasupplementary set to all\n"))
}
if ((mcaadjustcoords==TRUE)&(mcasupplementary=="all")) {
  cat(paste("\n WARNING Using adjusted coordinates in the results but not correcting for the Burt diagonal in the bootstrapping is not recommended \n\n"))
}
if ((mcaadjustcoords==FALSE)&(mcasupplementary=="offdiag")) {
  cat(paste("\n WARNING Correcting for the Burt diagonal in the bootstrapping but not using adjusted coordinates in the usual results is not recommended \n\n"))
}
if (mcalikertnoise<0) stop(paste("Must be non-negative"))
if ((catype=="mca")&(any(mcatype==c("indicator","doubled")))&(mcavariant=="mca")&(nboots>1)) {
  if (mcaindividualboot) {
    cat(paste("\n WARNING bootstrap standard deviations calculated from indicator/doubled matrix \n are based on highly experimental methods and should not be relied upon\n"))
  } else {
    cat(paste("\n WARNING bootstrap standard deviations calculated from indicator/doubled matrix \n do not make sense because rows of sample and bootstrap matrices \n do not correspond to each other\n"))
  }
}

IndicatorMatrix <- matrix(0)

# READ DATA FILE OR DATA OBJECT
# add row and column names if needed, or if currently has default "1" and "V1" etc

if (catype=="sca") {

if (is.null(xobject)) { # data file

Xtable <- read.table(file = datafile)
if ((scainput=="CT")&(row.names(Xtable)[[1]]=="1")&(names(Xtable)[[1]]=="V1")) {
  for (i in 1:dim(Xtable)[1]) rownames(Xtable)[i] <- paste("r",i,sep="")
  for (i in 1:dim(Xtable)[2]) colnames(Xtable)[i] <- paste("c",i,sep="")
}

} else { # data object

# horrible bodge because if xobject was created with table() then as.data.frame
# will undo the tabulation for no good reason, and as.array() does not work
#if (class(xobject)=="table") {
#  class(xobject) <- "array"
#  cat("Note: input class has been changed from table to array so that\n as.data.frame will not undo the tabulation\n\n")
#}

Xtable <- as.data.frame.matrix(xobject) # seems to coerce to data frame without undoing table()
if (scainput=="CT") {
  if ((is.null(rownames(xobject)))|(row.names(Xtable)[[1]]=="1")) {
    for (i in 1:dim(Xtable)[1]) rownames(Xtable)[i] <- paste("r",i,sep="")
  }
  if ((is.null(colnames(xobject)))|(names(Xtable)[[1]]=="V1")) {
    for (i in 1:dim(Xtable)[2]) colnames(Xtable)[i] <- paste("c",i,sep="")
  }
}

} # input file or object

if (scainput=="CT") {
  X <- as.matrix(Xtable)
  p <- 2
  if (dim(X)[1]>=dim(X)[2])  {
    rowlabels <- rownames(Xtable)
    collabels <- colnames(Xtable)
    varnames <- c("rows","columns")
  } else {
    cat("Note: input data transposed so that rows > columns \n\n")
    X <- t(X)
    rowlabels <- colnames(Xtable)
    collabels <- rownames(Xtable)
    varnames <- c("rows","columns") # too confusing otherwise
  }
} else { # n by p
  Xlist <- convert(Xtable)
  Xind <- Xlist$result
  varnames <- Xlist$varnames
  cats <- Xlist$catnames
  Jk <- Xlist$Jk
  p <- Xlist$p
  if (p>2) stop(paste("More than 2 categorical variables, need to use mca not sca\n\n"))
  X <- t(Xind) %*% Xind
  if (Jk[1]>=Jk[2]) {
    rowlabels <- cats[1]
    collabels <- cats[2]
  } else {
    X <- t(X)
    rowlabels <- cats[2]
    collabels <- cats[1]
  }
}

} # sca

if (catype=="mca") {

if (is.null(xobject)) { # data file
  Xtable <- read.table(file = datafile)
} else { # data object
  Xtable <- as.data.frame(xobject)
}

if ((mcainput=="nbypcounts")&(names(Xtable)[[1]]=="V1")&(names(Xtable)[[2]]=="V2")) {
  names(Xtable)[[1]] <- "Counts"
  for (i in 2:dim(Xtable)[2]) colnames(Xtable)[i] <- paste("V",i-1,sep="")
}

if (any(mcainput==c("nbyp","nbypcounts"))) {
  if (mcainput=="nbypcounts") {
    Xnp <- convert(Xtable,input=mcainput,output="nbyp",varandcat=varandcat)$result
  }
  Xindlist <- convert(Xtable,input=mcainput,output="indicator",Jk=Jk,varandcat=varandcat)
  Xind <- Xindlist$result
  varnames <- Xindlist$varnames
  cats <- Xindlist$catnames
  Jk <- Xindlist$Jk
  p <- Xindlist$p
  if (mcatype=="Burt") {
    X <- t(Xind) %*% Xind
    rowlabels <- colnames(Xind)
    collabels <- colnames(Xind)
  } else {
    if (mcatype=="indicator") {
      X <- Xind
      rowlabels <- rownames(Xtable)
      collabels <- colnames(Xind)
    }
  }
}

if (mcainput=="indicator") {
  Xind <- as.matrix(Xtable)
  if (any(Xind>1)) stop(paste("Not an indicator matrix\n\n"))
  p <- length(Jk)
  varnames <- character(p)
  for (i in 1:p) varnames[i] <- paste("V",i,sep="")
  #cats <- vector("list", length=p)
  #for (k in 1:p) { cats[[k]] <- 1:Jk[k] }
  if (mcatype=="Burt") {
    X <- t(Xind) %*% Xind
    rowlabels <- colnames(Xtable)
    collabels <- colnames(Xtable)
  } else {
    if (mcatype=="indicator") {
      X <- Xind
      rowlabels <- rownames(Xtable)
      collabels <- colnames(Xtable)
    }
    if (mcaindividualboot) {
      Xnp <- convert(Xind,input="indicator",output="nbyp",Jk=Jk,varandcat=varandcat)$result
    }
  }
}

if (mcatype=="doubled") {
  X <- getdoubled(Xind,input="indicator",Jk=Jk)
  rowlabels <- rownames(Xtable)
  collabels <- colnames(X)
}

if ((mcastoreindicator==TRUE)) { IndicatorMatrix <- Xind }

  if (mcaindividualboot) {
    for (k in 1:p) {
      if (!(is.numeric(Xnp[,k]))) stop(paste("Only numerical categories make sense for bootstrapping individuals\n  If input is an indicator matrix then categories will be treated as numeric\n\n"))
    }
  }

} # mca

rows <- dim(X)[1]
cols <- dim(X)[2]
if (catype=="sca") {
  Jk = c(rows,cols)
  n <- sum(X)
}

S <- switch(catype, "sca"=sca(X), "mca"=sca(X,catype,mcatype,p) )

# most axes for which variances can be calculated
if ( (catype=="mca") & (mcatype=="Burt") & ( (mcasupplementary=="offdiag") | (mcaadjustinertias==TRUE) ) ) {
  axisvariances <- min(lastaxis,sum(S@mu>1/p))
} else {
  axisvariances <- min(lastaxis,rows-1,cols-1)
}

if ((lastaxis >= maxrearrange)&(cols >=maxrearrange+2))
  cat(paste("\n WARNING variances for the ", maxrearrange, "th axis and above may be unreliable\n\n"))
# if (lastaxis>axisvariances)
#   cat(paste("\n WARNING there are only", axisvariances, "axes in the solution\n\n"))

# if (mcavariant=="omca") { hdS <- ht(Xind,Jk) }

# zero rows/cols to be omitted from bootstrapping and summaries
# rows/cols with one non-zero element, will have variance zero
# rows/cols with two non-zero elements, will use chi^2 with df 1

zerorowS <- rowSums(X>0)==0
zerocolS <- colSums(X>0)==0
onerowS <- rowSums(X>0)==1
onecolS <- colSums(X>0)==1
tworowS <- rowSums(X>0)==2
twocolS <- colSums(X>0)==2


# BOOTSTRAPPING

# Initialise sums of squares matrices
#  (computationally poor and may cause problems if large,
#   but should be OK as values will tend to be close to zero)

RSBsum <- matrix(0,rows,axisvariances)
RSBsumsq <- matrix(0,rows,axisvariances)
RSBsumcp <- array(0,c(rows,axisvariances,axisvariances))
CSBsum <- matrix(0,cols,axisvariances)
CSBsumsq <- matrix(0,cols,axisvariances)
CSBsumcp <- array(0,c(cols,axisvariances,axisvariances))
rownB <- matrix(nboots,rows,axisvariances)
colnB <- matrix(nboots,cols,axisvariances)
#rownB <- rep(nboots,rows)
#colnB <- rep(nboots,cols)
RSBvar <- RSBsum
CSBvar <- CSBsum
RSBcov <- RSBsumcp
CSBcov <- CSBsumcp
sameaxisorder <- 0

# in fact cb will be changed for rowzeros/colzeros and/or when not all boot matrices full rank
# sorting for boot crit vals will give lots of zeros - start at first real value
alphas = c(0.9,0.95,0.99)
cb = (nboots+1) * alphas;
bootcritR <- array(dim=c(rows,axisvariances,axisvariances,length(alphas)))
bootcritC <- array(dim=c(cols,axisvariances,axisvariances,length(alphas)))


if (nboots>0) {

RSBB <- array(dim=c(rows,axisvariances,nboots))
CSBB <- array(dim=c(cols,axisvariances,nboots))
RowsBB <- array(0,c(rows,axisvariances,axisvariances,nboots))
ColsBB <- array(0,c(cols,axisvariances,axisvariances,nboots))

# Used when bootstrapping indicator matrix (equivalent to using n by p) in MCA
# note that bootstrapping for Burt is still done on indicator matrix

if (catype=="mca") { # bootstrap indicator matrix
  Xindrows <- dim(Xind)[1]
  if (resampledistn=="nonparametric") {
    bno <- matrix( sample( rep(1:Xindrows,nboots), replace=TRUE ), Xindrows, nboots)
  } else {
    if (resampledistn=="balanced") {
      bno <- matrix( sample( rep(1:Xindrows,nboots) ), Xindrows, nboots)
    }
  }
}

for (b in 1:nboots) {

if (catype=="sca") {

  if (resampledistn=="multinomial") {
    Xr <- vector("numeric",rows*cols)
    if (multinomialtype=="whole") { Xr <- rmultinom(1,n,X) }
    if (multinomialtype=="rowsfixed") { for (i in 1:rows) { Xr[ seq(i,(cols-1)*rows+i,by=rows) ] <- rmultinom(1,sum(X[i,]),X[i,]) } }
    if (multinomialtype=="columnsfixed") { for (i in 1:cols) { Xr[((i-1)*rows+1):(i*rows)] <- rmultinom(1,sum(X[,i]),X[,i]) } }
  } else {
    if (resampledistn=="Poisson") {
      Xr <- rpois(rows*cols,X+poissonzeronewmean*(X==0))
    } else {
      if (resampledistn=="myresample") { Xr <- myresamplefn(X) }
    }
  }

} else { # mca

  # resample rows, so row i in boot does not correspond to row i in sample
  if (any(mcatype==c("indicator","doubled"))) {
    Xrinitial <- X[bno[,b],] # standard version, balanced or not
    Xr <- Xrinitial
  } else { # Burt
    Xrinitial <- Xind[bno[,b],] # standard version, balanced or not
    Xr <- t(Xrinitial) %*% Xrinitial
  }
  if ( (any(mcatype==c("indicator","doubled")))&(mcaindividualboot) ) { # match boot rows to sample rows
    # i,j element is cost of source i to destination j, solution same
    # source i = i-th row in sample; destination j = j-th row in bootstrap
    Xrnp <- Xnp[bno[,b],]
    val <- matrix(0,Xindrows,Xindrows)
    if (mcalikertnoise>0) { # add noise/jitter to both to avoid tied matches
      Xrnpnoisy <- Xrnp + matrix(rnorm(rows*p,0,mcalikertnoise),nrow=rows,ncol=p)
      Xnpnoisy <- Xnp + matrix(rnorm(rows*p,0,mcalikertnoise),nrow=rows,ncol=p)
      for (i in 1:Xindrows) { for (j in 1:Xindrows) { val[i,j] <- sum((Xnpnoisy[i,]-Xrnpnoisy[j,])^2) } }
    } else {
      for (i in 1:Xindrows) { for (j in 1:Xindrows) { val[i,j] <- sum((Xnp[i,]-Xrnp[j,])^2) } }
    }
    # Want i-th row in reordered boot to correspond to i-th row in sample
    # 1 at (1,2) allocates 2nd boot row to correspond with 1st sample row
    assign <- lp.assign(val, direction="min")
    Xr <- (assign$solution) %*% Xrinitial
  } # likert reordering

} # mca

XB <- matrix(data=Xr,nrow=rows,ncol=cols)

if (newzeroreset==1) { XB <- (XB==0 & X>0)+XB } # set new zeros to 1

B <- switch(catype, "sca"=sca(XB), "mca"=sca(XB,catype,mcatype,p) )

# may have problem if X B more sparse than X S, as will then have different Jk and some zeros
# if (mcavariant=="omca") { hdB <- hd(Xrind,Jk) }

# check for "genuine" bootstrap axes so don't include meaningless ones
if ( (catype=="mca") & (mcatype=="Burt") & ( (mcasupplementary=="offdiag")|(mcaadjustinertias==TRUE) ) ) {
  bootaxes <- sum(B@mu>1/p)
} else {
  bootaxes <- axisvariances
}
fakeaxes <- rep(FALSE,axisvariances)
if (bootaxes < axisvariances) { fakeaxes[(bootaxes+1):axisvariances] <- TRUE }

# Check for zero rows/cols
zerorowB <- rowSums(XB>0)==0
zerocolB <- colSums(XB>0)==0

# reduce effective number of boots by 1 if zero row/col or meaningless axis
rownB <- rownB - 1 + as.matrix(1-zerorowB) %*% (1-fakeaxes)
colnB <- colnB - 1 + as.matrix(1-zerocolB) %*% (1-fakeaxes)

# Rearrange bootstrap axes if needed

if (rearrangemethod=="lpassign") {
  Re <- rearrange( S@Raxes, B@Raxes, S@Caxes, B@Caxes, B@realr, reflectonly, catype, mcatype, mcaindividualboot, maxrearrange)
} else {
  Re <- rearrange_old( S@Raxes, B@Raxes, S@Caxes, B@Caxes, B@realr)
}

RaxesBRe <- B@Raxes
CaxesBRe <- B@Caxes

# T is such that 1 at (2,1) means that 2nd boot axis is matched to 1st sample axis
#                1 at (1,2) means that 1st boot axis is matched to 2nd sample axis
# i.e. i,j-th element matches i-th boot axis with j-th sample axis
# Note that it is transposed from lp.assign solution
RaxesBRe[ ,1:Re$numre] <- RaxesBRe[ ,1:Re$numre] %*% Re$T
CaxesBRe[ ,1:Re$numre] <- CaxesBRe[ ,1:Re$numre] %*% Re$T

RSB <- ( B@Rprofile - S@Rprofile ) %*% B@Rweights %*% RaxesBRe[ ,1:axisvariances]
CSB <- ( B@Cprofile - S@Cprofile ) %*% B@Cweights %*% CaxesBRe[ ,1:axisvariances]

# Only use off-diagonal elements of Burt to calculate coordinates and hence supplementary points.
# Use singular values to get back to usual or adjusted MCA coordinates.
#  - using the difference between profiles already eliminates the diagonal element.
# currently only corrects for Burt, not Indicator

if ( (catype=="mca") & (mcatype=="Burt") & (mcasupplementary=="offdiag") ) {
  truebsvs <- B@mu[B@mu>1/p]
  if (mcaadjustcoords==TRUE) { # use offdiag only and adjust coords
    svcorrection <- (p/(p-1))*diag(1,max(S@realr,Re$numre))
  } else { # use offdiag only but usual MCA coords
    svcorrection <- diag(truebsvs/(truebsvs-1/p))
  }
  svcorrection[1:Re$numre,1:Re$numre] <- svcorrection[1:Re$numre,1:Re$numre] %*% Re$T
  # If B has fewer >1/p than sample then add extra zeros to fill size
  dimsvc <- dim(svcorrection)[1]
  if (dimsvc<axisvariances) {
    svc <- diag(0,axisvariances)
    svc[1:dimsvc,1:dimsvc] <- svcorrection
    svcorrection <- svc
  }
  # If meaningless axes then last RSB and CSB will be set to zero as they are also meaningless
  RSB <- RSB %*% svcorrection[1:axisvariances,1:axisvariances]
  CSB <- CSB %*% svcorrection[1:axisvariances,1:axisvariances]
}

# if ( (catype=="mca") & (mcatype=="nbyptoburt") & (mcaadjustcoords==TRUE) ) {
# adjust principal coordinates as in Greenacre and Blasius, p68, but only for "columns"
# (not needed here as adjustment done below)
# adjsvs <- (p/(p-1)) * (B@mu[B@mu>(1/p)]-(1/p))
# svcorrection <- diag(adjsvs/(B@mu[B@mu>(1/p)]))
# svcorrection[1:Re$numre,1:Re$numre] <- svcorrection[1:Re$numre,1:Re$numre] %*% Re$T
# CSB <- CSB %*% svcorrection[1:axisvariances,1:axisvariances]
# }

# Might also need to set to zero when Burt and mcaadjustinertias but not offdiag,
# but shouldn't do that in the first place
RSB <- RSB * (1-zerorowS) * (1-zerorowB)
CSB <- CSB * (1-zerocolS) * (1-zerocolB)
sameaxisorder <- sameaxisorder + Re$same

# Unlikely to want to bootstrap standard coordinates in practice, but may be useful in research

if (bootstdcoords==TRUE) {
dmum1 <- diag( 1/(B@mu + (B@mu==0)) * (1-(B@mu==0)) )
dmum1[1:Re$numre,1:Re$numre] <- dmum1[1:Re$numre,1:Re$numre] %*% Re$T
dmum1 <- dmum1[1:axisvariances,1:axisvariances]
RSB <- RSB %*% dmum1
CSB <- CSB %*% dmum1
}

# bootstrap-derived critical values, re-using bootstraps
RSBB[ , , b] <- RSB
CSBB[ , , b] <- CSB

RSBsum <- RSBsum + RSB
RSBsumsq <- RSBsumsq + RSB*RSB
CSBsum <- CSBsum + CSB
CSBsumsq <- CSBsumsq + CSB*CSB
if (axisvariances>1) {
for (a1 in 1:(axisvariances-1)) {
  for (a2 in (a1+1):axisvariances) {
    RSBsumcp[ ,a1,a2] <- RSBsumcp[ ,a1,a2] + RSB[ ,a1]*RSB[ ,a2]
    CSBsumcp[ ,a1,a2] <- CSBsumcp[ ,a1,a2] + CSB[ ,a1]*CSB[ ,a2]
} } }

} # boots

RSBmean <- ( 1/( rownB + (rownB==0) ) ) * (1-(rownB==0)) * RSBsum
CSBmean <- ( 1/( colnB + (colnB==0) ) ) * (1-(colnB==0)) * CSBsum

rbm1 <- ( 1/( rownB-1 + 2*(rownB<=1) ) ) * (1-(rownB<=1))
cbm1 <- ( 1/( colnB-1 + 2*(colnB<=1) ) ) * (1-(colnB<=1))

RSBvar <- rbm1 * ( RSBsumsq - rownB * RSBmean * RSBmean )
CSBvar <- cbm1 * ( CSBsumsq - colnB * CSBmean * CSBmean )
if (axisvariances>1) {
for (a1 in 1:(axisvariances-1)) {
  for (a2 in (a1+1):axisvariances) { # later axis gives effective sample size
  RSBcov[ ,a1,a2] <- rbm1[ ,a2] * ( RSBsumcp[ ,a1,a2] - rownB[ ,a2] * RSBmean[ ,a1] * RSBmean[ ,a2] )
  CSBcov[ ,a1,a2] <- cbm1[ ,a2] * ( CSBsumcp[ ,a1,a2] - colnB[ ,a2] * CSBmean[ ,a1] * CSBmean[ ,a2] )
} } }

# bootstrap-derived critical values, re-using bootstraps
# must correct cb when not all boots used due to rowzeros or not full rank
# start at first genuine value and then replace nboots with rownB and round
for (a1 in 1:(axisvariances-1)) {
  for (a2 in (a1+1):axisvariances) {

for (i in 1:rows) {
  detcovmat <- RSBvar[i,a1]*RSBvar[i,a2]-RSBcov[i,a1,a2]^2
  if (detcovmat > eps) {
    Sm1 <- (1/detcovmat)*matrix(c(RSBvar[i,a2],-RSBcov[i,a1,a2],-RSBcov[i,a1,a2],RSBvar[i,a1]),nrow=2)
    for (bb in 1:nboots) {
      RowsBB[i,a1,a2,bb] <- RSBB[i,c(a1,a2),bb] %*% Sm1 %*% as.matrix(RSBB[i,c(a1,a2),bb])
    }
  } else {
    for (bb in 1:nboots) { RowsBB[i,a1,a2,bb] <- 0 }
  }
}
for (i in 1:rows) {
  RowsBB[i,a1,a2,] <- sort(RowsBB[i,a1,a2,])
  cb = round( nboots - rownB[i,a2] + (rownB[i,a2]+1) * alphas )
  if (cb[3]>nboots) { cb <- cb-1 } # above gives nboots+1 if rownB=0
  bootcritR[i,a1,a2,] <- RowsBB[i,a1,a2,cb]
}

for (i in 1:cols) {
  detcovmat <- CSBvar[i,a1]*CSBvar[i,a2]-CSBcov[i,a1,a2]^2
  if (detcovmat > eps) {
    Sm1 <- (1/detcovmat)*matrix(c(CSBvar[i,a2],-CSBcov[i,a1,a2],-CSBcov[i,a1,a2],CSBvar[i,a1]),nrow=2)
    for (bb in 1:nboots) {
      ColsBB[i,a1,a2,bb] <- CSBB[i,c(a1,a2),bb] %*% Sm1 %*% as.matrix(CSBB[i,c(a1,a2),bb])
    }
  } else {
    for (bb in 1:nboots) { ColsBB[i,a1,a2,bb] <- 0 }
  }
}
for (i in 1:cols) {
  ColsBB[i,a1,a2,] <- sort(ColsBB[i,a1,a2,])
  cb = round( nboots - colnB[i,a2] + (colnB[i,a2]+1) * alphas )
  if (cb[3]>nboots) { cb <- cb-1 } # above gives nboots+1 if colnB=0
  bootcritC[i,a1,a2,] <- ColsBB[i,a1,a2,cb]
}

} } # a1, a2

} # checking boots>0


# OTHER CALCULATIONS

Fmat <- S@Rprofile %*% S@Rweights %*% S@Raxes
Gmat <- S@Cprofile %*% S@Cweights %*% S@Caxes

dmum1 <- diag( 1/(S@mu + (S@mu==0)) * (1-(S@mu==0)) )
Gbi <- Gmat %*% dmum1
Fbi <- Fmat %*% dmum1

# Calc inertia sum

# Calculate adjusted inertias if mcaadjustinertias is true (Greenacre and Blasius, p68),
# also adjust principal coordinates if mcaadjustcoords is true.
# Use sum of adjusted inertias if mcauseadjustinertiasum is true (Benzecri),
# otherwise use average of off-diagonal inertias (Greenacre)
#  - this will be wrong if Jk is used to include zero categories

if ( (catype=="mca") & (mcaadjustinertias==TRUE) & (!(mcatype=="doubled")) ) {
  genuinesvs <- S@mu[1:S@realr]
  if (mcatype=="Burt") {
    adjsvs <- (p/(p-1)) * (genuinesvs-(1/p))
  } else { # indicator
    adjsvs <- (p/(p-1)) * (genuinesvs^2-(1/p))
  }
  if (mcaadjustcoords==TRUE) {
    svcorrection <- diag(adjsvs/genuinesvs)
    Gmat[,1:S@realr] <- Gmat[,1:S@realr] %*% svcorrection
    Fmat[,1:S@realr] <- Fmat[,1:S@realr] %*% svcorrection
  }
  Fsq <- Fmat[,1:S@realr]^2
  Gsq <- Gmat[,1:S@realr]^2
  inertia <- adjsvs^2
  if (mcauseadjustinertiasum==TRUE) { # sum of adjusted inertias
    inertiasum <- sum(inertia)
  } else { # average of off-diagonal inertias
    if (mcatype=="Burt") {
      inertiasum <- (p/(p-1)) * ( sum(S@mu^2) - (cols-p)/p^2 )
    } else { # indicator
      inertiasum <- (p/(p-1)) * ( sum(S@mu^4) - (cols-p)/p^2 )
    }
  }
} else {
  Fsq <- Fmat^2
  Gsq <- Gmat^2
  inertia <- (S@mu)^2
  inertiasum <- sum(inertia)
}

dmum2 <- diag( 1/(inertia + (inertia==0)) * (1-(inertia==0)) )
inertiapc <- 100*inertia/inertiasum
cuminertiapc <- cumsum(inertiapc)
inertiapc <- round(100*inertiapc)/100
cuminertiapc <- round(100*cuminertiapc)/100
inertias <- cbind(inertia,inertiapc,cuminertiapc)

# Calc contributions and correlations
# if mcaadjustmassctr is true then adjust ColCTR by *p so sum to p overall (i.e. average 1 each)

Xstd <- X/sum(X)
dr <- diag( as.vector(rowSums(Xstd)) )
dc <- diag( as.vector(colSums(Xstd)) )

RowCTR <- dr %*% Fsq %*% dmum2
Frs <- diag(1/rowSums(Fsq))
RowREP <- Frs %*% Fsq

ColCTR <- dc %*% Gsq %*% dmum2
Grs <- diag(1/rowSums(Gsq))
ColREP <- Grs %*% Gsq

# If inertias and coords adjusted, and mcaadjustmassctr then total CTR should sum to p
# if only inertias adjusted, use adjusted inertia for decomposition
# Should agree with SCA for p=2
if ( (catype=="mca") & (mcaadjustinertias==TRUE) & (!(mcatype=="doubled")) ) { # allows for sum being over all p variables
  if (mcaadjustmassctr==TRUE) { ColCTR <- ColCTR * p }
  if (mcaadjustcoords==FALSE) { # correct for coordinates not being adjusted
    ColCTR <- ColCTR %*% diag(inertia) %*% diag( 1/genuinesvs^2 )
  }
}

bootca <- new("cabootcrsresults", br=S,
  DataMatrix=X, rows=rows, columns=cols,
  rowlabels=rowlabels, collabels=collabels, varnames=varnames,
  Rowprinccoord=Fmat, Colprinccoord=Gmat, Rowstdcoord=Fbi, Colstdcoord=Gbi,
  RowCTR=RowCTR, RowREP=RowREP, ColCTR=ColCTR, ColREP=ColREP,
  RowVar=RSBvar, RowCov=RSBcov, ColVar=CSBvar, ColCov=CSBcov,
  inertiasum=inertiasum, inertias=inertias,
  nboots=nboots, resampledistn=resampledistn,
  multinomialtype=multinomialtype, sameaxisorder=sameaxisorder,
  poissonzeronewmean=poissonzeronewmean, newzeroreset=newzeroreset,
  printdims=printdims, axisvariances=axisvariances,
  bootcritR=bootcritR, bootcritC=bootcritC, usebootcrits=usebootcrits,
  catype=catype, mcatype=mcatype, mcaindividualboot=mcaindividualboot,
  IndicatorMatrix=IndicatorMatrix, Jk=Jk, p=p,
  mcalikertnoise=mcalikertnoise,
  mcaadjustinertias=mcaadjustinertias, mcauseadjustinertiasum=mcauseadjustinertiasum,
  mcaadjustcoords=mcaadjustcoords, mcaadjustmassctr=mcaadjustmassctr, mcasupplementary=mcasupplementary )

if (showresults==TRUE) {
summaryca(bootca, datasetname=as.character(datafile))
plotca(bootca, datasetname=as.character(datafile),
       groupings=groupings, grouplabels=grouplabels,
       plotsymbolscolours=plotsymbolscolours, picsize=NULL, likertarrows=likertarrows,
       othersmonochrome=othersmonochrome, crpercent=crpercent, usebootcrits=usebootcrits,
       mcaoneploteach=mcaoneploteach, mcashowindividuals=mcashowindividuals,
       mcavariablecolours=mcavariablecolours, mcacategorycolours=mcacategorycolours, eps=eps)
}

bootca

}

