
#' Maxwell's dream data set
#'
#' Reported disturbance of dreams among boys
#'
#' @format A contingency table with 5 rows and 4 columns
#' \describe{
#'   \item{rows}{Age group: 5-7 (A), 8-9 (B), 10-11 (C), 12-13 (D), 13-14 (E)}
#'   \item{columns}{Severity of disturbance of dream: lowest (a) to highest (d)} 
#' }
#' @source G. Iliopoulos, M. Kateri and I. Ntzoufras,
#' \emph{Bayesian estimation of unrestricted and order-restricted association
#' models for a two-way contingency table},
#' Computational Statistics and Data Analysis 51 (2007), pp. 4643--4655.
"DreamData"

#' Maxwell's dream data set with added totally random column
#'
#' Reported disturbance of dreams among boys, plus a random column
#'
#' @format A matrix of 223 individuals by 3 variables
#' \describe{
#'   \item{Age group - R}{5-7 (A), 8-9 (B), 10-11 (C), 12-13 (D), 13-14 (E)}
#'   \item{Severity of disturbance of dream - C}{Severity of disturbance of dream: lowest (a) to highest (d)}
#'   \item{Random fake variable - V}{1 to 3}
#' }
#' @source Adapted from G. Iliopoulos, M. Kateri and I. Ntzoufras,
#' \emph{Bayesian estimation of unrestricted and order-restricted association
#' models for a two-way contingency table},
#' Computational Statistics and Data Analysis 51 (2007), pp. 4643--4655.
"DreamData223by3"

#' van Ijzendoorn's attachment data
#' 
#' Classification of mother's attachment to her child and child's reaction
#'
#' @format A contingency table with 4 rows and 4 columns
#' \describe{
#'   \item{rows}{Infant response: Avoidant, Secure, Resistant, Disorganised}
#'   \item{columns}{Mother's Classification: Dismissing, Autonomous, Preoccupied, Unresolved}
#' }
#' @source E.J. Beh,
#' \emph{Elliptical confidence regions for simple correspondence analysis},
#' Journal of Statistical Planning and Inference 140 (2010), pp. 2582--2588.
"AttachmentData"

#' Nishisato's Singapore data
#' 
#' Questionnaire data collected at a Dual Scaling workshop in Singapore
#'
#' @format A matrix of 13 individuals by 4 variables each with 3 possible answers
#' \describe{
#'   \item{Age - a}{20-29 (1), 30-39 (2), 40+ (3)}
#'   \item{Children are less disciplined now - b}{Agree (1), Disagree (2), Can't tell (3)}
#'   \item{Children are less fortunate now - c}{Agree (1), Disagree (2), Can't tell (3)}
#'   \item{Religions should not be taught in school - d}{Agree (1), Disagree (2), Indifferent (3)}
#' }
#' @source Nishisato, S. (1994).
#' \emph{Elements of Dual Scaling: An Introduction to Practical Data Analysis}.
#' Lawrence Erlbaum Associates, New Jersey. (p153)
"NishData"

#' Suicide data
#' 
#' Methods of suicide in Germany, 1974-1977
#'
#' @format A contingency table with 34 rows and 9 columns
#' \describe{
#'   \item{rows}{Gender and age of individual: Females aged 10-15 (F10) to males aged 90+ (M90)}
#'   \item{columns}{Method: drugs/poison (Mat), gas at home (Gas.h), gas-others (Gas.o), hanging (Hang), 
#'                  drowning (Drown), gunshot (Gun), stabbing (Stab), jumping (Jump), Other} 
#' }
#' @source Nishisato, S. (1994).
#' \emph{Elements of Dual Scaling: An Introduction to Practical Data Analysis}.
#' Lawrence Erlbaum Associates, New Jersey. (p12)
"SuicideData"

#' Asbestos data
#'
#' Cases and severity of asbestosis, classified by years working with asbestos
#'
#' @format A contingency table with 5 rows and 4 columns
#' \describe{
#'   \item{rows}{Years working with asbestos: 0-9, 10-19, 20-29, 30-39, 40+}
#'   \item{columns}{Has asbestosis or not: No (N), severity graded as 1-3 (G1-G3)}
#' }
#' @source Still looking for it
"AsbestosData"


