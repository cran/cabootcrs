
#' Maxwell's dream data set, with simplified labels
#'
#' Reported disturbance of dreams among boys, using labels A, a etc
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

#' Maxwell's dream data set, using full original labels
#'
#' Reported disturbance of dreams among boys, using ages and original code for severities
#'
#' @format A contingency table with 5 rows and 4 columns
#' \describe{
#'   \item{rows}{Age group: 5-7 (A), 8-9 (B), 10-11 (C), 12-13 (D), 13-14 (E)}
#'   \item{columns}{Severity of disturbance of dream: lowest (1) to highest (4)}
#' }
#' @source G. Iliopoulos, M. Kateri and I. Ntzoufras,
#' \emph{Bayesian estimation of unrestricted and order-restricted association
#' models for a two-way contingency table},
#' Computational Statistics and Data Analysis 51 (2007), pp. 4643--4655.
"DreamDataNames"

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

#' Osteoarchaeological data with categories given as numbers
#'
#' Animal bones classified by 11 different variables
#'
#' @format A matrix of 6027 bones by 11 variables, each with 2-12 categories
#' \describe{
#'   \item{Species/Taxon - Taxa}{1-10}
#'   \item{Anatomical element - Elem}{1-12}
#'   \item{Recent breakage - Rec}{P/A}
#'   \item{Cut marks - Cut}{P/A}
#'   \item{Gnawing - Gnaw}{P/A}
#'   \item{Weathering stage - Weat}{1-4 or A}
#'   \item{Thermal stage - Therm}{1-5 or A}
#'   \item{Trampling - Tram}{P/A}
#'   \item{Root etching - Root}{P/A}
#'   \item{Post-depositional - Post}{P/A}
#'   \item{Context - Context}{1-4}
#' }
#' @source Macheridis, S.
#' \emph{The Use of Multiple Correspondence Analysis (MCA) in Taphonomy: The Case of Middle Helladic Asine, Greece}
#' International Journal of Osteoarchaeology 27 (2017), pp. 477–-487.
"OsteoData"

#' Osteoarchaeological data with named categories
#'
#' Animal bones classified by 11 different variables
#'
#' @format A matrix of 6027 bones by 11 variables, each with 2-12 categories
#' \describe{
#'   \item{Species/Taxon - Taxa}{Sheep/goat,Pig,Cattle,Deer,Dog,Equid,Large,Medium,Small,Size:indet}
#'   \item{Anatomical element - Elem}{Horn/antler,Head,Neck,Axial,Upper_front,Lower_front,Upper_hind,Lower_hind,Feet,Metapodials,Long_bone,Element:indet}
#'   \item{Recent breakage - Rec}{P/A}
#'   \item{Cut marks - Cut}{P/A}
#'   \item{Gnawing - Gnaw}{P/A}
#'   \item{Weathering stage - Weat}{stage 1-4 or A}
#'   \item{Thermal stage - Therm}{stage 1-5 or A}
#'   \item{Trampling - Tram}{P/A}
#'   \item{Root etching - Root}{P/A}
#'   \item{Post-depositional - Post}{P/A}
#'   \item{Context - Context}{Secondary,Room_fills,Primary,Floors}
#' }
#' @source Macheridis, S.
#' \emph{The Use of Multiple Correspondence Analysis (MCA) in Taphonomy: The Case of Middle Helladic Asine, Greece}
#' International Journal of Osteoarchaeology 27 (2017), pp. 477–-487.
"OsteoDataNames"





