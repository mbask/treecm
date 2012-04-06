###############################################################################
#
# treecm: An R package to assess tree centre of mass
# author: Marco Bascietto <marco.bascietto@ibaf.cnr.it>
#
# This is released under a GPL license.
#
# Documentation was created using roxygen:
# package.skeleton(name="treecm", code_files="treecm.R")
# library(roxygen2); roxygenize('treecm', 'treecm', overwrite=TRUE, unlink.target=FALSE, copy.package=FALSE)
###############################################################################

#' @title Assessment of x, y, z coordinates of centre of mass of trees
#'
#' @description Given a few data about branchiness of a tree the package computes and plots the centre of mass of the tree itself.
#' The centre of mass is a crucial data for arborists in order to consolidate a tree using steel or dynamic cables.
#' The tree stem is ideally sectioned in logs. The weight of tree components is assessed based on
#' \itemize{
#'   \item the sum of volume of stem logs
#'   \item the sum of branches biomass
#' }
#' Field measures to be taken on logs and branches include:
#' \itemize{
#'   \item{\bold{Diameter at base}: diameter at insertion point on the stem, for branches, diameter of the lower section for logs, mandatory}
#'   \item{\bold{Diameter at top}: 0 for branches, diameter of the higher section for logs, optional, defaults to 0}
#'   \item{\bold{Distance}: Length of branch or log projection on the ground, starting from tree base to tip of branch or log, mandatory}
#'   \item{\bold{Length}: Length of logs (no use for branches), mandatory}
#'   \item{\bold{Height}: height of branch insertion on the stem or height of lower section of the log, to be used to compute z coordinate of CM, optional, defaults to NA}
#'   \item{\bold{Azimuth}: mean angle of orientation of the branch or log measured from the base of the tree (usually with magnetic north as reference, measured clockwise), mandatory}
#'   \item{\bold{Tilt}: mean branch or log tilt from the horizontal plane (eg a vertical branch is 90 degrees, an horizontal branch is 0 degrees), to be used to compute z coordinate of CM, optional, defaults to 0. Note however that the tree tip should be considered as a branch, not a log, in order to account for foliage biomass. In this case tilt value should be recorded otherwise it would default to 0, ie an horizontal branch} 
#'   \item{\bold{To be pruned}: a boolean value, optional, defaults to FALSE}
#' }
#'
#' In order to help the arborist in the pruning selection process a simple plot of branch coefficient of slenderness is implemented.
#' @note Branch biomass is computed by allometric equations relating its weight (wood + leaves) to its diameter at point of insertion on the stem. Log biomass is computed by converting its volume to weight using wood fresh density. Volume is computed using Smalian's formula (see \code{\link{logBiomass}} description).
#' A sample \code{.CSV} file is provided to guide through data filling in the field
#' @seealso \code{\link{logBiomass}}
#' \code{\link{fieldData}}
#' @name treecm-package
#' @aliases treecm
#' @docType package
#' @import plyr
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
#' @keywords package
#' @examples
#' data(treeData)
#' vectors  <- treeVectors(treeData)
#' CM       <- centreOfMass(vectors)
#' plot(vectors, 
#'    main = "Centre Of Mass", 
#'    col = "grey30", 
#'    txtcol = "grey30")
#' plot(CM)
#' summary(CM)
#' @references Source code is hosted at GitHub (\url{https://github.com/mbask/treecm})
NULL




#' Green wood density data for a few tree species
#' 
#' Wood density is used to convert wood volume measures in the field to their
#' fresh weights. It is measured in \eqn{\frac{kg}{m^3}}.
#' 
#' Density is measured at humidity level 50%, very close to mean living tree
#' wood humidity. The dataset is provided as a reference only, please be
#' cautioned about using these values on your samples.
#' 
#' @name Dst
#' @docType data
#' @format A data frame with 170 observations on the following 3 variables. \code{
#' data.frame:  170 obs. of  3 variables:
#'   $ species: chr  "Abies alba" "Abies alba" "Abies balsama" "Abies grandis" ...
#'   $ group  : Factor w/ 2 levels "conifer","dicot": 1 1 1 1 1 1 1 1 1 1 ...
#'   $ density: int  545 577 529 449 465 673 689 497 673 577 ...}
#' @source Niklas, K. J. and Spatz, H.-C. (2010) Worldwide correlations of
#' mechanical properties and green wood density. American Journal of Botany,
#' 97, 1587-1594
#' @keywords datasets
#' @examples
#' 
#' data(Dst)
NULL 



#' Raw CSV file of field recorded values for a stone pine tree
#' 
#' Required data for the assessment of the centre of mass have been recorded in
#' the field for a stone pine (\emph{Pinus pinea} L.). This is an example of
#' csv file that should be fed to \code{\link{treeBiomass}} to assess tree
#' centre of mass.
#' 
#' 
#' @name fieldData
#' @docType data
#' @format \code{
#' "code","dir","dBase","dTip","length","tipD","height","tilt","toBePruned"
#' "L1",275,73,41,10.2,2.5,0,80, "L2",275,41,16,3.9,2.75,10.2,80,
#' "B1",190,15,0,,7.95,10.1,, "B2",200,22,0,,7.95,10.4,,
#' "B3",230,15,0,,7.95,10.4,, "B4",200,18,0,,7.95,11.15,,
#' "B5",180,7,0,,7.95,11.3,, "B6",150,6,0,,7.95,11.3,,
#' "B7",340,16,0,,3.95,11.3,, "B8",220,13,0,,7.95,11.8,,
#' "B9",165,19,0,,7.95,11.8,, "B10",280,8,0,,3.95,11.9,,
#' "B11",170,9,0,,7.95,11.9,, "B12",265,8,0,,7.95,12.2,,
#' "B13",75,6,0,,3.95,12.2,, "B14",180,6,0,,7.95,12.2,,
#' "B15",170,6,0,,7.95,12.6,, "B16",120,5,0,,7.95,12.6,,
#' "B17",10,14,0,,3.95,13,, "B18",180,13,0,,7.95,13,,
#' "B19",260,13,0,,7.95,13.2,, "B20",75,6,0,,3.95,13.2,,
#' "B21",75,10,0,,3.95,13.75,, "B22",215,7,0,,7.95,13.75,,
#' "B23",140,7,0,,7.95,13.75,, "C",275,16,0,3,3,14.1,80, }
#' @source Original data collected by the author
#' @keywords datasets
#' @examples
#' 
#' \dontrun{
#'   treeData <- importFieldData("fieldData.csv", 530, branchBiomassPinePorte)
#'   }
NULL


#' Field recorded values for a stone pine tree
#' 
#' Required data for the assessment of the centre of mass have been recorded in
#' the field for a stone pine (\emph{Pinus pinea} L.).
#' \code{\link{treeBiomass}} has already been run on the dataset, vectors have
#' yet to be computed.
#' 
#' 
#' @name treeData
#' @docType data
#' @format The format is: List of 4
#' 
#' \tabular{ll}{ $ fieldData : \tab 'data.frame': 26 obs. of 9 variables:\cr
#' ..$ dir : \tab int [1:26] 275 275 190 200 230 200 180 150 340 220 ...\cr ..$
#' dBase : \tab int [1:26] 73 41 15 22 15 18 7 6 16 13 ...\cr ..$ dTip : \tab
#' num [1:26] 41 16 0 0 0 0 0 0 0 0 ...\cr ..$ length : \tab num [1:26] 10.2
#' 3.9 NA NA NA NA NA NA NA NA ...\cr ..$ tipD : \tab num [1:26] 2.5 2.75 7.95
#' 7.95 7.95 7.95 7.95 7.95 3.95 7.95 ...\cr ..$ height : \tab num [1:26] 0
#' 10.2 10.1 10.4 10.4 ...\cr ..$ tilt : \tab num [1:26] 80 80 0 0 0 0 0 0 0 0
#' ...\cr ..$ toBePruned: \tab logi [1:26] FALSE FALSE FALSE FALSE FALSE FALSE
#' ...\cr ..$ biomass : \tab num [1:26] 1741 184 120 247 120 ...\cr $ density :
#' \tab num 620\cr $ allometryFUN:\tab function (x, diameter) ..- attr(*,
#' "source")= chr [1:5] "function(x, diameter) " ...\cr $ branchesCM : \tab num
#' 1\cr }
#' @source Original data collected by the author
#' @keywords datasets
#' @examples
#' 
#' data(treeData)
#' vectors  <- treeVectors(treeData)
#' CM       <- centreOfMass(vectors)
#' summary(CM)
NULL




#' @title Plots a segment
#'
#' @description Plots a segmente given two set of polar coordinates (angle, distance
#' from tree base), may be used to represent buildings close to the tree on the CM plot
#' 
#' @param a0 angle of first set of coordinates
#' @param d0 distance of first set of coordinates
#' @param a1 angle of second set of coordinates
#' @param d1 distance of second set of coordinates
#' @return \code{NULL}
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
plotPolarSegment <- function(a0, d0, a1, d1) {
  xy0 <- toCartesianXY(a0, d0)
  xy1 <- toCartesianXY(a1, d1)
  points(xy0[1], xy0[2])
  points(xy1[1], xy1[2])
  segments(xy0[1], xy0[2], xy1[1], xy1[2])
}

#' @title Imports field data from csv file
#'
#' @description Imports \code{csv} file holding field recorded data returning a list holding field and other key data provided as arguments. 
#' Missing data for \code{dTip} and \code{tilt} is defaulted to 0, missing data for \code{toBePruned} is defaulted to \code{FALSE}.
#'
#' @param fileName Name of csv file holding field data
#' @param dst Fresh density of wood of the tree
#' @param branchesAllometryFUN the function that should compute branch biomass from its diameter
#' @param bCM Estimated position of the centre of mass of branches, a real number from 0.01 (CM at branch base) to 1.00 (CM at branch tip). As a rule of thumb, average live branches, with an average amount of foliage, have CM approx. from \eqn{1/3} to \eqn{2/3} of their length. bCM = 1.0 (default value)
#' @seealso \code{\link{getCoordinatesAndMoment}}
#' @return a list of 4 elements: field data, wood fresh density, allometryF function and branches CM
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
importFieldData <- function(fileName, dst, branchesAllometryFUN, bCM = 1) {
  ## il file .csv deve contenere il nome del ramo come prima colonna
  tree <- read.csv(fileName, row.names = 1)
  
  tree <- within(tree, {
    dTip[is.na(dTip)] <- 0
    tilt[is.na(tilt)] <- 0
    toBePruned[is.na(toBePruned)] <- FALSE
  })
  
  list(
    fieldData    = tree, 
    density      = dst, 
    allometryFUN = branchesAllometryFUN,
    branchesCM   = bCM
  )
  
}

#' @title Stores branches CM in an object
#'
#' @description Stores branches CM in the object provided as an argument. branchesCM has to be in the range 0.01 - 1
#'
#' @note Method \code{\link{treeVectors}} must be invoked to take changes into effect
#'
#' @param object the object of class \code{treeData}
#' @param value the new branch CM
#' @return the updated list
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
setBranchesCM <- function(object, value) {
  if (is.list(object) && value > 0 && value <=1) 
    object$branchesCM <- value
  return(object)
}

#' @title Sets pruning status of a branch
#'
#' @description Switches pruning status of a record in the raw data frame from field measures
#'
#' @note Method \code{\link{treeVectors}} must be invoked to take changes into effect
#'
#' @param object the object of class \code{treeData}
#' @param value number of row or vector of numbers of rows to have pruning status switched
#' @return the updated list
#' @examples
#' data(treeData)
#' treeData <- switchBranchPruningStatus(treeData, c(7, 11, 13, 15, 17, 23, 19, 22, 18, 25, 8, 10))
#' CM       <- centreOfMass(treeVectors(treeData))
#' summary(CM)
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
switchBranchPruningStatus <- function(object, value) {
  if (is.list(object))
    object$fieldData$toBePruned[value] <- !object$fieldData$toBePruned[value]
  return(object)
}


