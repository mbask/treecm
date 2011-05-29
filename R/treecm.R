###############################################################################
#
# treecm: An R package to assess tree centre of mass
# author: Marco Bascietto <marco.bascietto@ibaf.cnr.it>
#
# This is released under a GPL license.
#
# Documentation was created using roxygen:
# package.skeleton(name="treecm", code_files="treecm.R")
# roxygenize('treecm', roxygen.dir='treecm', copy.package=FALSE, unlink.target=FALSE, use.Rd2=TRUE)
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
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
#' @keywords package
#' @examples
#' data(treeData)
#' vectors  <- treeVectors(treeData)
#' CM       <- centreOfMass(vectors)
#' plot.vectors(vectors, 
#'    CM = CM, 
#'    main = "Centre Of Mass", 
#'    col = "grey30", 
#'    txtcol = "grey30")
#' summary(CM)
#' @references Source code is hosted at GitHub (\url{https://github.com/mbask/treecm})
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
#' @description Imports \code{csv} file holding field recorded data returning a list holding field and other key data provided as arguments
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
  ## il file .csv e deve contenere il nome del ramo come prima colonna
  tree <- read.csv(fileName, row.names = 1)
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


