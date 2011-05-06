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

#' Assessment of x, y, z coordinates of centre of mass of trees
#'
#' \tabular{ll}{
#' Package: \tab treecm\cr
#' Type: \tab Package\cr
#' Version: \tab 0.0.1\cr
#' Date: \tab 2011-05-01\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab no\cr
#' }
#'
#' Given a few data about branchiness of a tree the package computes and plots the centre of mass of the tree itself.
#' The centre of mass is a crucial data for arborists in order to consolidate a tree using steel or dynamic cables.
#' Field measures to be taken include:
#' \itemize{
#'   \item{Diameter at base: }{diameter at insertion point on the trunk, for branches, diameter of the lower section for logs, mandatory}
#'   \item{Diameter at top: }{0 for branches, diameter of the higher section for logs, optional, defaults to 0}
#'   \item{Distance: }{Length of branch or log projection on the ground, starting from tree base to tip of branch or log, mandatory}
#'   \item{Length: }{Length of logs (no use for branches), mandatory}
#'   \item{Height: }{height of branch insertion on the trunk or height of lower section of the log, to be used to compute z coordinate of CM, optional, defaults to NA}
#'   \item{Direction: }{mean angle of orientation of the branch or log measured from the base of the tree (usually with magnetic north as reference, measured clockwise), mandatory}
#'   \item{Tilt: }{mean branch or log tilt from the horizontal plane (eg a vertical branch is 90 degrees, an horizontal branch is 0 degrees), to be used to compute z coordinate of CM, optional, defaults to 0. Note however that the tree tip should be considered as a branch, not a log, in order to account for foliage biomass. In this case tilt value should be recorded otherwise it would default to 0, ie an horizontal branch} 
#'   \item{To be pruned: }{a boolean value, optional, defaults to FALSE}
#' }
#' Branch biomass is computed by allometric equations relating its dry weight (wood + leaves) to its diameter at point of insertion on the trunk.
#' Log biomass is computed by converting its volume to dry weight using wood basal density. Volume is computed using Smalian's formula (see logBiomass description)
#' @seealso \code{\link{logBiomass}}
#' \code{\link{fieldData}}
#' @note An example \code{.CSV} file is provided to guide through data filling in the field
#' @title Assessment of x, y, z coordinates of centre of mass of trees
#' @name treecm-package
#' @aliases treecm
#' @docType package
#' @title Tree Centre of Mass for R
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
#' @keywords package
#' @examples
#' data(treeData)
#' vectors  <- treeVectors(treeData)
#' CM       <- centreOfMass(vectors)
#' plot.vectors(vectors, CM = CM, main = "Centre Of Mass", col = "grey30", txtcol = "grey30")
#' summary(CM)
#' @references Source code is hosted at GitHub (\url{https://github.com/mbask/treecm})
NULL

#' Computes the x cartesian coordinate
#'
#' Computes the x cartesian coordinate from a set of polar coordinates
#'
#' @param angle The angle in degree (measured clockwise from the North or from 
#' any other relevant direction defined in the field)
#' @param distance The distance
#' @return The x coordinate (a real number) measured in the same unit as the distance parameter
#'
#' @note Attention: the function assumes the angle is measured clockwise whereas
#' trigonometric functions require a conventional counterclockwise 
#' measured angle. Thus the function computes x coordinate as the sine of 
#' the angle, enabling a correct representation of them on a cartesian plot.
#'
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
toCartesianX <- function(angle, distance) {
  angleRad <- angle * pi / 180
  sin(angleRad) * distance
}

#' Computes the y cartesian coordinate
#'
#' Computes the y cartesian coordinate from a set of polar coordinates
#' @note Attention: the function assumes the angle is measured clockwise whereas
#' trigonometric functions require a conventional counterclockwise 
#' measured angle. Thus the function computes x coordinate as the cosine of 
#' the angle, enabling a correct representation of them on a cartesian plot.
#'
#' @param angle The angle in degree (measured clockwise from the North or from 
#' any other relevant direction defined in the field)
#' @param distance The distance
#' @return The y coordinate (a real number) measured in the same unit as the distance parameter
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
toCartesianY <- function(angle, distance) {
  angleRad <- angle * pi / 180
  cos(angleRad) * distance
}

#' Converts cartesian (x, y) into polar (angle, distance) coordinates
#'
#' Converts cartesian coordinates (x, y) into polar (angle, distance) ones, assuming (0, 0) as origin of axes
#'
#' @param x abscissa coordinate
#' @param y ordinate coordinate
#' @return A vector holding angle in degrees (integer) and distance (real) in the same unit as x and y
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
toPolar <- function(x, y) {
  d <- sqrt(x^2 + y^2)
  a <- (atan2(x, y) * 180 / pi) %% 360
  c(as.integer(a), d)
}

# -------------moment
#' Computes the cartesian coordinates of centre of mass of branches and logs
#'
#' Computes the cartesian coordinates of centre of mass of branches and 
#' logs along with their x, y, z moments
#' The x and y coordinates are computed from the polar coordinates (angle and distance, 
#' defined as the length of its projection on ground), measured in the field. 
#' The z coordinate is computed by adding the height of branch insertion on the trunk 
#' (measured in the field) to the height of the branch (calculated through its 
#' mean inclination, in case it was measured in the filed).
#' The x, y, z coordinates are corrected to take into account where the actual
#' centre of mass lies on the branches themselves by multiplying them by branchesCM,
#' a real number from 0.01 (CM at branch base) to 1.00 (CM at branch tip). 
#' As a rule of thumb, average live branches, with an average amount of foliage, 
#' have CM approx. from 1/3 to 2/3 of their length, ie. branchesCM = 0.33-0.66.
#' #' x, y, z moments are computed by multiplying the cartesian coordinate by 
#' branch or log mass.
#' @note BranchCM is assumed to have same value in branches and logs. This is not the case in real world. As a measure of safety one should use higher values than 1/3, eg 1 for branchesCM.
#' @param object A data.frame holding the appropriate colums
#' @param angle The name of the data.frame column holding the angle of branch orientation
#' @param distance The name of the data.frame column holding the length of the 
#' branch projection on the ground
#' @param height The name of the data.frame column holding the height of branch
#' insertion on the trunk or the height of log lower section
#' @param incl The name of the data.frame column holding the inclination of
#' the branch or log in degrees
#' @param mass The name of the data.frame column holding the mass of the branch or log
#' @param branchesCM a real number varying from 0.01 to 1 proportional to the centre of
#' mass position along the branch (0.01 branch base, 1 branch tip)
#' @return a vector holding 5 reals:
#' \itemize{
#'  \item{the x coordinate of branch CM}
#'  \item{the y coordinate of branch CM}
#'  \item{the x moment of the branch}
#'  \item{the y moment of the branch}
#'  \item{the z moment of the branch}}
#' @note z coordinate of CM is not returned because it would be useless in a 2D plot. It is computed using mz, which is, as a matter of facts, returned
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
getCoordinatesAndMoment <- function (object, angle, distance, height, incl, mass, branchesCM) {
  ## get variables
  angle    <- as.integer(object[angle])
  distance <- as.real(object[distance])
  mass     <- as.real(object[mass])
  inclRad  <- as.integer(object[incl]) * pi / 180
  # height (h) to be added to branch height (z), as a function of the 
  # angle of its tilt (0° = horiz., 90° = vert.), its distance (length of its 
  # projection on the ground, 
  # from tree base to branch tip), and the estimated position of the centro of mass
  h  <- distance * sin(inclRad) * branchesCM
  ## computes cartesian coordinates of centre of mass of branches and their moments (mx, my, mz).
  ## When branchesCM = 1 x and y are coordinates of branch tip
  x   <- toCartesianX(angle, (distance * branchesCM))
  y   <- toCartesianY(angle, (distance * branchesCM))
  z   <- as.real(object[height]) + h

  mx  <- mass * x
  my  <- mass * y
  mz  <- mass * z
  c(x, y, mx, my, mz)
}

#' Estimates the wood biomass of logs and truncated branches by
#' computing their volume (using Smalian's formula) and converting it
#' to dry weight using basal density.
#' Smalian's formula: \eqn{V=\frac{Sb+Sd}{2}l} where \eqn{V} is the log volume, 
#' \eqn{Sb} is the aerea of the basal (lower) section, \eqn{Sd} is the 
#' area of the higher section and \eqn{l} is the length of the log.
#'
#' @note Attention: diameters used to compute section areas should be measured under the bark layer! When this is not the case and diameters include bark thickness the log biomass is over-estimated!
#' @param x the data.frame holding the measures needed to perform the estimation
#' @param lowerD The name of the data.frame column holding diameter of the lower section in cm
#' @param higherD The name of the data.frame column holding the diameter of the higher section (usually smaller!) in cm
#' @param logLength The name of the data.frame column holding the length of the log or branch in m
#' @param basalDensity The name of the data.frame column holding the basal Density of the wood, defined as \eqn{D=\frac{V_f}{W_d}} where \eqn{V_f} is wood volume measured in the field (i.e. satured with water) in \eqn{m^3} and \eqn{W_d} is wood dry weight in kg. Basal density is measured in \eqn{\frac{kg}{m^3}}
#' @references la Marca, O. Elementi di dendrometria, 2004, Patron Editore (Bologna), p. 119
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
logBiomass <- function (x, lowerD, higherD, logLength, basalDensity) {
  lowerS   <- pi * (as.real(x[lowerD]) / 200)^2 
  higherS  <- pi * (as.real(x[higherD]) / 200)^2
  l        <- as.real(x[logLength])
  volume   <- (lowerS + higherS) / 2 * l
  volume * basalDensity
}

## --------------- Math functions ---------------
##

#' Applies the pure quadratic equation \eqn{Y = a + bX^2}
#' given \eqn{a}, \eqn{b} and \eqn{X}
#'
#' @param a the parameter a (a real number) in the pure quadratic equation
#' @param b the parameter b (a real number) in the pure quadratic equation
#' @param x the dependent variable (a real number)
#' @return the dependent variable (y)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
pureQuadraticEquation <- function(a, b, x) {
  a + b * x^2
}

#' Applies the exponential equation \eqn{Y = a * X^b}
#' given \eqn{a}, \eqn{b} and \eqn{X}
#'
#' @param a the parameter a (a real number) in the exponential equation
#' @param b the parameter b (a real number) in the exponential equation
#' @param x the dependent variable (a real number)
#' @return the dependent variable (y)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
powerEquation <- function(a, b, x) {
  a * x^b
}

#' Plots a segment
#'
#' Plots a segmente given two set of polar coordinates (angle, distance
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
  x0 <- toCartesianX(a0, d0)
  y0 <- toCartesianY(a0, d0)
  x1 <- toCartesianX(a1, d1)
  y1 <- toCartesianY(a1, d1)
  
  points(x0, y0)
  points(x1, y1)
  segments(x0, y0, x1, y1)
}

#' Imports field data from csv file
#'
#' Imports csv file holding field recorded data, and sets a list holding other key data
#'
#' @param fileName Name of csv file holding field data
#' @param basalDensity Basal Density of wood of the tree
#' @param branchesAllometryFUN the function that should compute branch biomass from its diameter
#' @param bCM Estimated position of the centre of mass of branches, a real number from 0.01 (CM at branch base) to 1.00 (CM at branch tip). As a rule of thumb, average live branches, with an average amount of foliage, have CM approx. from 1/3 to 2/3 of their length. bCM = 1.0 (default value)
#' @seealso \code{\link{getCoordinatesAndMoment}}
#' @return a list holding the data
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
importFieldData <- function(fileName, basalDensity, branchesAllometryFUN, bCM = 1) {
  ## il file .csv e deve contenere il nome del ramo come prima colonna
  tree <- read.csv(fileName, row.names = 1)
  list(
    fieldData    = tree, 
    bslDensity   = basalDensity, 
    allometryFUN = branchesAllometryFUN,
    branchesCM   = bCM
  )
}




#' Stores branches CM
#'
#' Method "calcVectors" must be invoked to take changes into effect
#'
#' @param object the list holding tree data
#' @param value the new branches CM (0.01 to 1)
#' @return the updated list
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
setBranchesCM <- function(object, value) {
  if (is.list(object) && value > 0 && value <=1) 
    object$branchesCM <- value
  return(object)
}


#' Sets pruning status of a branch
#'
#' Switches pruning status of a record in the raw data.frame from field measures
#' Method "calcVectors" must be invoked to take changes into effect
#'
#' @param object the list holding tree data
#' @param value number of row or vector of numbers of rows to be switched
#' @return the updated list
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
switchBranchPruningStatus <- function(object, value) {
  if (is.list(object))
    object$fieldData$toBePruned[value] <- !object$fieldData$toBePruned[value]
  return(object)
}


#' Computes masses of branches and logs
#'
#' Computes branches biomass using an allometric function provided in \code{allometryFUN} and logs weight using Smalian's formula. Branches are telled apart from logs in the raw data.frame (\code{fieldData}) because their final diameter is 0 (ie they have a tip) whereas logs have a final diameter > 0.
#'
#' @param object a list holding tree data
#' @seealso \code{\link{logBiomass}}
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
treeBiomass <- function(object) {
  
  object$fieldData <- within(object$fieldData, {
    dTip[is.na(dTip)] <- 0
    tilt  [is.na(tilt)]   <- 0
    toBePruned    [is.na(toBePruned)]     <- FALSE
  })
  
  object <- within(object, {
    ## gets trunk and cut branches (ie. diameter at tip > 0) biomass, by converting its fresh volume to dry weight
    fieldData$biomass[(fieldData$dTip > 0)] <- as.vector(
      apply(
        fieldData[(fieldData$dTip > 0),], 
        1, 
        logBiomass, 
        lowerD    = "dBase", 
        higherD   = "dTip", 
        logLength = "length",
        bslDensity
      )
    )
    
    ## gets branches (ie. diameter at tip = 0) total biomass (wood + leaves) 
    fieldData$biomass[(fieldData$dTip == 0)] <- as.vector(
      apply(
        fieldData[(fieldData$dTip == 0),], 
        1, 
        allometryFUN, 
        diameter = "dBase"
      )
    )
  })
  return(object)
}


#' Computes cartesian coordinates and moments of branches and logs 
#'
#' A data.frame is populated width branch and log masses, along with x, y cartesian coordinates and x, y, and z moments.
#' z coordinates and moments are calculated only if branches height from the ground (and inclination) have been recorded in the field.
#'
#' @param object an object of class vectors 
#' @seealso \code{\link{getCoordinatesAndMoment}}
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
treeVectors <- function(object) {

  ## vectors data.frame is populated
  vectors <- subset(object$fieldData, select = c(dir, tipD, biomass, height, tilt, toBePruned))

  ## computes cartesian coordinates of branch tip and its x, y and z moments are added to vectors slot
  vectors <- cbind(vectors, 
    t(
      apply(
        vectors, 
        1, 
        getCoordinatesAndMoment, 
        angle      = "dir", 
        distance   = "tipD", 
        height     = "height",
        incl       = "tilt",
        mass       = "biomass",
        branchesCM = object$branchesCM
      )
    )
  )
  colnames(vectors) <- c("Direction", "Distance", "Biomass", "Height", "Tilt", "toBePruned", "x", "y", "mx", "my", "mz")
  class(vectors) <- c("vector", class(vectors))
  return(vectors)
}

#' Computes the centre of mass of the tree
#'
#'  The \eqn{x} coordinate of the centre of mass is defined as \eqn{\frac{\sum(m_ix_i)}{\sum(m_i)}} where \eqn{m_i} is the biomass of the \eqn{i^{th}} branch and \eqn{x_i} is the \eqn{x} coordinate of the \eqn{i^{th}} branch. \eqn{y} coordinate is similarly computed.
#' The centre of mass computation excludes branches to be pruned 
#'
#' @param object A data.frame of class CM
#' @param ...    Arguments to be passed to plot.default
#' @return A vector holding x, y, z coordinates of the centre of mass
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
centreOfMass <- function(object) {
  # sums the masses of tree branches and their x and y moments
  treeVectors <- subset(object, !toBePruned, select = c("Biomass", "mx", "my", "mz"))
  col.sums    <- apply(treeVectors, 2, sum)

  # cartesian coordinates of centre of mass of the tree
  M <- c(
    col.sums["mx"] / col.sums["Biomass"], 
    col.sums["my"] / col.sums["Biomass"], 
    col.sums["mz"] / col.sums["Biomass"]
  )
  names(M) <- c("x", "y", "z")
  class(M) <- c("CM", class(M))
  return(M)
}

#' Plots branches and logs CM and tree CM
#'
#' The 2d plot represents branch, log and tree CM as vectors pointing inwards. CMs vector radii are proportional to CM magnitude. Tree CM is connected to tree base by an arrow showing the direction the tree would take in case of it falling down. z coordinate of tree CM is printed alongside its vector (if branch height have been recorded in the field).
#' Branches to be pruned are not shown on graph.
#'
#' @param x      vectors list
#' @param y      unused
#' @param CM     Centre of mass list
#' @param txtcol Colour of text labels, defaults to "grey80"
#' @param ...    Arguments to be passed to plot.default
#' @return \code{NULL}
#' @method plot vectors
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
plot.vectors <- function(x, y = NULL, CM, txtcol = "grey80", ...) {
  treeVectors <- subset(x, !toBePruned)

  ## plots branch masses
  ## size of points is proprortional to branch biomass
  maxPointSize <- 10
  pointSize <- maxPointSize * as.numeric(as.character(treeVectors$Biomass)) / max(treeVectors$Biomass)
  
  plot(treeVectors[c("x", "y")], cex = pointSize, pch = 13, ...)
  abline(h = 0, v = 0, col = "gray70")
  
  # print vector labels
  chw <- par()$cxy[1] 
  text(treeVectors[c("x", "y")] - chw, labels = row.names(treeVectors), adj = 0, cex = 0.8, col = txtcol) 
   
  ## plots and labels centre of mass point
  cmText <- paste("CM (z=", sprintf("%.2f", CM["z"]), ")")
  points(CM["x"], CM["y"], pch = 13, col = 2, cex = 3)
  text(
    CM["x"] - chw, 
    CM["y"] - chw, 
    labels = cmText, 
    adj = 0, 
    cex = 0.8,
    col = 2
  ) 
  arrows(0, 0, CM["x"], CM["y"], col = 2, lwd = 2)
  
  ## draws vector (branch) arrows; to avoid warnings
  ## only vector whose tip has coordinates != (0, 0) should be drawn
#    tmpVector <- treeVectors[(treeVectors[, "1"] != 0 | treeVectors[, "2"] != 0),]
#    s <- seq(nrow(tmpVector))
#    arrows(0, 0, tmpVector[s, "1"], tmpVector[s, "2"])
#    points(tmpVector[s, "1"], tmpVector[s, "2"], pch = 13)
}


#' Summary of Centre of Mass data
#'
#' Prints in a human-readable format the polar and cartesian coordinates of tree CM
#'
#' @method summary cm
#' @param object An object of class CM
#' @param ...    Additional arguments affecting the summary produced
#' @return       \code{NULL}
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
summary.CM <- function(object, ...) {

  cat("Coordinates of the centre of mass:\n")
  cat("Cartesian (x/m, y/m, z/m):", 
    sprintf("%.2f", object["x"]), ",", 
    sprintf("%.2f", object["y"]), ",", 
    sprintf("%.2f", object["z"]), "\n"
  )

  polar <- toPolar(object["x"], object["y"])
  cat("Polar (angle/degrees, distance/m, height/m):", 
    polar[1], ",", 
    sprintf("%.2f", polar[2]), ",", 
    sprintf("%.2f", object["z"]), "\n"
  )
}

#' Returns total biomass of a tree or branch (wood and leaves) in kg given the 
#' diameter at breast height, using an allometric equation for stone pine
#'
#' @references Cutini, A.; Hajny, M.; Gugliotta, O.; Manetti, M. & Amorini, E. 
#'   Effetti della struttura del popolamento sui modelli di stima del volume e 
#'   della biomassa epigea (Pineta di Castelfusano - Roma) Forest@@, 2009, 6, 75-84 
#'   Tipo B
#' @param x a data.frame of branches along with their diameters as a column
#' @param diameter the name (a character) of the column holding diameter of the x data.frame, diameters should be in cm 
#' @return the total biomass of the branch of a stone pine (in kg)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
branchBiomassPine <- function(x, diameter) {
  a <- -198.236
  b <- 0.620
  pureQuadraticEquation(a, b, as.real(x[diameter]))
}

#' Returns the woody biomass of a branch (no leaves!) in kg given the 
#' diameter, using an allometric equation for maritime pine
#'
#' @note Important: the allometric equation has been validated for 1-10 cm diameter branches
#' @references Porté, A.; Trichet, P.; Bert, D. & Loustau, D. Allometric relationships for branch and tree woody biomass of Maritime pine (\emph{Pinus pinaster} Ait.) Forest Ecology and Management, 2002, 158, 71-83
#' @param x a data.frame of branches along with their diameters as a column
#' @param diameter the name (a character) of the column holding diameter of the x data.frame, diameters should be in cm 
#' @return the woody biomass of the branch of a maritime pine (in kg)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
branchBiomassPinePorte <- function(x, diameter) {
  a <- 21.228
  b <- 2.818
  powerEquation(a, b, as.real(x[diameter])) / 1000
}

#' Returns the fresh biomass of a branch in kg given the 
#' diameter, using an allometric equation for stone pine branches
#'
#' @note Important: the allometric equation has been validated for 8-16 cm diameter branches
#' @references Data collected by A. Ascarelli, non linear regression by M. Bascietto
#' @param x a data.frame of branches along with their diameters as a column
#' @param diameter the name (a character) of the column holding diameter of the x data.frame, diameters should be in cm 
#' @return the fresh biomass of the branch of a stone pine (in kg)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
branchBiomassPineAsca <- function(x, diameter) {
  a <- 0.7201
  b <- 1.8882
  powerEquation(a, b, as.real(x[diameter]))
}

