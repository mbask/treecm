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
#'   \item{\bold{Direction}: mean angle of orientation of the branch or log measured from the base of the tree (usually with magnetic north as reference, measured clockwise), mandatory}
#'   \item{\bold{Tilt}: mean branch or log tilt from the horizontal plane (eg a vertical branch is 90 degrees, an horizontal branch is 0 degrees), to be used to compute z coordinate of CM, optional, defaults to 0. Note however that the tree tip should be considered as a branch, not a log, in order to account for foliage biomass. In this case tilt value should be recorded otherwise it would default to 0, ie an horizontal branch} 
#'   \item{\bold{To be pruned}: a boolean value, optional, defaults to FALSE}
#' }
#'
#' @note Branch biomass is computed by allometric equations relating its dry weight (wood + leaves) to its diameter at point of insertion on the stem. Log biomass is computed by converting its volume to weight using wood fresh density. Volume is computed using Smalian's formula (see \code{\link{logBiomass}} description).
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

#' @title Computes the x cartesian coordinate
#'
#' @description Computes the \eqn{x} cartesian coordinate from a set of polar coordinates
#'
#' @param angle The angle in degrees (measured clockwise from the North or  
#' any other relevant bearing system defined in the field)
#' @param distance The distance
#' @return The \eqn{x} coordinate expressed in the same unit as the distance argument
#'
#' @note The function assumes the angle is measured clockwise whereas
#' trigonometric functions require a conventional counterclockwise 
#' measured angle. Thus the function computes \eqn{x} coordinate as the sine of 
#' the angle, enabling a correct representation of them on a cartesian plot.
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
toCartesianX <- function(angle, distance) {
  angleRad <- angle * pi / 180
  sin(angleRad) * distance
}

#' @title Computes the y cartesian coordinate
#'
#' @description Computes the \eqn{y} cartesian coordinate from a set of polar coordinates
#'
#' @note The function assumes the angle is measured clockwise whereas
#' trigonometric functions require a conventional counterclockwise 
#' measured angle. Thus the function computes \eqn{y} coordinate as the cosine of 
#' the angle, enabling a correct representation of them on a cartesian plot.
#'
#' @param angle The angle in degree (measured clockwise from the North or from 
#' any other relevant bearing system defined in the field)
#' @param distance The distance
#' @return The \eqn{y} coordinate expressed in the same unit as the distance parameter
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
toCartesianY <- function(angle, distance) {
  angleRad <- angle * pi / 180
  cos(angleRad) * distance
}

#' @title Converts cartesian (x, y) into polar (angle, distance) coordinates
#'
#' @description Converts cartesian coordinates (\eqn{x}, \eqn{y}) into polar (angle, distance) ones, assuming (0, 0) as origin of axes and, incidentally, the position of tree base
#'
#' @param x Abscissa coordinate
#' @param y Ordinate coordinate
#' @return A vector holding angle in degrees and distance in the same unit as \eqn{x} and \eqn{y}
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
toPolar <- function(x, y) {
  d <- sqrt(x^2 + y^2)
  a <- (atan2(x, y) * 180 / pi) %% 360
  c(as.integer(a), d)
}


#' @title Returns the coordinates of centre of mass of branches and logs
#'
#' @description Computes the cartesian coordinates of centre of mass of branches and 
#' logs along with their \eqn{x}, \eqn{y}, \eqn{z} moments
#'
#' The \eqn{x} and \eqn{y} coordinates are computed from the polar coordinates (angle and distance, 
#' defined as the length of its projection on ground), measured in the field. 
#' The \eqn{z} coordinate is computed by adding the height of branch insertion on the stem 
#' (measured in the field) to the height of the branch (calculated through its 
#' mean tilt, in case it was measured in the filed).
#' The \eqn{x}, \eqn{y}, \eqn{z} coordinates are corrected to take into account where the actual
#' centre of mass lies on the branches themselves by multiplying them by branchesCM,
#' a real number from 0.01 (CM at branch base) to 1.00 (CM at branch tip). 
#' As a rule of thumb, average live branches, with an average amount of foliage, 
#' have CM approx. from \eqn{1/3} to \eqn{2/3} of their length, ie. branchesCM = 0.33-0.66.
#' \eqn{x}, \eqn{y}, \eqn{z} moments are computed by multiplying the cartesian coordinate by 
#' branch or log mass.
#'
#' @param object A data frame holding the appropriate colums
#' @param angle The name of the data frame column holding the angle of branch orientation
#' @param distance The name of the data frame column holding the length of the 
#' branch projection on the ground
#' @param height The name of the data frame column holding the height of branch
#' insertion on the stem or the height of log lower section
#' @param incl The name of the data frame column holding the inclination of
#' the branch or log in degrees
#' @param mass The name of the data frame column holding the mass of the branch or log
#' @param branchesCM a real number varying from 0.01 to 1 proportional to the centre of
#' mass position along the branch (0.01 branch base, 1 branch tip)
#' @return a vector holding 5 reals:
#' \itemize{
#'  \item{the \eqn{x} coordinate of branch CM}
#'  \item{the \eqn{y} coordinate of branch CM}
#'  \item{the \eqn{x} moment of the branch}
#'  \item{the \eqn{y} moment of the branch}
#'  \item{the \eqn{z} moment of the branch}}
#' @note BranchCM is assumed to have same value in branches and logs. This is not the case in the real world. As a measure of safety one should use higher values than \eqn{1/3}, eg 1 for branchesCM.
#'
#' @note \eqn{z} coordinate of CM is not returned because it would be useless in a 2D plot. It is computed using \eqn{mz}, which is, as a matter of facts, returned
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

#' @title Estimates the wood biomass of logs and truncated branches
#'
#' @description Estimates the wood biomass of logs and truncated branches by
#' computing their volume (using Smalian's formula) and converting it
#' to fresh weight using wood fresh density.
#'
#' Smalian's formula: \eqn{V=\frac{Sb+Sd}{2}l} where \eqn{V} is the log volume, 
#' \eqn{Sb} is the aerea of the basal (lower) section, \eqn{Sd} is the 
#' area of the higher section and \eqn{l} is the length of the log.
#'
#' @note Diameters used to compute section areas should be measured under the bark layer! When this is not the case (scarcely ever!) and diameters include bark thickness the log biomass is somewhat over-estimated!
#' @param x the data frame holding the measures needed to perform the estimation
#' @param lowerD The name of the data frame column holding diameter of the lower section in cm
#' @param higherD The name of the data frame column holding the diameter of the higher section (usually smaller!) in cm
#' @param logLength The name of the data frame column holding the length of the log or branch in m
#' @param density The name of the data frame column holding the fresh density of the wood, defined as \eqn{D=\frac{V_f}{W_f}} where \eqn{V_f} is wood volume measured in the field (i.e. satured with water) in \eqn{m^3} and \eqn{W_f} is wood fresh weight in kg. Fresh density is measured in \eqn{\frac{kg}{m^3}}
#' @references la Marca, O. \emph{Elementi di dendrometria} 2004, Patron Editore (Bologna), p. 119
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
logBiomass <- function (x, lowerD, higherD, logLength, density) {
  lowerS   <- pi * (as.real(x[lowerD]) / 200)^2 
  higherS  <- pi * (as.real(x[higherD]) / 200)^2
  l        <- as.real(x[logLength])
  volume   <- (lowerS + higherS) / 2 * l
  volume * density
}

#' @title Returns the result of a pure quadratic equation
#'
#' @description Returns the result of the pure quadratic equation \eqn{Y = a + bX^2}
#' given \eqn{a}, \eqn{b} and \eqn{X}
#'
#' @param a the parameter \eqn{a} in the pure quadratic equation
#' @param b the parameter \eqn{a} in the pure quadratic equation
#' @param x the dependent variable
#' @return the dependent variable (\eqn{Y})
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
pureQuadraticEquation <- function(a, b, x) {
  a + b * x^2
}

#' @title Returns the result of an exponential equation
#'
#' @description Returns the result of the exponential equation \eqn{Y = a * X^b}
#' given \eqn{a}, \eqn{b} and \eqn{X}
#'
#' @param a the parameter \eqn{a} in the exponential equation
#' @param b the parameter \eqn{b} in the exponential equation
#' @param x the independent variable
#' @return the dependent variable (\eqn{Y})
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
powerEquation <- function(a, b, x) {
  a * x^b
}

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
  x0 <- toCartesianX(a0, d0)
  y0 <- toCartesianY(a0, d0)
  x1 <- toCartesianX(a1, d1)
  y1 <- toCartesianY(a1, d1)
  
  points(x0, y0)
  points(x1, y1)
  segments(x0, y0, x1, y1)
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


#' @title Computes masses of branches and logs
#'
#' @description Computes branches biomass using an allometric function provided in \code{object$allometryFUN} and logs weight using Smalian's formula.
#'
#' Branches are telled apart from logs in the raw data frame (\code{object$fieldData}) because their final diameter is 0 (ie they have a tip) whereas logs have a final diameter > 0.
#'
#' @param object an object of \code{treeData} class
#' @return an object of \code{treeData} class
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
    ## gets stem and cut branches (ie. diameter at tip > 0) biomass, by converting its fresh volume to dry weight
    fieldData$biomass[(fieldData$dTip > 0)] <- as.vector(
      apply(
        fieldData[(fieldData$dTip > 0),], 
        1, 
        logBiomass, 
        lowerD    = "dBase", 
        higherD   = "dTip", 
        logLength = "length",
        density
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


#' @title Computes cartesian coordinates and moments of branches and logs 
#'
#' @description A data frame is populated with branch and log masses, along with \eqn{x}, \eqn{y} cartesian coordinates and \eqn{x}, \eqn{y}, and \eqn{z} moments.
#' \eqn{z} coordinates and moments are calculated only if branches height from the ground (and tilt) have been measured in the field.
#'
#' @param object an object of class \code{vectors} 
#' @seealso \code{\link{getCoordinatesAndMoment}}
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
treeVectors <- function(object) {

  ## vectors data frame is populated
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

#' @title Computes the centre of mass of the tree
#'
#' @description The \eqn{x} coordinate of the centre of mass is defined as \eqn{\frac{\sum(m_ix_i)}{\sum(m_i)}} where \eqn{m_i} is the biomass of the \eqn{i^{th}} branch and \eqn{x_i} is the \eqn{x} coordinate of the \eqn{i^{th}} branch. \eqn{y} and \eqn{z} coordinates are similarly computed.
#' The centre of mass computation excludes branches to be pruned (ie: those whose \code{toBePruned} value is set to \code{TRUE}).
#'
#' @param object A data frame of class \code{vectors}
#' @return A vector holding \eqn{x}, \eqn{y}, \eqn{z} coordinates of the centre of mass
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

#' @title Plots branches, logs, and tree CM
#'
#' @description Plots branches, logs, and tree CM
#'
#' The 2d plot represents branch, log and tree CM as vectors pointing inwards. CMs vector radii are proportional to CM magnitude. Tree CM is connected to tree base by an arrow showing the direction the tree would take in case of it falling down. \eqn{z} coordinate of tree CM is printed alongside its vector (if branch height has been recorded in the field).
#' Branches to be pruned are not shown on graph.
#'
#' @param x      vectors object
#' @param y      unused
#' @param CM     Centre of mass object
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


#' @title Summary of Centre of Mass data
#'
#' @description Prints in a human-readable format the polar and cartesian coordinates of tree CM
#'
#' @param object An object of class \code{CM}
#' @param ...    Additional arguments, not used
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

#' @title Returns the biomass of a stone pine tree
#'
#' @description Returns total biomass of a stone pine tree (wood and leaves, dry state) in kg given the 
#' diameter at breast height, using an allometric equation
#'
#' @note Use this function at you own risk, it has been validated for trees (ie: >40 cm diameters). 
#' @note The allometric equation takes the form of a pure quadratic equation
#' @seealso \code{\link{pureQuadraticEquation}}
#' @references Cutini, A. and Hajny, M. and Gugliotta, O. and Manetti, M. and Amorini, E. 2009, Effetti della struttura del popolamento sui modelli di stima del volume e della biomassa epigea (Pineta di Castelfusano - Roma) \emph{Forest@@}, \bold{6}, 75--84 
#'   Tipo B
#' @param x a data frame holding diameters of branches
#' @param diameter the name of the column holding diameter of the x data frame, diameters should be in cm 
#' @return the total biomass of the branch of a stone pine (in kg, dry state)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
allometryCutini2009 <- function(x, diameter) {
  a <- -198.236
  b <- 0.620
  pureQuadraticEquation(a, b, as.real(x[diameter]))
}

#' @title Returns the biomass of a maritime pine branch
#'
#' @description Returns the woody biomass of a maritime pine branch (dry state, no leaves!) in kg given the 
#' diameter, using an allometric equation
#'
#' @note The allometric equation has been validated for <10 cm diameter branches, extrapolation on larger branches my yield unreasonable results.
#' @note The allometric equation takes the form of a power equation
#' @seealso \code{\link{powerEquation}}
#' @references Port\'{e}, A. and Trichet, P. and Bert, D. and Loustau, D. 2002, Allometric relationships for branch and tree woody biomass of Maritime pine (\emph{Pinus pinaster} Ait.) \emph{Forest Ecology and Management}, \bold{158}, 71--83
#' @param x a data frame holding diameters of branches
#' @param diameter the name of the column holding diameter of the x data frame, diameters should be in cm 
#' @return the woody biomass (dry state, no leaves!) of the branch of a maritime pine (in kg)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
allometryPorte2002 <- function(x, diameter) {
  a <- 21.228
  b <- 2.818
  powerEquation(a, b, as.real(x[diameter])) / 1000
}

#' @title Returns the fresh weight of a stone pine branch
#'
#' @description Returns the fresh biomass of a stone pine branch in kg given the 
#' diameter, using an allometric equation
#'
#' @note The allometric equation has been validated for 8-16 cm diameter branches. 
#' @note The allometric equation takes the form of a power equation
#' @seealso \code{\link{powerEquation}}
#' @references Data collected by A. Ascarelli, non linear regression by M. Bascietto
#' @param x a data frame holding diameters of branches
#' @param diameter the name of the column holding diameter of the x data frame, diameters should be in cm 
#' @return the fresh biomass of the branch of a stone pine (in kg)
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
allometryAsca2011 <- function(x, diameter) {
  a <- 0.7201
  b <- 1.8882
  powerEquation(a, b, as.real(x[diameter]))
}

