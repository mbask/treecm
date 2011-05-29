#' @title Computes cartesian coordinates and moments of branches and logs 
#'
#' @description A data frame is populated with branch and log masses, along with \eqn{x}, \eqn{y} cartesian coordinates and \eqn{x}, \eqn{y}, and \eqn{z} moments.
#' \eqn{z} coordinates and moments are calculated only if branches height from the ground (and tilt) have been measured in the field.
#'
#' @param object an object of \code{treeData} class
#' @return an object of class \code{vectors} 
#' @seealso \code{\link{getCoordinatesAndMoment}}
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
treeVectors <- function(object) {

  ## vectors data frame is populated
  vectors <- subset(object$fieldData, select = c(azimuth, tipD, biomass, height, tilt, toBePruned))

  ## computes cartesian coordinates of branch tip and its x, y and z moments are added to vectors slot
  vectors <- cbind(vectors, 
    t(
      apply(
        vectors, 
        1, 
        getCoordinatesAndMoment, 
        angle      = "azimuth", 
        distance   = "tipD", 
        height     = "height",
        incl       = "tilt",
        mass       = "biomass",
        branchesCM = object$branchesCM
      )
    )
  )
  colnames(vectors) <- c("Azimuth", "Distance", "Biomass", "Height", "Tilt", "toBePruned", "x", "y", "mx", "my", "mz")
  class(vectors) <- c("vector", class(vectors))
  return(vectors)
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
