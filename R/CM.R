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
  M <- list(
    x = col.sums[["mx"]] / col.sums[["Biomass"]], 
    y = col.sums[["my"]] / col.sums[["Biomass"]], 
    z = col.sums[["mz"]] / col.sums[["Biomass"]]
  )
  class(M) <- c("CM", class(M))
  return(M)
}


#' @title Summary of Centre of Mass data
#'
#' @description Prints in a human-readable format the polar and cartesian coordinates of tree CM
#'
#' @param object An object of class \code{CM}
#' @param ...    Additional arguments, not used
#' @return       \code{NULL}
#' @method summary CM
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
summary.CM <- function(object, ...) {

  cat("Coordinates of the centre of mass:\n")
  cat("Cartesian (x/m, y/m, z/m):", 
    sprintf("%.2f", object$x), ",", 
    sprintf("%.2f", object$y), ",", 
    sprintf("%.2f", object$z), "\n"
  )

  polar <- toPolar(object$x, object$y)
  cat("Polar (angle/degrees, distance/m, height/m):", 
    polar[1], ",", 
    sprintf("%.2f", polar[2]), ",", 
    sprintf("%.2f", object["z"]), "\n"
  )
}


#' @title Plots tree CM
#'
#' @description Plots tree centre of mass as a layer on top of the \code{plot.vector}.
#'
#' CMs vector radii are proportional to CM magnitude. Tree CM is connected to tree base by an arrow showing the direction the tree would take in case of it falling down. \eqn{z} coordinate of tree CM is printed alongside its vector (if branch height has been recorded in the field).
#'
#' @param x      CM object
#' @param y      unused
#' @param ...    Arguments to be passed to plot.default
#' @return \code{NULL}
#' @method plot CM
#' @export
#' @author Marco Bascietto \email{marco.bascietto@@ibaf.cnr.it}
plot.CM <- function(x, y = NULL, ...) {
  chw <- par()$cxy[1] 
  cmText <- paste("CM (z=", sprintf("%.2f", x["z"]), ")")
  points(x$x, x$y, pch = 13, col = 2, cex = 3)
  text(
    x$x - chw, 
    x$y - chw, 
    labels = cmText, 
    adj = 0, 
    cex = 0.8,
    col = 2
    ) 
  arrows(0, 0, x$x, x$y, col = 2, lwd = 2) 
}