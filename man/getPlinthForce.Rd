\name{getPlinthForce}
\alias{getPlinthForce}
\title{Computes the force on the plinth on the ground}
\usage{
  getPlinthForce(l.stem, d, logs, treeMoment, CM)
}
\arguments{
  \item{l.stem}{the distance from tree base to anchor
  point, along the stem}

  \item{d}{the length of the cable (in metres)}

  \item{logs}{a data frame holding the selected logs (see
  \code{\link{logPathSelection}})}

  \item{treeMoment}{the moment of the tree as computed by
  \code{\link{calcMoment}}}

  \item{CM}{an object of \code{CM} class}
}
\value{
  a named list of 6 elements: \item{force}{the force in
  Newton on the plinth} \item{distanceOnGround}{the
  distance from tree base to the plinth}
  \item{anchorAlongStem}{the distance from tree base to the
  anchor (\emph{ie} \code{l.stem})} \item{cableLength}{the
  length of the cable (\emph{ie} \code{d})}
  \item{anchorHeight}{true height of the anchor over
  ground} \item{azimuth}{the azimuth of the plint relative
  to the tree base}
}
\description{
  To stabilize the tree a steel cable is connected from an
  anchor point on the tree to a plinth on the ground. The
  function computes the force on the plinth (needed to
  choose the appropriate steel cable and to build the
  plinth itself) and the maximum security azimuth (the
  angle relative to the North from the tree base). Force is
  computed by comparing the moment of the tree and the
  moment of the anchor, whose arm is the vector from tree
  base to the anchor point. The anchor point is defined as
  the distance from tree base, along the stem. Note that
  this distance equals anchor height only when the stem is
  perfectly vertical and straight.
}
\note{
  The function is vectorized both for anchor distance from
  tree base (\code{l.stem} parameter) and for cable length
  (\code{d}). It is not possible to pass invalid
  \code{l.stem} values, see \code{\link{anchorRange}}.
}
\examples{
library(treecm)
data(stonePine1TreeData)
vectors <- treeVectors(stonePine1TreeData)
CM <- centreOfMass(vectors)
treeMoment <- buildTreeMomentObject(
  centreOfMassModulus(CM)
  , treeTotalBiomass(stonePine1TreeData)
  , centreOfMassAngle(CM)
  )
treeMoment <- calcMoment(treeMoment)
logs <- logPathSelection(stonePine1TreeData)
plinth <- data.frame(getPlinthForce(10, 20, logs, getMoment(treeMoment), CM))
}
\seealso{
  Other Stabilization: \code{\link{anchorRange}},
  \code{\link{centreOfMassAngle}},
  \code{\link{centreOfMassModulus}},
  \code{\link{logPathSelection}},
  \code{\link{toCartesianXYZ}}
}

