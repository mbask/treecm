\name{centreOfMassAngle}
\alias{centreOfMassAngle}
\title{Returns the angle between CM modulus and the tree weight vector}
\usage{
  centreOfMassAngle(object)
}
\arguments{
  \item{object}{an object of \code{CM} class}
}
\value{
  a real number in radians
}
\description{
  Returns the angle between CM modulus and the tree weight
  vector
}
\note{
  This function is mainly needed to compute the moment of
  the tree. The angle is need to compute the projection of
  the tree weight normal to the CM modulus
}
\examples{
library(treecm)
data(treeData)
vectors <- treeVectors(treeData)
CM <- centreOfMass(vectors)
print(centreOfMassAngle(CM))
treeMoment <- buildTreeMomentObject(
 centreOfMassModulus(CM)
 , treeTotalBiomass(vectors)
 , centreOfMassAngle(CM)
 )
}
\seealso{
  Other Stabilization: \code{\link{anchorRange}},
  \code{\link{centreOfMassModulus}},
  \code{\link{getPlinthForce}},
  \code{\link{logPathSelection}},
  \code{\link{toCartesianXYZ}}
}

