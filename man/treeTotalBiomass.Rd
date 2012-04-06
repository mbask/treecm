\name{treeTotalBiomass}
\alias{treeTotalBiomass}
\title{Returns the total biomass of the tree}
\usage{
  treeTotalBiomass(object)
}
\arguments{
  \item{object}{an object of \code{vector} class}
}
\value{
  a real number
}
\description{
  This is just a helper function, it sums the biomass of
  all logs and branches, as computed by
  \code{\link{treeBiomass}}
}
\note{
  This function is mainly needed to compute the moment of
  the tree. The tree biomass (multiplied by g) is the tree
  force applied to its CM.
}
\examples{
library(treecm)
data(treeData)
vectors <- treeVectors(treeData)
CM <- centreOfMass(vectors)
print(treeTotalBiomass(vectors))
treeMoment <- buildTreeMomentObject(
 centreOfMassModulus(CM)
 , treeTotalBiomass(vectors)
 , centreOfMassAngle(CM)
 )
}
\seealso{
  Other Stabilization: \code{\link{anchorRange}},
  \code{\link{centreOfMassAngle}},
  \code{\link{centreOfMassModulus}},
  \code{\link{getPlinthForce}},
  \code{\link{logPathSelection}},
  \code{\link{toCartesianXYZ}}
}

