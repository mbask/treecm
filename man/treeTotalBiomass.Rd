\name{treeTotalBiomass}
\alias{treeTotalBiomass}
\title{Returns the total biomass of the tree}
\usage{
  treeTotalBiomass(treeData)
}
\arguments{
  \item{treeData}{A named list that includes a
  \code{fieldData} data frame element, holding a
  \code{biomass}-named column. Note that the \code{biomass}
  column is added to the data frame by a previous call to
  \code{\link{treeBiomass}} function}
}
\value{
  a real number or \code{FALSE} is the \code{biomass}
  column is \code{NA}
}
\description{
  This is just a helper function, it sums the biomass of
  all logs and branches, as previously computed by
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
}
\seealso{
  Other Biomass: \code{\link{allometryABDC}},
  \code{\link{allometryAsca2011}},
  \code{\link{allometryCutini2009}},
  \code{\link{allometryPorte2002}},
  \code{\link{logBiomass}}, \code{\link{powerEquation}},
  \code{\link{pureQuadraticEquation}},
  \code{\link{treeBiomass}}
}

