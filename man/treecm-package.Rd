\docType{package}
\name{treecm-package}
\alias{treecm}
\alias{treecm-package}
\title{Assessment of x, y, z coordinates of centre of mass of trees}
\description{
  Given a few data about branchiness of a tree the package
  computes and plots the centre of mass of the tree itself.
  The centre of mass is a crucial data for arborists in
  order to consolidate a tree using steel or dynamic
  cables. The tree stem is ideally sectioned in logs. The
  weight of tree components is assessed based on \itemize{
  \item the sum of volume of stem logs \item the sum of
  branches biomass } Field measures to be taken on logs and
  branches are described in the \code{\link{fieldData}}
  dataset. In order to help the arborist in the pruning
  selection process a simple plot of branch coefficient of
  slenderness is implemented.
}
\note{
  \bold{Branch biomass} is computed by allometric equations
  relating its weight (wood + leaves) to its diameter at
  point of insertion on the stem. \bold{Log biomass} is
  computed by converting its volume to weight using wood
  fresh density. Volume is computed using Smalian's formula
  (see \code{\link{logBiomass}} description). A sample
  \code{.CSV} file is provided to guide through data
  filling in the field
}
\examples{
data(treeData)
vectors  <- treeVectors(treeData)
CM       <- centreOfMass(vectors)
plot(vectors,
   main = "Centre Of Mass",
   col = "grey30",
   txtcol = "grey30")
plot(CM)
summary(CM)
}
\author{
  Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}
}
\references{
  Source code is hosted at GitHub
  (\url{https://github.com/mbask/treecm})
}
\seealso{
  \code{\link{logBiomass}} \code{\link{fieldData}}
}
\keyword{package}

