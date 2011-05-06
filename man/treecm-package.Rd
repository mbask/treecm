\name{treecm-package}
\alias{treecm-package}
\alias{treecm-package}
\title{Assessment of x, y, z coordinates of centre of mass of trees}
\description{Assessment of x, y, z coordinates of centre of mass of trees}
\details{\tabular{ll}{
Package: \tab treecm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-05-01\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab no\cr
}

Given a few data about branchiness of a tree the package computes and plots the centre of mass of the tree itself.
The centre of mass is a crucial data for arborists in order to consolidate a tree using steel or dynamic cables.
Field measures to be taken include:
\itemize{
\item{Diameter at base: }{diameter at insertion point on the trunk, for branches, diameter of the lower section for logs, mandatory}
\item{Diameter at top: }{0 for branches, diameter of the higher section for logs, optional, defaults to 0}
\item{Distance: }{Length of branch or log projection on the ground, starting from tree base to tip of branch or log, mandatory}
\item{Length: }{Length of logs (no use for branches), mandatory}
\item{Height: }{height of branch insertion on the trunk or height of lower section of the log, to be used to compute z coordinate of CM, optional, defaults to NA}
\item{Direction: }{mean angle of orientation of the branch or log measured from the base of the tree (usually with magnetic north as reference, measured clockwise), mandatory}
\item{Tilt: }{mean branch or log tilt from the horizontal plane (eg a vertical branch is 90 degrees, an horizontal branch is 0 degrees), to be used to compute z coordinate of CM, optional, defaults to 0. Note however that the tree tip should be considered as a branch, not a log, in order to account for foliage biomass. In this case tilt value should be recorded otherwise it would default to 0, ie an horizontal branch} 
\item{To be pruned: }{a boolean value, optional, defaults to FALSE}
}
Branch biomass is computed by allometric equations relating its dry weight (wood + leaves) to its diameter at point of insertion on the trunk.
Log biomass is computed by converting its volume to weight using wood fresh density. Volume is computed using Smalian's formula (see logBiomass description)}
\seealso{\code{\link{logBiomass}}
\code{\link{fieldData}}}
\note{An example \code{.CSV} file is provided to guide through data filling in the field}
\alias{treecm}
\docType{package}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\keyword{package}
\references{Source code is hosted at GitHub (\url{https://github.com/mbask/treecm})}
\examples{data(treeData)
vectors  <- treeVectors(treeData)
CM       <- centreOfMass(vectors)
plot.vectors(vectors, CM = CM, main = "Centre Of Mass", col = "grey30", txtcol = "grey30")
summary(CM)}

