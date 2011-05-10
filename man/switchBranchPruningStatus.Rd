\name{switchBranchPruningStatus}
\alias{switchBranchPruningStatus}
\title{Sets pruning status of a branch}
\usage{switchBranchPruningStatus(object, value)
}
\description{Switches pruning status of a record in the raw data frame from field measures}
\note{Method \code{\link{treeVectors}} must be invoked to take changes into effect}
\value{the updated list}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{object}{the object of class \code{treeData}}
\item{value}{number of row or vector of numbers of rows to have pruning status switched}
}
\examples{data(treeData)
treeData <- switchBranchPruningStatus(treeData, c(7, 11, 13, 15, 17, 23, 19, 22, 18, 25, 8, 10))
CM       <- centreOfMass(treeVectors(treeData))
summary(CM)}

