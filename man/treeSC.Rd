\name{treeSC}
\alias{treeSC}
\title{Computes slenderness coefficient for tree branches}
\usage{treeSC(treeObject, vectorObject)
}
\description{Slenderness coefficient is an important index of stability of trees and branches}
\note{The coefficient takes into account branch angle: 
\eqn{SL_c=\frac{L}{D} \cdot (1 + cos(a))}, 
where \eqn{a} is the branch angle (0 degrees = horizontal, 90 degrees vertical). 
Vertical branches have \eqn{SL = SL_c}}
\value{an object of class \code{SC}}
\references{Mattheck, C. and Breloer, H. \emph{The Body Language of Trees: A Handbook for Failure Analysis (Research for Amenity Trees)} 1995, HMSO (London)}
\seealso{\code{\link{branchSC}}}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{treeObject}{an object of \code{treeData} class}
\item{vectorObject}{an object of \code{vectors} class}
}

