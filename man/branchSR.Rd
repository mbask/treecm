\name{branchSR}
\alias{branchSR}
\title{Compute the slenderness ratio}
\usage{branchSR(x, diameter, length, tilt)
}
\description{slenderness ratio is an important index of stability of trees and branches}
\note{The coefficient takes into account branch angle: 
\eqn{SL_c=\frac{L}{D} \cdot (1 + cos \alpha)}, 
where \eqn{\alpha} is the branch angle (0 degrees = horizontal, 90 degrees vertical),
\eqn{L} is branch length in m, \eqn{D} is branch diameter in cm
Vertical branches have \eqn{SL = SL_c}}
\value{slenderness ratio}
\references{Mattheck, C. and Breloer, H. \emph{The Body Language of Trees: A Handbook for Failure Analysis (Research for Amenity Trees)} 1995, HMSO (London)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{the data frame holding the measures needed to perform the computation}
\item{diameter}{The name of the data frame column holding diameter of the branch}
\item{length}{The name of the data frame column holding length of the branch}
\item{tilt}{The name of the data frame column holding tilt of the branch}
}

