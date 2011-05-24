\name{branchSC}
\alias{branchSC}
\title{Compute the slenderness coefficient}
\usage{branchSC(x, diameter, length, tilt)
}
\description{Slenderness coefficient is an important index of stability of trees and branches}
\note{The coefficient takes into account branch angle: 
\eqn{SL_c=\frac{L}{D} \cdot (1 + cos(a))}, 
where \eqn{a} is the branch angle (0 degrees = horizontal, 90 degrees vertical). 
Vertical branches have \eqn{SL = SL_c}}
\value{Slenderness coefficient}
\references{Mattheck, C. and Breloer, H. \emph{La stabilit\`{a} degli alberi} 1993, Il Verde Editoriale (Milano), 281 pp}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{the data frame holding the measures needed to perform the computation}
\item{diameter}{The name of the data frame column holding diameter of the branch}
\item{length}{The name of the data frame column holding length of the branch}
\item{tilt}{The name of the data frame column holding tilt of the branch}
}

