\name{calcMoment}
\alias{calcMoment}
\title{Computes moment and returns the moment object}
\usage{
  calcMoment(object, g = 9.81)
}
\arguments{
  \item{object}{an instance of moment class}

  \item{g}{the standard gravity}
}
\value{
  the updated moment object
}
\description{
  Moment is computed as \eqn{M=l \cdot F}, where l is
  moment arm, F is the component of the force (\emph{mass
  times g}) normal to moment arm
}
\seealso{
  \code{\link{getPlinthForce}}

  Other Stabilization momentClass:
  \code{\link{buildMomentObject}},
  \code{\link{buildTreeMomentObject}},
  \code{\link{getMoment}}
}

