\name{toCartesianY}
\alias{toCartesianY}
\title{Computes the y cartesian coordinate}
\usage{toCartesianY(angle, distance)
}
\description{Computes the \eqn{y} cartesian coordinate from a set of polar coordinates}
\note{The function assumes the angle is measured clockwise whereas
trigonometric functions require a conventional counterclockwise 
measured angle. Thus the function computes \eqn{y} coordinate as the cosine of 
the angle, enabling a correct representation of them on a cartesian plot.}
\value{The \eqn{y} coordinate expressed in the same unit as the distance parameter}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{angle}{The angle in degree (measured clockwise from the North or from 
any other relevant bearing system defined in the field)}
\item{distance}{The distance}
}

