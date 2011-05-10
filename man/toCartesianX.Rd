\name{toCartesianX}
\alias{toCartesianX}
\title{Computes the x cartesian coordinate}
\usage{toCartesianX(angle, distance)
}
\description{Computes the \eqn{x} cartesian coordinate from a set of polar coordinates}
\value{The \eqn{x} coordinate expressed in the same unit as the distance argument}
\note{The function assumes the angle is measured clockwise whereas
trigonometric functions require a conventional counterclockwise 
measured angle. Thus the function computes \eqn{x} coordinate as the sine of 
the angle, enabling a correct representation of them on a cartesian plot.}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{angle}{The angle in degrees (measured clockwise from the North or  
any other relevant bearing system defined in the field)}
\item{distance}{The distance}
}

