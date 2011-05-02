\name{toCartesianX}
\alias{toCartesianX}
\title{Computes the x cartesian coordinate...}
\usage{toCartesianX(angle, distance)
}
\description{Computes the x cartesian coordinate}
\details{Computes the x cartesian coordinate from a set of polar coordinates}
\value{The x coordinate (a real number) measured in the same unit as the distance parameter}
\note{Attention: the function assumes the angle is measured clockwise whereas
trigonometric functions require a conventional counterclockwise 
measured angle. Thus the function computes x coordinate as the sine of 
the angle, enabling a correct representation of them on a cartesian plot.}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{angle}{The angle in degree (measured clockwise from the North or from 
any other relevant direction defined in the field)}
\item{distance}{The distance}
}

