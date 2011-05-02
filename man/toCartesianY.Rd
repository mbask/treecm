\name{toCartesianY}
\alias{toCartesianY}
\title{Computes the y cartesian coordinate...}
\usage{toCartesianY(angle, distance)
}
\description{Computes the y cartesian coordinate}
\details{Computes the y cartesian coordinate from a set of polar coordinates}
\note{Attention: the function assumes the angle is measured clockwise whereas
trigonometric functions require a conventional counterclockwise 
measured angle. Thus the function computes x coordinate as the cosine of 
the angle, enabling a correct representation of them on a cartesian plot.}
\value{The y coordinate (a real number) measured in the same unit as the distance parameter}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{angle}{The angle in degree (measured clockwise from the North or from 
any other relevant direction defined in the field)}
\item{distance}{The distance}
}

