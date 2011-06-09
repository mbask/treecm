\name{allometryABDC}
\alias{allometryABDC}
\title{Returns the fresh weight of a stone pine branch}
\usage{allometryABDC(x, diameter)
}
\description{Returns the fresh biomass of a stone pine branch in kg given the 
diameter, using an allometric equation}
\note{The allometric equation has been validated for 5-16 cm diameter branches.}
\note{The allometric equation takes the form of a power equation. This equation 
yields more correct results than \code{\link{allometryAsca2011}} since it has been
built on a wider range of branch diameters and it superseeds it.}
\seealso{\code{\link{powerEquation}}}
\references{Data collected by A. Ascarelli and integrated by small diameter branches by 
M. Bascietto and B. De Cinti, non linear regression by M. Bascietto}
\value{the fresh biomass of the branch of a stone pine (in kg)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{a data frame holding diameters of branches}
\item{diameter}{the name of the column holding diameter of the x data frame, diameters should be in cm}
}

