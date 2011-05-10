\name{allometryAsca2011}
\alias{allometryAsca2011}
\title{Returns the fresh weight of a stone pine branch}
\usage{allometryAsca2011(x, diameter)
}
\description{Returns the fresh biomass of a stone pine branch in kg given the 
diameter, using an allometric equation}
\note{The allometric equation has been validated for 8-16 cm diameter branches.}
\note{The allometric equation takes the form of a power equation}
\seealso{\code{\link{powerEquation}}}
\references{Data collected by A. Ascarelli, non linear regression by M. Bascietto}
\value{the fresh biomass of the branch of a stone pine (in kg)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{a data frame holding diameters of branches}
\item{diameter}{the name of the column holding diameter of the x data frame, diameters should be in cm}
}

