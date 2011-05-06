\name{branchBiomassPineAsca}
\alias{branchBiomassPineAsca}
\title{Returns the fresh biomass of a branch in kg given the...}
\usage{branchBiomassPineAsca(x, diameter)
}
\description{Returns the fresh biomass of a branch in kg given the 
diameter, using an allometric equation for stone pine branches}
\note{Important: the allometric equation has been validated for 8-16 cm diameter branches}
\references{Data collected by A. Ascarelli, non linear regression by M. Bascietto}
\value{the fresh biomass of the branch of a stone pine (in kg)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{a data.frame of branches along with their diameters as a column}
\item{diameter}{the name (a character) of the column holding diameter of the x data.frame, diameters should be in cm}
}

