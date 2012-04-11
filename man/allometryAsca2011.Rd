\name{allometryAsca2011}
\alias{allometryAsca2011}
\title{Returns the fresh weight of a stone pine branch}
\usage{
  allometryAsca2011(x, diameter)
}
\arguments{
  \item{x}{a data frame holding diameters of branches}

  \item{diameter}{the name of the column holding diameter
  of the x data frame, diameters should be in cm}
}
\value{
  the fresh biomass of the branch of a stone pine (in kg)
}
\description{
  Returns the fresh biomass of a stone pine branch in kg
  given the diameter, using an allometric equation
}
\note{
  The allometric equation has been validated for 8-16 cm
  diameter branches.

  The allometric equation takes the form of a power
  equation
}
\references{
  Data collected by A. Ascarelli, non linear regression by
  M. Bascietto
}
\seealso{
  \code{\link{powerEquation}}

  Other Biomass: \code{\link{allometryABDC}},
  \code{\link{allometryCutini2009}},
  \code{\link{allometryPorte2002}},
  \code{\link{logBiomass}}, \code{\link{powerEquation}},
  \code{\link{pureQuadraticEquation}},
  \code{\link{treeBiomass}}, \code{\link{treeTotalBiomass}}
}

