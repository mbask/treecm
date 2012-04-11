\name{allometryPorte2002}
\alias{allometryPorte2002}
\title{Returns the biomass of a maritime pine branch}
\usage{
  allometryPorte2002(x, diameter)
}
\arguments{
  \item{x}{a data frame holding diameters of branches}

  \item{diameter}{the name of the column holding diameter
  of the x data frame, diameters should be in cm}
}
\value{
  the woody biomass (dry state, no leaves!) of the branch
  of a maritime pine (in kg)
}
\description{
  Returns the woody biomass of a maritime pine branch (dry
  state, no leaves!) in kg given the diameter, using an
  allometric equation
}
\note{
  The allometric equation has been validated for <10 cm
  diameter branches, extrapolation on larger branches my
  yield unreasonable results.

  The allometric equation takes the form of a power
  equation
}
\references{
  Port\'{e}, A. and Trichet, P. and Bert, D. and Loustau,
  D. 2002, Allometric relationships for branch and tree
  woody biomass of Maritime pine (\emph{Pinus pinaster}
  Ait.) \emph{Forest Ecology and Management}, \bold{158},
  71--83
}
\seealso{
  \code{\link{powerEquation}}

  Other Biomass: \code{\link{allometryABDC}},
  \code{\link{allometryAsca2011}},
  \code{\link{allometryCutini2009}},
  \code{\link{logBiomass}}, \code{\link{powerEquation}},
  \code{\link{pureQuadraticEquation}},
  \code{\link{treeBiomass}}, \code{\link{treeTotalBiomass}}
}

