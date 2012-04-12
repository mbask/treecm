\name{allometryCutini2009}
\alias{allometryCutini2009}
\title{Returns the biomass of a stone pine tree}
\usage{
  allometryCutini2009(x, diameter)
}
\arguments{
  \item{x}{a data frame holding diameters of branches}

  \item{diameter}{the name of the column holding diameter
  of the x data frame, diameters should be in cm}
}
\value{
  the total biomass of the branch of a stone pine (in kg,
  dry state)
}
\description{
  Returns total biomass of a stone pine tree (wood and
  leaves, dry state) in kg given the diameter at breast
  height, using an allometric equation
}
\note{
  Use this function at you own risk, it has been validated
  for trees (ie: >24 cm diameters).

  The allometric equation takes the form of a pure
  quadratic equation
}
\references{
  Cutini, A. and Hajny, M. and Gugliotta, O. and Manetti,
  M. and Amorini, E. 2009, Effetti della struttura del
  popolamento sui modelli di stima del volume e della
  biomassa epigea (Pineta di Castelfusano - Roma)
  \emph{Forest@}, \bold{6}, 75--84 Tipo B
}
\seealso{
  \code{\link{pureQuadraticEquation}}

  Other Biomass: \code{\link{allometryABDC}},
  \code{\link{allometryAsca2011}},
  \code{\link{allometryPorte2002}},
  \code{\link{logBiomass}}, \code{\link{powerEquation}},
  \code{\link{pureQuadraticEquation}},
  \code{\link{treeBiomass}}, \code{\link{treeTotalBiomass}}
}

