\name{allometryCutini2009}
\alias{allometryCutini2009}
\title{Returns the biomass of a stone pine tree}
\usage{allometryCutini2009(x, diameter)
}
\description{Returns total biomass of a stone pine tree (wood and leaves, dry state) in kg given the 
diameter at breast height, using an allometric equation}
\note{Use this function at you own risk, it has been validated for trees (ie: >40 cm diameters).}
\note{The allometric equation takes the form of a pure quadratic equation}
\seealso{\code{\link{pureQuadraticEquation}}}
\references{Cutini, A. and Hajny, M. and Gugliotta, O. and Manetti, M. and Amorini, E. 2009, Effetti della struttura del popolamento sui modelli di stima del volume e della biomassa epigea (Pineta di Castelfusano - Roma) \emph{Forest@}, \bold{6}, 75--84 
Tipo B}
\value{the total biomass of the branch of a stone pine (in kg, dry state)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{a data frame holding diameters of branches}
\item{diameter}{the name of the column holding diameter of the x data frame, diameters should be in cm}
}

