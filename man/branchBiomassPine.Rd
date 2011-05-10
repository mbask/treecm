\name{branchBiomassPine}
\alias{branchBiomassPine}
\title{Returns total biomass of a tree or branch (wood and leaves, dry state) in kg given the...}
\usage{branchBiomassPine(x, diameter)
}
\description{Returns total biomass of a tree or branch (wood and leaves, dry state) in kg given the 
diameter at breast height, using an allometric equation for stone pine}
\references{Cutini, A. and Hajny, M. and Gugliotta, O. and Manetti, M. and Amorini, E. 2009, Effetti della struttura del popolamento sui modelli di stima del volume e della biomassa epigea (Pineta di Castelfusano - Roma) \emph{Forest@}, \bold{6}, 75--84 
Tipo B}
\value{the total biomass of the branch of a stone pine (in kg, dry state)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{a data.frame of branches along with their diameters as a column}
\item{diameter}{the name (a character) of the column holding diameter of the x data.frame, diameters should be in cm}
}

