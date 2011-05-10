\name{branchBiomassPinePorte}
\alias{branchBiomassPinePorte}
\title{Returns the woody biomass of a branch (dry state, no leaves!) in kg given the...}
\usage{branchBiomassPinePorte(x, diameter)
}
\description{Returns the woody biomass of a branch (dry state, no leaves!) in kg given the 
diameter, using an allometric equation for maritime pine}
\note{Important: the allometric equation has been validated for 1-10 cm diameter branches}
\references{Port\'{e}, A. and Trichet, P. and Bert, D. and Loustau, D. 2002, Allometric relationships for branch and tree woody biomass of Maritime pine (\emph{Pinus pinaster} Ait.) \emph{Forest Ecology and Management}, \bold{158}, 71--83}
\value{the woody biomass (dry state, no leaves!) of the branch of a maritime pine (in kg)}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{a data.frame of branches along with their diameters as a column}
\item{diameter}{the name (a character) of the column holding diameter of the x data.frame, diameters should be in cm}
}

