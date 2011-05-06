\name{importFieldData}
\alias{importFieldData}
\title{Imports field data from csv file...}
\usage{importFieldData(fileName, basalDensity, branchesAllometryFUN, bCM=1)
}
\description{Imports field data from csv file}
\details{Imports csv file holding field recorded data, and sets a list holding other key data}
\seealso{\code{\link{getCoordinatesAndMoment}}}
\value{a list holding the data}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{fileName}{Name of csv file holding field data}
\item{basalDensity}{Basal Density of wood of the tree}
\item{branchesAllometryFUN}{the function that should compute branch biomass from its diameter}
\item{bCM}{Estimated position of the centre of mass of branches, a real number from 0.01 (CM at branch base) to 1.00 (CM at branch tip). As a rule of thumb, average live branches, with an average amount of foliage, have CM approx. from 1/3 to 2/3 of their length. bCM = 1.0 (default value)}
}

