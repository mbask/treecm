\name{logBiomass}
\alias{logBiomass}
\title{Estimates the wood biomass of logs and truncated branches by...}
\usage{logBiomass(x, lowerD, higherD, logLength, basalDensity)
}
\description{Estimates the wood biomass of logs and truncated branches by
computing their volume (using Smalian's formula) and converting it
to dry weight using basal density.
Smalian's formula: \eqn{V=\frac{Sb+Sd}{2}l} where \eqn{V} is the log volume, 
\eqn{Sb} is the aerea of the basal (lower) section, \eqn{Sd} is the 
area of the higher section and \eqn{l} is the length of the log.}
\note{Attention: diameters used to compute section areas should be measured under the bark layer! When this is not the case and diameters include bark thickness the log biomass is over-estimated!
If no direct measures of basal density are available one can use mean values found in literature (http://www.ricercaforestale.it/index.php?module=CMpro&func=viewpage&pageid=713)
\tabular{ll}{
Abete rosso\tab  380\cr
Abete bianco\tab 380\cr
Larice\tab       560\cr
Pini montani\tab 470\cr
Pini mediterranei\tab 530\cr
Faggio\tab 610\cr
Cerro\tab 690\cr
Faggio\tab 610\cr
Castagno\tab 490\cr
Aceri\tab 660\cr
Altre querce\tab 650\cr
Cerro\tab 690\cr
Querce sempreverdi\tab 720\cr
Altre latifoglie\tab 530\cr
Eucalitti\tab 540\cr
}}
\references{la Marca, O. Elementi di dendrometria, 2004, Patron Editore (Bologna), p. 119}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{the data.frame holding the measures needed to perform the estimation}
\item{lowerD}{The name of the data.frame column holding diameter of the lower section in cm}
\item{higherD}{The name of the data.frame column holding the diameter of the higher section (usually smaller!) in cm}
\item{logLength}{The name of the data.frame column holding the length of the log or branch in m}
\item{basalDensity}{The name of the data.frame column holding the basal Density of the wood, defined as \eqn{D=\frac{V_f}{W_d}} where \eqn{V_f} is wood volume measured in the field (i.e. satured with water) in \eqn{m^3} and \eqn{W_d} is wood dry weight in kg. Basal density is measured in \eqn{\frac{kg}{m^3}}}
}

