\name{treeBiomass}
\alias{treeBiomass}
\title{Computes masses of branches and logs...}
\usage{treeBiomass(object)
}
\description{Computes masses of branches and logs}
\details{Computes branches biomass using an allometric function provided in \code{allometryFUN} and logs weight using Smalian's formula. Branches are telled apart from logs in the raw data.frame (\code{fieldData}) because their final diameter is 0 (ie they have a tip) whereas logs have a final diameter > 0.}
\seealso{\code{\link{logBiomass}}}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{object}{a list holding tree data}
}

