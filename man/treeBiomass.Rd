\name{treeBiomass}
\alias{treeBiomass}
\title{Computes masses of branches and logs}
\usage{treeBiomass(object)
}
\description{Computes branches biomass using an allometric function provided in \code{object$allometryFUN} and logs weight using Smalian's formula.}
\details{Branches are telled apart from logs in the raw data frame (\code{object$fieldData}) because their final diameter is 0 (ie they have a tip) whereas logs have a final diameter > 0.}
\value{an object of \code{treeData} class}
\seealso{\code{\link{logBiomass}}}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{object}{an object of \code{treeData} class}
}

