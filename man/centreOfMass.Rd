\name{centreOfMass}
\alias{centreOfMass}
\title{Computes the centre of mass of the tree...}
\usage{centreOfMass(object)
}
\description{Computes the centre of mass of the tree}
\details{The \eqn{x} coordinate of the centre of mass is defined as \eqn{\frac{\sum(m_ix_i)}{\sum(m_i)}} where \eqn{m_i} is the biomass of the \eqn{i^{th}} branch and \eqn{x_i} is the \eqn{x} coordinate of the \eqn{i^{th}} branch. \eqn{y} coordinate is similarly computed.
The centre of mass computation excludes branches to be pruned}
\value{A vector holding x, y, z coordinates of the centre of mass}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{object}{A data.frame of class CM}
\item{...}{Arguments to be passed to plot.default}
}

