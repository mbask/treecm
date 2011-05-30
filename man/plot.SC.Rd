\name{plot.SC}
\alias{plot.SC}
\title{Plots slenderness coefficient of branches}
\usage{\method{plot}{SC}(x, y, safeSC=70, ...)
}
\description{Plots the branches as arrows whose length is proportional to their slenderness coefficient.
A red circle holds ``safe'' branches (\eqn{SC_c<SC_{70}}).}
\note{A circleis drawn to encompass 
the 70- values for coefficient of slenderness. Branches with 70+ values for the coefficient of 
slenderness are considered dangerous. Please note that Mattheck coefficient is corrected to account 
for branch tilt (the more it deviates from the verticality the higher its coefficient)}
\value{\code{NULL}}
\seealso{\code{\link{treeSC}}}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{SC object}
\item{y}{unused}
\item{safeSC}{SC threshold, risky branches are red-coloured}
\item{...}{Arguments to be passed to plot.default}
}

