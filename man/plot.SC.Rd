\name{plot.SC}
\alias{plot.SC}
\title{Plots slenderness coefficient of branches}
\usage{\method{plot}{SC}(x, y, txtcol="grey80", ...)
}
\description{Plots the branches as arrows whose length is proportional to their slenderness coefficient}
\note{Two circles (or ellipses according to x and y scales) are drawn to encompass 
the 30 and 50 values for coefficient of slenderness. Branches with 50+ values for the coefficient of 
slenderness are considered dangerous. Please note that Mattheck coefficient is corrected to account 
for branch tilt (the more it deviates from the verticality the higher its coefficient)}
\value{\code{NULL}}
\seealso{\code{\link{treeSC}}}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{SC object}
\item{y}{unused}
\item{txtcol}{Colour of text labels, defaults to "grey80"}
\item{...}{Arguments to be passed to plot.default}
}

