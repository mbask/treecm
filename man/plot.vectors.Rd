\name{plot.vectors}
\alias{plot.vectors}
\title{Plots branches, logs, and tree CM}
\usage{\method{plot}{vectors}(x, y, CM, txtcol="grey80", ...)
}
\description{Plots branches, logs, and tree CM}
\details{The 2d plot represents branch, log and tree CM as vectors pointing inwards. CMs vector radii are proportional to CM magnitude. Tree CM is connected to tree base by an arrow showing the direction the tree would take in case of it falling down. \eqn{z} coordinate of tree CM is printed alongside its vector (if branch height has been recorded in the field).
Branches to be pruned are not shown on graph.}
\value{\code{NULL}}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{x}{vectors object}
\item{y}{unused}
\item{CM}{Centre of mass object}
\item{txtcol}{Colour of text labels, defaults to "grey80"}
\item{...}{Arguments to be passed to plot.default}
}

