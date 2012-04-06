\name{plot.vector}
\alias{plot.vector}
\title{Plots branches and logs}
\usage{
  \method{plot}{vector} (x, y = NULL, txtcol = "grey80",
    ...)
}
\arguments{
  \item{x}{vectors object}

  \item{y}{unused}

  \item{txtcol}{Colour of text labels, defaults to
  "grey80"}

  \item{...}{Arguments to be passed to plot.default}
}
\value{
  \code{NULL}
}
\description{
  Plots branches and logs

  The 2d plot represents branches and logs as vectors
  pointing inwards. Branches to be pruned are not shown on
  graph.
}

