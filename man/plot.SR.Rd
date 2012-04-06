\name{plot.SR}
\alias{plot.SR}
\title{Plots slenderness ratio of branches}
\usage{
  \method{plot}{SR} (x, y = NULL, safeSR = 70, ...)
}
\arguments{
  \item{x}{SR object}

  \item{y}{unused}

  \item{safeSR}{SR threshold, risky branches are
  red-coloured}

  \item{...}{Arguments to be passed to plot.default}
}
\value{
  \code{NULL}
}
\description{
  Plots the branches as arrows whose length is proportional
  to their slenderness ratio. A red circle holds ``safe''
  branches (\eqn{SR_c \leq 70}).
}
\note{
  A circle is drawn to encompass the 70- values for
  slenderness ratio. Branches with 70+ values for the
  slenderness ratio are considered dangerous. Please note
  that Mattheck coefficient is corrected to account for
  branch tilt (the more it deviates from the verticality
  the higher its coefficient)
}
\seealso{
  \code{\link{treeSR}}
}

