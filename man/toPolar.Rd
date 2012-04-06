\name{toPolar}
\alias{toPolar}
\title{Converts cartesian (x, y) into polar (angle, distance) coordinates}
\usage{
  toPolar(x, y)
}
\arguments{
  \item{x}{Abscissa coordinate}

  \item{y}{Ordinate coordinate}
}
\value{
  A vector holding angle in degrees and distance in the
  same unit as \eqn{x} and \eqn{y}
}
\description{
  Converts cartesian coordinates (\eqn{x}, \eqn{y}) into
  polar (angle, distance) ones, assuming (0, 0) as origin
  of axes and, incidentally, the position of tree base
}

