\name{treeVectors}
\alias{treeVectors}
\title{Computes cartesian coordinates and moments of branches and logs}
\usage{
  treeVectors(object)
}
\arguments{
  \item{object}{an object of \code{treeData} class}
}
\value{
  an object of class \code{vectors}
}
\description{
  A data frame is populated with branch and log masses,
  along with \eqn{x}, \eqn{y} cartesian coordinates and
  \eqn{x}, \eqn{y}, and \eqn{z} moments. \eqn{z}
  coordinates and moments are calculated only if branches
  height from the ground (and tilt) have been measured in
  the field.
}
\seealso{
  \code{\link{getCoordinatesAndMoment}}
}

