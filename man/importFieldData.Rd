\name{importFieldData}
\alias{importFieldData}
\title{Imports field data from csv file}
\usage{
  importFieldData(fileName, dst, branchesAllometryFUN,
    bCM = 1)
}
\arguments{
  \item{fileName}{Name of csv file holding field data}

  \item{dst}{Fresh density of wood of the tree}

  \item{branchesAllometryFUN}{the function that should
  compute branch biomass from its diameter}

  \item{bCM}{Estimated position of the centre of mass of
  branches, a real number from 0.01 (CM at branch base) to
  1.00 (CM at branch tip). As a rule of thumb, average live
  branches, with an average amount of foliage, have CM
  approx. from \eqn{1/3} to \eqn{2/3} of their length. bCM
  = 1.0 (default value)}
}
\value{
  a list of 4 elements: field data, wood fresh density,
  allometryFUN function and branches CM
}
\description{
  Imports \code{csv} file holding field recorded data
  returning a list holding field and other key data
  provided as arguments. Missing data for \code{dTip} and
  \code{tilt} is defaulted to 0, missing data for
  \code{toBePruned} is defaulted to \code{FALSE}.
}
\seealso{
  \code{\link{getCoordinatesAndMoment}}
}

