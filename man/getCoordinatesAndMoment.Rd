\name{getCoordinatesAndMoment}
\alias{getCoordinatesAndMoment}
\title{Returns the coordinates of centre of mass of branches and logs}
\usage{getCoordinatesAndMoment(object, angle, distance, height, incl, mass, branchesCM)
}
\description{Computes the cartesian coordinates of centre of mass of branches and 
logs along with their \eqn{x}, \eqn{y}, \eqn{z} moments}
\details{The \eqn{x} and \eqn{y} coordinates are computed from the polar coordinates (angle and distance, 
defined as the length of its projection on ground), measured in the field. 
The \eqn{z} coordinate is computed by adding the height of branch insertion on the stem 
(measured in the field) to the height of the branch (calculated through its 
mean tilt, in case it was measured in the filed).
The \eqn{x}, \eqn{y}, \eqn{z} coordinates are corrected to take into account where the actual
centre of mass lies on the branches themselves by multiplying them by branchesCM,
a real number from 0.01 (CM at branch base) to 1.00 (CM at branch tip). 
As a rule of thumb, average live branches, with an average amount of foliage, 
have CM approx. from \eqn{1/3} to \eqn{2/3} of their length, ie. branchesCM = 0.33-0.66.
\eqn{x}, \eqn{y}, \eqn{z} moments are computed by multiplying the cartesian coordinate by 
branch or log mass.}
\value{a vector holding 5 reals:
\itemize{
\item{the \eqn{x} coordinate of branch CM}
\item{the \eqn{y} coordinate of branch CM}
\item{the \eqn{x} moment of the branch}
\item{the \eqn{y} moment of the branch}
\item{the \eqn{z} moment of the branch}}}
\note{BranchCM is assumed to have same value in branches and logs. This is not the case in the real world. As a measure of safety one should use higher values than \eqn{1/3}, eg 1 for branchesCM.}
\note{\eqn{z} coordinate of CM is not returned because it would be useless in a 2D plot. It is computed using \eqn{mz}, which is, as a matter of facts, returned}
\author{Marco Bascietto \email{marco.bascietto@ibaf.cnr.it}}
\arguments{\item{object}{A data frame holding the appropriate colums}
\item{angle}{The name of the data frame column holding the angle of branch orientation}
\item{distance}{The name of the data frame column holding the length of the 
branch projection on the ground}
\item{height}{The name of the data frame column holding the height of branch
insertion on the stem or the height of log lower section}
\item{incl}{The name of the data frame column holding the inclination of
the branch or log in degrees}
\item{mass}{The name of the data frame column holding the mass of the branch or log}
\item{branchesCM}{a real number varying from 0.01 to 1 proportional to the centre of
mass position along the branch (0.01 branch base, 1 branch tip)}
}

