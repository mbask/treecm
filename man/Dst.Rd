\name{Dst}
\alias{Dst}
\docType{data}
\title{
Green wood density data for a few tree species
}
\description{
Wood density is used to convert wood volume measures in the field to their fresh weights. It is measured in \eqn{\frac{kg}{m^3}}.
}
\usage{data(Dst)}
\format{
  A data frame with 170 observations on the following 3 variables.
  \describe{
    \item{\code{species}}{a character vector}
    \item{\code{group}}{a factor with levels \code{conifer} \code{dicot}}
    \item{\code{density}}{a numeric vector}
  }
}
\details{Density is measured at humidity level 50\%, very close to mean living tree wood humidity. The dataset is provided as a reference only, please be cautioned about using these values on your samples.
}
\source{Niklas, K. J. and Spatz, H.-C. (2010) Worldwide correlations of mechanical properties and green wood density. American Journal of Botany, 97, 1587-1594
}

\examples{
data(Dst)
}
\keyword{datasets}
