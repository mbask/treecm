\name{Dst}
\alias{Dst}
\docType{data}
\title{
Wood density data for a few tree species
}
\description{
Wood density is used to convert wood volume measures in the field to their fresh weights. It is measured in \eqn{\frac{kg}{m^3}}.
}
\usage{data(Dst)}
\format{
A data frame with 39 observations on the following variable.
\describe{
  \item{\code{density}}{a numeric vector}
  }
}
\details{Density is measured at humidity level 12\%, ie it is not a real measure on fresh wood. Actual fresh values are usually greater than those reported in this data set. As a result the dataset is provided only as a reference, please be cautioned about using these values.
}
\source{Nardi Berti, R. (1979) La struttura anatomica del legno ed il riconoscimento dei legnami italiani di pi\`{u} corrente impiego. CNR (Firenze), 155p.
}
\examples{
data(Dst)
}
\keyword{datasets}
