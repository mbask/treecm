\docType{data}
\name{Dst}
\alias{Dst}
\title{Green wood density data for a few tree species}
\format{A data frame with 170 observations on the following 3 variables. \code{
data.frame:  170 obs. of  3 variables:
  $ species: chr  "Abies alba" "Abies alba" "Abies balsama" "Abies grandis" ...
  $ group  : Factor w/ 2 levels "conifer","dicot": 1 1 1 1 1 1 1 1 1 1 ...
  $ density: int  545 577 529 449 465 673 689 497 673 577 ...}}
\source{
  Niklas, K. J. and Spatz, H.-C. (2010) Worldwide
  correlations of mechanical properties and green wood
  density. American Journal of Botany, 97, 1587-1594
}
\description{
  Wood density is used to convert wood volume measures in
  the field to their fresh weights. It is measured in
  \eqn{\frac{kg}{m^3}}.
}
\details{
  Density is measured at humidity level 50%, very close to
  mean living tree wood humidity. The dataset is provided
  as a reference only, please be cautioned about using
  these values on your samples.
}
\examples{
data(Dst)
}
\keyword{datasets}

