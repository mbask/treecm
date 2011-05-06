\name{treeData}
\alias{treeData}
\docType{data}
\title{
Field recorded values for a stone pine tree
}
\description{
Required data for the assessment of the centre of mass have been recorded in the field for a stone pine (\emph{Pinus pinea} L.). \code{\link{treeBiomass}} has already been run on the dataset, vectors have yet to be computed. 
}
\usage{data(treeData)}
\format{
  The format is: List of 4

    \tabular{ll}{
 $ fieldData   : \tab 'data.frame':  26 obs. of  9 variables:\cr
  ..$ dir      : \tab int [1:26] 275 275 190 200 230 200 180 150 340 220 ...\cr
  ..$ dBase 	: \tab int [1:26] 73 41 15 22 15 18 7 6 16 13 ...\cr
  ..$ dTip      : \tab num [1:26] 41 16 0 0 0 0 0 0 0 0 ...\cr
  ..$ length    : \tab num [1:26] 10.2 3.9 NA NA NA NA NA NA NA NA ...\cr
  ..$ tipD      : \tab num [1:26] 2.5 2.75 7.95 7.95 7.95 7.95 7.95 7.95 3.95 7.95 ...\cr
  ..$ height    : \tab num [1:26] 0 10.2 10.1 10.4 10.4 ...\cr
  ..$ tilt      : \tab num [1:26] 80 80 0 0 0 0 0 0 0 0 ...\cr
  ..$ toBePruned: \tab logi [1:26] FALSE FALSE FALSE FALSE FALSE FALSE ...\cr
  ..$ biomass   : \tab num [1:26] 1741 184 120 247 120 ...\cr
 $ density  : \tab num 620\cr
 $ allometryFUN:\tab function (x, diameter)  
  ..- attr(*, "source")= chr [1:5] "function(x, diameter) " ...\cr
 $ branchesCM  : \tab num 1\cr
    }

}

\source{
  Original data collected by the author
}

\examples{
data(treeData)
vectors  <- treeVectors(treeData)
CM       <- centreOfMass(vectors)
summary(CM)
}
\keyword{datasets}
