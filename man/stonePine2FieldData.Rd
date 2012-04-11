\docType{data}
\name{stonePine2FieldData}
\alias{stonePine2FieldData}
\title{Raw CSV file of field recorded values for a stone pine tree}
\format{\code{
"code","azimuth","dBase","dTip","length","tipD","height","tilt","toBePruned","pathToTip"
"L1",0,67,40,"6.8",0,0,90,,"TRUE"
"B1",250,40,,,"7.8","6.8",,,
"B2",240,32,,,"8.9","7.8",,,
"B3",55,25,,,9,9,,,
"B4",260,10,,,"5.5",10,,"TRUE",
"B5",80,36,,,"8.2","10.6",,,
"B6",255,27,,,"4.5","10.8",,,
"B7",0,40,,,"6.5","6.8",70,,"TRUE"
}}
\source{
  Original data collected by the author
}
\description{
  Required data for the assessment of the centre of mass
  have been recorded in the field for a stone pine
  (\emph{Pinus pinea} L.). This is an example of csv file
  that should be fed to \code{\link{treeBiomass}} to assess
  tree centre of mass.
}
\details{
  This dataset has been collected for a \eqn{\approx 11}
  metres tall stone pine with a small number of very large
  branches.

  The \code{.csv} file must contain all column headings
  listed in \code{\link{importFieldData}}, regardless of
  them being optional (no data in them) or mandatory.
}
\examples{
library("treecm")
  treeData <- importFieldData(system.file("data/stonePine2FieldData.csv.gz", package = "treecm"), 650, allometryABDC)
head(treeData$fieldData)
}
\keyword{datasets}

