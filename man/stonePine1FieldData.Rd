\docType{data}
\name{stonePine1FieldData}
\alias{stonePine1FieldData}
\title{Raw CSV file of field recorded values for a stone pine tree}
\format{\code{
"code","azimuth","dBase","dTip","length","tipD","height","tilt","toBePruned","pathToTip"
"L1",275,73,41,"10.2","2.5",0,80,"FALSE","TRUE"
"L2",275,41,16,"3.9","2.75","10.2",80,"FALSE","TRUE"
"B1",190,15,0,"7.95","10.1",,,"FALSE","FALSE"
"B2",200,22,0,"7.95","10.4",,,"FALSE","FALSE"
"B3",230,15,0,"7.95","10.4",,,"FALSE","FALSE"
"B4",200,18,0,"7.95","11.15",,,"FALSE","FALSE"
"B5",180,7,0,"7.95","11.3",,,"FALSE","FALSE"
"B6",150,6,0,"7.95","11.3",,,"FALSE","FALSE"
"B7",340,16,0,"3.95","11.3",,,"FALSE","FALSE"
"B8",220,13,0,"7.95","11.8",,,"FALSE","FALSE"
"B9",165,19,0,"7.95","11.8",,,"FALSE","FALSE"
"B10",280,8,0,"3.95","11.9",,,"FALSE","FALSE"
"B11",170,9,0,"7.95","11.9",,,"FALSE","FALSE"
"B12",265,8,0,"7.95","12.2",,,"FALSE","FALSE"
"B13",75,6,0,"3.95","12.2",,,"FALSE","FALSE"
"B14",180,6,0,"7.95","12.2",,,"FALSE","FALSE"
"B15",170,6,0,"7.95","12.6",,,"FALSE","FALSE"
"B16",120,5,0,"7.95","12.6",,,"FALSE","FALSE"
"B17",10,14,0,"3.95",13,,,"FALSE","FALSE"
"B18",180,13,0,"7.95",13,,,"FALSE","FALSE"
"B19",260,13,0,"7.95","13.2",,,"FALSE","FALSE"
"B20",75,6,0,"3.95","13.2",,,"FALSE","FALSE"
"B21",75,10,0,"3.95","13.75",,,"FALSE","FALSE"
"B22",215,7,0,"7.95","13.75",,,"FALSE","FALSE"
"B23",140,7,0,"7.95","13.75",,,"FALSE","FALSE"
"C",275,16,0,3,3,"14.1",80,"FALSE","TRUE"
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
  This dataset has been collected for a 17.1 metres tall
  stone pine whose stem was tilted approx. 20 degrees from
  the vertical plane (or 80 degrees from the horizontal
  plane). The stem has been sectioned in two logs
  (\code{L1} and \code{L2}), and a final branch (\code{C}).

  The \code{.csv} file must contain all column headings
  listed in \code{\link{importFieldData}}, regardless of
  them being optional (no data in them) or mandatory.
}
\examples{
library("treecm")
treeData <- importFieldData(
  system.file("data/stonePine1FieldData.csv.gz", package = "treecm"),
  650,
  allometryABDC
)
head(treeData$fieldData)
}
\keyword{datasets}

