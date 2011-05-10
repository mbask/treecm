\name{fieldData}
\alias{fieldData}
\docType{data}
\title{
Raw CSV file of field recorded values for a stone pine tree
}
\description{
Required data for the assessment of the centre of mass have been recorded in the field for a stone pine (\emph{Pinus pinea} L.). This is an example of csv file that should be fed to \code{\link{treeBiomass}} to assess tree centre of mass.
}
\usage{data(fieldData)}
\format{
  \code{
"code","dir","dBase","dTip","length","tipD","height","tilt","toBePruned"
"L1",275,73,41,10.2,2.5,0,80,
"L2",275,41,16,3.9,2.75,10.2,80,
"B1",190,15,0,,7.95,10.1,,
"B2",200,22,0,,7.95,10.4,,
"B3",230,15,0,,7.95,10.4,,
"B4",200,18,0,,7.95,11.15,,
"B5",180,7,0,,7.95,11.3,,
"B6",150,6,0,,7.95,11.3,,
"B7",340,16,0,,3.95,11.3,,
"B8",220,13,0,,7.95,11.8,,
"B9",165,19,0,,7.95,11.8,,
"B10",280,8,0,,3.95,11.9,,
"B11",170,9,0,,7.95,11.9,,
"B12",265,8,0,,7.95,12.2,,
"B13",75,6,0,,3.95,12.2,,
"B14",180,6,0,,7.95,12.2,,
"B15",170,6,0,,7.95,12.6,,
"B16",120,5,0,,7.95,12.6,,
"B17",10,14,0,,3.95,13,,
"B18",180,13,0,,7.95,13,,
"B19",260,13,0,,7.95,13.2,,
"B20",75,6,0,,3.95,13.2,,
"B21",75,10,0,,3.95,13.75,,
"B22",215,7,0,,7.95,13.75,,
"B23",140,7,0,,7.95,13.75,,
"C",275,16,0,3,3,14.1,80,
  }
}

\source{
  Original data collected by the author
}

\examples{
\dontrun{
  treeData <- importFieldData("fieldData.csv", 530, branchBiomassPinePorte)
  }
}
\keyword{datasets}
