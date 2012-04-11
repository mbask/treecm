\docType{data}
\name{fieldData}
\alias{fieldData}
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
  Field measures to be taken on logs and branches include:
  \itemize{ \item{\bold{Azimuth} (\code{azimuth}): mean
  angle of orientation of the branch or log measured from
  the base of the tree (usually with magnetic north as
  reference, measured clockwise), \emph{mandatory}}
  \item{\bold{Diameter at base} (\code{dBase}): diameter at
  insertion point on the stem, for branches, diameter of
  the lower section for logs, \emph{mandatory}}
  \item{\bold{Diameter at top} (\code{dTip}): always 0 for
  branches, diameter of the higher section for logs,
  defaults to 0, \emph{mandatory} only for logs}
  \item{\bold{Length} (\code{length}): length of logs or
  branches, \emph{mandatory} for logs, \emph{mandatory} for
  branches only if their slenderness coefficient is to be
  computed} \item{\bold{Distance} (\code{tipD}): length of
  branch or log projection on the ground, starting from
  tree base to tip of branch or log, \emph{mandatory}}
  \item{\bold{Height} (\code{height}): height of branch
  insertion on the stem or height of lower section of the
  log, to be used to compute \code{z} coordinate of CM,
  defaults to NA, \emph{mandatory} only for \code{z}
  determination of CM} \item{\bold{Tilt} (\code{tilt}):
  mean branch tilt or log tilt from the horizontal plane
  (eg a vertical branch is 90 degrees, an horizontal branch
  is 0 degrees), to be used to compute \code{z} coordinate
  of CM, defaults to 0, \emph{mandatory} only for \code{z}
  determination of CM. Note however that the tree tip
  should be considered as a branch, not a log, in order to
  account for foliage biomass. In this case the tilt value
  should be recorded otherwise it would default to 0,
  \emph{ie} an horizontal branch} \item{\bold{To be pruned}
  (\code{toBePruned}): a boolean value, defaults to FALSE,
  \emph{optional}} \item{\bold{Path to tip}
  (\code{pathToTip}): a boolean value TRUE on logs and
  branches that make part of the ``main stem'' of the tree,
  defaults to FALSE, \emph{optional}} }

  The \code{.csv} file must contain all column headings
  listed, regardless of them being optional (no data in
  them) or mandatory.
}
\examples{
library("treecm")
  treeData <- importFieldData(system.file("data/fieldData.csv.gz", package = "treecm"), 530, allometryABDC)
head(treeData$fieldData)
}
\keyword{datasets}

