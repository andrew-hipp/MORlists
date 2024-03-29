% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.area.collists.R
\name{make.area.collLists}
\alias{make.area.collLists}
\title{Make collection lists for each area}
\usage{
make.area.collLists(
  x,
  fileHeader = "",
  outdir = format(Sys.time(), "tablesByArea.snapshot.\%Y-\%m-\%d_\%H.\%M"),
  verbose = TRUE,
  area.list = NA,
  do.all = FALSE,
  ...
)
}
\description{
The master function, generally the only one you call after \code{read.mor}.
}
\examples{
 \dontrun{
   workingDir <- 'C:/Users/ahipp/Dropbox/Herbarium/BRAHMS/COLL.LISTS.2017/data/AAA.ACTIVE.SHEETS'
   dat <- read.mor(workingDir)
   lastCollList <- make.area.collLists(dat,
                       sciQuery = "Quercus|Malus|Tilia|Ulmus",
                       do.all = T,
                       useCollSheets = T,
                       excludeYes = TRUE, # change to T
                       fileHeader = "")
 }
}
