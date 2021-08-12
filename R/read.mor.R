#' Read in collections lists data
#'
#' Reads in the tables for making collections lists, packages them up nice.
#' Searches for tables with these strings in their names... FINISH DOCS.
#'
#' @param directory Working directory where files are located
#' @import readr
#' @export

read.mor <- function(directory = getwd()) {
  # change data table names here if needed --- pattern matching allows you to change file dates without changing here
  herb.filename <- dir(directory, patt = 'COLLEXTRACT', full = TRUE)
  #  herb.link.filename <- dir(directory, patt = 'COLLLINK', full = TRUE)
  lc.filename <- dir(directory, patt = 'LIVINGEXTRACT', full = TRUE)
  fl.filename <- dir(directory, patt = 'flower', full = TRUE)
  fr.filename <- dir(directory, patt = 'fruit', full = TRUE)
  bg.filename <- dir(directory, patt = 'botAreas', full = TRUE)
  coll.sheets.filename <- dir(directory, patt = 'Accessions-collected', full = TRUE)

  # read data
  out <- list(
    herb.table = read.csv(herb.filename, as.is = T),
    lc.table = read.csv(lc.filename, as.is = T),
    fl.table = read.csv(fl.filename, as.is = T),
    fr.table = read.csv(fr.filename, as.is = T),
    bg.table = read.csv(bg.filename, as.is = T)
  ) # close out

  names(out$herb.table) <- tolower(names(out$herb.table))
  names(out$lc.table) <- tolower(names(out$lc.table))
  if(length(coll.sheets.filename) == 1) out$coll.table <- read.csv(coll.sheets.filename, as.is = T)
  if(length(coll.sheets.filename) != 1) out$coll.table <- NULL

  return(out)
}
