#' Make collection lists for each area
#'
#' The master function, generally the only one you call after `read.mor`.
#'
#' @examples
#'  \dontrun{
#'    workingDir <- 'C:/Users/ahipp/Dropbox/Herbarium/BRAHMS/COLL.LISTS.2017/data/AAA.ACTIVE.SHEETS'
#'    dat <- read.mor(workingDir)
#'    lastCollList <- make.area.collLists(dat,
#'                        sciQuery = "Quercus|Malus|Tilia|Ulmus",
#'                        do.all = T,
#'                        useCollSheets = T,
#'                        excludeYes = TRUE, # change to T
#'                        fileHeader = "")
#'  }
#' @export
make.area.collLists <- function(x,
                                fileHeader = '',
                                outdir = format(Sys.time(), "tablesByArea.snapshot.%Y-%m-%d_%H.%M"),
                                verbose = TRUE,
                                area.list = NA,
                                do.all = FALSE, ...) {
  ## extract tables
  # dat.herb = x$herb.table
  # dat.lc = x$lc.table
  # dat.coll = x$coll.table
  # btable = x$bg.table

  dir.create(outdir)
  if(is.na(area.list[[1]])) {
    area.list <- sort(unique(x$bg.table$COLLECTING.LIST[x$bg.table$INCLUDE]))
  }
  out <- vector('list', length(area.list)+1)
  names(out) <- c(area.list, 'all')
  for(i in area.list) {
    if(verbose) message(paste('... doing area', i))
    x.temp <- x
    x.temp$lc.table <-
      x.temp$lc.table[x.temp$lc.table$gardenlocalityname %in% x.temp$bg.table$bglocation[x.temp$bg.table$COLLECTING.LIST == i], ]
    temp <- make.collList(x.temp,
                          makeSnapshot = FALSE,
                          ...)
    write.csv(temp$acc, paste(outdir, '/', gsub(" ", "_", i), '.accessions.table.', format(Sys.time(), "%Y-%m-%d.csv"), sep = ''), row.names = TRUE)
    write.csv(temp$plant, paste(outdir, '/', gsub(" ", "_", i), '.plants.table.', format(Sys.time(), "%Y-%m-%d.csv"), sep = ''), row.names = FALSE)
    out[[i]] <- temp
  }
  if(do.all){
    if(verbose) message(paste('... doing all data -- almost done!'))
    temp <- make.collList(x, makeSnapshot = FALSE, ...)
    write.csv(temp$acc, paste(outdir, '/all.accessions.table.', format(Sys.time(), "%Y-%m-%d.csv"), sep = ''), row.names = TRUE)
    write.csv(temp$plant, paste(outdir, '/all.plants.table.', format(Sys.time(), "%Y-%m-%d.csv"), sep = ''), row.names = FALSE)
    writeLines(temp$accReport, paste(outdir, '/all.plants.accessions.report.', format(Sys.time(), "%Y-%m-%d.txt"), sep = ''))
    out$all <- temp
  }
  out
  } # close make.area.collLists
