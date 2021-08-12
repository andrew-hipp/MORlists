## not exported; used internally

sortOrderGrid <- function(grid, primarySort = NA) {
  grid <- as.character(grid)
  grid.StoN <- c('AP', 'AQ', 'AR', 'AS', 'AT', 'AU', 'AV', 'AW', 'AX', 'AY', 'AZ', LETTERS, paste (LETTERS, LETTERS, sep = ''))
  grid.WtoE <- c(paste("0", 23:1, sep = ''), 0:148)
  sort.SN <- sapply(strsplit(grid, "[-/]"), function(x) match(x[1], grid.StoN))
  sort.WE <- sapply(strsplit(grid, "[-/]"), function(x) match(x[2], grid.WtoE))
  if(is.na(primarySort[1])) {
    out <- order(sort.SN,
               sort.WE,
               as.numeric(sapply(strsplit(grid, "[-/]"), function(x) x[3])),
               as.numeric(sapply(strsplit(grid, "[-/]"), function(x) x[4])))
             } # the sort if there is no primary sort vector
  else {
    out <- order(primarySort,
               sort.SN,
               sort.WE,
               as.numeric(sapply(strsplit(grid, "[-/]"), function(x) x[3])),
               as.numeric(sapply(strsplit(grid, "[-/]"), function(x) x[4])))

  } # the sort if there is a primary sort vector
  out
}

sortOrderAcc <- function(acc, primarySort = NA) {
  acc <- try(strsplit(as.character(acc), "-", fixed = TRUE))
  if(class(acc) == 'try-error') {
    message('sortOrderAcc failed... returning unsorted data')
    out <- seq(length(acc))
  }
  else {
      acc.yr <- sapply(acc, function(x) x[2])
      acc.no <- sapply(acc, function(x) x[1])
      if(is.na(primarySort[1])) out <- order(as.numeric(acc.no), as.numeric(acc.yr))
      else out <- order(primarySort, as.numeric(acc.no), as.numeric(acc.yr))
  }
  out
}
