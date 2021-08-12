## not exported; used internally

phenologyMatch <- function(x) {
  # grab names
  fr.table = x$fr.table
  fl.table = x$fl.table
  lc.spp = x$lc.table$calcfullname

  mos <- structure(c('Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct'),
                          names = c('Mr', 'Ap', 'My', 'Jn', 'Jl', 'Au', 'Se', 'Oc'))
  fr.table$matchName <- gsub(" ", "", fr.table$realName)
  fr.table <- fr.table[!duplicated(fr.table$matchName), ]
  row.names(fr.table) <- fr.table[['matchName']]
  fl.table$matchName <- gsub(" ", "", fl.table$realName) # as of 3/10/17, fl.table names are in better shape
  fl.table <- fl.table[!duplicated(fl.table$matchName), ]
  row.names(fl.table) <- fl.table$matchName
  allNames <- unique(sort(c(fl.table$matchName, fr.table$matchName)))
  fr.table <- fr.table[allNames, ]
  fl.table <- fl.table[allNames, ]
  phenoTable <- matrix('', length(allNames), length(mos), dimnames = list(allNames, names(mos)))
  for(i in 1:length(mos)) {
    phenoTable[, i] <- gsub("NA", "", paste( ifelse(apply(fl.table[, grep(mos[i], names(fl.table), value = T)], 1, function(x) any(x != '')), 'L', ''),
                              ifelse(apply(fr.table[, grep(mos[i], names(fr.table), value = T)], 1, function(x) any(x != '')), 'R', ''),
                             sep = ''))
  } # close i
 phenoTable <- as.data.frame(phenoTable)
 phenoTable
}
