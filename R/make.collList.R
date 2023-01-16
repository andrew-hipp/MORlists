#' Make collections lists
#'
#' Makes a single collection list from a `read.mor` object
#'
#' This function is generally called from `make.area.collLists`, to make a set of
#' collection lists for all areas, as well as a master list.
#'
#' @examples
#'  \dontrun{
#'    workingDir <- 'C:/Users/ahipp/Dropbox/Herbarium/BRAHMS/COLL.LISTS.2017/data/AAA.ACTIVE.SHEETS'
#'    dat <- read.mor(workingDir)
#'    out <- make.collList(dat, sciQuery = 'Quercus|Acer|Tilia')
#'  }
#' @export
make.collList <- function(x,
                          excludeYes = TRUE, # excludes plants and accessions for which fruit and flower are collected
                          excludeAreas = x$bg.table$bglocation[!x$bg.table$INCLUDE],
                          makeSnapshot = TRUE,
                          sortTables = TRUE,
                          trimL = 40,
                          sciQuery = NA, # query by any string in the calcfullname name
                          famQuery = NA, # query by any string in the family name
                          orderQuery = NA, # query by any string in the order name
                          queryIgnoreCase = FALSE, # whether to ignore case in querying
                          phenoAdd = TRUE,
                          useCollSheets = FALSE,
						              makeAccReport = TRUE,
                          outdir = format(Sys.time(), "table.snapshot.%Y-%m-%d_%H.%M"),
						              reportOnlyAccessionsWeCollect = TRUE,
                          cleanFamCol = TRUE, # remove family column b/f writing tables
                          frFams = c('Cupressaceae',
                                        'Pinaceae',
                                        'Cephalotaxaceae',
                                        'Taxaceae'), # if !is.na(frFams[1]), specimens are required from these families only if they need fr, no fls
                          verbose = FALSE,
                          ...) {
  ## get tables from x:
  dat.herb = x$herb.table
  dat.lc = x$lc.table
  dat.coll = x$coll.table
  btable = x$bg.table

  if(!is.na(sciQuery)) dat.lc <- dat.lc[grep(sciQuery, dat.lc$calcfullname, ignore.case = queryIgnoreCase), ]
  if(!is.na(famQuery)) dat.lc <- dat.lc[grep(famQuery, dat.lc$familyname, ignore.case = queryIgnoreCase), ]
  if(!is.na(orderQuery)) dat.lc <- dat.lc[grep(orderQuery, dat.lc$ordername, ignore.case = queryIgnoreCase), ]
  accessions <- unique(dat.lc$accession)
	dat.herb$lc.acc <- sapply(dat.herb$livingcollectionsource, function(x) strsplit(x, "*", fixed = T)[[1]][1])
  out.acc <- data.frame(
					  lc.acc = accessions,
	          sciname = sapply(accessions, function(x) dat.lc$calcfullname[which(dat.lc$accession == x)[1]]),
            FL = ifelse(is.na(sapply(accessions, function(x) any(dat.herb$flowercode[dat.herb$lc.acc == x] == "Y"))), "_", "Y"),
					  FR = ifelse(is.na(sapply(accessions, function(x) any(dat.herb$fruitcode[dat.herb$lc.acc == x] == "Y"))), "_", "Y"),
            V = ifelse(is.na(sapply(accessions, function(x) any(dat.herb$veg_ind[dat.herb$lc.acc == x] == "Y"))), "_", "Y"),
					  hrbN = sapply(accessions, function(x) sum(dat.herb$lc.acc == x, na.rm = T)),
					  bgLoc = sapply(accessions, function(x) paste(sort(unique(dat.lc$gardenlocalityname[which(dat.lc$accession == x)])), collapse = "|")),
            family = sapply(accessions, function(x) dat.lc$familyname[which(dat.lc$accession == x)[1]])
					  )

  if(useCollSheets) {
    dat.coll$acc <- sapply(dat.coll$plantid, function(x) strsplit(x, "*", fixed = TRUE)[[1]][1])
	  dat.coll <- dat.coll[dat.coll$acc %in% row.names(out.acc), ]
	  out.acc[dat.coll$acc[grep('FL', dat.coll$parts)], 'FL'] <- 'Y'
	  out.acc[dat.coll$acc[grep('FR', dat.coll$parts)], 'FR'] <- 'Y'
    out.acc[dat.coll$acc[grep('V', dat.coll$parts)], 'V'] <- 'Y'
    } # close useCollSheets


	out.plant <- data.frame(
	              plantID = dat.lc$plantid,
	              sciname = substr(dat.lc$calcfullname, 1, trimL),
	              grid = paste(dat.lc$gardensubarea1, dat.lc$gardensubarea2, sep = '/'),
	              bgLoc = substr(gsub(":::|::", ":",
	                             paste(dat.lc$gardenlocalityname,
  	                              ifelse(dat.lc$gardensubarea3 != '', dat.lc$gardensubarea3, ''),
  	                              ifelse(dat.lc$gardensubarea5 != '', dat.lc$gardensubarea5, ''),
  	                              sep = ":") # close paste
	                               ), 1, trimL),
      				  bgLocTemp = dat.lc$gardenlocalityname,
                FL = out.acc[dat.lc$accession, 'FL'],
                FR = out.acc[dat.lc$accession, 'FR'],
                V = out.acc[dat.lc$accession, 'V'],
                hrbN = out.acc[dat.lc$accession, 'hrbN'],
                family = dat.lc$familyname
              ) # close data.frame for out.plant

  missingPlantID <- grep("*", as.character(out.plant$plantID), fixed = T, invert = T)
  if(length(missingPlantID) > 0) out.plant <- out.plant[-missingPlantID, ]
  if(excludeYes) {
	  out.acc <- out.acc[apply(out.acc[, c('FL','FR')], 1, paste, collapse = "") != "YY",]
	  out.plant <- out.plant[apply(out.plant[, c('FL','FR')], 1, paste, collapse = "") != "YY",]
    }


  if(!is.na(frFams[1])) {
    out.acc <- out.acc[!((out.acc$family %in% frFams) & out.acc$FR == "Y"), ]
    out.plant <- out.plant[!((out.plant$family %in% frFams) & out.plant$FR == "Y"), ]
    } # exclude rows where family is in frFams and FR == "Y"

	if(!is.na(excludeAreas[1])) {
	  # out.acc <- out.acc[!out.acc$bgLoc %in% excludeAreas, ]
	  out.plant <- out.plant[!out.plant$bgLocTemp %in% excludeAreas, ]
	}

  if(sortTables) {
    out.plant <- out.plant[sortOrderGrid(out.plant$grid), ]
    out.acc <- out.acc[sortOrderAcc(out.acc$lc.acc), ]
  }

  if(phenoAdd) {
    phenoTable <- phenologyMatch(x)
    out.acc <- cbind(out.acc, phenoTable[gsub(" ", "", out.acc$sciname), ])
    out.plant <- cbind(out.plant, phenoTable[gsub(" ", "", out.plant$sciname), ])
    out.acc$noPh <- ifelse(is.na(out.acc$Mr), '**', '')
    out.plant$noPh <- ifelse(is.na(out.plant$Mr), '**', '')
    out.acc[is.na(out.acc)] <- ''
    out.plant[is.na(out.plant)] <- ''
  } # close phenoAdd

	if(makeAccReport) {
	  message('making accessions report')
	  out.report <- character(0)
    for(i in seq(dim(out.acc)[1])) {
		## debugging stuff
		if(verbose) message(paste('doing out.acc line', i))
		#print(sapply(strsplit(as.character(out.plant$plantID), "*", fixed = TRUE), function(x) x[[1]][1] == as.character(accNo)))
      accNo <- out.acc[i, 'lc.acc']
      j <- try(
		which(
			sapply(
				strsplit(as.character(out.plant$plantID), "*", fixed = TRUE), function(x) x[[1]][1] == as.character(accNo))))
      if((length(j) == 0 & reportOnlyAccessionsWeCollect) | class(j) == "try-error") next
      sci <- out.acc[i, 'sciname']
      fam <- as.character(dat.lc$familyname[which(dat.lc$accession == accNo)][1])
	    fl <- ifelse(out.acc[i, 'FL'] == '', '_', 'X')
	    fr <- ifelse(out.acc[i, 'FR'] == '', '_', 'X')
      v <- ifelse(out.acc[i, 'V'] == '', '_', 'X')
	    hrbNo <- out.acc[i, 'hrbN']
	    out.report <- c(out.report, paste('\n', accNo, '\t', sci, ' [', toupper(fam), ']',
                                        '  ---  ',
                                        'V:', out.acc[i, 'V'],
                                        ' | Fl:', out.acc[i, 'FL'],
                                        ' | Fr:', out.acc[i, 'FR'],
                                        ' | Total herbarium sheets:', hrbNo,
                                        sep = ''))
      if(length(j) == 0) out.report <- c(out.report, paste('\t--- NOT CURRENTLY BEING COLLECTED; plants located in', out.acc[i, 'bgLoc']))
      else {
        j <- j[sortOrderGrid(out.plant[j, 'grid'],
                             primarySort = btable$COLLECTING.LIST[match(as.character(out.plant[j, 'bgLocTemp']), btable$bglocation)])]
        out.report <- c(out.report,
                      paste('\t', out.plant[j, 'plantID'],
                            '\t', out.plant[j, 'grid'],
                            '\t', btable$COLLECTING.LIST[match(as.character(out.plant[j, 'bgLocTemp']), btable$bglocation)],
#                            'Area:', as.character(out.plant[j, 'bgLocTemp'],
                            sep = ''))
                          } # close else
	  } # close i
	} # close makeAccReport

	out.plant$bgLocTemp <- NULL
	out <- list(acc = out.acc, plant = out.plant)
  if(cleanFamCol) out.acc$family <- out.plant$family <- NULL
	if(makeAccReport) out$accReport <- out.report

  if(makeSnapshot) {
    dir.create(outdir)
    writeLines(out.report, paste(outdir, '/accessions.report.', format(Sys.time(), "%Y-%m-%d.txt"), sep = ''))
    write.csv(out.acc, paste(outdir, '/accessions.table.', format(Sys.time(), "%Y-%m-%d.csv"), sep = ''), row.names = TRUE)
    }
  out
	}
