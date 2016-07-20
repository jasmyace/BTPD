estimateTime <- function(shp){
  
  # shp <- CO
  
  #   ---- Get folder structure.  
  tblFolders <- getFolderStatus()
  
  #   ---- Get the current list of who has what.  
  assign <- getCellStatus()
  
  #   ---- Get users. 
  tblNames <- checkUser(userID=100)
  
  #   ---- Get sampling.
  tblRanks <- getRankStatus()
  tblRanks <- tblRanks[order(tblRanks$sampleID),]
  
  #   ---- Calculate elapsed time spent while digitizing.
  done <- assign[assign$doneStatus == 1,c('Grid_ID','digiUserID','digiPartner','digiDouble','digiStartTime','digiEndTime','jErrStatus')]
  done$digiStartTime <- as.POSIXct(done$digiStartTime,format="%m/%d/%Y %H:%M",tz="America/Denver")
  done$digiEndTime <- as.POSIXct(done$digiEndTime,format="%m/%d/%Y %H:%M",tz="America/Denver")  
  done$time <- as.numeric(done$digiEndTime - done$digiStartTime) / 60
  done <- done[done$jErrStatus == 0,]

  #   ---- Count the number of towns per cell.  
  towns <- data.frame(nTowns=tapply(shp@data$Town_ID,factor(shp@data$Grid_ID),function(x) length(x)))
  towns$Grid_ID <- rownames(towns)
  rownames(towns) <- NULL
  
  #   ---- Put together times and town counts.  
  done <- merge(done,towns,by=c('Grid_ID'),all.x=TRUE)
  done$nTowns[is.na(done$nTowns)] <- 0
  
  #   ---- Focus on singly digitized.  
  singly <- done[done$digiDouble == 0,]
  
  #   ---- Calculate mean singly number of towns.  
  avgTowns <- mean(singly$nTowns)
  
  #   ---- Calculate time as a function of number of towns.  
  avgMinTown <- data.frame(avgMin=tapply(as.numeric(singly$time),factor(singly$nTowns),function(x) mean(x)))
  avgMinTown$nTowns <- as.numeric(rownames(avgMinTown))
  rownames(avgMinTown) <- NULL
  
  #   ---- Make distribution of town counts. 
  counts=hist(singly$nTowns,plot=FALSE,breaks=seq(0,max(singly$nTowns) + 1,1),right=FALSE)$counts
  nTowns=hist(singly$nTowns,plot=FALSE,breaks=seq(0,max(singly$nTowns) + 1,1),right=FALSE)$breaks
  townDist <- data.frame(counts=counts,nTowns=nTowns[1:length(counts)])
  
  #   ---- Calculate proportion of cells having the town distribution.  
  townCounts <- merge(avgMinTown,townDist,by=c('nTowns'),all.x=TRUE,all.y=TRUE)
  townCounts$prop <- townCounts$counts / sum(townCounts$counts)
  
  #   ---- Find the remaining number of cells, and estimate time to completion.
  nLeft <- nrow(assign) - nrow(done)
  townCounts$propLeft <- nLeft*townCounts$prop
  townCounts$totMins <- townCounts$propLeft*townCounts$avgMin
  townCounts$totMins[is.na(townCounts$totMins)] <- 0
  
  #   ---- Method 1: 
  m1TotMins <- mean(as.numeric(singly$time)) * nLeft
  m1TotHours <- m1TotMins / 60
    
    
  totHours <- sum(townCounts$totMins) / 60 
  
  plot(factor(singly$nTowns),as.numeric(singly$time))
  
  
  #   ---- Person-based.  How many cells are people doing in a work day?
  singly <- done[done$digiDouble == 0,]
  
  #   ---- Note that I build each day by 24 hours.  This won't work if we straddle a time change.  
  dayLabels <- unique(strftime(as.POSIXlt(singly$digiEndTime),format="%D"))
  dayLabels <- c(dayLabels[order(dayLabels)],"Total")
  singly$jDay <- as.numeric(floor(julian(as.POSIXlt(singly$digiEndTime),origin=as.POSIXct("2016-05-21",tz="America/Denver"))) + 1)
  
  counts <- as.matrix(table(singly$digiUserID,singly$jDay))
  counts <- cbind(counts,rowSums(counts))
  counts <- rbind(counts,colSums(counts))
  colnames(counts) <- dayLabels
  dayTotals <- counts[nrow(counts),]
  dayTotalsSingle <- dayTotals
  
  
  #   ---- Person-based.  How many cells are people doing in a work day?
  singly <- done[done$digiDouble == 1,]
  
  #   ---- Note that I build each day by 24 hours.  This won't work if we straddle a time change.  
  dayLabels <- unique(strftime(as.POSIXlt(singly$digiEndTime),format="%D"))
  dayLabels <- c(dayLabels[order(dayLabels)],"Total")
  singly$jDay <- as.numeric(floor(julian(as.POSIXlt(singly$digiEndTime),origin=as.POSIXct("2016-05-21",tz="America/Denver"))) + 1)
  
  counts <- as.matrix(table(singly$digiUserID,singly$jDay))
  counts <- cbind(counts,rowSums(counts))
  counts <- rbind(counts,colSums(counts))
  colnames(counts) <- dayLabels
  dayTotals <- counts[nrow(counts),]
  dayTotalsDouble <- dayTotals
  
  
  return(list(dayTotalsSingle,dayTotalsDouble))
}