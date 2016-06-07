#' @export getSkippers
#'   
#' @title Find skipped BAS cells.

getSkippers <- function(){
  
  #   ---- Get the current list of who has what.  
  assign <- getCellStatus()
 
  #   ---- Get the BAS rankings.  
  ranks <- getRankStatus()
  
  #   ---- Make a master data frame.
  master <- merge(assign,ranks,by=c("Grid_ID"),all.x=TRUE,all.y=TRUE)
  master <- master[order(master$sampleID),c('Grid_ID','openStatus','digiStatus','buffStatus','buffLockGrid_ID','doneStatus','sampleID','dblSamp')]
  
  #   ---- Find latest completed cell.  
  master <- master[order(master$sampleID,decreasing=TRUE),]
  
  for(i in 1:nrow(master)){
    if( master[i,]$doneStatus == 1 ){
      theFirstOne <- master[i,]$sampleID
      break
    }
  }

  #   ---- Reduce to the dataset of interest.  
  master <- master[order(master$sampleID),]
  skips <- master[master$done == 0 & master$sampleID <= theFirstOne,]
  rownames(skips) <- NULL
  
  return(skips)
}