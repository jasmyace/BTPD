updateAssign <- function(userID,out){
  
  #   ---- Read in the stored assign, and use it to replace
  #   ---- the old one in Database.  
  assign <- read.csv(paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/",userID,"/tblCellStatusTMP.csv"))
  
  #   ---- The real deal version.  
  write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus.csv"),row.names=FALSE)
  
  #   ---- The running versions. 
  vList <- dir("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus")
  version <- max(substr(vList,nchar(vList) - 9,nchar(vList) - 4))
  newVersion <- sprintf("%06d",(as.numeric(version) + 1))
  write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus/tblCellStatus",newVersion,".csv"),row.names=FALSE)
  
  #   ---- Make an instruction sheet for this cell.  
  makeInstructions(userID,out)
  
  #   ---- Remove the lock.  
  file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
  cat(paste0("The status of your new cell has successfully been updated in tblCellStatus.csv.\n"))
  
}