#' @export updateAssign
#'   
#' @title Update the assignment history in the appropriate csv.

updateAssign <- function(userID,out){
  
  #   ---- Read in the stored assign, and use it to replace
  #   ---- the old one in Database.  
  assign <- read.csv(paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/",userID,"/tblCellStatusTMP.csv"),stringsAsFactors=FALSE)
  
  #   ---- The real deal version.  
  write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus.csv"),row.names=FALSE)
  
  
  
#   #   ---- Organize stuff for next steps.
#   #   ---- Use this to force the creation of makeInstructions.
#   assign <- "ugh"   # Don't change this.
#   theNext <- "CO136656"
#   theRange <- "Block 52 CO136302 - CO137020/"
#   double <- 1
#   pFirstName <- "Becky"
#   sFirstName <- "Ashleigh"
#   
#   theNextdf <- data.frame(theNext=theNext,stringsAsFactors=FALSE)
#   theRangedf <- data.frame(theRange=theRange,stringsAsFactors=FALSE)
#   thepFirstNamedf <- data.frame(thepFN=pFirstName,stringsAsFactors=FALSE)
#   if(double == 1){
#     thesFirstNamedf <- data.frame(thesFN=sFirstName,stringsAsFactors=FALSE)
#   } else {
#     thesFirstNamedf <- data.frame(thesFN="No",stringsAsFactors=FALSE)
#   }
#   
#   out <- list(assign=assign,theNext=theNextdf,theRange=theRangedf,thepFirstName=thepFirstNamedf,thesFirstName=thesFirstNamedf)
  
  
  
  
  #   ---- Make an instruction sheet for this cell.  
  makeInstructions(userID,out)
  
  #   ---- The running versions. 
  vList <- dir("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus")
  version <- max(substr(vList,nchar(vList) - 9,nchar(vList) - 4))
  newVersion <- sprintf("%06d",(as.numeric(version) + 1))
  write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus/tblCellStatus",newVersion,".csv"),row.names=FALSE)
  
  #   ---- Can take a few seconds to put the csv file.  Give the program time to catch up.  
  Sys.sleep(15)
  
  #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
  if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
    if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
      invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
    }
  }
  
  cat(paste0("The status of your new cell has successfully been updated in tblCellStatus.csv.\n"))
  
}