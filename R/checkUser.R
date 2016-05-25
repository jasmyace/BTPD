checkUser <- function(userID,tblDir="//lar-file-srv/Data/BTPD_2016/Analysis/Database/"){
  
  # userID <- 102
  # tblDir <- "//lar-file-srv/Data/BTPD_2016/Analysis/Database/"
  
  #   ---- Find the user.
  tblNames <- read.csv(paste0(tblDir,"tblNames.csv"))
  tblNamesUserID <- tblNames[tblNames$userID == userID,]
  
  #   ---- Check user status and active status.  
  if( nrow(tblNamesUserID) == 0 ){
    stop("The userID submitted was not found.  Try again.",call.=FALSE)
  } else if( nrow(tblNamesUserID) == 1 & (tblNamesUserID$doubleActive == 0 & tblNamesUserID$singleActive == 0) ){
    stop("The userID submitted was found but is inactive.  Try again.",call.=FALSE)    
  }

  tblNames
}
