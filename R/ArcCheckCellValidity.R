

tool_exec <- function(in_params,out_params){
  
  #   ---- Ensure we have all the necessary helper functions.  
  source("//lar-file-srv/Data/BTPD_2016/Analysis/R/checkOutCell.R")   
  source("//lar-file-srv/Data/BTPD_2016/Analysis/R/checkUser.R")   
  source("//lar-file-srv/Data/BTPD_2016/Analysis/R/getFolderStatus.R")   
  source("//lar-file-srv/Data/BTPD_2016/Analysis/R/getRankStatus.R")   
  source("//lar-file-srv/Data/BTPD_2016/Analysis/R/makeBuffer.R")   
  source("//lar-file-srv/Data/BTPD_2016/Analysis/R/getCellStatus.R")   
  
  #   ---- Get all necessary packages for this function.
  packages <- c("rgdal","rgeos","raster")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
  
  require(rgdal)
  require(rgeos)
  require(raster)
  
  #   ---- Get stuff from the ArcMap GUI.  
  userID <- in_params[[1]]
  
  
  #   ---- Extract some output.  Maybe not necessary.  
  out <- checkOutCell(userID)
  
  #   ---- Obtain weird behavior if these files are already present and the arc.write is used.
  delete2 <- dir(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID),pattern="theNext*",full.names=TRUE,ignore.case=TRUE)
  file.remove(delete2)
  
  delete3 <- dir(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID),pattern="theRange*",full.names=TRUE,ignore.case=TRUE)
  file.remove(delete3)
  
  delete4 <- dir(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID),pattern="thepFirstName*",full.names=TRUE,ignore.case=TRUE)
  file.remove(delete4)
  
  delete5 <- dir(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID),pattern="thesFirstName*",full.names=TRUE,ignore.case=TRUE)
  file.remove(delete5)
  
  #   ---- Write out files for this round.
  arc.write(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/theNext.dbf"),out[[2]])
  arc.write(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/theRange.dbf"),out[[3]])
  arc.write(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/thepFirstName.dbf"),out[[4]])
  arc.write(paste0("//lar-file-srv/Data/BTPD_2016/Analysis/Database/",userID,"/thesFirstName.dbf"),out[[5]])
  
  #   ---- Items in list out:
  #   ----  * out[[1]] <- assign data frame
  #   ----  * out[[2]] <- theNext Grid_ID assigned in this check-out go-around.
  #   ----  * out[[3]] <- theRange range assigned in this check-out go-around.
  #   ----  * out[[4]] <- thepFirstName First Name (primary) assigned in this check-out go-around.
  #   ----  * out[[5]] <- thesFirstName First Name (secondary) assigned in this check-out go-around. Could be "".
  
}