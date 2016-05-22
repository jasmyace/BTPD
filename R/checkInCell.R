#' @export checkInCell
#'   
#' @title Check in a cell when complete.


checkInCell <- function(theNext,userID){
  
  # theNext <- "CO156212"
  # userID <- 100
  
#   #   ---- Ensure we have all the necessary helper functions.  
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/getFolderStatus.R")   
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/checkCellValidity.R")  
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/checkInCell.R")   
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/getRankStatus.R")   
#   
#   #   ---- Ensure we have all the necessary packages.  
#   packages <- c("rgdal","rgeos","maptools")
#   if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
#     install.packages(setdiff(packages, rownames(installed.packages())))  
#   }
#   
#   #   ---- Add in necessary packages.
#   require(rgdal)
#   require(rgeos)
#   require(maptools)
  
  #   ---- Check for a lock on table tblCellStatus.csv
  lock <- grep("tblCellStatusLOCK",dir("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database"),fixed=TRUE)
  if(length(lock) > 0){
    stop("The function is currently locked;  try again in a minute.")
  } else {
    #   ---- Lock the table tblCellStatus so that two users cannot update
    #   ---- it at the same time. 
    lockdf <- data.frame(userID=userID)
    write.table(lockdf,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",row.names=FALSE)
  }
  
  assign <- read.csv("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus.csv",as.is = TRUE)
  if( assign[assign$Grid_ID == theNext,]$open == 1 | assign[assign$Grid_ID == theNext,]$doneStatus == 1 ){
    
    #   ---- Remove the lock.  
    file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
    stop("It appears as if the Grid_ID of the cell you're attempting to check in hasn't been checked out, or is already complete.  Investigate.")
  }
  
  #   ---- Get folder structure.  
  tblFolders <- getFolderStatus()
  theRange <- tblFolders[tblFolders$Grid_ID == theNext,]$Range
  
  folder <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Digitizing/",theRange,"/",theNext)
  files <- dir(folder)
  
  #   ---- Determine if this is a singly or doubly sampled cell.  
  ranks <- getRankStatus()
  double <- 0
  if( ranks[ranks$Grid_ID == theNext,]$dblSamp == 1){
    double <- 1
  }
  
  if( double == 1 ){
    
    #   ---- Double, so make sure there is an 's' and 'reconciling' shapefile. 
    sInd <- substr(files,1,1) == 's' & 
      substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
      grepl("Towns",files,fixed=TRUE)
  
    sShpC <- substr(files[sInd],1,nchar(files[sInd]) - 4)
  
    #   ---- Double, so make sure there is an 's' and 'reconciling' shapefile. 
    rInd <- (substr(files,1,11) == 'reconciling' & 
      ( substr(files,nchar(files) - 3,nchar(files)) == ".shp" |
        substr(files,nchar(files) - 3,nchar(files)) == ".txt" ) )

    
    rShpC <- substr(files[rInd],1,nchar(files[rInd]) - 4)
    
    if( is.na(rShpC) | is.na(sShpC) | is.null(sShpC) | is.null(rShpC) | length(sShpC) == 0 | length(rShpC) == 0 ){
      stop("The folder tied to the doubly sampled Grid_ID provided lacks the requisite 's' and 'reconciling' shapefiles.  Investigate.")
    }
  } 
  
  #   ---- Find the primary shapefiles.  
  pShp <- sShp <- NA
  
  pInd <- substr(files,1,1) == 'p' & 
    substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
    grepl("Towns",files,fixed=TRUE)
  
  pShpC <- substr(files[pInd],1,nchar(files[pInd]) - 4)
  
  if( is.na(pShpC) | is.null(pShpC) | length(pShpC) == 0 ){
    stop("The folder tied to the doubly sampled Grid_ID provided lacks the requisite 'p' shapefile.  Investigate.\n")
  }


  
  #   ---- Need to see if we have shapefiles with no features. This happens often.  
  checkShp <- function(folder,shp){                                                                 
    if(is.null(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message)){
      shp2 <- tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)
    } else if(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message == "no features found" ){
      shp2 <- 'no features found'                                         
    }  
  }
  
  #   ---- Read in shapefiles, allowing for no features found.  
  pShp <- checkShp(folder,pShpC)
  if( double == 1){sShp <- checkShp(folder,sShpC)}
  
  #   ---- Check and see if there are errors that should be remedied via 
  #   ---- the town tool in Arc.  
  readCheckCellValidity <- function(Shp){
    out <- tryCatch(
      {
        checkCellValidity(Shp)
      },
      warning = function(cond){
        return(1)
      }
    )
    return(out)
  }
  
  #   ---- Apply the checking function, collecting any evidence of a warning.
  checkApply <- function(shp){
    if(class(get(paste0(substr(shp,1,1),'Shp'))) == "SpatialPolygonsDataFrame"){
      lapply(shp,readCheckCellValidity)
    } else {
      cat(paste0("The check of ",shp," found no towns to check.  Be sure this is correct. If so, continue.\n"))
    }
  }
  
  pShpCCheck <- checkApply(pShpC)
  if( double == 1){sShpCCheck <- checkApply(sShpC)}
  
  
  #   ---- Update the tblCellStatus csv so that this cell is officially checked in.
  #   ---- The real deal version.  
  assign$digiEndTime <- as.POSIXct(assign$digiEndTime,tz="America/Denver")
  assign$buffEndTime <- as.POSIXct(assign$buffEndTime,tz="America/Denver")
  
  #   ---- First, update the cell that was digitized.  
  assign[assign$Grid_ID == theNext,]$openStatus <- 1
  assign[assign$Grid_ID == theNext,]$digiStatus <- 0
  assign[assign$Grid_ID == theNext,]$digiEndTime <- as.POSIXct(Sys.time(),tz="America/Denver",format="%Y%m%d %H%M%S")
  assign[assign$Grid_ID == theNext,]$doneStatus <- 1

  #   ---- Next, update the cells that were locked via buffering.
  assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffStatus <- 0
  assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffUserID <- NA
  assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffPartner <- NA
  assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffEndTime <- as.POSIXct(Sys.time(),tz="America/Denver",format="%Y%m%d %H%M%S")
  assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffLockGrid_ID <- NA

  #   ---- The running versions. 
  vList <- dir("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus")
  version <- max(substr(vList,nchar(vList) - 9,nchar(vList) - 4))
  newVersion <- sprintf("%06d",(as.numeric(version) + 1))
  write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus/tblCellStatus",newVersion,".csv"),row.names=FALSE)
  
  #   ---- Remove the lock.  
  file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
  cat(paste0("\nThe status of your new cell has successfully been updated in tblCellStatus.csv.\n"))
  
  write.csv(assign,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus.csv",row.names=FALSE)

  cat(paste0("Cell ",theNext," is now checked in.  Feel free to start digitizing another.\n"))
}  
  


