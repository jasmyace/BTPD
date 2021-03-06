#' @export checkInCell
#'   
#' @title Check in a cell when complete.


checkInCell <- function(theNext,userID){
       
      # theNext <- "CO167004"
      # userID <- 296
      
#   putDownLock(userID)
# 
#   #   ---- Make sure the user who put down the lock can continue.  
#   if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
#     if(userID != read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
#       stop("Function call locked out after several attempts.  Try again.\n")
#     }
#   }
#   
#   Sys.sleep(15)

      #   ---- Check for a lock on table tblCellStatus.csv
      lock <- file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
      if(lock == TRUE){
        stop("The function is currently locked;  try again in a minute.")
      } else if(lock == FALSE){
        #   ---- Lock the table tblCellStatus so that two users cannot update
        #   ---- it at the same time. 
        lockdf <- data.frame(userID=userID)
        write.table(lockdf,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",row.names=FALSE)
        Sys.sleep(15)
      } else {
        stop("Something is really wrong.\n")
      }
      
  out <- tryCatch(
    {
          
      assign <- read.csv("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus.csv",as.is = TRUE)
      #assign <- read.csv("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus/tblCellStatus001588.csv",as.is = TRUE)
      if( assign[assign$Grid_ID == theNext,]$digiUserID == userID & (assign[assign$Grid_ID == theNext,]$open == 1 | assign[assign$Grid_ID == theNext,]$doneStatus == 1) ){
        
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
          }
        }
        stop("It appears as if the Grid_ID of the cell you're attempting to check in hasn't been checked out, or is already complete.  Investigate.\n")
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
        
        #   ---- It could be neither primary nor secondary found a town, and they skipped the makeReconcile
        #   ---- step.  In that case, there is neither a reconcile shapefile nor text file.
        if( length(rShpC) >= 1 ){
          
          if( is.na(rShpC) | is.na(sShpC) | is.null(sShpC) | is.null(rShpC) | length(sShpC) == 0 | length(rShpC) == 0 ){
          
            #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
            if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
              if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
                file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
              }
            }
            stop("The folder tied to the doubly sampled Grid_ID provided lacks the requisite 's' and 'reconciling' shapefiles.  Investigate.")
          }
        }
      } 
  
      #   ---- Find the primary shapefiles.  
      pShp <- sShp <- NA
      
      pInd <- substr(files,1,1) == 'p' & 
        substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
        grepl("Towns",files,fixed=TRUE) &
        !grepl("Copy",files,fixed=TRUE)
      
      pShpC <- substr(files[pInd],1,nchar(files[pInd]) - 4)
      
      if( is.na(pShpC) | is.null(pShpC) | length(pShpC) == 0 ){
        
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
          }
        }
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
            checkCellValidity(Shp,userID)
          },
          warning = function(cond){
            return(1)
          }
        )
        return(out)
      }
      
      #   ---- A helper function to make nrow work, regardless of object.  
      nrowCool <- function(obj){
        if( class(obj) == "character" ){
          ans <- 0
        } else {
          ans <- nrow(obj)
        }
        return(ans)
      }
      
      #   ---- If either primary or secondary recorded one town, we MUST have a 
      #   ---- reconciling shapefile.  Abort if we do not.  
      if( double == 1 ){
        if( ( nrowCool(pShp) + nrowCool(sShp) ) > 0 & sum(rInd) == 0 ){
        
          #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
          if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
            if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
              file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
            }
          }
          stop("The primary or secondary shapefile contains a town, but no reconciling file exists.  Check if step 6 was performed.\n")
        }
      }
  
      #   ---- Apply the checking function, collecting any evidence of a warning.
      checkApply <- function(shp){
        if(class(get(paste0(substr(shp,1,1),'Shp'))) == "SpatialPolygonsDataFrame"){
          lapply(shp,readCheckCellValidity)
          
          #   ---- The function call to readCheckCellValidity gets rid of the lock.  Put it back for the 
          #   ---- function checkInCell.
          
          #   ---- Check for a lock on table tblCellStatus.csv
#           lock <- grep("tblCellStatusLOCK",dir("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database"),fixed=TRUE)
#           if(length(lock) > 0){
#             stop("The function is currently locked;  try again in a minute.")
#           } else {
#             #   ---- Lock the table tblCellStatus so that two users cannot update
#             #   ---- it at the same time. 
#             lockdf <- data.frame(userID=userID)
#             write.table(lockdf,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",row.names=FALSE)
#           }
          
        } else {
          cat(paste0("The check of ",shp," found no towns to check.  Be sure this is correct. If so, continue.\n"))
        }
      }
  
      pShpCCheck <- checkApply(pShpC)
      if( double == 1){sShpCCheck <- checkApply(sShpC)}
      
      #   ---- At this point, for double cells, it could be the user is jumping directly to checking in the 
      #   ---- cell, without creating the reconciling mxd nor shapefile.  This is okay if there are no towns
      #   ---- found by neither the primary nor secondary.  But, we need to put down a text file, so we know 
      #   ---- that the cell is done, and that no towns was the final conclusion.  
      
      #   ---- Hopefully they just don't check in the cell without truly reconciling.  We don't really 
      #   ---- check for that at this point.  
      txtFileDir <- dir(paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theRange,"/",theNext))
      txtFileHere <- txtFileDir == paste0("reconciling_Towns_",theNext,".txt")
      if( double == 1 & sum(txtFileHere) == 0 & class(pShp) != "SpatialPolygonsDataFrame" & class(sShp) != "SpatialPolygonsDataFrame" ){
        
        #   ---- Note that I don't call setUpReconcile...that throws a lock down.  What happens if somebody
        #   ---- steals it in the middle of this function call?  Do I already have a lock?  Doesn't matter,
        #   ---- I don't think.  
        allShps <- data.frame(Recon_T_ID=character(),new1=character(),new2=character(),Recon_DIE=character())
        
        tblNames <- checkUser(userID)
        pName <- tblNames[tblNames$userID == assign[assign$Grid_ID == theNext,]$digiPrimary,]$FirstName
        sName <- tblNames[tblNames$userID == assign[assign$Grid_ID == theNext,]$digiSecondary,]$FirstName
        
        names(allShps)[names(allShps) == "new1"] <- paste0(substr(pName,1,5),"_T_ID")
        names(allShps)[names(allShps) == "new2"] <- paste0(substr(sName,1,5),"_T_ID")
        
        write.table(allShps,paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theRange,"/",theNext,"/","reconciling_Towns_",theNext,".txt"),row.names=FALSE)
      }
  
      #   ---- Update the tblCellStatus csv so that this cell is officially checked in.
      #   ---- The real deal version.  Note that we convert these columns to POSIX, which make 
      #   ---- them look different than the others (dates are now separated with a '-' instead of
      #   ---- a '/', but it doesn't matter as long as they all look the same in the end.)
      assign$digiEndTime <- as.POSIXlt(assign$digiEndTime,format="%m/%d/%Y %H:%M",tz="America/Denver")      
      assign$buffEndTime <- as.POSIXlt(assign$buffEndTime,format="%m/%d/%Y %H:%M",tz="America/Denver")
      
      #   ---- First, update the cell that was digitized.  
      assign[assign$Grid_ID == theNext,]$openStatus <- 1
      assign[assign$Grid_ID == theNext,]$digiStatus <- 0
      assign[assign$Grid_ID == theNext,]$digiEndTime <- as.POSIXlt(Sys.time(),format="%m/%d/%Y %H:%M",tz="America/Denver")
      assign[assign$Grid_ID == theNext,]$doneStatus <- 1
    
      #   ---- Next, update the cells that were locked via buffering.
      assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffStatus <- 0
      assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffUserID <- NA
      assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffPartner <- NA
      assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffEndTime <- as.POSIXlt(Sys.time(),tz="America/Denver",format="%m/%d/%Y %H:%M")
      assign[assign$buffLockGrid_ID == theNext & !is.na(assign$buffLockGrid_ID),]$buffLockGrid_ID <- NA
      
      #   ---- Put these date fields back to character, so Excel doesn't get confused. 
      assign$digiEndTime <- strftime(assign$digiEndTime,format="%m/%d/%Y %H:%M")
      assign$buffEndTime <- strftime(assign$buffEndTime,format="%m/%d/%Y %H:%M")
      
      #   ---- The running versions. 
      vList <- dir("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus")
      version <- max(substr(vList,nchar(vList) - 9,nchar(vList) - 4))
      newVersion <- sprintf("%06d",(as.numeric(version) + 1))
      write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus/tblCellStatus",newVersion,".csv"),row.names=FALSE)
      
      write.csv(assign,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus.csv",row.names=FALSE)
      
      #   --- Give the computer time to catch up.  
      Sys.sleep(20)
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
        }
      }
      
      cat(paste0("\nThe status of your new cell has successfully been updated in tblCellStatus.csv.\n"))
      cat(paste0("Cell ",theNext," is now checked in.  Feel free to start digitizing another.\n"))
    },
    error=function(cond){
      message("It appears you broke the function;  however, any lock originally set has been removed.\n")
      message("Determine the cause of failure, remedy, and then try again.  Ask for help if this result seems surprising.\n")
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
        }
      }
      
      message("Here's the original error message:\n")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )
}  



