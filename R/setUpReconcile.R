#' @export setUpReconcile
#'   
#' @title Set up files for reconciliation.

setUpReconcile <- function(theNext,userID){
  out <- tryCatch(
    {
      
      # theNext <- "CO165342"
  
      #   ---- Get folder structure.  
      tblFolders <- getFolderStatus()
      theRange <- tblFolders[tblFolders$Grid_ID == theNext,]$Range
      
      folder <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Digitizing/",theRange,"/",theNext)
      files <- dir(folder)
    
      #   ---- Find the primary and secondary shapefiles.  
      pShp <- sShp <- NA
     
      pInd <- substr(files,1,1) == 'p' & 
              substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
              grepl("Towns",files,fixed=TRUE) &
             !grepl("Copy",files,fixed=TRUE)
      
      pShpC <- substr(files[pInd],1,nchar(files[pInd]) - 4)
      
      sInd <- substr(files,1,1) == 's' & 
              substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
              grepl("Towns",files,fixed=TRUE) &
              !grepl("Copy",files,fixed=TRUE)
      
      sShpC <- substr(files[sInd],1,nchar(files[sInd]) - 4)
      
      if( is.na(pShpC) | is.na(sShpC) | is.null(pShpC) | is.null(sShpC) ){
        stop("The folder tied to the Grid_ID provided lacks the requisite 'p' and 's' 'Towns' shapefiles.  Investigate.")
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
      sShp <- checkShp(folder,sShpC)

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
  
      #   ---- Apply the checking function, collecting any evidence of a warning.
      checkApply <- function(shp){
        if(class(get(paste0(substr(shp,1,1),'Shp'))) == "SpatialPolygonsDataFrame"){
          lapply(shp,readCheckCellValidity)
          
          #   ---- The function call to readCheckCellValidity gets rid of the lock.  Put it back for the 
          #   ---- function checkInCell.
          
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
          
        } else {
          cat(paste0("The check of ",shp," found no towns to check.  Be sure this is correct. If so, continue.\n"))
        }
      }
  
      pShpCCheck <- checkApply(pShpC)
      sShpCCheck <- checkApply(sShpC)
      
      #   ---- Stop the wrapper function if there is a shapefile problem.   
      if( !is.null(pShpCCheck[[1]][1]) ){stop(paste0("Shapefile ",pShpC," has errors. Investigate."))}
      if( !is.null(sShpCCheck[[1]][1]) ){stop(paste0("Shapefile ",sShpC," has errors. Investigate."))}
      
      #   ---- Get names.
      pName <- unlist(strsplit(pShpC,"_",fixed=TRUE)[[1]])[1]
      pName <- substr(pName,2,nchar(pName))
    
      sName <- unlist(strsplit(sShpC,"_",fixed=TRUE)[[1]])[1]
      sName <- substr(sName,2,nchar(sName))
     
      townShps <- vector("list",2)
      townShps[[1]] <- pShp
      townShps[[2]] <- sShp
      
      #   ---- Given the list of neighboring towns, paste together 
      #   ---- into one nice shapefile; i.e., build reconciling set.  
      allShps <- NULL
      if( !(class(townShps[[1]]) == "SpatialPolygonsDataFrame") ){
        allShps <- townShps[[2]]
      } else if( !(class(townShps[[2]]) == "SpatialPolygonsDataFrame") ){
        allShps <- townShps[[1]]
      } else {
        for(j in 1:length(townShps)){
          if(!is.null(townShps[[j]])){
            nR <- length(slot(townShps[[j]],"polygons"))
            if(j == 1){
              uidR          <- 1
              allShps       <- spChFIDs(townShps[[j]], as.character(uidR:(uidR + nR - 1)))
              uidR          <- uidR + nR      
            } else {                                                          
              townShps[[j]] <- spChFIDs(townShps[[j]], as.character(uidR:(uidR + nR - 1)))
              uidR          <- uidR + nR                                                     
              allShps       <- spRbind(allShps,townShps[[j]])                       
            }   
          }
        }
      }
    
      #   ---- Add in necessary columns. 
      if( class(allShps) == "SpatialPolygonsDataFrame" ){
        allShps@data$Recon_T_ID <- rep("",nrow(allShps))
      
        allShps@data$new1 <- rep('x',nrow(allShps))
        names(allShps@data)[names(allShps@data) == "new1"] <- paste0(substr(pName,1,5),"_T_ID")
      
        allShps@data$new2 <- rep('x',nrow(allShps))
        names(allShps@data)[names(allShps@data) == "new2"] <- paste0(substr(sName,1,5),"_T_ID")  
      
        allShps@data$Recon_DIE <- rep('',nrow(allShps))
        allShps@data$Town_ID <- NULL
        
        #   ---- Write shapefile for reconciling to folder.  
        writeOGR(allShps,folder,paste0("reconciling_Towns_",theNext),overwrite_layer=TRUE,driver="ESRI Shapefile",verbose=FALSE)
      
      } else {
    
        # fileList <- list.files("//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Digitizing_Template",full.names=TRUE)
        # to <- paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theRange,"/",theNext)
        # file.copy(fileList,to,overwrite=TRUE,recursive = FALSE,copy.mode = TRUE)
        # ext <- c("cpg","dbf","prj","sbn","sbx","shp","shx")
        # file.rename(paste0(to,"/BTPD_Digitizing_Template.",ext),paste0(to,"/reconciling_Towns_",theNext,".",ext))
        # 
        # allShps <- readOGR(to,paste0("reconciling_Towns_",theNext),verbose=FALSE)
        # 
        allShps <- data.frame(Recon_T_ID=character(),new1=character(),new2=character(),Recon_DIE=character())
        names(allShps)[names(allShps) == "new1"] <- paste0(substr(pName,1,5),"_T_ID")
        names(allShps)[names(allShps) == "new2"] <- paste0(substr(sName,1,5),"_T_ID")
        
        write.table(allShps,paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theRange,"/",theNext,"/","reconciling_Towns_",theNext,".txt"),row.names=FALSE)
        
      }
  
      #   ---- Make an empty .mxd (although it connects to the NAIP imagery).
      fileList <- list.files("//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Digitizing_Template",full.names=TRUE)
      fileList <- fileList[substr(fileList,nchar(fileList) - 3,nchar(fileList)) == ".mxd"]
      file.copy(fileList,folder,overwrite=TRUE,recursive = FALSE,copy.mode = TRUE)
      file.rename(paste0(folder,"/BlankCO_NAIP.mxd"),paste0(folder,"/reconciling_",theNext,".mxd"))
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
        }
      }
      
    },
    error=function(cond){
      message("It appears you broke the function;  however, any lock originally set has been removed.\n")
      message("Determine the cause of failure, remedy, and then try again.  Ask for help if this result seems surprising.\n")
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
        }
      }
      
      message("Here's the original error message:\n")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    }
  )
}  
