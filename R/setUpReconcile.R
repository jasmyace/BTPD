#' @export setUpReconcile
#'   
#' @title Set up files for reconciliation.

setUpReconcile <- function(theNext){
  
  # theNext <- "CO156212"
  
#   #   ---- Ensure we have all the necessary helper functions.  
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/getFolderStatus.R")   
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/checkCellValidity.R")  
#   source("//lar-file-srv/Data/BTPD_2016/Analysis/R/setUpReconcile.R")   
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
  
  #   ---- Get folder structure.  
  tblFolders <- getFolderStatus()
  theRange <- tblFolders[tblFolders$Grid_ID == theNext,]$Range
  
  folder <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Digitizing/",theRange,"/",theNext)
  files <- dir(folder)
  
  #   ---- Find the primary and secondary shapefiles.  
  pShp <- sShp <- NA
  
  pInd <- substr(files,1,1) == 'p' & 
          substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
          grepl("Towns",files,fixed=TRUE)
  
  pShpC <- substr(files[pInd],1,nchar(files[pInd]) - 4)
  
  sInd <- substr(files,1,1) == 's' & 
    substr(files,nchar(files) - 3,nchar(files)) == ".shp" &
    grepl("Towns",files,fixed=TRUE)
  
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
  
  # ---- Set up the items in the out list.
  # theNextdf <- data.frame(theNext=theNext,stringsAsFactors=FALSE)
  # theRangedf <- data.frame(theRange=theRange,stringsAsFactors=FALSE)
  # thepFirstNamedf <- data.frame(thepFN=pName,stringsAsFactors=FALSE)
  # thesFirstNamedf <- data.frame(thesFN=sName,stringsAsFactors=FALSE)
  # out <- list(assign=assign,theNext=theNextdf,theRange=theRangedf,thepFirstName=thepFirstNamedf,thesFirstName=thesFirstNamedf)
  # return(out)
}
