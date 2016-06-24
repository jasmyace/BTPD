#' @export checkCellValidity
#'   
#' @title Check cell validity.


checkCellValidity <- function(shp,userID){

  # shp <- "pAli_Towns_CO141712"#"sCarissa_Towns_CO156212"
  # shp <- "reconciling_Towns_CO137799"   userID <- 873

      
  #   ---- Because we also call checkCellValidity when we check in a cell, which itself calls
  #   ---- for a lock, remove it, if it exists, and then throw down another.  
  
  #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
  if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
    if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
      invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
    }
  }
  
  #   ---- Check for a lock on table tblCellStatus.csv
  lock <- file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
  if(lock == TRUE){
    stop("The function is currently locked;  try again in a minute.")
  } else if(lock == FALSE){
    #   ---- Lock the table tblCellStatus so that two users cannot update
    #   ---- it at the same time. 
    lockdf <- data.frame(userID=userID)
    write.table(lockdf,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",row.names=FALSE)
  } else {
    stop("Something is really wrong.\n")
  }
      
  proj3857  <- "+init=epsg:3857"   # used by esri online naip imagery.  
  Grid_ID <- unlist(strsplit(shp,"_",fixed=TRUE)[[1]])[3]
      
  #   ---- Get folder structure.  
  tblFolders <- getFolderStatus()
  theRange <- tblFolders[tblFolders$Grid_ID == Grid_ID,]$Range
      
  #   ---- Now that we know the pieces, find the folder.  
  shpDir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Digitizing/",theRange,"/",Grid_ID)
      
  #   ---- If this is a reconciling cell, it could be a text file, instead of a 
  #   ---- shapefile, by design.  This happens when reconciliation leads to 
  #   ---- no towns digitized.  Check for this to prevent errors below.
  if( length(dir(shpDir)[dir(shpDir) == paste0(shp,".txt")]) == 1){
    stop("This Grid_ID's set of reconciled towns numbers zero.  No checks will be performed.  If correct, continue to step 9.\n")
  }

  #   ---- Need to see if we have shapefiles with no features. This happens often.  
  checkShp <- function(folder,shpA){
    if(is.null(tryCatch(readOGR(folder,shpA,verbose=FALSE), warning = function(w) w)$message)){
      shp2 <- tryCatch(readOGR(folder,shpA,verbose=FALSE), warning = function(w) w)
    } else if(tryCatch(readOGR(folder,shpA,verbose=FALSE), warning = function(w) w)$message == "no features found" ){
      shp2 <- 'no features found'
    }
  }
  
  #   ---- Get the shapefile to check.
  shpfile <- tryCatch(
    {
      checkShp(shpDir,shp)
    },
    error=function(cond){
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
        }
      }
      
      message(cond)
      message("\nYou possibly no longer possess this cell.  Investigate.\n")
      return(1)
    }
  )
    
  localGrid <- checkShp(shpDir,paste0("LocalGrid_",Grid_ID))
 
  #   ---- If the shapefile has no features, there is nothing to check. 
  if( !(class(shpfile) == "SpatialPolygonsDataFrame") ){
    warning("No digitized towns found.  Make sure this is correct.  If so, continue.\n")
  }
        
  if( class(shpfile) == "SpatialPolygonsDataFrame" ){
        
    #   ---- Convert the line shapefile we read in into a 
    #   ---- polygonal one -- necessary for g functions.
    if( class(localGrid) == "SpatialLinesDataFrame" ){
      localGridp <- as(localGrid,"SpatialPoints")
      c1 <- localGridp@coords
      P1 = Polygon(c1)
      Ps1 = Polygons(list(P1), ID="a")
      theCell = SpatialPolygons(list(Ps1))
      theCell@proj4string <- shpfile@proj4string
    } else {
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
        }
      }
      stop("The shapefile LocalGrid_",Grid_ID," lost its features.  Recreate and try again.")
    }
        
    grid <- readOGR("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Grid_CO_Ranked","BTPD_Grid_CO_Ranked",verbose=FALSE)
    grid <- as(grid,"SpatialLinesDataFrame")
    
    #   ---- Define what we mean by 'No' in the town shapefiles.
    NO <- c("x","X"," x","x "," X","X "," x "," X ")
    
    checkA <- checkB <- checkC <- checkD <- checkE <- 0
    #   ---- Check for loop-the-loops, etc.  Project into the
    #   ---- projection utilized by the online NAIP imagery, 
    #   ---- so as to make sure of coordinates returned. 
    checkA <- tryCatch(
      {
        gValid <- gIsValid(spTransform(shpfile,CRS(proj3857)),byid=TRUE)
        if(sum(gValid) == nrow(shpfile)){
          checkA <- 0
        }
      },
      warning=function(cond){
        
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
          }
        }
        
        message(cond)
        if(substr(cond$message,1,5) == "Ring "){
          message("\nThis Grid_ID's set of towns has at least one sliver.  Investigate via the coordinates provided.\n")
        }
        return(1)
      }
    )
      
    #   ---- Check for floaters.
    checkB <- tryCatch(
      {
        if( sum(gDisjoint(shpfile,theCell,byid=TRUE)) != 0 ){
          stop("At least one town is disjointed from Grid_ID ",Grid_ID,".  Investigate.\n")
        } else {
          checkB <- 0
        }
      },
      error=function(cond){
          
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
          }
        }
        message(cond)
        return(1)
      }
    )

      
    #   ---- Make sure towns don't overlap one another.  
    checkC <- tryCatch(
      {
        if(substr(shp,1,5) == "recon"){
          nGoodTowns <- nrow(shpfile[!(shpfile@data$Recon_T_ID %in% NO),]@data)
          if(nGoodTowns > 0){
            gInts <- gIntersects(shpfile[!(shpfile@data$Recon_T_ID %in% NO),],shpfile[!(shpfile@data$Recon_T_ID %in% NO),],byid=TRUE)
            nTowns <- nrow(shpfile@data[!(shpfile@data$Recon_T_ID %in% NO),])
          } else {
            gInts <- nrow(shpfile@data)
            nTowns <- nrow(shpfile@data)
          }
        } else {
          gInts <- gIntersects(shpfile,shpfile,byid=TRUE)
          nTowns <- nrow(shpfile@data)
        }
          
        if(sum(gInts) != nTowns){
          stop("This Grid_ID's set of towns has at least one town that overlaps another.  Investigate.\n")
        } else {
          checkC <- 0
        }
      },
      error=function(cond){
        
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
          }
        }
        message(cond)
        return(1)
      }
    )
    
          
    #   ---- Check for towns numbered zero. 
    checkD <- tryCatch(
      {
        if( substr(shp,1,5) == "recon" ) {
          df <- shpfile@data
          df <- df[!(df$Recon_T_ID %in% NO),]
          df <- df[order(df$Recon_T_ID),]
          if( sum(df$Recon_T_ID == 0) > 0 ){
            stop("This Grid_ID's set of towns has at least one Recon_T_ID labeled with a zero.  Investigate.\n")
          } else {
            checkD <- 0
          }
        } else {
          df <- data.frame(Town_ID=shpfile@data[order(shpfile@data$Town_ID),])
          if( sum(df$Town_ID == 0) > 0 ){
            stop("This Grid_ID's set of towns has at least one Town_ID labeled with a zero.  Investigate.\n")
          } else {
            checkD <- 0
          }
        }
      },
      error=function(cond){
          
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
          }
        }
        message(cond)
        return(1)
      }
    )


    #   ---- Check for sequentially numbered towns.
    checkE <- tryCatch(
      {
        if( substr(shp,1,5) == "recon" ){
          df <- shpfile@data
          df <- df[!(df$Recon_T_ID %in% NO),]
          
          if(nrow(df) > 0){
            df <- df[order(df$Recon_T_ID),]
            df$seq <- seq(1,nTowns,1)
            if(df[nTowns,]$Recon_T_ID != df[nTowns,]$seq){
              stop("This Grid_ID's set of towns has poor Recon_T_ID numbering.  Investigate variable Recon_T_ID.\n")
            } else {
              checkE <- 0
            }
          } else {
            checkE <- 0
          }
        } else {
          df <- data.frame(Town_ID=shpfile@data[order(shpfile@data$Town_ID),])
          df$seq <- seq(1,nTowns,1)
          if(df[nTowns,]$Town_ID != df[nTowns,]$seq){
            stop("This Grid_ID's set of towns has poor Town_ID numbering.  Investigate variable Town_ID.\n")
          } else {
            checkE <- 0
          }
        }
        # return(0)
      },
      error=function(cond){
        
        #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
        if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
          if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
            invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
          }
        }
        message(cond)
        return(1)
      }
    )
    
     
    assign("last.warning", NULL, envir = baseenv())
    if( sum(checkA,checkB,checkC,checkD,checkE) == 0){
      thing <- as.character(paste0("The check of ",shp," is complete, with no errors found.\n"))
      cat(thing)
      
      #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
      if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
        if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
          invisible(file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))
        }
      }
    }
  } else {
    
    #   ---- If we get here, no towns were found.  So get rid of the lock we threw down and 
    #   ---- call it a day.  
    
    #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
    if(invisible(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt"))){
      if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
        file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
      }
    }
    
  } 
  
  
  
#     },
#     error=function(cond){
#       message("It appears you broke the function;  however, any lock originally set has been removed.\n")
#       message("Determine the cause of failure, remedy, and then try again.  Ask for help if this result seems surprising.\n")
#       
#       #   ---- Remove the lock, if it exists, and the user calling the function placed it there.
#       if(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")){
#         if(userID == read.table("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",stringsAsFactors=FALSE)[2,1]){
#           file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
#         }
#       }
#       
#       message("Here's the original error message:\n")
#       message(cond)
#       message(report)
#       # Choose a return value in case of error
#       return(NA)
#     },
#     warning=function(cond){
#       #message("Here's the original warning message:")
#       message(cond)
#       if(substr(cond$message,1,5) == "Ring "){
#         message("\nThis Grid_ID's set of towns has at least one sliver.  Investigate via the coordinates provided.\n")
#       }
#       # Choose a return value in case of warning
#       return(NA)
#     }
#   )
    
    
}  