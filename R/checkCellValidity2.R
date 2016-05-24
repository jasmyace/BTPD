#' @export checkCellValidity
#'   
#' @title Check cell validity.


checkCellValidity <- function(shp){

  # shp <- "pJason_Towns_CO114668"#"sCarissa_Towns_CO156212"
  # shp <- "reconciling_Towns_CO116397"
  
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
  checkShp <- function(folder,shp){                                                                 
    if(is.null(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message)){
      shp2 <- tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)
    } else if(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message == "no features found" ){
      shp2 <- 'no features found'                                         
    }  
  }
  
  #   ---- Get the shapefile to check.
  shpfile <- checkShp(shpDir,shp)
  localGrid <- readOGR(shpDir,paste0("LocalGrid_",Grid_ID),verbose=FALSE)
  
  #   ---- If the shapefile has no features, there is nothing to check. 
  if( !(class(shpfile) == "SpatialPolygonsDataFrame") ){
    warning("No digitized towns found.  Make sure this is correct.  If so, continue.\n")
  }
  
  if( class(shpfile) == "SpatialPolygonsDataFrame" ){
    
    #   ---- Convert the line shapefile we read in into a 
    #   ---- polygonal one -- necessary for g functions.
    localGridp <- as(localGrid,"SpatialPoints")
    c1 <- localGridp@coords
    P1 = Polygon(c1)
    Ps1 = Polygons(list(P1), ID="a")
    theCell = SpatialPolygons(list(Ps1))
    theCell@proj4string <- shpfile@proj4string
    
    grid <- readOGR("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Grid_CO_Ranked","BTPD_Grid_CO_Ranked",verbose=FALSE)
    grid <- as(grid,"SpatialLinesDataFrame")
    
    
    #   ---- Check for loop-the-loops, etc.  Project into the
    #   ---- projection utilized by the online NAIP imagery, 
    #   ---- so as to make sure of coordinates returned.  
    gValid <- gIsValid(spTransform(shpfile,CRS(proj3857)),byid=TRUE)
    if(sum(gValid) < length(gValid)){
      
      #   ---- Remove the lock, if it exists.  
      if(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")){file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")}
      
      stop("\nThis Grid_ID's set of towns has at least one sliver.  Investigate via the coordinates provided.\n")
      
    }

    
    #   ---- Check for floaters.
    if( sum(gDisjoint(shpfile,theCell,byid=TRUE)) != 0 ){
      plot(shpfile)
      plot(theCell)
      x <- (theCell@bbox[1,2] + theCell@bbox[1,1] ) / 2
      y <- (theCell@bbox[2,2] + theCell@bbox[2,1] ) / 2
      text(x,y,paste0("Problem with cell ",Grid_ID,"."))
      stop("At least one town is disjointed from Grid_ID ",Grid_ID,".  Investigate.\n")
    }

  
    #   ---- Make sure towns don't overlap one another.  
    gInts <- gIntersects(shpfile,shpfile,byid=TRUE)
    nTowns <- nrow(shpfile@data)
    
    if(sum(gInts) != nTowns){
      
      #   ---- Remove the lock, if it exists.  
      if(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")){file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")}

      stop("This Grid_ID's set of towns has at least one town that overlaps another.  Investigate.\n")
      
    }

    
    #   ---- Check for towns numbered zero.  
    if( substr(shp,1,5) == "recon" ) {
      df <- data.frame(Town_ID=shpfile@data[order(shpfile@data$Recon_T_ID),])
    } else {
      df <- data.frame(Town_ID=shpfile@data[order(shpfile@data$Town_ID),])
      
    }
    

    

    #   ---- Check for sequentially numbered towns.  
    df <- data.frame(Town_ID=shpfile@data[order(shpfile@data$Town_ID),])
    df$seq <- seq(1,nTowns,1)
    if(df[nTowns,]$Town_ID != df[nTowns,]$seq){
      
      #   ---- Remove the lock, if it exists.  
      if(file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")){file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")}
      
      stop("This Grid_ID's set of towns has poor Town_ID numbering.  Investigate.\n")
      
    }
    
  
    assign("last.warning", NULL, envir = baseenv())
    cat(paste0("The check of ",shp," is complete, with no errors found.\n"))
    
  }
  
}
  