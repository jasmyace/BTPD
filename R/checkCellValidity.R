checkCellValidity <- function(shp){

  # shp <- "pJason_Towns_CO156212"#"sCarissa_Towns_CO156212"
  
  proj3857  <- "+init=epsg:3857"   # used by esri online naip imagery.  
  Grid_ID <- unlist(strsplit(shp,"_",fixed=TRUE)[[1]])[3]
  
  #   ---- Get folder structure.  
  tblFolders <- getFolderStatus()
  theRange <- tblFolders[tblFolders$Grid_ID == Grid_ID,]$Range
  
  #   ---- Now that we know the pieces, find the folder.  
  shpDir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Digitizing/",theRange,"/",Grid_ID)
  
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
      stop("\nThis Grid_ID's set of towns has at least one sliver.  Investigate via the coordinates provided.\n")
    }
    
    
    #   ---- Check for floaters.
    #   ---- Actually, we expect these.  
    # gWithin <- gWithin(shpfile,theCell,byid=TRUE)
    # if(sum(gWithin) < length(gWithin)){
    #   warning("This Grid_ID's set of towns has at least one floater town.  Investigate.\n")
    # }
  
  
    #   ---- Check for towns that tangent a grid-cell border.  
    #   ---- Can't seem to trigger this 5/17/2016.
    # gg <- gIntersection(shpfile,grid,byid=TRUE)
  
  
    #   ---- Make sure towns don't overlap one another.  
    gInts <- gIntersects(shpfile,shpfile,byid=TRUE)
    nTowns <- nrow(shpfile@data)
    
    if(sum(gInts) != nTowns){
      stop("This Grid_ID's set of towns has at least one town that overlaps another.  Investigate.\n")
    }


    #   ---- Check for sequentially numbered towns.  
    df <- data.frame(Town_ID=shpfile@data[order(shpfile@data$Town_ID),])
    df$seq <- seq(1,nTowns,1)
    if(df[nTowns,]$Town_ID != df[nTowns,]$seq){
      stop("This Grid_ID's set of towns has poor Town_ID numbering.  Investigate.\n")
    }
  
    assign("last.warning", NULL, envir = baseenv())
    cat(paste0("The check of ",shp," is complete, with no errors found.\n"))
    
  }
  
}
  