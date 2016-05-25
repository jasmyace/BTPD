makeBigShape <- function(){
  
  
  #   ---- Define useful projections.  
  projAEAc <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
  
  #   ---- Get folder structure.  
  tblFolders <- getFolderStatus()
  
  #   ---- Get the current list of who has what.  
  assign <- getCellStatus()
  
  #   ---- Get users. 
  tblNames <- checkUser(userID=100)
  
  #   ---- Get sampling.
  tblRanks <- getRankStatus()
  tblRanks <- tblRanks[order(tblRanks$sampleID),]
  
  done <- assign[assign$doneStatus == 1,]
  done <- merge(done[,c('Grid_ID','doneStatus','digiDouble','digiUserID')],tblFolders[,c('Grid_ID','Range')],by=c('Grid_ID'),all.x=TRUE)
  done <- merge(done,tblNames[,c('userID','FirstName')],by.x=c('digiUserID'),by.y=c('userID'),all.x=TRUE)
  done <- merge(done,tblRanks[,c('Grid_ID','sampleID')],by=c('Grid_ID'),all.x=TRUE)
  
  done <- done[order(done$sampleID),]
  
  #   ---- Get the shapefiles that should be done.  
  done$folder <- paste0('//lar-file-srv/Data/BTPD_2016/Digitizing/',done$Range,'/',done$Grid_ID)
  
  #   ---- Need to see if we have shapefiles with no features. This happens often.  
  checkShp <- function(folder,shp){                                                                 
    if(is.null(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message)){
      shp2 <- tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)
    } else if(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message == "no features found" ){
      shp2 <- "no features found"                                     
    }  
  }
  
  #   ---- Assemble all the towns in all cells digitized up to this point.  
  nShps <- nrow(done)
  townShps <- vector('list',nShps)
  theCells <- vector('list',nShps)
  theTowns <- NULL
  for(i in 1:nShps){
    townShps[[i]] <- checkShp(done[i,]$folder,paste0('p',done[i,]$FirstName,'_Towns_',done[i,]$Grid_ID))
    theCells[[i]] <- readOGR(done[i,]$folder,paste0('LocalGrid_',done[i,]$Grid_ID),verbose=FALSE)
    
    if( class(townShps[[i]]) == "SpatialPolygonsDataFrame" ){
      iTowns <- townShps[[i]]@data
      townShps[[i]]@data$Grid_ID <- done[i,]$Grid_ID
    } else {
      iTowns <- data.frame(Town_ID=NA,Grid_ID=NA)
    }
    iTowns$Grid_ID <- done[i,]$Grid_ID
    theTowns <- rbind(theTowns,iTowns)
    
    #   ---- Convert the line shapefile we read in into a 
    #   ---- polygonal one -- necessary for g functions.
    localGridp <- as(theCells[[i]],"SpatialPoints")
    c1 <- localGridp@coords
    P1 = Polygon(c1)
    Ps1 = Polygons(list(P1), ID="a")
    theCell = SpatialPolygons(list(Ps1))
    theCell@proj4string <- CRS(projAEAc) 
    theCells[[i]] <- theCell
    
    #   ---- Check for floaters.
    if( class(townShps[[i]]) == "SpatialPolygonsDataFrame" ){
      if( sum(gDisjoint(townShps[[i]],theCells[[i]],byid=TRUE)) != 0 ){
        plot(theCells[[i]])
        plot(townShps[[i]],add=TRUE)
        x <- (theCells[[i]]@bbox[1,2] + theCells[[i]]@bbox[1,1] ) / 2
        y <- (theCells[[i]]@bbox[2,2] + theCells[[i]]@bbox[2,1] ) / 2
        text(x,y,paste0("Problem with cell ",done[i,]$Grid_ID,"."))
        stop
      }
    }
  }
  
  #   ---- Given the list of towns, paste together into one nice shapefile. 
  allShps <- NULL
  first <- 0
  triggered <- 0
  for(i in 1:nShps){
    if( class(townShps[[i]]) == "SpatialPolygonsDataFrame" ){
      nR <- length(slot(townShps[[i]],"polygons")); 
      if(triggered == 0){
        first <- 1
      }
      if(first == 1){                                                                                                                                                                               # for 1st shp file, do this
        uidR          <- 1;                                                                                                                                                               # make a unique id
        allShps       <- spChFIDs(townShps[[i]], as.character(uidR:(uidR + nR - 1)));                                                                                                    # make feature id of polygons unique
        uidR          <- uidR + nR;   
        triggered     <- 1
        first         <- 0
      } else {                                                                                                                                                                                  # for other than 1st shp file, do this
        townShps[[i]] <- spChFIDs(townShps[[i]], as.character(uidR:(uidR + nR - 1)));                                                                                                    # make feature id of polygons unique
        uidR          <- uidR + nR;                                                                                                                                                       # make unique id for all polys
        allShps       <- spRbind(allShps,townShps[[i]]);                                                                                                                                   # union ith shp file with all previous ones
      }   
    }
  }
  
  nTowns <- nrow(allShps)
  
  #   ---- Check for loop-the-loops, etc.  
  gValid <- gIsValid(allShps,byid=TRUE)
  if( sum(gValid) != nTowns ){
    warning("Check gValid. Probably at least one town with remaining sliver.")
  }
  
  grid <- readOGR("//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Grid_CO_Ranked","BTPD_Grid_CO_Ranked",verbose=FALSE)

  #   ---- Check for towns that tangent a grid-cell border.  
  gg <- gIntersection(allShps,grid,byid=TRUE)
  if( !(class(gg) == "SpatialPolygons") ){
    warning("Intersection of towns with the grid found something other than SpatialPolygons.")
  }
  
  #   ---- Make sure towns don't overlap one another.  
  gInts <- gIntersects(allShps,allShps,byid=TRUE)
  nTowns <- nrow(allShps)
  if( sum(gInts) != nTowns ){
    warning("Probable self-town intersection.")      
  }
  
  #   ---- Check for towns numbered zero.  
  theZeros <- theTowns[theTowns$Town_ID == 0 & !is.na(theTowns$Town_ID),]
  if( nrow(theZeros) != 0 ){
    warning("Town_IDs of zeros observed.")
  }
  
  #   ---- Check for sequentially numbered towns.  
  theReals <- theTowns[!is.na(theTowns$Town_ID),]
  theReals <- theReals[order(theReals$Grid_ID,theReals$Town_ID),]
  theReals$checkRank <- unlist(tapply(theReals$Town_ID,factor(theReals$Grid_ID),function(x) seq(1,length(x),1)))
  misNumbered <- theReals[theReals$Town_ID != theReals$checkRank,]
  if( nrow(misNumbered) > 0 ){
    warning("Town_IDs appear misnumbered.")
  }
  
  
  
  #   ---- Store for safekeeping.
  #hold <- allShps
  allShps <- hold
  
  
  #   ---- Bring in information on towns.
  allShps@data$ID <- sapply(allShps@polygons,function(x) slot(x,"ID"))
  allShps@data$R_ID <- seq(1,nrow(allShps@data),1)
  allShps@data$AreaM2 <- gArea(allShps,byid=TRUE)
  allShps@data$AreaAcres <- gArea(allShps,byid=TRUE) * 0.000247105
  allShps@data$PerimM <- gLength(allShps,byid=TRUE)
  
  #   ---- Determine the number of holes per town.
  nTowns <- dim(allShps)[1]
  indTowns <- vector("list",nTowns)
  holes <- matrix(NA,nTowns,ncol=2,nrow=nTowns)
  for(i in 1:nTowns){
    indTowns[[i]] <- SpatialPolygons(list(allShps@polygons[[i]]))
    holes[i,1] <- indTowns[[i]]@polygons[[1]]@ID
    holes[i,2] <- sum(sapply(indTowns[[i]]@polygons[[1]]@Polygons,function(x) slot(x,"hole")))
  }
  holes <- data.frame(ID=holes[,1],nHoles=holes[,2],stringsAsFactors=FALSE)
  
  allShps@data <- merge(allShps@data,holes,by=c('ID'),all.x=TRUE,all.y=TRUE)
  allShps@data <- allShps@data[order(allShps@data$R_ID),]
  
  allShps@data$CFactor <- ifelse(allShps@data$nHoles == 0,(allShps@data$PerimM)^2 / 4 / pi / allShps@data$AreaM2,NA)
  
  allShps@data <- merge(allShps@data,tblRanks[,c('Grid_ID','sampleID')],by=c('Grid_ID'),all.x=TRUE)
  allShps@data <- allShps@data[order(allShps@data$R_ID),]
  
  towns <- data.frame(nTowns=tapply(allShps@data$Town_ID,factor(allShps@data$Grid_ID),function(x) length(x)))
  towns$Grid_ID <- rownames(towns)
  rownames(towns) <- NULL
  
  allShps@data <- merge(allShps@data,towns,by=c('Grid_ID'),all.x=TRUE)
  allShps@data <- allShps@data[order(allShps@data$R_ID),]
  
  allShps@data <- merge(allShps@data,done[,c('Grid_ID','time','digiUserID','digiPartner')],by=c('Grid_ID'),all.x=TRUE)
  allShps@data <- merge(allShps@data,tblNames[,c('userID','FirstName')],by.x=c('digiUserID'),by.y=c('userID'),all.x=TRUE)
  allShps@data <- allShps@data[order(allShps@data$R_ID),]
  
  rownames(allShps@data) <- NULL
  
  return(allShps)
  
}