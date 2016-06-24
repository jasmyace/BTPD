#' @export checkOutCell
#'   
#' @title Check out a cell.

checkOutCell <- function(userID,tblDir="//lar-file-srv/Data/BTPD_2016/Digitizing"){

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
      
  out <- tryCatch(
    {  
      #   ---- Define useful projections.  
      projAEAc  <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
      
      #   ---- Check to make sure user is legitimate.  
      tblNames <- checkUser(userID)
      pFirstName <- as.character(droplevels(tblNames[tblNames$userID == userID,]$FirstName))
      singleActive <- tblNames[tblNames$userID == userID,]$singleActive
      doubleActive <- tblNames[tblNames$userID == userID,]$doubleActive
      # doubleActive <- 1    # ---- Here for testing purposes.  
      
      #   ---- Get folder structure.  
      tblFolders <- getFolderStatus()
      
      #   ---- Get the current list of who has what.  
      assign <- getCellStatus()
      #assign <- read.csv("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatus/tblCellStatus001589.csv",as.is = TRUE)
      
      assign$digiStartTime <- as.POSIXlt(strptime(assign$digiStartTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
      assign$digiEndTime <- as.POSIXlt(strptime(assign$digiEndTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")  
      assign$buffStartTime <- as.POSIXlt(strptime(assign$buffStartTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
      assign$buffEndTime <- as.POSIXlt(strptime(assign$buffEndTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
  
      anyOpen <- assign[assign$digiStatus == 1 & assign$digiUser == userID & assign$digiPartner == 998,]
      if(nrow(anyOpen) > 0){
        file.remove("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
        stop("Records indicate you have checked out singly digitized cell ",anyOpen[1,]$Grid_ID,".  Finish and close it, and then try again.")
      }
  
      #   ---- Get the BAS rankings.  
      ranks <- getRankStatus()
      
      #   ---- Make a master data frame.
      master <- merge(assign,ranks,by=c("Grid_ID"),all.x=TRUE,all.y=TRUE)
      master <- master[order(master$digiStatus,master$sampleID),]
      
      #   ---- A safeguard to ensure we have all 11,101 records.
      nTotal <- nrow(assign)
      if( !(nTotal == 11101) ){
        warning("Creation of data frame assign does not have the correct number of records.  Disaster!!!  Investigate.\n")
        stop
      }
      
      #   ---- Get the big grid so we can subset to the buffer below.  
      shpDir <- "//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Grid_CO_Ranked"
      shp <- "BTPD_Grid_CO_Ranked"
      shpObj <- readOGR(shpDir,shp,verbose=FALSE)
      
      #   ---- We now need to find a cell whose buffer is available.
      open <- master$openStatus    # The cell still needs to be digitizing.  
      digi <- master$digiStatus    # The cell is locked for digitizing.
      buff <- master$buffStatus    # The cell is locked for buffering.  
      done <- master$doneStatus    # The cell has been digitized and reconciled, if necessary.
      doub <- master$dblSamp       # The cell is a doubly-sampled cell. 
  
      #   ---- Could have a few open cells, but they're locked for digitizing or buffering. 
      #   ---- If somebody is turned off for doubly-sampling, here is where we exclude 
      #   ---- those cells from them.  
      if( doubleActive == 0 ){
        valid <- open & !digi & !buff & !done & !doub    # Want:    1  &  0  &  0  &  0  &  0
      } else {
        valid <- open & !digi & !buff & !done            # Want:    1  &  0  &  0  &  0  
      }
      nValid <- length(valid[valid == TRUE])  
  
      if( sum(open) == 0 & sum(digi) == 0 & sum(buff) == 0 & sum(done) == nTotal ){
        warning("Digitizing is completely done.")
      } else if( sum(open) == 0 & sum(buff) > 0 & sum(done) != nTotal ){
        warning("No new cells left to digitize, but ",sum(buff)," cells currently locked in buffering.  Wait for their release.")
      } else if( nValid > 0 ){
      
        #   ---- Reduce our options to only the list that we know is available for this userID. 
        master <- master[valid,]
        
        #   ---- Assign an index that is the absolute position (in rows) in master.
        i <- 1
        found <- FALSE
        
        #   ---- We don't want to assign a Grid_ID that has neighboring Grid_IDs
        #   ---- currently being digitized.  So, given the first option in the 
        #   ---- list, we have to check and see if its neighbors are available. 
        
        repeat{
          
          #   ---- Check if we found a cell to digitize.
          if( found == TRUE ) break  
          
          #   ---- Get the next available cell in the already-ordered ranking.  
          theNext <- master[i,]$Grid_ID
          theBASN <- master[i,]$sampleID
          theFolder <- tblFolders[tblFolders$Grid_ID == theNext,]
          theRange <- theFolder$Range
          
          #   ---- Pull Grid_IDs that are to buffer the new Grid_ID. 
          shpBuf <- makeBuffer(shpObj,cell=theNext,radius=2.5,cellDim.m=3218.69,inner=TRUE)
          shpBuf <- shpBuf[shpBuf@data$Grid_ID != theNext,]
          shpGID <- makeBuffer(shpObj,cell=theNext,radius=1.0,cellDim.m=3218.69,inner=TRUE)
          
          #   ---- Make sure we have no Out-for-Digitizing Grid_IDs in the selected
          #   ---- Grid_ID's buffer.  Note that buff indicator used above is the set 
          #   ---- of buffering cells UP to this possibly new cell.  Note that we go 
          #   ---- back to the assign, since master only has the valid -- missing the 
          #   ---- buffering cells by default. 
          checkBuf <- assign[assign$Grid_ID %in% shpBuf@data$Grid_ID,]
          
          #   ---- We now need to ensure all cells are available.
          Copen <- checkBuf$openStatus    # The cell still needs to be digitized.  
          Cdigi <- checkBuf$digiStatus    # The cell is locked for digitizing.
          Cbuff <- checkBuf$buffStatus    # The cell is locked for buffering.  
          Cdone <- checkBuf$doneStatus    # The cell has been digitized and reconciled, if necessary.
          
          #   ---- Check.  The cell cannot currently be out for digitizing or buffering.  
          Cvalid <-  !Cdigi & !Cbuff     # Want:     0  &  0  
          nCValid <- length(Cvalid[Cvalid == TRUE])  
      
          #   ---- Actually do the check.  
          if( nCValid == nrow(checkBuf) ){
            
            #   ---- We found a valid cell with a good buffer.
            cat("Found a candidate cell...fetching buffering towns and preparing for release.\n")
            
            #   ---- See if it's a double sample cell.  
            double <- master[master$Grid_ID == theNext,]$dblSamp
            
            #   ---- Update the records in data frame assign for the digitizing Grid_ID.
            assign[assign$Grid_ID == theNext,]$openStatus <- 0
            assign[assign$Grid_ID == theNext,]$digiStatus <- 1
            assign[assign$Grid_ID == theNext,]$digiUserID <- userID
            assign[assign$Grid_ID == theNext,]$digiSingle <- 1
            assign[assign$Grid_ID == theNext,]$digiDouble <- 0
            
            #   ---- Get a partner for reconciling.
            if(double == 1){
          
              #   ---- The function sample is weird when you only want one.  So go a different route.
              partnerValid <- tblNames[tblNames$doubleActive == 1 & tblNames$userID != userID,]
              # partnerValid <- tblNames[tblNames$userID %in% c(219),]    #   ---- This is here for testing.
              if(nrow(partnerValid) == 0){
                stop("Possibly only one digitizer is active for partnering.  Investigate.")
              }
              
              #   ---- The doublyMethod variable identifies how partners are to be found for doubly
              #   ---- digitized cells.  doublyMethod == 1 means that the digitizer pulling the 
              #   ---- the cell (so the userID) is always primary, and secondary is simply 
              #   ---- randmoly selected from the list of doubly digitizers available.  doublyMethod 
              #   ---- == 2 means that we call function balanceAssign.R, which tries to be smart
              #   ---- in how it maintains a balance between primary and secondary, along with 
              #   ---- the set of available digitizers.  
              
              #   ---- Set the method to use for assigning doubly partners.  
              doublyMethod <- 2
              
              if(doublyMethod == 1){

                
                #   ---- We need to be smart about assigning primary vs. secondary.  
                #   ---- Get the distribution so far in the project, and assign the 
                #   ---- roles based on who needs what, so as to force a balance.  
                #   ---- Note I don't restrict to done...could have half-done cells
                #   ---- that have been assigned, but not yet reconciled.  
                #   ---- Need to consider partnerValid list as well.  
                #   ---- HOLD UNTIL I HAVE SOME DATA TO TINKER WITH.  5/19/2016.
                #doubSoFar <- assign[assign$digiDouble == 1,]
                #table(done$digiUserID,done$digiPrimary)
              
                #   ---- Assign partner and primary randomly at same time, i.e., 2-way?
                partnerValid$randUni <- runif(nrow(partnerValid))
                partner <- partnerValid[1,]$userID 
              
                assign[assign$Grid_ID == theNext,]$digiPartner <- partner
                sFirstName <- as.character(droplevels(tblNames[tblNames$userID == partner,]$FirstName))
          
                #   ---- Method 1:  assign the person calling the function 
                #   ---- primary, and the partner brought in secondary.  
                assign[assign$Grid_ID == theNext,]$digiPrimary <- userID
                assign[assign$Grid_ID == theNext,]$digiSecondary <- partner
              
              } else if(doublyMethod == 2){
                
                #   ---- Note that we already check for at least two digitizers above.  
                assignInfo <- tryCatch(
                  {
                    balanceAssign(userID,assign=assign,tblNames=tblNames,partnerValid=partnerValid$userID)
                  },
                  error=function(condAI){
                    message("Something is wrong with the balanceAssign function.  Investigate.\n")
                    message("Here's the original error message:\n")
                    message(condAI)
                    # Choose a return value in case of error
                    return(NA)
                  }
                )
                
                #   ---- Method 2:  assign Primary and Secondary based on balancing.  
                if( assignInfo$userIDAssign == "Primary" ){
                  assign[assign$Grid_ID == theNext,]$digiPrimary <- assignInfo$userID
                  assign[assign$Grid_ID == theNext,]$digiSecondary <- assignInfo$thePartnerAssign
                  
                  #   ---- We may have swapped which names go with which role:  primary and 
                  #   ---- secondary.  Set this explicitly to be sure.
                  pFirstName <- as.character(droplevels(tblNames[tblNames$userID == assignInfo$userID,]$FirstName))
                  sFirstName <- as.character(droplevels(tblNames[tblNames$userID == assignInfo$thePartnerAssign,]$FirstName))
                } else {
                  assign[assign$Grid_ID == theNext,]$digiPrimary <- assignInfo$thePartnerAssign
                  assign[assign$Grid_ID == theNext,]$digiSecondary <- assignInfo$userID      
                  
                  #   ---- We may have swapped which names go with which role:  primary and 
                  #   ---- secondary.  Set this explicitly to be sure.  
                  pFirstName <- as.character(droplevels(tblNames[tblNames$userID == assignInfo$thePartnerAssign,]$FirstName))
                  sFirstName <- as.character(droplevels(tblNames[tblNames$userID == assignInfo$userID,]$FirstName))
                }
              }
              
              #   ---- Update the record set with the fact this is a doubly cell.  Do this after the
              #   ---- fact, since we use assign in doublyMethod == 2.
              assign[assign$Grid_ID == theNext,]$digiSingle <- 0
              assign[assign$Grid_ID == theNext,]$digiDouble <- 1
              
              theTwoDigis <- c(assignInfo$userID,assignInfo$thePartnerAssign)
              partner <- theTwoDigis[theTwoDigis != userID]
              assign[assign$Grid_ID == theNext,]$digiPartner <- partner
              

              
            } else {
              partner <- 998
              assign[assign$Grid_ID == theNext,]$digiPartner <- partner
            }
        
            #   ---- Update the time records.  At the least, we don't want endTimes that are before startTimes.
            assign[assign$Grid_ID == theNext,]$digiStartTime <- as.POSIXlt(Sys.time(),format="%m/%d/%Y %H:%M",tz="America/Denver")
            assign[assign$Grid_ID == theNext,]$digiEndTime <- as.POSIXlt(Sys.time(),format="%m/%d/%Y %H:%M",tz="America/Denver")

            #   ---- Need to see if we have shapefiles with no features. This happens often.  
            checkShp <- function(folder,shp){                                                                 
              if(is.null(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message)){
                shp2 <- tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)
              } else if(tryCatch(readOGR(folder,shp,verbose=FALSE), warning = function(w) w)$message == "no features found" ){
                shp2 <- 'no features found'                                         
              }  
            }
        
            #   ---- Update the data frame assign with the cells that are buffering, 
            #   ---- and ID the Grid_ID causing the lock.  
            townShps <- vector("list",nrow(shpBuf@data))
            for( j in 1:nrow(shpBuf@data) ){
              
              bufGrid_ID <- as.character(droplevels(shpBuf@data[j,]$Grid_ID))
              assign[assign$Grid_ID == bufGrid_ID,]$buffStatus <- 1
              assign[assign$Grid_ID == bufGrid_ID,]$buffLockGrid_ID <- theNext
              assign[assign$Grid_ID == bufGrid_ID,]$buffUserID <- userID
              assign[assign$Grid_ID == bufGrid_ID,]$buffPartner <- partner
              assign[assign$Grid_ID == bufGrid_ID,]$buffStartTime <- as.POSIXlt(Sys.time(),format="%m/%d/%Y %H:%M",tz="America/Denver")
              assign[assign$Grid_ID == bufGrid_ID,]$buffEndTime <- as.POSIXlt(Sys.time(),format="%m/%d/%Y %H:%M",tz="America/Denver")
              
              #   ---- Compile all town shapefiles from neighbors of the locking Grid_ID
              #   ---- and place in the folder so digitizer knows they are there.  
              bufFolders <- tblFolders[tblFolders$Grid_ID %in% shpBuf@data$Grid_ID,]
              bufRange <- bufFolders[bufFolders$Grid_ID == bufGrid_ID,]$Range
              bufFolder <- paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",bufRange,"/",bufGrid_ID,"/") 
              bufDone <- assign[assign$Grid_ID == bufGrid_ID,]$doneStatus
              
              #   ---- Find out if the neighboring buffering cell was singly or 
              #   ---- doubly digitized.  This tells us which shapefile actually
              #   ---- holds the towns we care about.  
              bufBASN <- ranks[ranks$Grid_ID == bufGrid_ID,]$sampleID
              bufDoub <- ranks[ranks$Grid_ID == bufGrid_ID,]$dblSamp
          
              #   ---- Get any valid town shapefiles and place in a list. 
              #   ---- Note we only get those with a higher BAS Number.  
              #   ---- This *could* create a conflict if a higher-BAS-
              #   ---- Number town has been drawn that overlaps with the
              #   ---- current town of interest.  But the current cell
              #   ---- gets first dibs.  
              
              #   ---- Note that this process happens for all cells, even if 
              #   ---- it's a double.  It's just easier to exclude the Local_Towns
              #   ---- shapefile construction at the point of function writeOGR
              #   ---- in the case of a doubly cell.  
              if(bufDone == 1){
                bufUserID <- assign[assign$Grid_ID == bufGrid_ID,]$digiUserID
                FirstName <- tblNames[tblNames$userID == bufUserID,]$FirstName
                if(bufDoub == 0){
                 if( file.exists(paste0(bufFolder,"p",FirstName,"_Towns_",bufGrid_ID,".shp")) & (bufBASN < theBASN) ){
                    townShps[[j]] <- checkShp(substr(bufFolder,1,nchar(bufFolder) - 1),paste0("p",FirstName,"_Towns_",bufGrid_ID))
                    #townShps[[j]] <- checkShp(readOGR(substr(bufFolder,1,nchar(bufFolder) - 1),paste0("p",FirstName,"_Towns_",bufGrid_ID),verbose=FALSE)
                  }
                } else {
                  if( file.exists(paste0(bufFolder,"reconciling_Towns_",bufGrid_ID,".shp")) & (bufBASN < theBASN) ){
                    townShps[[j]] <- checkShp(substr(bufFolder,1,nchar(bufFolder) - 1),paste0("reconciling_Towns_",bufGrid_ID))
                    townShps[[j]]@data <- data.frame(Town_ID=as.numeric(townShps[[j]]@data$Recon_T_ID))
                    #names(townShps[[j]]@data)[names(townShps[[j]]@data) == "Recon_T_ID"] <- "Town_ID"
                    
                    #townShps[[j]] <- readOGR(substr(bufFolder,1,nchar(bufFolder) - 1),paste0("reconciling_Towns_",bufGrid_ID),verbose=FALSE)
                  }
                }
              }
            }
        
            #   ---- Given the list of neighboring towns, paste together 
            #   ---- into one nice shapefile.  
            allShps <- NULL
            first <- 0
            triggered <- 0
            for(j in 1:length(townShps)){
              if( class(townShps[[j]]) == "SpatialPolygonsDataFrame" ){
                #print(j)
                nR <- length(slot(townShps[[j]],"polygons")) 
                if(triggered == 0){
                  first <- 1
                }
                if(first == 1){
                  uidR          <- 1
                  allShps       <- spChFIDs(townShps[[j]], as.character(uidR:(uidR + nR - 1)))
                  uidR          <- uidR + nR
                  triggered     <- 1
                  first         <- 0
                } else {
                  townShps[[j]] <- spChFIDs(townShps[[j]], as.character(uidR:(uidR + nR - 1)))
                  uidR          <- uidR + nR
                  allShps       <- spRbind(allShps,townShps[[j]])
                }   
                #cat(paste0("uidR: ",uidR,"\n"))
                #cat(paste0("nR: ",nR,"\n"))
              }
            }
        
            #   ---- Polygon allShps contains all the polygons of towns drawn
            #   ---- by neighboring cells in the buffer of the cell of interest,
            #   ---- excluding any towns with a higher BAS number.  Save it.
            otherTowns <- 0
            #otherTownsp <- 0
            if(!is.null(allShps) & double == 0){  
              writeOGR(allShps,paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theFolder$Range,"/",theNext),paste0("LocalTowns_",theNext),overwrite_layer=TRUE,driver="ESRI Shapefile")
              otherTowns <- 1
            }
            
            #   ---- Make a grid shapefile of the buffer region for placement
            #   ---- in the theNext folder.
            localGrid <- as(shpGID,"SpatialLinesDataFrame")
            localGrid <- spTransform(localGrid,CRS(projAEAc))
            writeOGR(localGrid,paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theFolder$Range,"/",theNext),paste0("LocalGrid_",theNext),overwrite_layer=TRUE,driver="ESRI Shapefile")
            
            #   ---- Determine length in meters of one cell; i.e., transect length. 
            cellUnit <- 3218.694
            
            #   ---- Make a mini-grid shapefile of the buffer region for placement
            #   ---- into the theNext folder.  Currently assumes a 3x3 big local grid.
            r <- raster(extent(gBuffer(shpGID,width=cellUnit)),nrow=15,ncol=15,crs=shpBuf@proj4string)
            #r <- raster(extent(shpBuf@bbox),nrow=15,ncol=15,crs=shpBuf@proj4string)            
            r[] <- 1:ncell(r)
            miniGrid <- as(r, "SpatialPolygonsDataFrame")
            miniGrid <- as(miniGrid,"SpatialLinesDataFrame")
            miniGrid <- spTransform(miniGrid,CRS(projAEAc))
            writeOGR(miniGrid,paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theFolder$Range,"/",theNext),paste0("LocalMiniGrid_",theNext),overwrite_layer=TRUE,driver="ESRI Shapefile")
            
            #   ---- Make the primary.  This is always made.
            fileList <- list.files("//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Digitizing_Template",full.names=TRUE)
            to <- paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theFolder$Range,"/",theFolder$Grid_ID)
            file.copy(fileList,to,overwrite=TRUE,recursive = FALSE,copy.mode = TRUE)
            ext <- c("cpg","dbf","prj","sbn","sbx","shp","shx")
            file.rename(paste0(to,"/BTPD_Digitizing_Template.",ext),paste0(to,"/p",pFirstName,"_Towns_",theNext,".",ext))
        
            #   ---- Make an empty .mxd (although it connects to the NAIP imagery).
            file.rename(paste0(to,"/BlankCO_NAIP.mxd"),paste0(to,"/p",pFirstName,"_",theNext,".mxd"))
          
            #   ---- Make the secondary.  This is made if the cell is a double-sample.
            if(double == 1){
              fileList <- list.files("//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Digitizing_Template",full.names=TRUE)
              to <- paste0("//lar-file-srv/Data/BTPD_2016/Digitizing/",theFolder$Range,"/",theFolder$Grid_ID)
              file.copy(fileList,to,overwrite=TRUE,recursive = FALSE,copy.mode = TRUE)
              ext <- c("cpg","dbf","prj","sbn","sbx","shp","shx")
              file.rename(paste0(to,"/BTPD_Digitizing_Template.",ext),paste0(to,"/s",sFirstName,"_Towns_",theNext,".",ext))
              
              #   ---- Make an empty .mxd (although it connects to the NAIP imagery).
              file.rename(paste0(to,"/BlankCO_NAIP.mxd"),paste0(to,"/s",sFirstName,"_",theNext,".mxd"))
            }
            
            #   ---- Plot so the user can see where to go geographically.
            cat("Preparation complete. ")
            cat(paste0("Your new cell to digitize is ",theNext,".\n"))
            if( otherTowns == 1){
              cat("--- *** ---> Previously digitized towns are in your buffer.  These areas are off-limits for digitizing. <--- *** ---\n")
            } else {
              cat(paste0("No towns found within the buffering radius of cell ",theNext,". All areas open for digitizing.\n"))
            }
            found <- TRUE
        
            #   ---- Make an easy map, so people have an idea of where they're going.
            getStatus("All",plotOnly=TRUE)
            
            #   ---- Add a red ring to easily pick out the new cell.  Accessed 5/24/2016.
            #   ---- http://stackoverflow.com/questions/29624895/how-to-add-a-hole-to-a-polygon-within-a-spatialpolygonsdataframe
            AddHoleToPolygon <-function(poly,hole){
              
              # poly <- outCircle
              # hole <- inCircle
              
              # invert the coordinates for Polygons to flag it as a hole
              coordsHole <-  hole@polygons[[1]]@Polygons[[1]]@coords
              newHole <- Polygon(coordsHole,hole=TRUE)
              
              # punch the hole in the main poly
              listPol <- poly@polygons[[1]]@Polygons
              listPol[[length(listPol)+1]] <- newHole
              punch <- Polygons(listPol,poly@polygons[[1]]@ID)
              
              # make the polygon a SpatialPolygonsDataFrame as the entry
              new <- SpatialPolygons(list(punch),proj4string=poly@proj4string)
              #new <- SpatialPolygonsDataFrame(new,data=as(poly,"data.frame"))
              
              return(new)
            }
        
            if(double == 0){
              plot(shpGID,add=TRUE,col="#d7191c",border="white")
            } else {
              
              #   ---- We have a double cell.  Color it for the userID calling the check-out.
              if( assignInfo$userIDAssign == "Secondary"){
                plot(shpGID,add=TRUE,col="#ffffbf",border="white")
              } else {
                plot(shpGID,add=TRUE,col="#fdae61",border="white")
              }

            }
            
            outCircle <- gBuffer(gCentroid(shpGID),byid=TRUE,width=12000)
            inCircle <- gBuffer(gCentroid(shpGID),byid=TRUE,width=9000)        
            
            ring <- AddHoleToPolygon(outCircle,inCircle)
            plot(ring,add=TRUE,col="red",border="red")
            
            if( is.null(allShps) ){ 
              
              #   ---- No towns in buffer.
              mtext(side=3,line=-0.75,"Your newly checked-out cell is circled in red.")
            } else {
              
              #   ---- Towns in buffer.
              mtext(side=3,line=-0.75,"Your newly checked-out cell is circled in red.")
              mtext(side=1,line=-1.00,"Be wary of possible towns in your new cell's buffer.",col="red")
            }
            
            #   ---- Now, plot the buffer of our new cell.  But NOT where we have already plotted
            #   ---- a closed cell.  We want to exclude these!
            dontPlot <- assign[assign$doneStatus == 1 & !is.na(assign$doneStatus),]$Grid_ID
            plot(shpBuf[!(shpBuf@data$Grid_ID %in% dontPlot),],add=TRUE,col="#a6d96a",border="white")
            

          
          } else {   
            #   ---- One of the 8 cells is locked.  
          }
      
          #   ---- Increment by one, so as to try the next available Grid_ID in master.
          i <- i + 1
      
        } #   ---- Close the repeat.
    
      }  #   ---- Close the if.
      
      
      #   ---- Put these date fields back to character, so Excel doesn't get confused. 
      assign$digiStartTime <- strftime(assign$digiStartTime,format="%m/%d/%Y %H:%M")
      assign$digiEndTime <- strftime(assign$digiEndTime,format="%m/%d/%Y %H:%M")
      assign$buffStartTime <- strftime(assign$buffStartTime,format="%m/%d/%Y %H:%M")
      assign$buffEndTime <- strftime(assign$buffEndTime,format="%m/%d/%Y %H:%M")
  
      #   ---- Organize stuff for next steps.  
      write.csv(assign,paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/",userID,"/tblCellStatusTMP.csv"),row.names=FALSE)
      theNextdf <- data.frame(theNext=theNext,stringsAsFactors=FALSE)
      theRangedf <- data.frame(theRange=theRange,stringsAsFactors=FALSE)
      thepFirstNamedf <- data.frame(thepFN=pFirstName,stringsAsFactors=FALSE)
      if(double == 1){
        thesFirstNamedf <- data.frame(thesFN=sFirstName,stringsAsFactors=FALSE)
      } else {
        thesFirstNamedf <- data.frame(thesFN="No",stringsAsFactors=FALSE)
      }
      out <- list(assign=assign,theNext=theNextdf,theRange=theRangedf,thepFirstName=thepFirstNamedf,thesFirstName=thesFirstNamedf)
      #return(out)
      
      #  ---- Update the assignment table so we know who has which cells.  
      updateAssign(userID,out)
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
    },
    warning=function(cond){
      message(cond)
      return(NULL)
    }
  )
}  