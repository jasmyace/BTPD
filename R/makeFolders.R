makeFolders <- function(dir,shp,var,tblDir){
  
  # dir <- paste0(btpdStem,digiStem)
  # shp <- allShps[[2]]
  # var <- "Grid_ID"
  # tblDir <- paste0(btpdStem,analStem,baseStem)
  
  #   ---- Get a vector of to-be folder names.  
  folderNames <- shp@data[,var][order(shp@data[,var])]
  
  #   ---- Make first levels of a factor, to break up to-be folder names into groups.
  nNames <- length(folderNames)
  quantCut <- quantile(seq(1,nNames,by=1),seq(0,1,by=0.10))
  L1 <- cut(as.numeric(factor(folderNames)),quantCut,labels=FALSE,include.lowest=TRUE)
  L2 <- NULL
  
  #   ---- Make second level of a factor, dependent on how the first falls out.
  for(i in 1:max(L1)){
    
    ind <- L1 == i
    
    nNames <- length(folderNames[ind])
    quantCut <- quantile(seq(1,nNames,by=1),seq(0,1,by=0.10))
    L2bit <-  cut(as.numeric(factor(folderNames[ind])),quantCut,labels=FALSE,include.lowest=TRUE)
    L2 <- c(L2,L2bit)
  }
  
  #   ---- Put together a listing of all Grid_IDs with their folder groupings.
  coordMat <- data.frame(cbind(as.character(droplevels(shp@data[,var])),L1,L2))
  names(coordMat) <- c("Grid_ID","L1","L2")
  
  #   ---- Get first and last folder grouping labels.  
  coordMatLabels <- ddply(coordMat, c("L1","L2"), function(x) x[c(1, nrow(x)), ])
  coordMatLabels$Spot <- c("First","Last")
  
  #   ---- Make left and right data frames in preparation for cbind "merge."
  coordMatLabelsFirst <- coordMatLabels[coordMatLabels$Spot == "First",]
  coordMatLabelsLast <- coordMatLabels[coordMatLabels$Spot == "Last",]  
  
  #   ---- Rename variables.  
  names(coordMatLabelsFirst) <- c("Grid_IDFirst","L1First","L2First","First")
  names(coordMatLabelsLast) <- c("Grid_IDLast","L1Last","L2Last","Last")
  
  #   ---- Combine left and right data frames.  Order by factor levels.  Make range variable.
  theLabels <- cbind(coordMatLabelsFirst,coordMatLabelsLast)
  theLabels <- theLabels[order(as.numeric(levels(theLabels$L1First))[theLabels$L1First],as.numeric(levels(theLabels$L2First))[theLabels$L2First]),]
  rownames(theLabels) <- NULL
  theLabels$Range <- paste0("Block ",rownames(theLabels)," ",theLabels$Grid_IDFirst," - ",theLabels$Grid_IDLast)
  
  #   ---- Reduce labels data frame to minimum.  
  theLabels <- theLabels[,c('L1First','L2First','Range')]
  names(theLabels)[names(theLabels) == 'L1First'] <- 'L1'
  names(theLabels)[names(theLabels) == 'L2First'] <- 'L2'  
  
  #   ---- Make a master data frame that maps folders to be made to their range. 
  coordMat <- merge(coordMat,theLabels,by=c('L1','L2'),all.x=TRUE)
  coordMat <- coordMat[order(as.numeric(levels(coordMat$L1))[coordMat$L1],as.numeric(levels(coordMat$L2))[coordMat$L2]),]
  
  # write.csv(coordMat,paste0(btpdStem,analStem,baseStem,"tblFolders.csv"))
  
  #   ---- Now actually make the folders.  
  # for(i in 1:nL1){                   # Dig into level L1.
  #   for(j in 1:nL2){                 # Dig into level L2.
  # 
  #     #   ---- Pick off this block.  
  #     ijCoordMat <- coordMat[coordMat$L1 == i & coordMat$L2 == j,]
  # 
  #     #   ---- Loop through folders to be made.  
  #     for(k in 1:nrow(ijCoordMat)){
  #       
  #       # #   ---- Make individual folders. 
  #       # theDir1 <- paste0(dir,ijCoordMat[k,]$Range,"/",ijCoordMat[k,]$Grid_ID,"/")
  #       # ifelse(!dir.exists(file.path(theDir1)), dir.create(file.path(theDir1), showWarnings=TRUE, recursive=TRUE), FALSE)
  #       # 
  #       # #   ---- Make individual folders. 
  #       # theDir2 <- paste0(dir,ijCoordMat[k,]$Range,"/",ijCoordMat[k,]$Grid_ID,"/Epsilons/")
  #       # ifelse(!dir.exists(file.path(theDir2)), dir.create(file.path(theDir2), showWarnings=TRUE, recursive=TRUE), FALSE)
  #       
  #       #   ---- Make individual folders. 
  #       theDir3 <- paste0(dir,ijCoordMat[k,]$Range,"/",ijCoordMat[k,]$Grid_ID,"/Helper/")
  #       ifelse(!dir.exists(file.path(theDir3)), dir.create(file.path(theDir3), showWarnings=TRUE, recursive=TRUE), FALSE)
  #       
  # 
  #     }
  #   }
  # }
  
  # #   ---- Make initial status file. 
  Grid_ID <- coordMat$Grid_ID
  openStatus <- 1
  digiStatus <- 0
  digiUserID <- NA
  digiPartner <- NA
  digiSingle <- NA
  digiDouble <- NA
  digiPrimary <- NA
  digiSecondary <- NA
  digiStartTime <- as.POSIXct(as.Date("1950-01-01"),tz="America/Denver") + 60*60*7
  digiEndTime <- as.POSIXct(as.Date("1950-01-01"),tz="America/Denver") + 60*60*7
  buffStatus <- 0
  buffLockGrid_ID <- NA
  buffUserID <- NA
  buffPartner <- NA
  buffStartTime <- as.POSIXct(as.Date("1950-01-01"),tz="America/Denver") + 60*60*7
  buffEndTime <- as.POSIXct(as.Date("1950-01-01"),tz="America/Denver") + 60*60*7
  doneStatus <- 0

  csvRecap <- data.frame(Grid_ID,openStatus,digiStatus,digiUserID,digiPartner,
                         digiSingle,digiDouble,digiPrimary,digiSecondary,digiStartTime,
                         digiEndTime,buffStatus,buffLockGrid_ID,buffUserID,
                         buffPartner,buffStartTime,buffEndTime,doneStatus)
  names(csvRecap) <- c("Grid_ID","openStatus","digiStatus","digiUserID","digiPartner",
                       "digiSingle","digiDouble","digiPrimary","digiSecondary","digiStartTime",
                       "digiEndTime","buffStatus","buffLockGrid_ID","buffUserID",
                       "buffPartner","buffStartTime","buffEndTime","doneStatus")
  write.csv(csvRecap,paste0(tblDir,"tblCellStatus.csv"),row.names=FALSE)
  write.csv(csvRecap,paste0(tblDir,"tblCellStatusOriginal.csv"),row.names=FALSE)
}