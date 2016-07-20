#' @export getStatus
#'   
#' @title Allow a digitizer to see his/her current cell status.


getStatus <- function(userID,plotOnly=FALSE){
  
  # userID <- 873
  # userID <- "All"
  # plotOnly <- FALSE
  
  #   ---- Get the master cell status and master grid.  
  assign <- getCellStatus()
  grid <- readOGR("//lar-file-srv/Data/BTPD_2016/Analysis/data/Shapefiles/BTPD_Grid_CO_Ranked","BTPD_Grid_CO_Ranked",verbose=FALSE)
  names <- read.csv("//lar-file-srv/Data/BTPD_2016/Analysis/Database/tblNames.csv",stringsAsFactors=FALSE)
  
  #   ---- Get the first name associated with the userID.
  if(userID == 'All'){
    userID <- names$userID
    firstName <- "Everybody"
  } else {
    checkUser(userID)
    firstName <- names[names$userID == userID,]$FirstName
  }
  
  #   ---- Get the different types of grid cells, based on this userID.
  singly <- assign[assign$digiUserID %in% userID & !is.na(assign$digiUserID) & assign$openStatus == 0 & assign$digiSingle == 1 & !is.na(assign$digiSingle),]
  doubly <- assign[( ( assign$digiUserID %in% userID  & !is.na(assign$digiUserID ) ) |
                     ( assign$digiPartner %in% userID & !is.na(assign$digiPartner) ) ) & assign$openStatus == 0 & assign$digiDouble == 1 & !is.na(assign$digiDouble),]
  buffer <- assign[assign$buffUserID %in% userID & !is.na(assign$buffUserID),]
  closed <- assign[assign$digiUserID %in% userID & !is.na(assign$digiUserID) & assign$doneStatus == 1 & !is.na(assign$doneStatus),]
  
  #   ---- Manipulate the results of querying into a useful form.
  singly <- data.frame(Grid_ID=singly[,c('Grid_ID')])
  if(nrow(singly) > 0){singly$singly <- 1}
  
  doubly <- data.frame(Grid_ID=doubly[,c('Grid_ID')],
                       digiUserID=doubly[,c('digiUserID')],
                       digiPartner=doubly[,c('digiPartner')],
                       digiPrimary=doubly[,c('digiPrimary')],
                       digiSecondary=doubly[,c('digiSecondary')] )
  if(nrow(doubly) > 0){
    
    #   ---- Identify if a user is primary or secondary for doubly-sampled cells. 
    #doubly <- data.frame(Grid_ID=c("CO159922","CO114951","CO124423","CO142881","CO134755"),digiUserID=c(100,555,555,100,666),digiPartner=c(345,776,100,345,100))
    doubly$doubly <- rep(0,nrow(doubly))
    if(firstName == "Everybody"){
      
      #   ---- When looking at everybody combined, doublys are both primary
      #   ---- and secondary;  so the distinction is unimportant.
#     doubly$doubly <- 1
    } else {
#       if(nrow(doubly[doubly$digiUserID == userID,]) > 0){
#         doubly[doubly$digiUserID == userID,]$doubly <- 1
#       }
#       if(nrow(doubly[doubly$digiPartner == userID,]) > 0){
#         doubly[doubly$digiPartner == userID,]$doubly <- 2
#       }
    }
  }
  
  buffer <- data.frame(Grid_ID=buffer[,c('Grid_ID')])
  if(nrow(buffer) > 0){buffer$buffer <- 1}
  
  closed <- data.frame(Grid_ID=closed[,c('Grid_ID')])
  if(nrow(closed) > 0){closed$closed <- 1}
  
  #   ---- Add in Grid_IDs for which we have information, based on this userID.
  #   ---- Also, report cells of interest to the user. 

    
  if(nrow(singly) > 0){
    grid@data <- merge(grid@data,singly,by=c('Grid_ID'),all.x=TRUE)
    if( !(plotOnly == TRUE) ){
      if(nrow(singly) == 1){
        cat(paste0(firstName,"'s list of singly-digitized cells includes Grid_ID ",singly$Grid_ID[1],".\n"))
      } else {
        cat(paste0(firstName,"'s list of singly-digitized cells includes Grid_IDs ",paste0(singly$Grid_ID,collapse=", "),".\n"))
      }
    }
  }
    
  cat("\n")
  if(nrow(doubly) > 0){
    grid@data <- merge(grid@data,doubly[,c('Grid_ID','digiPrimary','digiSecondary')],by=c('Grid_ID'),all.x=TRUE)
    if( !(plotOnly == TRUE) ){
      if(nrow(doubly[doubly$digiPrimary %in% userID,]) == 1){
        cat(paste0(firstName,"'s list of doubly-digitized cells as the primary includes Grid_ID ",doubly[doubly$digiPrimary %in% userID,]$Grid_ID[1],".\n"))
      } else if(nrow(doubly[doubly$digiPrimary %in% userID,]) > 1) {
        cat(paste0(firstName,"'s list of doubly-digitized cells as the primary includes Grid_IDs ",paste0(doubly[doubly$digiPrimary %in% userID,]$Grid_ID,collapse=", "),".\n"))
      } 
      cat("\n")
      if(nrow(doubly[doubly$digiSecondary %in% userID,]) == 1){
        cat(paste0(firstName,"'s list of doubly-digitized cells as the secondary includes Grid_ID ",doubly[doubly$digiSecondary %in% userID,]$Grid_ID[1],".\n"))
      } else if(nrow(doubly[doubly$digiSecondary %in% userID,]) > 1) {
        cat(paste0(firstName,"'s list of doubly-digitized cells as the secondary includes Grid_IDs ",paste0(doubly[doubly$digiSecondary %in% userID,]$Grid_ID,collapse=", "),".\n"))
      } 
    }
  }
  
  
  
  #   ---- Summarize the doubly digitizing for this person.  
  if( length(userID) == 1 & nrow(doubly) >= 1 ){
    namesReport <- names
    namesReport$digiUserID <- namesReport$userID
  
    doublyReport <- doubly
    doublyReport <- doublyReport[order(doublyReport$digiUserID,doublyReport$digiPartner),c('Grid_ID','digiUserID','digiPartner')]
    doublyReport <- merge(doublyReport,namesReport[,c('digiUserID','FirstName')],by=c('digiUserID'),all.x=TRUE)
    
    doublyReport2 <- merge(doublyReport,assign[,c('Grid_ID','digiStartTime','jErrStatus')],by=c("Grid_ID"),all.x=TRUE)
    doublyReport2$DaysOpen <- as.integer(round(as.Date(Sys.Date(),format="%m/%d/%Y") - as.Date(as.POSIXlt(doublyReport2$digiStartTime,format="%m/%d/%Y")),0))
    
    doublyReport <- doublyReport2
    
    names(doublyReport)[names(doublyReport) == "FirstName"] <- "Checking-Out Digitizer"
    
    namesReport$digiPartner <- namesReport$userID
    doublyReport <- merge(doublyReport,namesReport[,c('digiPartner','FirstName')],by=c('digiPartner'),all.x=TRUE)
    names(doublyReport)[names(doublyReport) == "FirstName"] <- "Partner"
    
    doublyReport$digiPartner <- doublyReport$digiUserID <- doublyReport$jErrStatus <- doublyReport$digiStartTime <- NULL
    doublyReport <- doublyReport[,c('Grid_ID','Checking-Out Digitizer','Partner','DaysOpen')]
    
    cat("\n")
    cat(paste0("A summary of ",firstName,"'s doubly pairings follows.\n\n"))
    print(doublyReport)
    #cat("\n")
  }
  
  
  
  cat("\n")
  if(nrow(buffer) > 0){
    grid@data <- merge(grid@data,buffer,by=c('Grid_ID'),all.x=TRUE)
    if( !(plotOnly == TRUE) ){
      if(nrow(buffer) == 1){
        cat(paste0(firstName,"'s list of buffer cells includes Grid_ID ",buffer$Grid_ID[1],".\n"))
      } else {
        cat(paste0(firstName,"'s list of buffer cells includes Grid_IDs ",paste0(buffer$Grid_ID,collapse=", "),".\n"))
      }
    }  
  }
    
  if(nrow(closed) > 0){
    cat("\n")
    grid@data <- merge(grid@data,closed,by=c('Grid_ID'),all.x=TRUE)
    if( !(plotOnly == TRUE) ){
      if(nrow(closed) == 1){
        cat(paste0(firstName,"'s list of closed cells includes Grid_ID ",closed$Grid_ID[1],".\n"))
      } else {
        #cat(paste0(firstName,"'s list of closed cells includes Grid_IDs ",paste0(closed$Grid_ID,collapse=", "),".\n"))
        cat(paste0(firstName,"'s list of closed cells includes ",length(closed$Grid_ID)," Grid_IDs.\n"))
      }
    }
  }
  
  
    
  if(nrow(closed) == 0 & nrow(buffer) == 0 & nrow(doubly) == 0 & nrow(singly) == 0){
    cat(paste0(firstName," has nothing to report.  The Cells map will be blank."))
  }

  grid@data[is.na(grid@data)] <- 0

  #   ---- Make a simple plot. 
  par(mar=c(0.5,0.5,2.0,0.5))
  plot(grid,main=paste0(firstName,"'s Cells"),col="gray90",border="white")
  plot(grid[grid@data$buffer == 1,],col="#a6d96a",border="white",add=TRUE)   # blue
  plot(grid[grid@data$singly == 1,],col="#d7191c",border="white",add=TRUE)   # yellow
  plot(grid[grid@data$digiPrimary %in% userID,],col="#fdae61",border="white",add=TRUE)   
  if( length(userID) == 1 ){
    plot(grid[grid@data$digiSecondary %in% userID,],col="#ffffbf",border="white",add=TRUE)   
  }
  plot(grid[grid@data$closed == 1,],col="#1a9641",border="white",add=TRUE)   # green3
  legend("bottomleft",c('Open','Singly','Doubly -- Primary','Doubly -- Secondary','Buffer','Closed'),border=rep("white",4),fill=c("gray90","#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641"),cex=0.6)
  
}