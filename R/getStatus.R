#' @export getStatus
#'   
#' @title Allow a digitizer to see his/her current cell status.


getStatus <- function(userID){
  
  # userID <- 100
  # userID <- "All"
  
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
  closed <- assign[assign$digiUserID %in% userID & !is.na(assign$digiUserID) & assign$doneStatus == 1,]
  
  #   ---- Manipulate the results of querying into a useful form.
  singly <- data.frame(Grid_ID=singly[,c('Grid_ID')])
  if(nrow(singly) > 0){singly$singly <- 1}
  
  doubly <- data.frame(Grid_ID=doubly[,c('Grid_ID')],digiUserID=doubly[,c('digiUserID')],digiPartner=doubly[,c('digiPartner')])
  if(nrow(doubly) > 0){
    
    #   ---- Identify if a user is primary or secondary for doubly-sampled cells. 
    #doubly <- data.frame(Grid_ID=c("CO159922","CO114951","CO124423","CO142881","CO134755"),digiUserID=c(100,555,555,100,666),digiPartner=c(345,776,100,345,100))
    doubly$doubly <- rep(0,nrow(doubly))
    if(firstName == "Everybody"){
      
      #   ---- When looking at everybody combined, doublys are both primary
      #   ---- and secondary;  so the distinction is unimportant.
      doubly$doubly <- 1
    } else {
      if(nrow(doubly[doubly$digiUserID == userID,]) > 0){
        doubly[doubly$digiUserID == userID,]$doubly <- 1
      }
      if(nrow(doubly[doubly$digiPartner == userID,]) > 0){
        doubly[doubly$digiPartner == userID,]$doubly <- 2
      }
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
    if(nrow(singly) == 1){
      cat(paste0(firstName,"'s list of singly-digitized cells includes Grid_ID ",singly$Grid_ID[1],".\n"))
    } else {
      cat(paste0(firstName,"'s list of singly-digitized cells includes Grid_IDs ",paste0(singly$Grid_ID,collapse=", "),".\n"))
    }
  }
  
  if(nrow(doubly) > 0){
    grid@data <- merge(grid@data,doubly[,c('Grid_ID','doubly')],by=c('Grid_ID'),all.x=TRUE)
    if(nrow(doubly[doubly$doubly == 1,]) == 1){
      cat(paste0(firstName,"'s list of doubly-digitized cells as the primary includes Grid_ID ",doubly[doubly$doubly == 1,]$Grid_ID[1],".\n"))
    } else if(nrow(doubly[doubly$doubly == 1,]) > 1) {
      cat(paste0(firstName,"'s list of doubly-digitized cells as the primary includes Grid_IDs ",paste0(doubly[doubly$doubly == 1,]$Grid_ID,collapse=", "),".\n"))
    } 
    if(nrow(doubly[doubly$doubly == 2,]) == 1){
      cat(paste0(firstName,"'s list of doubly-digitized cells as the secondary includes Grid_ID ",doubly[doubly$doubly == 2,]$Grid_ID[1],".\n"))
    } else if(nrow(doubly[doubly$doubly == 2,]) > 1) {
      cat(paste0(firstName,"'s list of doubly-digitized cells as the secondary includes Grid_IDs ",paste0(doubly[doubly$doubly == 2,]$Grid_ID,collapse=", "),".\n"))
    } 
  }  
  
  if(nrow(buffer) > 0){
    grid@data <- merge(grid@data,buffer,by=c('Grid_ID'),all.x=TRUE)
    if(nrow(buffer) == 1){
      cat(paste0(firstName,"'s list of buffer cells includes Grid_ID ",buffer$Grid_ID[1],".\n"))
    } else {
      cat(paste0(firstName,"'s list of buffer cells includes Grid_IDs ",paste0(buffer$Grid_ID,collapse=", "),".\n"))
    }
  }  
  
  if(nrow(closed) > 0){
    grid@data <- merge(grid@data,closed,by=c('Grid_ID'),all.x=TRUE)
    if(nrow(closed) == 1){
      cat(paste0(firstName,"'s list of closed cells includes Grid_ID ",closed$Grid_ID[1],".\n"))
    } else {
      cat(paste0(firstName,"'s list of closed cells includes Grid_IDs ",paste0(closed$Grid_ID,collapse=", "),".\n"))
    }
  }
  
  if(nrow(closed) == 0 & nrow(buffer) == 0 & nrow(doubly) == 0 & nrow(singly) == 0){
    cat(paste0(firstName," has nothing to report.  The Cells map will be blank."))
  }
  
  grid@data[is.na(grid@data)] <- 0
  
  #   ---- Make a simple plot. 
  par(mar=c(0.5,0.5,2.0,0.5))
  plot(grid,main=paste0(firstName,"'s Cells"))
  plot(grid[grid@data$singly == 1,],col="yellow",add=TRUE)
  plot(grid[grid@data$doubly == 1,],col="red",add=TRUE)
  plot(grid[grid@data$doubly == 2,],col="orange",add=TRUE)  
  plot(grid[grid@data$buffer == 1,],col="blue",add=TRUE)
  plot(grid[grid@data$closed == 1,],col="green",add=TRUE)
  legend("bottomleft",c('Singly','Doubly -- Primary','Doubly -- Secondary','Buffer','Closed'),border=rep("black",4),fill=c("yellow","red","orange","blue","green"),cex=0.6)
  
}