#' @export userSummary
#'   
#' @title Check out a cell.

userSummary <- function(){ 

  #   ---- Get the current list of who has what.  
  assign <- getCellStatus()

  #   ---- Get users. 
  tblNames <- checkUser(userID=100)
  tblNames <- tblNames[!(tblNames$userID %in% c(437,798,982,998,999)),]
  names(tblNames)[names(tblNames) == 'userID'] <- 'digiUserID'
  
  assign <- merge(assign,tblNames[,c('digiUserID','FirstName')],by=c('digiUserID'),all.x=TRUE)
  assign <- assign[order(assign$Grid_ID),]

  assign$digiStartTime <- as.POSIXlt(strptime(assign$digiStartTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
  assign$digiEndTime <- as.POSIXlt(strptime(assign$digiEndTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")  

  times <- assign[assign$doneStatus == 1,]
  times$diffTimeHrs <- as.numeric(difftime(times$digiEndTime,times$digiStartTime) / 60 / 60)
  
  #   ---- Keep only singles.
  times <- times[times$digiSingle == 1,]  
  
  #   ---- Keep only non-errors.
  times <- times[times$jErrStatus == 0,]
  
  #   ---- Keep only those that start and finish on the same day.  
  times$digiStartTimeMDay <- times$digiStartTime$mday
  times$digiEndTimeMDay <- times$digiEndTime$mday
  times <- times[!(times$digiEndTimeMDay != times$digiStartTimeMDay),]
  
  times$digiEndTimeAsDate <- as.Date(times$digiEndTime)
  times$digiEndTimeAsDate2 <- paste0(times$digiEndTimeAsDate,"-",weekdays(as.Date(times$digiEndTime,format="%Y/%m/%d")))
  times$FirstName <- as.character(droplevels(times$FirstName))

  timesMat <- as.data.frame.matrix(table(times$digiEndTimeAsDate2,times$FirstName))
  timesMat2 <- cbind(timesMat,rowSums(timesMat))
  timesMat3 <- rbind(timesMat2,colSums(timesMat2))
  rownames(timesMat3)[dim(timesMat3)[1]] <- ""
  colnames(timesMat3)[dim(timesMat3)[2]] <- "Total"
  
  timesMat3
  
  cat(paste0("Counts reflect raw totals of singly digitized cells only, and are not standardized for actual time worked.\n\n"))
  return(timesMat3)


  #   ---- Maybe develop this later. 
#   times2 <- as.data.frame(table(times$digiEndTimeAsDate,times$FirstName))
#   times2$Var1 <- as.Date(times2$Var1)
# 
#   digis <- unique(times2$Var2)
#   xmin <- min(times2$Var1)
#   xmax <- max(times2$Var1)
#   ymin <- min(times2$Freq)
#   ymax <- max(times2$Freq)
#   for( i in 1:(length(digis)) ){
#     theDigi <- digis[i]
#     if(i == 1){
#       plot(times2[times2$Var2 == theDigi,]$Var1,times2[times2$Var2 == theDigi,]$Freq,xlim=c(xmin,xmax),ylim=c(ymin,ymax),type="l",xlab="Date",ylab="N Cells Digitized")
#       par(new=TRUE)  
#     } else {
#       plot(times2[times2$Var2 == theDigi,]$Var1,times2[times2$Var2 == theDigi,]$Freq,xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),type="l",xlab="",ylab="",ann=FALSE)
#       par(new=TRUE)  
#     }
#   }
  
}