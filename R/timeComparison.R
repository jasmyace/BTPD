summStats <- function(labrDate){
  
  # labrDate <- "062016"
  
  #   ---- Get the current list of who has what.  
  assign <- getCellStatus()
  
  #   ---- Get users and IDs.  
  tblNames <- checkUser(100)
  names(tblNames)[names(tblNames) == 'userID'] <- 'digiUserID'
  
  assign <- merge(assign,tblNames[,c('digiUserID','FirstName')],by=c('digiUserID'),all.x=TRUE)
  assign <- assign[order(assign$Grid_ID),]
  
  
  #   ---- Get database time worked.  
  assign$digiStartTime <- as.POSIXlt(strptime(assign$digiStartTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
  assign$digiEndTime <- as.POSIXlt(strptime(assign$digiEndTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")  
  assign$buffStartTime <- as.POSIXlt(strptime(assign$buffStartTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
  assign$buffEndTime <- as.POSIXlt(strptime(assign$buffEndTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
  
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
  
  hoursDB <- data.frame(DBHours=tapply(times$diffTimeHrs,times$FirstName,sum))
  hoursDB$FirstName <- rownames(hoursDB)
  rownames(hoursDB) <- NULL
  hoursDB <- hoursDB[!is.na(hoursDB$DBHours),]
  
  DBFreq <- as.data.frame(table(times$FirstName))
  names(DBFreq)[names(DBFreq) == "Var1"] <- "FirstName"
  names(DBFreq)[names(DBFreq) == "Freq"] <- "DBFreq"
  
  hoursDB <- merge(hoursDB,DBFreq,by=c('FirstName'),all.x=TRUE)
  
  #   ---- Get vision time worked.  
  source("//lar-file-srv/Data/BTPD_2016/Analysis/Overnights/Programs/getPhaseData.R")
  p2 <- getPhaseData(phase=2,labrDate,useJ=TRUE)
  p2[is.na(p2)] <- 0
  
  hours <- data.frame(VisionHours=tapply(p2$regHours,p2$empID,sum))
  hours$visionID <- rownames(hours)
  hours <- merge(hours,tblNames[,c('FirstName','visionID')],by=c('visionID'),all.x=TRUE)
  
  
  
  
  final <- merge(hours,hoursDB,by=c('FirstName'),all.x=TRUE)
  
  #   ---- Run some numbers. 
  doubleBreakOut <- as.data.frame(table(assign$doneStatus,assign$digiDouble))
  doubleBreakOut <- doubleBreakOut[doubleBreakOut$Var1 == 1,]
  doubleBreakOut$Var1 <- NULL
  names(doubleBreakOut)[names(doubleBreakOut) == "Var2"] <- "DoubleCell"
  
  
  userBreakOut <- as.data.frame(table(assign$doneStatus,assign$FirstName))
  userBreakOut <- userBreakOut[userBreakOut$Var1 == 1 & userBreakOut$Freq > 0,]
  userBreakOut$Var1 <- NULL
  names(userBreakOut)[names(userBreakOut) == "Var2"] <- "FirstName"
  names(userBreakOut)[names(userBreakOut) == "Freq"] <- "VisionFreq"
  userBreakOut <- merge(userBreakOut,final,by=c('FirstName'),all.x=TRUE)
  
  userBreakOut$visionID <- NULL

  
  userBreakOut$VisionCellsPerHour <- userBreakOut$VisionFreq / userBreakOut$VisionHours
  userBreakOut$DBCellsPerHour <- userBreakOut$DBFreq / userBreakOut$DBHours
  
  userBreakOut <- userBreakOut[,c("FirstName","VisionFreq","VisionHours","VisionCellsPerHour","DBFreq","DBHours","DBCellsPerHour")]
  userBreakOut[is.na(userBreakOut)] <- 0
  
  
  
  #   ---- Report results.
  doubleBreakOut
  userBreakOut
  
  x <- seq(0,10,1)
  y <- x
  
  x1 <- userBreakOut[userBreakOut$VisionFreq >= 100,]$VisionCellsPerHour
  y1 <- userBreakOut[userBreakOut$VisionFreq >= 100,]$DBCellsPerHour
  
  plot(x1,y1,xlim=c(3,7),ylim=c(3,7),xlab=NA,ylab=NA)
  par(new=TRUE)
  plot(x,y,type="l",xlim=c(3,7),ylim=c(3,7),xlab="Vision Cells per Hour",ylab="Database Cells per Hour")
  text(x1,y1,userBreakOut[userBreakOut$VisionFreq >= 100,]$FirstName,pos=1)
}