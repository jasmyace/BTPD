#' @export userSummary
#'   
#' @title Check out a cell.

userSummary <- function(summary="Single"){ 
  
  # summary <- "Single"

  #   ---- Get the current list of who has what.  
  assign <- getCellStatus()

  #   ---- Get users. 
  tblNames <- checkUser(userID=998)
  tblNames <- tblNames[!(tblNames$userID %in% c(437,798,998,999)),]
  names(tblNames)[names(tblNames) == 'userID'] <- 'digiUserID'
  
  assign <- merge(assign,tblNames[,c('digiUserID','FirstName')],by=c('digiUserID'),all.x=TRUE)
  assign <- assign[order(assign$Grid_ID),]

  assign$digiStartTime <- as.POSIXlt(strptime(assign$digiStartTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")
  assign$digiEndTime <- as.POSIXlt(strptime(assign$digiEndTime,format="%m/%d/%Y %H:%M"),tz="America/Denver")  

  times <- assign[assign$doneStatus == 1,]
  times$diffTimeHrs <- as.numeric(difftime(times$digiEndTime,times$digiStartTime) / 60 / 60)
  
  #   ---- DEVELOP A SIMPLE REPORT FOR SINGLES. 
  

  times2 <- times
  
  if(summary == "Single"){
    
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
  }
  
  #   ---- DEVELOP A SIMPLE REPORT FOR DOUBLES. 
  if(summary == "Double"){

    #   ---- Keep only doubles.
    times2 <- times2[times2$digiDouble == 1,]  
    
    #   ---- Keep only non-errors.
    times2 <- times2[times2$jErrStatus == 0,]
    
    #   ---- Get names for the partners.
    names(tblNames)[names(tblNames) == "digiUserID"] <- "digiPartner"
    names(times2)[names(times2) == "FirstName"] <- "digiFirst"
    times2 <- merge(times2,tblNames[,c('digiPartner','FirstName')],by=c("digiPartner"),all.x=TRUE)
    names(times2)[names(times2) == "FirstName"] <- "PartnerFirst"
    times2 <- times2[order(times2$Grid_ID),]
    
    times2$digiEndTimeAsDate <- as.Date(times2$digiEndTime)
    times2$digiEndTimeAsDate2 <- paste0(times2$digiEndTimeAsDate,"-",weekdays(as.Date(times2$digiEndTime,format="%Y/%m/%d")))
    times2$digiFirst <- as.character(droplevels(times2$digiFirst))
    times2$PartnerFirst <- as.character(droplevels(times2$PartnerFirst))
    
    timesMatA <- as.data.frame.matrix(table(times2$digiEndTimeAsDate2,times2$digiFirst))
    timesMatA2 <- cbind(timesMatA,rowSums(timesMatA))
    timesMatA3 <- rbind(timesMatA2,colSums(timesMatA2))
    rownames(timesMatA3)[dim(timesMatA3)[1]] <- ""
    colnames(timesMatA3)[dim(timesMatA3)[2]] <- "Total"
    
    timesMatB <- as.data.frame.matrix(table(times2$digiEndTimeAsDate2,times2$PartnerFirst))
    timesMatB2 <- cbind(timesMatB,rowSums(timesMatB))
    timesMatB3 <- rbind(timesMatB2,colSums(timesMatB2))
    rownames(timesMatB3)[dim(timesMatB3)[1]] <- ""
    colnames(timesMatB3)[dim(timesMatB3)[2]] <- "Total"
  
    timesMatA3
    timesMatB3
    
    #   ---- Dimensionality may vary between A3 and B3.  Check for this, and correct
    #   ---- if necessary.  
    allPeeps <- unique(sort(c(colnames(timesMatA3),colnames(timesMatB3))))
    allPeeps <- allPeeps[allPeeps != "Total"]
    
    for(i in 1:length(allPeeps) ){
      if( !(allPeeps[i] %in% colnames(timesMatA3)) ){
        timesMatA3$thing <- rep(0,nrow(timesMatA3))
        names(timesMatA3)[names(timesMatA3) == "thing"] <- allPeeps[i]
      }
    }
    
    timesMatA3 <- timesMatA3[,c(allPeeps,"Total")]
    
    for(i in 1:length(allPeeps) ){
      if( !(allPeeps[i] %in% colnames(timesMatB3)) ){
        timesMatB3$thing <- rep(0,nrow(timesMatB3))
        names(timesMatB3)[names(timesMatB3) == "thing"] <- allPeeps[i]
      }
    }
    
    timesMatB3 <- timesMatB3[,c(allPeeps,"Total")]
    
    timesMatA3 + timesMatB3
  }
  
  if(summary == "Single"){
    cat(paste0("Counts reflect raw totals of singly digitized cells only, and are not standardized for actual time worked.\n\n"))
    return(timesMat3)
  } else {
    cat(paste0("Counts reflect raw totals of doubly digitized cells only, and are not standardized for actual time worked.\n
The first matrix displays cells completed by the original digitizer checking out the cell, the second 
matrix displays the same for partners, while the third is a sum of the two.  Thus, the bottom-right 
total number reported in the third matrix is always even, and twice the number of completed doubly cells.\n\n"))
    #print(timesMatA3)
    #print(timesMatB3)
    #print(timesMatA3 + timesMatB3)
    return(list("Checking-Out Digitizer"=timesMatA3,"Partner"=timesMatB3,"The Two Summed"=timesMatA3 + timesMatB3))
  }
  
  
}