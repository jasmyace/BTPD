putDownLock <- function(userID,timeLimit=240){
  
  #   ---- Check for a lock on table tblCellStatus.csv
  beg <- Sys.time()
  
  #   ---- Get listing of active digitizers.  
  tblNames <- checkUser(userID)
  tblNames <- tblNames[tblNames$singleActive == 1 | tblNames$doubleActive == 1,c('userID','lockNum')]
  
  #   ---- For each number between 0 and 59, assign an active digitizer. 
  matcher <- cut(seq(0,59),breaks=seq(0,60,length.out=nrow(tblNames) + 1),right=FALSE,include.lowest=TRUE,labels=tblNames$userID)
  
  #   ---- For each number between 0 and 59, assign an active digitizer.  
  #   ---- Here, divvy up 30 seconds.  A few seconds should be a big enough window
  #   ---- to isolate a lock...right?
  matcher <- rep(tblNames$userID,60)[1:60]
  
  #   ---- Now, repeat over the next minute.  You can only exit if we hit the userIDs 
  #   ---- allocated set of seconds, or if we've waited for more than 3 minutes.  
  

  repeat{  
    
    Sys.sleep(1.0)
    
    #   ---- Check if a lock currently exists.  
    lock <- file.exists("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt")
    
    #   ---- See if the clock is in this userID's time period.  
    if( matcher[floor(as.POSIXlt(Sys.time())$s) + 1] == userID ){
      

      
      if(lock == FALSE){
        
        #   ---- Lock the table tblCellStatus so that two users cannot update
        #   ---- it at the same time. 
        cat("Putting down lock.\n")
        lockdf <- data.frame(userID=userID)
        write.table(lockdf,"//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Database/tblCellStatusLOCK.txt",row.names=FALSE)
        break
      } else if(lock == TRUE & difftime(Sys.time(),beg,units="secs") <= timeLimit){
        cat("Trying for a lock...\n")
      } else if( lock == TRUE & difftime(Sys.time(),beg,units="secs") > timeLimit ){
        
        #   ---- Note that "three" is hard-coded here. 
        #   ---- Move this statement to the actual function call (check-out or check-in).
        #cat("Function call locked out after three minutes.  There may be a problem.")
        break
      }
      
    } else {
      #print <- 1
    }
  }  
}