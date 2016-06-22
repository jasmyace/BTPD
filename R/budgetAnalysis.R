

getPhaseData <- function(phase,labrDate="061616",labrStem="//LAR-FILE-SRV/Data/BTPD_2016/Project Management/export"){
  
  labrStem <- paste0(labrStem,labrDate)
  
  p <- paste0(labrStem,"/Labor Detail phase ",phase,".txt")
  
  df <- read.table(p,fill=TRUE,skip=3,sep=",",header=TRUE,stringsAsFactors=FALSE)
  project <- df[1,]$groupHeader1_GroupColumn
  phaseDes <- df[1,]$groupHeader2_GroupColumn
  
  names(df)[names(df) == "detail_Employee"]  <- "empID"
  names(df)[names(df) == "detail_Name"]      <- "empName"
  names(df)[names(df) == "detail_transDate"] <- "date"
  names(df)[names(df) == "detail_regHrs"]    <- "regHours"
  names(df)[names(df) == "detail_ovtHrs"]    <- "otHours"
  names(df)[names(df) == "detail_totHrs"]    <- "totHours"
  names(df)[names(df) == "detail_totAmt"]    <- "totAmt"
  names(df)[names(df) == "Comment"]    <- "comment"
  
  df <- df[,c("empID","empName","date","regHours","otHours","totHours","totAmt","comment")]
  df <- df[-1,]
  df$phase <- cbind(df,phase=rep(phase,nrow(df)))
  
}

p1df <- getPhaseData(1)
p2df <- getPhaseData(2)

df <- rbind(p1df,p2df[p2df$empID != "JMITCHELL",])
df[is.na(df)] <- 0

summMetric <- function(df,var){
  
  summ <- data.frame(tapply(df[,c(var)],list(df$date,df$phase),sum))
  summ$date <- rownames(summ)
  rownames(summ) <- NULL
  
  phaseN <- length(unique(df$phase))
  
  #   ---- Assumes all phases inclusive are present.
  colnames(summ)[1:phaseN] <- paste0('phase',seq(1,phaseN,1))
  
  summ[is.na(summ)] <- 0
  summ$date <- as.POSIXlt(summ$date,format="%m/%d/%Y")
  summ <- summ[order(summ$date),]

}

summTotAmt <- summMetric(df,"totAmt")
summTotHours <- summMetric(df,"totHours")