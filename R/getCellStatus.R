getCellStatus <- function(tblDir="//lar-file-srv/Data/BTPD_2016/Analysis/Database/"){
  
  # tblDir <- "//lar-file-srv/Data/BTPD_2016/Analysis/Database/"
  
  assign <- read.table(paste0(tblDir,"tblCellStatus.csv"),stringsAsFactors=FALSE,sep=",",header=TRUE)
  
  assign
}