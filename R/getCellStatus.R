getCellStatus <- function(tblDir="//lar-file-srv/Data/BTPD_2016/Analysis/Database/"){
  
  # tblDir <- "//lar-file-srv/Data/BTPD_2016/Analysis/Database/"
  
  
#   #   ---- Find out where each cell is in the directory.
#   beg <- Sys.time()
#   filenames <- list.files(tblDir,pattern="*.csv",full.names=TRUE,recursive=TRUE)
#   dat <- lapply(filenames, function(x) read.csv(x,header=FALSE)) 
#   dat <- data.frame(lapply(data.frame(t(sapply(dat, `[`))), unlist))
#   end <- Sys.time()
#   
#   time <- end - beg
#   time
  
  assign <- read.table(paste0(tblDir,"tblCellStatus.csv"),stringsAsFactors=FALSE,sep=",",header=TRUE)
  
  assign
}