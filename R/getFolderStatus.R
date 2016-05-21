getFolderStatus <- function(tblDir="//lar-file-srv/Data/BTPD_2016/Analysis/Database/"){
  
  # tblDir <- "//lar-file-srv/Data/BTPD_2016/Analysis/Database/"
  
  folder <- read.table(paste0(tblDir,"tblFolders.csv"),stringsAsFactors=FALSE,sep=",",header=TRUE)
  
  folder
}