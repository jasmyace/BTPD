getRankStatus <- function(tblDir="//lar-file-srv/Data/BTPD_2016/Analysis/Database/"){
  
  # tblDir <- "//lar-file-srv/Data/BTPD_2016/Analysis/Database/"

  ranks <- read.csv(paste0(tblDir,"tblSampling.csv"))
  
  ranks
  
}