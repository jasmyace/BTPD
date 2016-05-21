getShapefiles <- function(dir){
  
  # dir <- paste0(btpdStem,analStem,dataStem,OGR.Stem)
  
  coShp <- readOGR(dir,"BTPD_Grid_CO",verbose=FALSE)
  
  allShps <- list(coShp)
  
}