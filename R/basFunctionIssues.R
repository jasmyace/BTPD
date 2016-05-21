
require(rgeos)
require(rgdal)
require(sp)


dir <- "//lar-file-srv/Data/BTPD - CO Summer 2016/Analysis/data/Shapefiles"

shp <- readOGR(dir,"BTPD_Grid_CO",verbose=FALSE)
nrow(shp)                             # 11101
sum(duplicated(shp@data$Grid_ID))     # 0 -- variable Grid_ID is a unique identifier.


perm <- bas.polygon2(shp,nrow(shp))
nrow(perm)                            # 11101 -- correct dimension. 
sum(duplicated(perm@data$Grid_ID))    # 2799 -- variable Grid_ID duplicated. 


permDat <- perm@data[,c('sampleID','Grid_ID','State')]
shp@data <- merge(shp@data,permDat,by=c('State','Grid_ID'),all.x=TRUE)  
nrow(shp)                             # 13850 



# okay, so maybe it only works on points. 

dir <- "//lar-file-srv/Data/BTPD - CO Summer 2016/Analysis/data/Shapefiles"
shp <- readOGR(dir,"BTPD_Grid_CO",verbose=FALSE)

#shp@data <- data.frame(shp@data,gCentroid(shp,byid=TRUE))

shpPoints <- gCentroid(shp,byid=TRUE)
shpPoints <- as(shpPoints,"SpatialPointsDataFrame")
shpPoints@data <- shp@data
names(shpPoints@data)[names(shpPoints@data) == 'x'] <- 'xCent'
names(shpPoints@data)[names(shpPoints@data) == 'y'] <- 'yCent'  

perm2 <- bas.point2(shpPoints,nrow(shpPoints))  #@data[,c('sampleID','Grid_ID','State')]
head(perm2@data)    # no sampleID?
