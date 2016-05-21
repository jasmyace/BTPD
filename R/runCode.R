
#   ---- Setup the required packages.  
require(rgeos)                              # shapefile manipulation
require(rgdal)                              # read/write shapefiles
require(maptools)                           # spRbind function
require(RColorBrewer)                       # pretty colors!
require(png)                                # read in pngs
require(plyr)                               # first + last in function makeFolders
require(data.table)
library(raster)                             # make easy mini-grids
library(sp) 

require(devtools)                           # function install_github
#install_github("tmcd82070/SDraw")
require(SDraw)                              # version 1.5.2 (source from github)

#   ---- Setup required file stems.  
btpdStem <- "//lar-file-srv/Data/BTPD_2016/"
analStem <- "Analysis/"
repoStem <- "R/"
dataStem <- "data/"
sampStem <- "Sampling/"
shpsStem <- "Shapefiles/"
OGR.Stem <- "Shapefiles"
digiStem <- "Digitizing/"
baseStem <- "Database/"
tempStem <- "BTPD_Digitizing_Template"

#   ---- Construct useful directories.  
progDir <- paste0(btpdStem,analStem,repoStem)
shpsDir <- paste0(btpdStem,analStem,dataStem,shpsStem)
sampDir <- paste0(btpdStem,analStem,dataStem,sampStem)
OGR.Dir <- paste0(btpdStem,analStem,dataStem,OGR.Stem)
digiDir <- paste0(btpdStem,digiStem)
baseDir <- paste0(btpdStem,analStem,baseStem)
dtdbDir <- paste0(btpdStem,analStem,baseStem,digiStem)
tempDir <- paste0(btpdStem,analStem,dataStem,shpsStem,tempStem)

#   ---- Define global variables.
nL1 <<- 10
nL2 <<- 10

#   ---- Define useful projections.  
projAEAc  <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
#projAEAj  <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4326  <- "+init=epsg:4326"
proj3857  <- "+init=epsg:3857"   # used by esri online naip imagery.  

#   ---- Obtain all helper programs.  
source(paste0(btpdStem,analStem,repoStem,"makeFolders.R"))         #   ---- Make a folder structure for digitizing.  
source(paste0(btpdStem,analStem,repoStem,"getShapefiles.R"))       #   ---- Read in shapefiles.  
source(paste0(btpdStem,analStem,repoStem,"getRankStatus.R"))       #   ---- Read in sampling ranks.  
source(paste0(btpdStem,analStem,repoStem,"getCellStatus.R"))       #   ---- Read in cell status.  
source(paste0(btpdStem,analStem,repoStem,"getFolderStatus.R"))     #   ---- Read in folder status.  
source(paste0(btpdStem,analStem,repoStem,"bas.polygon.GITHUB.R"))  #   ---- Get current bas.polygon.
source(paste0(btpdStem,analStem,repoStem,"makeBASSample.R"))       #   ---- Make a BAS Sample. 
source(paste0(btpdStem,analStem,repoStem,"makeBuffer.R"))          #   ---- Make a buffer for a cell.   
source(paste0(btpdStem,analStem,repoStem,"checkuser.R"))           #   ---- Check if userID is valid.   

tblDir <- paste0(btpdStem,analStem,baseStem)
nTotal <- 11101 

#   ---- Get all the shapefiles together.  
allShps <- getShapefiles(OGR.Dir)

#   ---- Shapefiles in allShps.
#   ---- 1. CO Grid of 11,101 cells.
#   ---- 2. CO Grid of 11,101 cells with cells ranked.  

#   ---- Form a BAS sample (spatial permutation) of all grid cells.  
allShps[[2]] <- makeBASSample(shp=allShps[[1]],prop=0.10,method=1,extra=25,dir=sampDir,tblDir=tblDir)

#   ---- Make all the folders associated with digitizing.
makeFolders(dir=paste0(btpdStem,digiStem),shp=allShps[[2]],var="Grid_ID",tblDir=tblDir)

#   ---- Make a master record of all grid cells.  






writeOGR(allShps[[2]],paste0(shpsDir,"BTPD_Grid_CO_Ranked"),"BTPD_Grid_CO_Ranked",overwrite_layer=TRUE,driver="ESRI Shapefile")

save.image("//lar-file-srv/Data/BTPD_2016/Analysis/R/working3.RData")
load("//lar-file-srv/Data/BTPD_2016/Analysis/R/working3.RData")

plot(allShps[[2]])
plot(allShps[[2]][allShps[[2]]@data$dblSamp == 1,],add=TRUE,col='red')