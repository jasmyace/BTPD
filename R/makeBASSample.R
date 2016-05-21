#' @export makeBASSample.R
#'   
#' @title Create a spatially balanced permutation of all polygons in a shapefile
#'   via the Balance-Acceptance Sampling (BAS) methodology.
#'   
#' @value shp A polygon shapefile of class \code{SpatialPolygonsDataFrame} with 
#'   a variable in the attribute data entitled \code{Grid_ID}.
#'   
#' @value prop A number between 0 and 1 indicating the proportion of polygonal 
#'   elements in \code{shp} which are to be doubly sampled.
#'   
#' @value method The method with which doubly sampled polygons are to be 
#'   shuffled into the BAS rankings of polygons.  See 'Details.'
#'   
#' @value extra An integer greater than or equal to zero specifiying the number 
#'   of extra polygons to be doubly sampled.  Usually utilized a safeguard.
#'   
#' @value tblDir A text string specifiying the directory in which database-like 
#'   tables are stored.
#'   
#' @description
#' 
#' @return A \code{SpatialPolygonsDataFrame} with all polygonal units ranked via
#'   the BAS Ranking algorithm via variable \code{sampleID}, with the proportion
#'   of doubly sampled cells identifiable via variable \code{dblSamp.}
#'   
#' @details Function \code{bas.polygon} within package \code{SDraw} performs the
#'   actual BAS ranking.
#'   
#' @section {Methods:}
#'   
#'   Method 1 randomly selects the proportion of doubly sampled cells, as 
#'   specified via variable \code{prop}, from within the BAS ranking.  Thus, the
#'   doubly sampled cells are randomly sprinkled throughout the list.
#'   
#'   Method 2 (not currently coded) places all cells selected to be doubly 
#'   sampled, as specified via variable \code{prop}, at the top of the BAS 
#'   ranking list.  Thus, these are examined first.
#'   
#'   Method 3 (not currently coded) places all cells selected to be doubly 
#'   sampled, as specified via variable \code{prop}, at the bottom of the BAS 
#'   ranking list.  Thus, these are examined last.
#'   
#'   Method 4 (not currently coded) places all cells selected to be doubly 
#'   sampled, as specified via variable \code{prop}, after so many of the 
#'   \eqn{1-\code{prop}} cells have been doubly examined.  Thus, these are 
#'   examined as a block within the BAS ranking..
#'   
#' @author Jason Mitchell (jmitchell@@west-inc.com)
#'   
#' @seealso \code{\link{bas.polygon}}
#'   
#' @references BAS ranking paper.  bas.polygon help file.
#'   
#' @keywords Balance-acceptance Sampling. BAS.
#'   
#' @examples
#' # Randomly permute all polygons in shp, identify 10% to be doubly
#' # sampled, sprinkle the doubly sampled randomly throughout the list,
#' # and identity an extra 25 polygons to be doubly sampled, in addition
#' # to the original 10%.  
#' makeBASSample(shp,prop=0.10,method=1,extra=25)
makeBASSample <- function(shp,prop,method,extra=0,dir,tblDir=tblDir){
  
  # shp <- allShps[[1]]
  # prop <- 0.10
  # method <- 1
  # extra <- 25
  # dir <- paste0(btpdStem,analStem,dataStem,sampStem)
  # tblDir <- tblDir
  
  #   ---- A safeguard to prevent bad function calls.  
  if( !(is.numeric(prop) & prop <= 1 & prop >= 0) ){
    warning("Problem with variable prop.  Try again.")
    stop
  }
  
  #   ---- A safeguard to prevent bad function calls.  
  if( !( (is.integer(extra) | is.numeric(extra) ) & extra > 0) ){
    warning("Problem with variable extra.  Try again.")
    stop
  }
  
  #   ---- Function bas.polygon creates duplicates.  So, sample more than needed.
  shp@data$R_ID <- seq(1,nrow(shp))
  
  shpPoints <- gCentroid(shp,byid=TRUE)
  shpPoints <- as(shpPoints,"SpatialPointsDataFrame")
  shpPoints@data <- shp@data
  names(shpPoints@data)[names(shpPoints@data) == 'x'] <- 'xCent'
  names(shpPoints@data)[names(shpPoints@data) == 'y'] <- 'yCent' 

  seed <- 42
  set.seed(seed)
  perm <- bas.point(shpPoints,nrow(shpPoints))

  #   ---- Rerank to make sure sampleID is 1 through nrow(shp@data).  
  perm@data <- perm@data[order(perm@data$sampleID),]
  perm@data$sampleID <- seq(1,nrow(perm@data))
  perm@data <- perm@data[order(perm@data$R_ID),]
  
  #   ---- Identify randomly the proportion prop we are to double sample.
  dblSampN <- round((nrow(perm@data)*prop),0) + 1 + extra
  
  seed <- 85
  set.seed(seed)
  samp <- bas.point(shpPoints,dblSampN)
  
  dblSamp <- data.frame(Grid_ID=sample(perm@data$Grid_ID,dblSampN,replace=FALSE),dblSamp=1)#data.frame(Grid_ID=samp@data$Grid_ID,dblSamp=1)#
  perm@data <- merge(perm@data,dblSamp,by=c("Grid_ID"),all.x=TRUE)
  perm@data$dblSamp <- ifelse(is.na(perm@data$dblSamp),0,1)
  
  #   ---- Done with points. Put back to polygons.
  shp@data <- merge(shp@data,perm@data[,c('R_ID','sampleID','dblSamp')],by=c("R_ID"),all.x=TRUE)
  shp@data <- shp@data[order(shp@data$R_ID),]
  
  #   ---- Make sure the rownames of attribute data in shp match the polygonal IDs.  
  rownames(shp@data) <- sapply(shp@polygons,function(x) slot(x,"ID"))
  
  #   ---- Make sure record counts are correct and as expected.  
  if( (nrow(perm) == nrow(shp@data)) ){
    cat("BAS sampling (permutation) successful.")
  } else {
   warning("BAS sampling (permutation) failed.") 
   stop
  }
  write.csv(shp@data,paste0(dir,"BAS Sampling Scheme (",seed,") ",Sys.Date(),".csv"),row.names=FALSE)
  write.csv(shp@data,paste0(tblDir,"tblSampling.csv"),row.names=FALSE)
  shp
}