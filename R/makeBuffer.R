#' @export makeBuffer.R
#'   
#' @title
#' 
#' @value
#' 
#' @description
#' 
#' @return
#' 
#' @details
#' 
#' @section
#' 
#' @author Jason Mitchell (jmitchell@@west-inc.com)
#'   
#' @seealso \code{\link{gBuffer}}, \code{\link{gIntersects}},
#'   \code{\link{gIntersection}}
#'   
#' @references 
#'   
#' @keywords
#' 
#' @examples
#' # make a buffer.
#' makeBuffer(shp,cell,radius=2.5,cellDim.m=3218.69,inner=TRUE)
makeBuffer <- function(shp,cell,radius=2.5,cellDim.m=3218.69,inner=TRUE){
  
  # shp <- allShps[[2]]
  # cell <- "CO132952"
  # radius <- 2.5
  # cellDim.m <- 3218.69
  # inner <- TRUE
  
  #   ---- Make sure the rownames of attribute data in shp match the polygonal IDs.  
  rownames(shp@data) <- sapply(shp@polygons,function(x) slot(x,"ID"))
 
  #   ---- Make the buffering circle.  
  center <- gCentroid(shp[shp@data$Grid_ID == cell,])
  circle <- gBuffer(center,byid=TRUE,width=radius*cellDim.m)
  

  #   ---- Identify set of possible outer safety cells.
  safetyOuterVec <- rownames(gIntersects(circle,shp,byid=TRUE))[gIntersects(circle,shp,byid=TRUE) == TRUE]
  
  #   ---- Restrict the outer safety cells to their own object.
  safetyOuter <- shp[rownames(shp@data) %in% safetyOuterVec,]
  
  #   ---- Identify the inner safety cells with cells entirely in circle.
  #   ---- Do this by seeing that 4 square miles = 10359994 square meters.
  preSafetyInnerVec <- gArea(gIntersection(circle,safetyOuter,byid=TRUE),byid=TRUE) >= 10359993
  safetyInnerVec <- unlist(strsplit(names(preSafetyInnerVec[preSafetyInnerVec == TRUE])," ",fixed=TRUE))[c(FALSE,TRUE)]
  
  #   ---- Restrict the inner safety cells to their own object.
  safetyInner <- shp[rownames(shp@data) %in% safetyInnerVec,]
 
  if( inner == TRUE){
    ans <- safetyInner
  } else {
    ans <- safetyOuter
  }
  
  ans
#   plot(shp)
#   plot(shp[shp@data$dblSamp == 1,],add=TRUE,col='red')
#   plot(circle,col="blue")
#   plot(safetyOuter,add=TRUE,col="green")
#   plot(safetyInner,add=TRUE,col="brown")
}