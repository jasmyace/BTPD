
#   ---- The version downloaded directly from Github on 5/11/2016.

bas.polygon.GITHUB <- function( x, n ){
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }
  
  #   Find bounding box around everything
  bb <- bbox( x )
  
  #   Find area of all polygons
  # Could call gArea at this point.  But, If x is not projected, 
  # gArea generates a warning.  Could suppress it, but, I've chosen 
  # to compute my own area, which accounts for holes and does not 
  # care whether x is projected.  I think i can drop dependency on rgeos 
  # if I do this.
  # Here is the call to gArea
  #area <- suppressWarnings((rgeos::gArea(x)))  
  
  # Here is the call to my routine.
  area <- polygonArea(x)
  
  #   Find fraction of the square Halton box covered by the polygons
  p <- min(1, area / max(diff(t(bb)))^2 )
  
  my.dim <- 2 # number of dimensions we are sampling. leave this here
  # to make it easier to expand to dim > 2
  
  #   Make sure there is a non-missing attribute associated with each polygon in x.
  #   This is because over() extracts attributes of x, and missingness is used 
  #   to flag which points are outside a polygon.
  if( inherits(x, "SpatialPolygonsDataFrame") ){
    #   x has a data frame
    df <- data.frame( sampleID=1:length(x), geometryID=row.names(x), data.frame(x) )
  } else {
    df <- data.frame( sampleID=1:length(x), geometryID=row.names(x),  row.names=row.names(x) )
  }
  
  x <- SpatialPolygonsDataFrame( x, data=df )
  
  crs.obj <- CRS(proj4string(x))
  
  #   Draw initial random start, but make sure the first point is inside the study area.
  q <- 1 - p
  z <- qnorm(0.90)
  n.init <- (1 / p) + (q*z*z/(2*p)) + (z / p)*sqrt(z*z*q*q/4 + q*1)  # term in sqrt is >0 because we have control on all terms
  n.init <- ceiling(n.init)
  if (!is.null(mxU <- get0("maxU", envir = .GlobalEnv, mode="function"))) {
    max.u <- mxU()
  } else {
    max.u <- SDraw::maxU()
  }
  
  repeat{
    m <- floor( runif( n.init*my.dim, min=0, max=max.u+1 ))
    m <- matrix(m,n.init,my.dim)
    halt.samp <- matrix(NA, n.init, my.dim)
    for(i in 1:n.init){
      halt.samp[i,] <- halton( 1, dim=my.dim, start=m[i,] )
    }
    
    #   Convert from [0,1] to a square box covering [bb]
    halt.samp <- bb[,"min"] + t(halt.samp) * rep( max(diff(t(bb))), 2)
    halt.samp <- t(halt.samp)
    
    halt.pts <- SpatialPointsDataFrame(halt.samp, data=data.frame(sampleID=1:n.init),
                                       proj4string=crs.obj )
    
    in.poly <- over( halt.pts, x )
    
    keep <- !is.na( in.poly$sampleID )
    
    if(any(keep)) break
  }
  
  
  # Keep first that is in a polygon
  which.pt.in <- which(keep)[1]  # or min(which(keep))
  m <- m[which.pt.in,]
  halt.pts <- halt.pts[which.pt.in,]
  
  #   Take initial number of Halton numbers that is approximately correct
  #   This is number of samples to take to be Alpha% sure that we get n 
  #   points in the study area.  99% of time this loop runs once.
  #   At this point, halt.pts has one point in it.
  q <- 1 - p
  z <- qnorm(0.99)
  halt.start <- m  # save for attributes later
  repeat{
    n.init <- (n / p) + (q*z*z/(2*p)) + (z / p)*sqrt(z*z*q*q/4 + q*n)  # term in sqrt is >0 because we have control on all terms
    n.init <- ceiling(n.init)
    halt.samp <- halton( n.init, dim=my.dim, start=m+1 )
    
    #   Convert from [0,1] to a square box covering [bb]
    halt.samp <- bb[,"min"] + t(halt.samp) * rep( max(diff(t(bb))), 2)
    halt.samp <- t(halt.samp)
    
    #   Check which are in the polygon, after first converting halt.samp to SpatialPoints
    #   And adding to points from previous iteration
    #   sampleID in this data frame gets overwritten below when assign to @data
    
    halt.pts2 <- SpatialPointsDataFrame(halt.samp, proj4string=crs.obj, data=data.frame(sampleID=1:nrow(halt.samp)) )
    halt.pts <- rbind(halt.pts, halt.pts2)
    
    
    in.poly <- over( halt.pts, x )
    
    #   Reject the points outside the polygon, and attach other attributes if present
    keep <- !is.na( in.poly$sampleID )  # in.poly$sampleID is row num of polygon in x
    
    
    halt.pts@data <- data.frame( in.poly )
    halt.pts <- halt.pts[ keep, ]
    
    #   The way we computed n.init, there should be more points in halt.pts than we need. Keep the initial ones.
    if( length(halt.pts) >= n ){
      halt.pts <- halt.pts[1:n,]
      halt.pts$sampleID <- 1:n   # renumber the site ID's because some (those outside polygon) were tossed above
      break
    } else {
      n <- n - length(halt.pts)
      m <- m + length(halt.pts)  # place in Halton sequence to start next iter
    }
    
  }  
  
  attr(halt.pts, "frame") <- deparse(substitute(x))
  attr(halt.pts, "frame.type") <- "polygon"
  attr(halt.pts, "sample.type") <- "BAS"
  attr(halt.pts, "random.start") <- halt.start
  
  halt.pts
  
}