cat("find.best.hab()  -  ")



find.best.hab <- function(choices.in, hab, origin=NA) {
   # this function finds the best habitat from the choices given in choices.in
   # include all cells of habitat2
  ch <- which(terra::rast(hab)[c(choices.in)]==2)
    # include the four cells furthest away from the supplied origin
  if(is.na(origin)) {
    
    s <- ifelse(sum(!is.na(terra::rast(hab)[c(choices.in)]))<3,sum(!is.na(terra::rast(hab)[c(choices.in)])),3)
    ch <- c(ch, psample(which(!is.na(terra::rast(hab)[c(choices.in)])), s))
    
  } else {
    dist <- sqrt((floor(terra::colFromCell(terra::rast(hab), origin)) - floor(terra::colFromCell(terra::rast(hab), c(choices.in))))^2 + (floor(terra::rowFromCell(terra::rast(hab), origin)) - floor(terra::rowFromCell(terra::rast(hab), c(choices.in))))^2)
    dist[is.na(terra::rast(hab)[c(choices.in)])] <- -999
    dist.ord <- order(dist, decreasing=TRUE)
    
    ch <- c(ch, dist.ord[1:3])
  }  

  return(choices.in[unique(ch)])
}

