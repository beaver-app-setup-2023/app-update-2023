cat("expand.territory2()  -  ")

expand.territory2 <- function(ter, id, hab) {
  current <- which(terra::values(ter) == id)
  adj <- terra::adjacent(terra::rast(ter), current, directions=8, pairs=FALSE)
  adj <-  base::unique(adj[which(ter[as.vector(adj)] == 0 & na.omit(terra::rast(hab)[as.vector(adj)]) == 2)]) #exclude occupied territory and null/dispersal habitat
  if(length(adj) == 0) {return(ter)} 
  
  if(length(adj) > 0)  {
  best <- psample(adj, 1) #randomly pick one of the best adjacent pixels
  ter[best] <- id
  return(ter)
  }
}
 