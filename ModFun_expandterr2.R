cat("expand.territory2()  -  ")

expand.territory2 <- function(ter, id, hab) {
 # ter <- values(ter) ##changed
  current <- which(terra::values(ter) == id)
  adj <- terra::adjacent(rast(ter), current, directions=8, pairs=FALSE)
  adj <-  adj[which(ter[adj] == 0 & hab[adj] == 2)] #exclude occupied territory and null/dispersal habitat
  if(length(adj) == 0) {return(ter)} 
  
  if(length(adj) > 0)  {
  best <- psample(adj, 1) #randomly pick one of the best adjacent pixels
  ter[best] <- id
  return(ter)
  }
}
 