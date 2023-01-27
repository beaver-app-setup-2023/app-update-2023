cat("adjacent.id()  -  ")

adjacent.id <- function(x, cells, pairs=FALSE, id, ...) {
  #wrapper for adjacent() that excludes given id from the target
  require(raster)
  target=(1:length(x))[-which(values(x)==id)]
  adjacent( x , cells=cells, target=target, pairs=pairs, ...) ## changed from raster(x)
}