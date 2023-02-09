cat("adjacent.id()  -  ")

adjacent.id <- function(x, cells, pairs=FALSE, id, directions) {
  #wrapper for adjacent() that excludes given id from the target
  require(terra)
  target=(1:length(terra::values(x)))[-which(terra::values(x)==id)]
  terra::adjacent(x, cells=cells, include=FALSE, pairs=pairs, directions=directions) ## changed from raster(x)
}