cat("helpers  -   ")

############## some  helper functions 
 
st_erase = function(x, y) st_difference(x, st_union(st_combine(y))) # erase all of y from x




matrix.to.sfPolys <- function (coords, proj=CRS(as.character(NA))) {
  
  if(!is.matrix(coords)) stop("## Error: in matrix.to.sfPolys coords needs to be a matrix")
  
  window.f <- data.frame(coords) %>% 
     st_as_sf(coords = c("X1", "X2"), crs = proj) %>% 
     summarise(geometry = st_combine(geometry)) %>% 
     st_cast("POLYGON") 
  
  return(window.f)
}





psample <- function(x, ...) x[sample(length(x), ...)]






`%nin%` = Negate(`%in%`)





# aes several scales made easy
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}


options(ggrepel.max.overlaps = Inf) 