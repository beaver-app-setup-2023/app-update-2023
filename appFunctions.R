
################################################################################    
################      BEAVER TRANSLOCATION SIMULTATION APP      ################
################        for NatureScot Nov. 2022                ################
################                                                ################
################       FuNCTIONS & par values                   ################
################         script sourced #1/2                    ################
################################################################################  
 



 
 
 

 
 
 

 


 
  
################# kernels from sf object function
st_kde <- function(points,cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster(matrix)
}


### testing output

# Nruns  <- st_read("C:/Users/Adm!n/Downloads/SimOutputs_test/SimNruns_test.shp")
# Hullss <- st_read("C:/Users/Adm!n/Downloads/SimOutputs_test/SimChulls_test.shp")
# pts    <- st_read("C:/Users/Adm!n/Downloads/SimOutputs_test/ReleasePoints_test.shp")
# initTerrs <- st_read("C:/Users/Adm!n/Downloads/SimOutputs_test/InitTerrs_test.shp")

#ggplot() +geom_sf(data=Hullss)
 
#ggplot()  + geom_sf(data=Nruns, aes(col=Nruns)) +geom_sf(data=initTerrs)+geom_sf(data=pts)+
 # facet_grid(year~Nruns)
