cat("beaver habitat: ")
 
## proj in use
## mercproj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"
 
 
###################### beaver habitat suitability map layer (covers scotland) 
###################### 
#window <- matrix.to.SpatPolys(window, proj=map@proj4string)
       cat("suitability map\n")
map <- terra::rast(stars::read_stars(here::here("data/bvr_rc_100")))
map.r <- terra::rast(terra::ext(map))
res(map.r) <- res(map)
terra::values(map.r) <- terra::values(map)
terra::crs(map.r) <-  mercproj

 ### script uses map.r - replace NA by 0 ? see how it goes with disp/wander functions
 #map.r[which(is.na(map.r))] <- 0       
  
 
 
source(here::here("initLoad_GIS_landscape.R"))   
       
        