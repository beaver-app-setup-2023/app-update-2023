cat("beaver habitat: ")
 
## proj in use
## mercproj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"
 
 
###################### beaver habitat suitability map layer (covers scotland) 
###################### 
#window <- matrix.to.SpatPolys(window, proj=map@proj4string)
       cat("suitability map\n")
       map <- readGDAL(here("data/bvr_rc_100"))
       map.r <- raster(extent(map))
       res(map.r) <- map@grid@cellsize
       map.r[] <- map@data$band1
       proj4string(map.r) <-  CRS(proj4string(map)) 

 ### script uses map.r - replace NA by 0 ? see how it goes with disp/wander functions
 #map.r[which(is.na(map.r))] <- 0       
  
 
 
source(here::here("initLoad_GIS_landscape.R"))   
       
        