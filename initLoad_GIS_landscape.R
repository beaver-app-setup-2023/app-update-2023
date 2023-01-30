cat("landscape layers: ")
 
## proj in use
## mercproj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"


 # UKcoastline  <-  st_read("E:/Beavers app update 2023/data/UKbound.shp" )$geometry  
 # st_crs(UKcoastline)  <- mercproj 
 
 # coastline <- st_crop(UKcoastline, region_box) 
 # st_write(coastline, here("data2/coastline.shp") , append=FALSE)




###################### rivs + lakes (pre-cropped to region extent)
###################### 
cat("rivers  -  ")
     rivlines <-  st_as_sf(st_union(st_read("./data2/rivlines.shp", quiet=TRUE)  ))  
     lakes <- st_as_sf(st_union(st_read("./data2/lakes.shp", quiet=TRUE)$geometry )) 
     rivlines  <- rbind( rivlines , lakes)
     rivlines  <- st_transform(rivlines, mercproj)
     st_geometry(rivlines) <- "geometry" 
     rivlines$layer<- "river"

     coastline <- st_read(here("data2/coastline.shp"), quiet=TRUE)$geometry
     coastline <- st_transform(coastline, mercproj)
     
###################### catchments (pre-cropped to region extent)
######################   
cat("catchments  -  ")
  intcatch <- st_sf(geometry=  st_read( paste0("./data2/intcatch.shp") , quiet=TRUE)$geometry, layer="water body intercatchments") 
  catch    <- st_sf(geometry=  st_read( paste0("./data2/catch.shp"), quiet=TRUE)$geometry,     layer="main catchments")
  catch    <- st_transform(catch, mercproj)  
  intcatch <- st_transform(intcatch, mercproj)  
  extra_HabLayers_l  <- st_read( here::here("data2/extra_HabLayers_l.shp") , quiet=TRUE)
  extra_HabLayers_l <- st_transform(extra_HabLayers_l ,mercproj)
  
   
  
  
        
 

##### maps as images 
##### for side bar
##### note borders may have to be cropped in paint or something when using as images (lighter for the UI) (boo)
cat("maps as images  -  ")
 img1   <- png::readPNG(here::here(paste0("www/GBMap.png")))            # image = all gb with extent - generated in script above then cropped in paint and save in www file for images
 imgScotMap    <- png::readPNG(here::here(paste0("www2/ScotMap.png")))  # image = extent area with rivers - crop by hand!
 HabMapLayers <- st_read("./data2/HabMapLayers.shp", quiet=TRUE)  # shapefile
 st_crs(HabMapLayers) <- mercproj
 HabMapLayers <- rbind(HabMapLayers,  rivlines)  
 imgsuit   <- png::readPNG(here::here(paste0("www2/SuitLayer.png")))    # image = only suitable hab layer in turquoise
  
 
   
 
 
        
###################### OS layers (pre-cropped to region extent) 
###################### 
cat("additional OS layers  -  ")
 listfil <- list.files("./data2/",pattern= "BackLayer.*\\.shp") # all
 
 listfil_txt <- list.files("./data2/",pattern= "BackLayer_txt.*\\.shp") # all txt
 listfil_pt  <- list.files("./data2/",pattern= "BackLayer_pt.*\\.shp")  # all pts
 listfil_pol <- listfil [!listfil %in% listfil_txt &  !listfil %in% listfil_pt] # all other
  
 BackLayers <- BackText <- BackPoints <- NULL  
 cat("shapefiles - polygons  ")    
 for ( fil in c(listfil_pol )){
   cat(".")
      shpfil <-  st_read(paste0("./data2/",fil), quiet=TRUE) 
      BackLayers <- rbind(shpfil,BackLayers)
 }  
 cat("    shapefiles - points  ")
 for ( fil in c(listfil_pt )){
   cat(".")
   shpfil <-  st_read(paste0("./data2/",fil), quiet=TRUE) 
      BackPoints  <- rbind(shpfil,BackPoints )
 } 
 cat("   shapefiles - text  ")
 for ( fil in c(listfil_txt)){
   cat(".")
   shpfil <-  st_read(paste0("./data2/",fil), quiet=TRUE) 
      BackText <- rbind(shpfil,BackText)
    }

BackLayers <- st_transform(BackLayers  ,mercproj)%>%  st_set_precision(1) %>% st_make_valid()
BackText   <- st_transform(BackText  ,mercproj)  %>%  st_set_precision(1) %>% st_make_valid()
BackPoints <- st_transform(BackPoints  ,mercproj)%>%  st_set_precision(1) %>% st_make_valid()



###################### merging some layers for ease of visualisation /mapping
######################  
 BackText <- BackText [-which(BackText$layer == "general spots" & nchar(BackText$label>1) ),]
 BackText$layer [BackText$layer %in% c("Railway Station","geographical area", "general spots" )] <- "named places" 
 BackText$layer [BackText$layer =="Spot Height"] <- "local altitude"
 
 BackText$layer2 <- BackText$layer ## additional category for plotting
 BackText$layer2[which(BackText$layer == "Railway Station")] <- "Railway Station"
 BackText$label[BackText$layer2 == "named places"] <- paste0(BackText$label[BackText$layer2 == "named places"],"\n")
 BackPoints$layer [BackPoints$layer =="Spot Height"] <- "local altitude"

 beLoch <- c('Loch','LOCH', 'River','FIRTH', 'Bay', 'Reservoir') 
 BackText$layer2[which(  BackText$label %in% unique (grep(paste(beLoch,collapse="|"),  BackText$label, value=TRUE)) )] <- "Loch"
 beForest <- c('Forest','Wood', 'F O R E S T', 'S T R A T H C O N O N' ) 
 BackText$layer2[which(  BackText$label %in% unique (grep(paste(beForest,collapse="|"),  BackText$label, value=TRUE)) )] <- "Loch"
#Sgurr = peak!  
   
  
###################### precision
cat("  -  precision  -   ") 
    coastline         <- coastline %>%  st_set_precision(1) %>% st_make_valid()
    rivlines          <- rivlines %>%  st_set_precision(1) %>% st_make_valid()
    BackLayers        <- BackLayers %>%  st_set_precision(1) %>% st_make_valid()
    extra_HabLayers_l <- extra_HabLayers_l %>%  st_set_precision(1) %>% st_make_valid()
    catch             <- catch %>%  st_set_precision(1) %>% st_make_valid()
    intcatch          <- intcatch %>%  st_set_precision(1) %>% st_make_valid()
 

    
    
source(here::here("initLoad_GIS_ggmapped.R"))      