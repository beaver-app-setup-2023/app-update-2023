   
#setwd("D:/Beavers 2022 App/Beavers 2022 App")  
#library(rsconnect)
# go on https://www.shinyapps.io/admin/#/profile
# rsconnect::setAccountInfo(name='naturalandenvironmentalscience', token='03E2DBCC6228F125D9432018F0E237B3', secret='eMpa7Wd56xVkEsMJkch1tjamn3wcwJ6tAc1cO3T1')

# rsconnect::deployApp('D:/Beavers 2022 App/Beavers 2022 App',     appName = "BeavsReloc",     account ='naturalandenvironmentalscience') 
 
# rsconnect::deployApp('D:/Beavers 2022 App/Beavers 2022 App',     appName = "NatureScotBeaverApp",     account ='naturalandenvironmentalscience') 

# dont include library(shiny) or refs to local files dir() in app script
# diagnostics:
# rsconnect::showLogs() 
##  startup timeout = loading lots of data outside shinyServer() function or in  global.R  if that operation takes longer then 60 seconds
##  can adjust the startup timeout in the advance settings page for your application in the shinyapps.io dashboard. 
  

# list.files("/.")

#### GIS input data
#### 
#### temp extent

# bufp <- st_sfc(st_buffer(st_point(c(240000,840000) ), 25000) )
# st_crs(bufp) <- mercproj
# plot(bufp, add=T)
# st_bbox(bufp)
 rasterOptions(maxmemory = 1e+09)
 rasterOptions(tmpdir="temp_files")

 
 scotcrop    <- as(as(extent(215000 ,  265000, 815000, 865000), "SpatialPolygons"), "sf") ## extent = 50km buff around relevant loc point
 scotcrop_sp    <-  as(extent(215000 ,  265000, 815000, 865000), "SpatialPolygons")  ## sf obj
  
 scotcrop0   <- as(as(extent(120000, 450000 ,590000, 1000000), "SpatialPolygons"), "sf") ## all scotland
 st_crs(scotcrop)  <- mercproj
 

   
 scotbound <-  st_read("./data/scotbound.shp" )$geometry  
 st_crs(scotbound)  <- mercproj
 
 # rivs + lakes, cropped beforehand but not saved together because cant save geom collection sf type
 rivlines <-  st_read("./data/rivlines.shp" )  
 st_crs(rivlines)  <- mercproj
 lakes <- st_read("./data/lakes.shp")$geometry 
 st_crs(lakes) <- mercproj
 rivlines  <- st_as_sf(st_union(st_crop(rivlines,scotcrop))) 
 rivlines  <- st_union(rivlines, lakes)
   
   
## habitat map layer (scotland) 
#window <- matrix.to.SpatPolys(window, proj=map@proj4string)
 map <- readGDAL(here("data/bvr_rc_100"))
 map.r <- raster(extent(map))
 res(map.r) <- map@grid@cellsize
 map.r[] <- map@data$band1
 proj4string(map.r) <-  CRS(proj4string(map)) 


  
############ compile OS layers for general maps (roads etc)  
############ note that this was run using data not included in the project, so that they are not loaded with the app
############ so init shapefiles are in D:/Beavers 2022 App
############ then the cropped shapefiles to be included with the app code are stored in ./data/ 
 rerunOSlayers= FALSE
 if(rerunOSlayers == TRUE) {
 
  ### crop each layer to assemble into  BackLayers on start

 listfil <- list.files("D:/Beavers 2022 App/strtgi_essh_gb/data/",pattern="shp$") ## unload all layers from strategi file and crop to extent
 listfil
       
  ## polygons and lines  - ALL
  for ( filnum in seq(1,length(listfil)) ) {
    fil <- listfil[filnum]
   shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
      if(nrow(shpfil)!=0 ) {
        if (unique(st_geometry_type(shpfil)) [1] != "POINT") {
        shpfil <- st_transform(st_union(shpfil), mercproj) 
        print("layer read")
        shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
        print("layer cropped")
        if( nrow(shpfil)!=0) {
                  print("layer ok")
        filname <- str_replace(fil, ".shp","")
        shpfil$layer <- str_replace(filname, "_"," ")
        st_write(shpfil, paste0("./data/BackLayer",filnum,".shp"), append=FALSE ) }
        }
 } } 
     
### text and points - which layers
 for ( filnum in seq(1,length(listfil)) ){
    fil <- listfil[filnum]
    shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
   if (unique(st_geometry_type(shpfil)) [1] == "POINT") {
     print(filnum)
     print(fil)} } ## c(3,10,13,14,20,22,23,24,25,26,27)
   
listfil[ c(3,10,13,14,20,22,23,24,25,26,27)]
    
        if (unique(st_geometry_type(shpfil)) [1] == "POINT") {
        shpfil <- subset(st_transform( shpfil , mercproj), select=ADMIN_NAME) }
        
  ## points and text
  filnum=3
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          nrow(shpfil)
          shpfil
          shpfil <- subset(st_transform( shpfil , mercproj), select=ADMIN_NAME)
         filname <- str_replace(fil, ".shp","")
         shpfil$layer <- str_replace(filname, "_"," ")
         names(shpfil) [1] <- "label"
         st_write(shpfil, paste0("./data/BackLayer_txt",filnum,".shp"), append=FALSE )  # "admin seed"
         
 ## points and text
  filnum=10
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          nrow(shpfil)
          shpfil
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil[shpfil$LEGEND == "Graphic Text, Geographical Area",],select=NAME)
          shpfil$layer <- "general spots"
         names(shpfil) [1] <- "label"
         
         st_write(shpfil, paste0("./data/BackLayer_txt",filnum,".shp"), append=FALSE ) # "general spots"
         ggplot()+geom_sf_text(data=shpfil,aes( label=label) )
  ## points and text
  filnum=13
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          nrow(shpfil)
          shpfil
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil ,select=NAME)
          shpfil$layer <- "geographical area"
         names(shpfil) [1] <- "label"
         #shpfil$label <- paste0(shpfil$label,"\n")
         st_write(shpfil, paste0("./data/BackLayer_txt",filnum,".shp"), append=FALSE ) #"geographical area"
         ggplot()+geom_sf_text(data=shpfil,aes( label=label) )
 ## points and text
  filnum=14
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          nrow(shpfil)
          shpfil
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil ,select=LEGEND)
          names(shpfil)
          tvshapfil <- st_as_sf(layer="TV or radio mast or tower", st_union( shpfil$geometry[shpfil$LEGEND == "Television, Radio Mast or Tower"]))
          marshshapfil <- st_as_sf( st_union(shpfil[shpfil$LEGEND == "Marsh",]),layer="marsh")
          windgenshapfil <- st_as_sf(layer ="Wind powered generator",st_union(shpfil[shpfil$LEGEND == "Wind Powered Generator",] ))
          shpfil <- rbind(tvshapfil, marshshapfil,windgenshapfil)
            st_write(shpfil, paste0("./data/BackLayer_pt",filnum,".shp"), append=FALSE ) 
         ggplot()+geom_sf (data=shpfil,aes(col=layer, shape=layer) )
  ## points and text
  filnum=20
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          nrow(shpfil)
          table(shpfil$LEGEND)
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil ,select=c(NAME,LEGEND) )   # layer =Railway Station
            names(shpfil)
           shpfil <- st_as_sf(layer="Railway Station", label= shpfil$NAME[shpfil$LEGEND == "Railway Station"], shpfil$geometry[shpfil$LEGEND == "Railway Station"]) 
           st_write(shpfil, paste0("./data/BackLayer_txt",filnum,".shp"), append=FALSE ) 
         ggplot()+geom_sf_text (data=shpfil,aes(col=layer, shape=layer, label=label) )
   ## points and text
  filnum=24
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          shpfil
          shpfil <-  st_transform( shpfil , mercproj) 
          names(shpfil)[1] <- "height"
           shpfil <- st_as_sf(layer="Spot Height", label= paste0("\n",shpfil$METRIC), shpfil$geometry ) # heights
              st_write(shpfil, paste0("./data/BackLayer_txt",filnum,".shp"), append=FALSE ) 
           p1 <-  ggplot()+geom_sf_text (data=shpfil,aes(col=layer, shape=layer, label=label) )
           shpfil <- st_as_sf(layer ="Spot Height",st_union(shpfil )) 
            st_write(shpfil, paste0("./data/BackLayer_pt",filnum,".shp"), append=FALSE ) 
     p1+geom_sf  (data=shpfil,aes(col=layer, shape=layer ) )
       
    ## points and text
  filnum=27
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) # road names
         shpfil <- st_transform( shpfil , mercproj) 
         shpfil <- st_as_sf( st_crop(shpfil,scotcrop)) 
          shpfil
          shpfil <-  st_transform( shpfil , mercproj) 
          
           shpfil <- shpfil[shpfil$LEGEND %in% c("Graphic Text, A Road Number","Graphic Text, B Road Number","Graphic Text, Primary Route Number"),]
           shpfil <- st_as_sf(layer="road names", label= shpfil$NAME, shpfil$geometry ) 
              st_write(shpfil, paste0("./data/BackLayer_txt",filnum,".shp"), append=FALSE ) 
         ggplot()+geom_sf_text (data=shpfil,aes(col=layer, shape=layer, label=label) )
 
         
         
 } # rerun with chosen layers? (no feedback as to which layers as of Jan2023)
         
         
##### assemble OS layers (from layers processed with code above)
 listfil <- list.files("./data/",pattern= "BackLayer.*\\.shp") # all
 listfil  
 
 listfil_txt <- list.files("./data/",pattern= "BackLayer_txt.*\\.shp") # all txt
 listfil_pt <- list.files("./data/",pattern= "BackLayer_pt.*\\.shp") # all pt
 listfil_pol <- listfil [!listfil %in% listfil_txt &  !listfil %in% listfil_pt] # all other
 
 
BackLayers <- BackText <- BackPoints <- NULL ## assemble cropped
    for ( fil in c(listfil_pol )){
      shpfil <-  st_read(paste0("./data/",fil)) 
      BackLayers <- rbind(shpfil,BackLayers)
    }
 
   for ( fil in c(listfil_pt )){
      shpfil <-  st_read(paste0("./data/",fil)) 
      BackPoints  <- rbind(shpfil,BackPoints )
    }
 
   for ( fil in c(listfil_txt)){
      shpfil <-  st_read(paste0("./data/",fil)) 
      BackText <- rbind(shpfil,BackText)
    }
st_crs(BackLayers) <- st_crs(BackText) <- st_crs(BackPoints) <- mercproj
          
nchar( BackText$label[BackText$layer == "general spots"])
 
  
################################### merging some layers for ease of visualisation /mapping
## merge text layer general spots and georgaphical  
## keep general spots over geog in name list 
## merge admin seeds and railway stations 
## keep gen spots with one char
BackText <- BackText [-which(BackText$layer == "general spots" & nchar(BackText$label>1) ),]
 railwaystations <- which(BackText$layer == "Railway Station")
 BackText$layer [BackText$layer %in% c("Railway Station","geographical area", "general spots" )] <- "named places" 
 BackText$layer [BackText$layer =="Spot Height"] <- "local altitude"
 BackText$layer2 <- BackText$layer ## still has category for   for plotting
 BackText$layer2[railwaystations] <- "Railway Station"
 BackText$layer2[which( grepl('Loch', BackText$label) )] <- "Loch"
 BackText$layer2[which( grepl('LOCH', BackText$label) )] <- "Loch"
 BackText$layer2[ which( grepl( 'River' , BackText$label) )] <- "Loch"
 BackText$layer2[ which( grepl( 'FIRTH' , BackText$label) )] <- "Loch"
 BackText$layer2[ which( grepl( 'Bay' , BackText$label) )] <- "Loch"
 BackText$layer2[which( grepl( 'Reservoir' , BackText$label) )] <- "Loch"
 BackText$layer2[which( grepl( 'Forest' , BackText$label) )] <- "Forest"
 BackText$layer2[which( grepl( 'Forest' , BackText$label) )] <- "Forest"
 BackText$layer2[which( grepl( 'Wood' , BackText$label) )] <- "Forest"
 BackText$layer2[which( grepl( 'F O R E S T' , BackText$label) )] <- "Forest"
 BackText$layer2[which( grepl( 'S T R A T H C O N O N' , BackText$label) )] <- "Forest"
  
BackText$label[BackText$layer2 == "named places"] <- paste0(BackText$label[BackText$layer2 == "named places"],"\n")
# ggplot()+geom_sf_text(data=BackText, aes(label=label, colour=layer2))

BackPoints$layer [BackPoints$layer =="Spot Height"] <- "local altitude"
#Sgurr = peak
 
 
##### maps as images for side bar
##### note borders may have to be cropped in paint or something when using as images (lighter for the UI) (boo)
img1       <- png::readPNG(here::here(paste0("www/GBMap.png")))    # image = all gb with extent - generated in script above then cropped in paint and save in www file for images
imgScotMap <- png::readPNG(here::here(paste0("www/ScotMap.png")))  # image = extent area with rivers
#imghab    <- png::readPNG(here::here(paste0("www/HabMap.png")))    # image with all habs - maybe use as overlay on small / whole extent intro map?
HabMapLayers <- st_read("./data/HabMapLayers.shp")  # shpefile
st_crs(HabMapLayers) <- mercproj  
# grid::grid.raster(imghab)
imgsuit    <- png::readPNG(here::here(paste0("www/SuitLayer.png"))) ## image only suitable layer in turquoise
SuitLayer  <- HabMapLayers[HabMapLayers$layer=="suitable",]
 
 



# UKbound  <-  st_read("./data/UKbound.shp" )$geometry  
# st_crs(UKbound)  <- mercproj 
# UKbound  <-   UKbound %>%  st_set_precision(1) %>% st_make_valid()

scotbound  <-  UKbound  <- scotbound %>%  st_set_precision(1) %>% st_make_valid()
rivlines  <- rivlines %>%  st_set_precision(1) %>% st_make_valid()
BackLayers <- BackLayers %>%  st_set_precision(1) %>% st_make_valid()
rivlines2 <-  rivlines 
st_geometry(rivlines2) <- "geometry" 
rivlines2$layer<- "river"
HabMapLayers <- rbind(HabMapLayers,  rivlines2 )  
   
 
 
#################
#plot( scotcrop)
#  plot(bufp, add=T)
#  plot(scotbound, add=T)
#  plot(rivlines, add=T)
# plot( UKbound, add=T)
 
    



 

  
 
# p1 <-png::readPNG(here::here(paste0("data/p1.png"))) 
# p0 <-png::readPNG(here::here(paste0("data/p0.png"))) 
 
 p0 <- ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                   annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                  #  annotation_custom(grid::rasterGrob(p0, interpolate = F)) +
                    scale_y_continuous(expand=c(0,0))+  scale_x_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(),#text(colour="beige",family="sans", size=12, face='plain' ) ,
                     plot.background = element_rect(fill = "transparent", colour = NA),plot.margin=grid::unit(c(0,0,0,0), "mm"))
 
 
 # ggsave( here::here(paste0("data/p0.png")))
  
  
  
 p1 <- ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                     annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                   #   annotation_custom(grid::rasterGrob(p1, interpolate = F)) +
                  scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(),# element_text(colour="beige",family="sans", size=12, face='plain' ) ,
                     plot.background = element_rect(fill = "transparent", colour = NA),plot.margin=grid::unit(c(0,0,0,0), "mm"))
   
#  ggsave( here::here(paste0("data/p1.png")))

 
p1b <- # "show roads"
  ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                      scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
      geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
      geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                             geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
      geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                               geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1  )+
 coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
        scale_fill_manual(values=c(  "urban region"="purple", 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                        "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
       scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",
                                    "urban region"=0, 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                       "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
       scale_shape_manual(values=c( "urban region"=0, 
                                     "minor road"=0,"primary road"=0,"railway line"=0,
                                       "named places"=0, "road names"=0, "local altitude"=17)  )+  
      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,
                                    "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                     "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                     "minor road"=1,"primary road"=1.5,"railway line"=2,
                                      "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )   

p1c <- #"show named places"
  ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                      scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) + 
                               geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1  )+
                           coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
        scale_fill_manual(values=c(  "urban region"="purple", 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                        "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
       scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",
                                    "urban region"=0, 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                       "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
       scale_shape_manual(values=c( "urban region"=0, 
                                     "minor road"=0,"primary road"=0,"railway line"=0,
                                       "named places"=0, "road names"=0, "local altitude"=17)  )+  
      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,
                                    "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                     "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                     "minor road"=1,"primary road"=1.5,"railway line"=2,
                                      "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )   
                      
# c("show habitat suitable for beaver settlement", "show roads")                     
p2a <-       ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                     annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                    scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
      geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
      geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                             geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
      geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                              coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
      
                               geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1  )+
  scale_fill_manual(values=c(  "urban region"="purple", 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                        "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
       scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",
                                    "urban region"=0, 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                       "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
       scale_shape_manual(values=c( "urban region"=0, 
                                     "minor road"=0,"primary road"=0,"railway line"=0,
                                       "named places"=0, "road names"=0, "local altitude"=17)  )+  
      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,
                                    "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                     "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                     "minor road"=1,"primary road"=1.5,"railway line"=2,
                                      "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   

#"show habitat suitable for beaver settlement","show named places"                  
p2b <-       ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                     annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                    scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
                     geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1  )+
                           coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
        scale_fill_manual(values=c(  "urban region"="purple", 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                        "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
       scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",
                                    "urban region"=0, 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                       "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
       scale_shape_manual(values=c( "urban region"=0, 
                                     "minor road"=0,"primary road"=0,"railway line"=0,
                                       "named places"=0, "road names"=0, "local altitude"=17)  )+  
      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,
                                    "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                     "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                     "minor road"=1,"primary road"=1.5,"railway line"=2,
                                      "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   

p2c <-  ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +  
                    scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
      geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
      geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                             geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
      geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                               geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1  )+
                               geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1  )+
                           coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
        scale_fill_manual(values=c(  "urban region"="purple", 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                        "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
       scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",
                                    "urban region"=0, 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                       "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
       scale_shape_manual(values=c( "urban region"=0, 
                                     "minor road"=0,"primary road"=0,"railway line"=0,
                                       "named places"=0, "road names"=0, "local altitude"=17)  )+  
      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,
                                    "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                     "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                     "minor road"=1,"primary road"=1.5,"railway line"=2,
                                      "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   

                      
      
                    
p3 <-       ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                     annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                    scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
      geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
      geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                             geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
      geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                               geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1  )+
                               geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1  )+
                           coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
        scale_fill_manual(values=c(  "urban region"="purple", 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                        "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
       scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",
                                    "urban region"=0, 
                                     "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                       "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
       scale_shape_manual(values=c( "urban region"=0, 
                                     "minor road"=0,"primary road"=0,"railway line"=0,
                                       "named places"=0, "road names"=0, "local altitude"=17)  )+  
      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,
                                    "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                     "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                     "minor road"=1,"primary road"=1.5,"railway line"=2,
                                      "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   
                           
     
      
      
 
    
# p1 <-png::readPNG(here::here(paste0("data/p1.png"))) 
# p0 <-png::readPNG(here::here(paste0("data/p0.png"))) 
  
# pixels:  410x380
# # change format to expand whole width?
 
  



  
#