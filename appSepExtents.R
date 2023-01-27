
################################################################################    
################      BEAVER TRANSLOCATION SIMULTATION APP      ################
################        for NatureScot Nov. 2022                ################
################                                                ################
################    data prep & loading & other material        ################
################         script sourced #2/2                    ################
################################################################################  


 
 





##### deploy stuff
# setwd("D:/Beavers 2022 App/Beavers 2022 App")  ~ here::here()
# library(rsconnect)  
# go on https://www.shinyapps.io/admin/#/profile
# rsconnect::setAccountInfo(name='naturalandenvironmentalscience', token='03E2DBCC6228F125D9432018F0E237B3', secret='eMpa7Wd56xVkEsMJkch1tjamn3wcwJ6tAc1cO3T1')
# rsconnect::deployApp('D:/Beavers 2022 App/Beavers 2022 App',     appName = "NatureScotBeaverApp",     account ='naturalandenvironmentalscience') 

# dont include library(shiny) or refs to local files dir() in app script obvs
# for diagnostics:
# rsconnect::showLogs() 
##  startup timeout = loading lots of data outside shinyServer() function or in  global.R  if that operation takes longer then 60 seconds
##  can adjust the startup timeout in the advance settings page for your application in the shinyapps.io dashboard. 
##  have to adjust Size to xxxlarge on  https://www.shinyapps.io/admin/#/application/8022516  
 




## proj in use
mercproj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"



#list.files("/.")

#### GIS input - in data2
    
 # region_box0   <- as(as(extent(120000, 450000 ,590000, 1000000), "SpatialPolygons"), "sf") ## all scotland
     
 


  
 # UKbound  <-  st_read("D:/Beavers 2022 App/data_init/UKbound.shp" )$geometry  
 # st_crs(UKbound)  <- mercproj 
 
 # scotbound  <- st_crop(UKbound, region_box) 
 # st_write(scotbound, here("data2/scotbound.shp") , append=FALSE)
 
  
######################### note  
######################### data_init contains large shapefiles - not loaded with app  
######################### but used to generate the GIS layers stored in data2
 
 # rivlines  <- st_read("D:/Beavers 2022 App/data_init/WatercourseLink.shp")$geometry
 # rivlines  <- st_crop(st_transform(rivlines, mercproj), region_box)
 # st_write(rivlines, here("data2/rivlines.shp") , append=FALSE)
 # rivs + lakes
 # cropped, not saved as such because geom collection
  rivlines <-  st_read("./data2/rivlines.shp", quiet=TRUE)
  st_crs(rivlines)  <- mercproj
 ## add lakes st_write(BackLayers[BackLayers$layer == "lakes region",],"./data2/lakes.shp")
  
 

 scotbound <-  st_read("./data2/scotbound.shp", quiet=TRUE) $geometry  
 st_crs(scotbound)  <- mercproj
 
     
   
## beaver habitat suitability map layer (scotland) 
#window <- matrix.to.SpatPolys(window, proj=map@proj4string)

 cat("beaver habitat suitability map  -  ")
 map <- readGDAL(here("data/bvr_rc_100"))
 map.r <- raster(extent(map))
 res(map.r) <- map@grid@cellsize
 map.r[] <- map@data$band1
 proj4string(map.r) <-  CRS(proj4string(map)) 

 ### script uses map.r - replace NA by 0 ? see how it goes with disp/wander functions
 #map.r[which(is.na(map.r))] <- 0       
  

 
 

  
############ compile OS layers for general maps (roads etc)  
############ note that this was run using data not included in the project, so that they are not loaded with the app
############ so init shapefiles are in D:/Beavers 2022 App
############ then the cropped shapefiles to be included with the app code are stored in ./data/  
 rerunOSlayers = FALSE
 
 if(rerunOSlayers == TRUE) {
 
  ### crop each layer to assemble into  BackLayers on start
  listfil <- list.files("D:/Beavers 2022 App/strtgi_essh_gb/data/",pattern="shp$") ## unload all layers from strategy file and crop to extent
  listfil
       
  ## polygons and lines  - ALL
  for ( filnum in seq(1,length(listfil)) ) {
    fil <- listfil[filnum]
    shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
      if(nrow(shpfil)!=0 ) {
        if (unique(st_geometry_type(shpfil)) [1] != "POINT") {
        shpfil <- st_transform(st_union(shpfil), mercproj) 
        print("layer read")
        shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
        if (filnum==21) {shpfil <- st_as_sf(st_union(st_cast(shpfil) [ st_geometry_type(st_cast(shpfil)) == "LINESTRING",]) )}
        print("layer cropped")
        if( nrow(shpfil)!=0) {
        print("layer ok")
        filname <- str_replace(fil, ".shp","")
        shpfil$layer <- str_replace(filname, "_"," ")
        st_write(shpfil, paste0("./data2/BackLayer",filnum,".shp"), append=FALSE ) }
        }
 } } 
     

  ## text and points - which layers
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
         shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
         shpfil <- subset(st_transform( shpfil , mercproj), select=ADMIN_NAME)
         filname <- str_replace(fil, ".shp","")
         shpfil$layer <- str_replace(filname, "_"," ")
         names(shpfil) [1] <- "label"
         st_write(shpfil, paste0("./data2/BackLayer_txt",filnum,".shp"), append=FALSE )  # "admin seed"
         
  ## points and text
  filnum=10
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
          shpfil <- st_transform( shpfil , mercproj) 
          shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil[shpfil$LEGEND == "Graphic Text, Geographical Area",],select=NAME)
          shpfil$layer <- "general spots"
          names(shpfil) [1] <- "label"
          st_write(shpfil, paste0("./data2/BackLayer_txt",filnum,".shp"), append=FALSE ) # "general spots"
          # ggplot()+geom_sf_text(data=shpfil,aes( label=label) )
  
  ## points and text
  filnum=13
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
          shpfil <- st_transform( shpfil , mercproj) 
          shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil ,select=NAME)
          shpfil$layer <- "geographical area"
          names(shpfil) [1] <- "label"
          #shpfil$label <- paste0(shpfil$label,"\n")
          st_write(shpfil, paste0("./data2/BackLayer_txt",filnum,".shp"), append=FALSE ) #"geographical area"
          # ggplot()+geom_sf_text(data=shpfil,aes( label=label) )

  ## points and text
  filnum=14
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
          shpfil <- st_transform( shpfil , mercproj) 
          shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
          nrow(shpfil)
          shpfil
          shpfil <-  st_transform( shpfil , mercproj)
          shpfil <-  subset(shpfil ,select=LEGEND)
          names(shpfil)
          tvshapfil <- st_as_sf(layer="TV or radio mast or tower", st_union( shpfil$geometry[shpfil$LEGEND == "Television, Radio Mast or Tower"]))
          marshshapfil <- st_as_sf( st_union(shpfil[shpfil$LEGEND == "Marsh",]),layer="marsh")
          windgenshapfil <- st_as_sf(layer ="Wind powered generator",st_union(shpfil[shpfil$LEGEND == "Wind Powered Generator",] ))
          shpfil <- rbind(tvshapfil, marshshapfil,windgenshapfil)
          st_write(shpfil, paste0("./data2/BackLayer_pt",filnum,".shp"), append=FALSE ) 
          # ggplot()+geom_sf (data=shpfil,aes(col=layer, shape=layer) )

  ## points and text
  filnum=20
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
            shpfil <- st_transform( shpfil , mercproj) 
            shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
            nrow(shpfil)
            table(shpfil$LEGEND)
            shpfil <-  st_transform( shpfil , mercproj)
            shpfil <-  subset(shpfil ,select=c(NAME,LEGEND) )   # layer =Railway Station
            names(shpfil)
            shpfil <- st_as_sf(layer="Railway Station", label= shpfil$NAME[shpfil$LEGEND == "Railway Station"], shpfil$geometry[shpfil$LEGEND == "Railway Station"]) 
            st_write(shpfil, paste0("./data2/BackLayer_txt",filnum,".shp"), append=FALSE ) 
            # ggplot()+geom_sf_text (data=shpfil,aes(col=layer, shape=layer, label=label) )
  
  ## points and text
  filnum=24
  fil <- listfil[filnum]
  shpfil <- st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) 
            shpfil <- st_transform( shpfil , mercproj) 
            shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
            shpfil
            shpfil <-  st_transform( shpfil , mercproj) 
            names(shpfil)[1] <- "height"
            shpfil <- st_as_sf(layer="Spot Height", label= paste0("\n",shpfil$METRIC), shpfil$geometry ) # heights
            st_write(shpfil, paste0("./data2/BackLayer_txt",filnum,".shp"), append=FALSE ) 
            p1 <-  ggplot()+geom_sf_text (data=shpfil,aes(col=layer, shape=layer, label=label) )
            shpfil <- st_as_sf(layer ="Spot Height",st_union(shpfil )) 
            st_write(shpfil, paste0("./data2/BackLayer_pt",filnum,".shp"), append=FALSE ) 
            p1+geom_sf  (data=shpfil,aes(col=layer, shape=layer ) )
       
  ## points and text
  filnum=27
  fil <- listfil[filnum]
  shpfil <-  st_read(paste0("D:/Beavers 2022 App/strtgi_essh_gb/data/",fil)) # road names
             shpfil <- st_transform( shpfil , mercproj) 
             shpfil <- st_as_sf( st_crop(shpfil,region_box)) 
             shpfil
             shpfil <-  st_transform( shpfil , mercproj) 
             shpfil <- shpfil[shpfil$LEGEND %in% c("Graphic Text, A Road Number","Graphic Text, B Road Number","Graphic Text, Primary Route Number"),]
             shpfil <- st_as_sf(layer="road names", label= shpfil$NAME, shpfil$geometry ) 
             st_write(shpfil, paste0("./data2/BackLayer_txt",filnum,".shp"), append=FALSE ) 
             #ggplot()+geom_sf_text (data=shpfil,aes(col=layer, shape=layer, label=label) )
 

 } # rerun this with chosen layers? (no feedback as to which layers are relevant as of Jan2023)
         
         
##### assemble OS layers (prev. processed with code above)  
 listfil <- list.files("./data2/",pattern= "BackLayer.*\\.shp") # all
 #listfil  
 
 listfil_txt <- list.files("./data2/",pattern= "BackLayer_txt.*\\.shp") # all txt
 listfil_pt <- list.files("./data2/",pattern= "BackLayer_pt.*\\.shp") # all pt
 listfil_pol <- listfil [!listfil %in% listfil_txt &  !listfil %in% listfil_pt] # all other
  
 BackLayers <- BackText <- BackPoints <- NULL ## assemble, cropped
 cat("shapefiles - polygons  ")    
 for ( fil in c(listfil_pol )){
   cat(".")
      shpfil <-  st_read(paste0("./data2/",fil), quiet=TRUE) 
      BackLayers <- rbind(shpfil,BackLayers)
 }  
 cat("\nshapefiles - points  ")
 for ( fil in c(listfil_pt )){
   cat(".")
   shpfil <-  st_read(paste0("./data2/",fil), quiet=TRUE) 
      BackPoints  <- rbind(shpfil,BackPoints )
 } 
 cat("\nshapefiles - text  ")
 for ( fil in c(listfil_txt)){
   cat(".")
   shpfil <-  st_read(paste0("./data2/",fil), quiet=TRUE) 
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
#Sgurr = peak!

 
 
# st_write(BackLayers[BackLayers$layer == "lakes region",],"./data2/lakes.shp")
  

###################### rivs + lakes
###################### n.b. cropped beforehand but not saved together because cant save geom collection sf type 
    rivlines <-  st_read("./data2/rivlines.shp", quiet=TRUE)   
    lakes <- st_as_sf(st_union(st_read("./data2/lakes.shp", quiet=TRUE)$geometry ))
    st_crs(lakes) <- st_crs(rivlines) <- mercproj
    rivlines  <- st_as_sf(st_union(rivlines))
    rivlines  <- rbind( rivlines , lakes)

###################### prec stuff
    scotbound  <-  UKbound  <- scotbound %>%  st_set_precision(1) %>% st_make_valid()
    rivlines  <- rivlines %>%  st_set_precision(1) %>% st_make_valid()
    BackLayers <- BackLayers %>%  st_set_precision(1) %>% st_make_valid()
    rivlines2 <-  rivlines 
    st_geometry(rivlines2) <- "geometry" 
    rivlines2$layer<- "river"
 


 
################ to (re)generate maps that cover whole Geln Affric - Beauly extent 
################ shapefiles stored in data2
################ images stored in www2
st_crs(region_box) <- mercproj
replot <- FALSE

if(replot == TRUE) {
 
  ####  ScotMap image 
  ggplot()+
    scale_y_continuous(expand=c(0,0))+  scale_x_continuous(expand=c(0,0))+ 
    geom_sf(data=region_box,  fill="lightseagreen" ) +
    geom_sf(data=scotbound, fill="beige" )+ 
    geom_sf(data=rivlines, col="lightseagreen", size=.9, alpha=.5 )+
    geom_sf(data=rivlines, col="steelblue", size=.5)+
    geom_sf(data=lakes, col="lightseagreen", fill="steelblue", size=.4)+
    coord_sf(crs = mercproj,expand=FALSE) + 
    theme_bw(base_size=8) +
    theme( panel.background= element_rect(fill =alpha("lightseagreen",.4)), panel.ontop=FALSE, axis.text=element_blank(),axis.ticks =element_blank(), plot.background = element_rect(fill = NA, colour = NA)) 
    # ggsave( here::here(paste0("www2/ScotMap.png")), width = 4, height = 2.7, dpi = 200, units = "in", device='png')
 

  #### habitat map into sf - for plotting
  SDMmap_r <- read_stars( "./data/bvr_rc_100/w001001.adf") 
  tempras <- st_warp(SDMmap_r, crs=mercproj) 
  tempras<- st_crop(tempras, region_box)
  tempras<- st_as_sf(tempras)
  names(tempras) <- c("SDMvalue", "geometry")
  tempras$layer <- "unsuitable"
  tempras$layer[tempras$SDMvalue==2] <- "suitable"
  tempras$layer[tempras$SDMvalue==1] <- "dispersal"
  SDMmap <- subset(tempras, select= c(layer))
  names(SDMmap) <- c("layer", "geometry")
  SDMmap <- SDMmap %>% group_by(layer) %>% summarize()
  st_write(SDMmap,  here::here(paste0("data2/HabMapLayers.shp")), append=FALSE)

  #### SuitLayer image 
  SuitLayer  <- SDMmap[SDMmap$layer=="suitable",]
  ggplot()+
     geom_sf(data=region_box,  fill=NA,col=NA ) +
     geom_sf(data=scotbound, fill=NA,col=NA)+  
     geom_sf(data=SuitLayer, shape=21,fill=alpha("turquoise",.6),  col=alpha("turquoise",.6), size=.2)+
     coord_sf(crs = mercproj,expand=FALSE) + 
     theme_bw(base_size=8) +
     theme_bw(base_size=0) + # eh?
     theme(panel.background= element_blank() ,axis.ticks=element_blank(),axis.text=element_blank(), panel.ontop=FALSE,panel.grid=element_blank(), plot.background = element_rect(fill = NA, colour = NA))  
   ggsave( here::here(paste0("www2/SuitLayer.png")), 
                         width = 674, height = 508, dpi = 400, units = "px", device='png') # adjust size based on prev image after cropping by hand -cant be the best way to do this
 
}
 

##### load maps as images 
##### for side bar
##### note borders may have to be cropped in paint or something when using as images (lighter for the UI) (boo)
 img1   <- png::readPNG(here::here(paste0("www/GBMap.png")))            # image = all gb with extent - generated in script above then cropped in paint and save in www file for images
 imgScotMap    <- png::readPNG(here::here(paste0("www2/ScotMap.png")))  # image = extent area with rivers - crop by hand!
 HabMapLayers <- st_read("./data2/HabMapLayers.shp", quiet=TRUE)  # shapefile
 st_crs(HabMapLayers) <- mercproj
 HabMapLayers <- rbind(HabMapLayers,  rivlines2 )  
 imgsuit   <- png::readPNG(here::here(paste0("www2/SuitLayer.png")))    # image = only suitable hab layer in turquoise
 #SuitLayer  <- HabMapLayers[HabMapLayers$layer=="suitable",] # now using as img
 # vis check:
 # grid::grid.raster(imgScotMap)  
 
  
 
 

#### pre-run maps in ggplot for visualisation in side bar
#### could do with those as images too for faster processing?
  
# just water  
 p0 <- ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                   annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                    scale_y_continuous(expand=c(0,0))+  scale_x_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(),  axis.title.y = element_blank(), panel.border = element_rect(fill =NA),  axis.text = element_blank(), plot.background = element_rect(fill = "transparent", colour = NA),plot.margin=grid::unit(c(0,0,0,0), "mm"))
 # ggsave( here::here(paste0("data2/p0.png")))
  
  
# suit hab  
 p1 <- ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                    annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                    annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+   
                    scale_y_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(), 
                    axis.title.y = element_blank(),   
                    panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                    plot.background = element_rect(fill = "transparent", colour = NA),plot.margin=grid::unit(c(0,0,0,0), "mm"))
#  ggsave( here::here(paste0("data2/p1.png")))
 
 
#  roads 
 p1b <-   ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
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
                      geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1 ,check_overlap = TRUE  )+
                      coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
                      scale_fill_manual(values=c(  "urban region"="purple",  "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" , "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                      scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",  "urban region"=0,  "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black", "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                      scale_shape_manual(values=c( "urban region"=0,   "minor road"=0,"primary road"=0,"railway line"=0, "named places"=0, "road names"=0, "local altitude"=17)  )+  
                      scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,  "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,  "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,  "minor road"=1,"primary road"=1.5,"railway line"=2, "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )   


BackText_randomHalf <- BackText [  BackText$layer2  %in% c("Loch", "named places"),]
BackText_randomHalf <- BackText_randomHalf[sample(x=seq(1:nrow(BackText_randomHalf)),size=round(nrow(BackText_randomHalf)/2)),]


# named places  
 p1c <-    ggplot() +   geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                        annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                        scale_y_continuous(expand=c(0,0))+  
                        coord_sf(crs = mercproj,expand=F) +
                        annotation_scale(location = "bl") +
                        theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1),  axis.title.y = element_blank(),  panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                        plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) + 
                        geom_sf_text  (data= BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1,check_overlap = TRUE  )+
                        coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
                        scale_fill_manual(values=c(  "urban region"="purple",   "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                        scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black","urban region"=0,   "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",   "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                        scale_shape_manual(values=c( "urban region"=0,   "minor road"=0,"primary road"=0,"railway line"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
                        scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,  "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,  "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,  "minor road"=1,"primary road"=1.5,"railway line"=2,  "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )   

# roads + road names               
 p2a <-       ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                          annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                          annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                          scale_y_continuous(expand=c(0,0))+  
                          theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1), axis.title.y = element_blank(),  panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                                plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
                          geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
                          geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                          geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
                          geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                          coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
                          geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1,check_overlap = TRUE  )+
                          scale_fill_manual(values=c(  "urban region"="purple",   "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" , "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                          scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black",  "urban region"=0,  "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black", "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                          scale_shape_manual(values=c( "urban region"=0,   "minor road"=0,"primary road"=0,"railway line"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
                          scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1, "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0, "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0, "minor road"=1,"primary road"=1.5,"railway line"=2,  "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   
                   

# habitat suitable for beaver settlement + show named places                   
 p2b <-       ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                          annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                          annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                          scale_y_continuous(expand=c(0,0))+  
                          theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1), axis.title.y = element_blank(),   
                                panel.border = element_rect(fill =NA),  axis.text = element_blank(), plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
                          geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1 ,check_overlap = TRUE )+
                          coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
                          scale_fill_manual(values=c(  "urban region"="purple",  "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                          scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black", "urban region"=0, "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black", "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                          scale_shape_manual(values=c( "urban region"=0,  "minor road"=0,"primary road"=0,"railway line"=0, "named places"=0, "road names"=0, "local altitude"=17)  )+  
                          scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1, "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0, "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0, "minor road"=1,"primary road"=1.5,"railway line"=2, "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   

# road and names
 p2c <-  ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                     annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +  
                     scale_y_continuous(expand=c(0,0))+  
                     theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1), axis.title.y = element_blank(),   
                           panel.border = element_rect(fill =NA),  axis.text = element_blank(),   plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
                     geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
                     geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                     geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
                     geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                     geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1,check_overlap = TRUE  )+
                     geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1,check_overlap = TRUE  )+
                     coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
                     scale_fill_manual(values=c(  "urban region"="purple",  "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" , "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                     scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black", "urban region"=0, "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black", "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                     scale_shape_manual(values=c( "urban region"=0,   "minor road"=0,"primary road"=0,"railway line"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
                     scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,  "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0, "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,  "minor road"=1,"primary road"=1.5,"railway line"=2, "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   

                      
      
# alluvitinnit                    
 p3 <-       ggplot() +  geom_sf(data= scotbound, col=3, fill=3, alpha=.2)+ 
                         annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                         annotation_custom(grid::rasterGrob(imgsuit, interpolate = F))+  
                         scale_y_continuous(expand=c(0,0))+   
                         theme(legend.position = "none", axis.title.x = element_blank(), panel.grid.major=element_line(colour=1), axis.title.y = element_blank(),   
                               panel.border = element_rect(fill =NA),  axis.text = element_blank(),  plot.background = element_rect(fill = "transparent", colour =NA),plot.margin=grid::unit(c(0,0,0,0), "mm")) +
                         geom_sf(data= BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1.5,alpha=.8 )+
                         geom_sf(data= BackLayers [BackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 ) +
                         geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,],   col= 1, size=.8 )+
                         geom_sf(data=BackLayers [BackLayers$layer %in% c("railway line") ,], linetype="dotted",  col= 1, size=2 )+
                         geom_sf_text (data=BackText [  BackText$layer  == "road names",], aes(label=label), col=1,check_overlap = TRUE  )+
                         geom_sf_text (data=BackText [  BackText$layer2  %in% c("Loch", "named places"),], aes(label=label), col=1 ,check_overlap = TRUE )+
                         coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") +
                         scale_fill_manual(values=c(  "urban region"="purple",   "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                         scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="red", "b road"="orange", "admin line"="black", "urban region"=0,   "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",  "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                         scale_shape_manual(values=c( "urban region"=0,   "minor road"=0,"primary road"=0,"railway line"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
                         scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,  "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,  "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,  "minor road"=1,"primary road"=1.5,"railway line"=2,  "Loch"=4.5,"Forest"=4, "named places"=2, "road names"=3, "local altitude"=3)  )   
                           
### load as images?  
# p1 <-png::readPNG(here::here(paste0("data2/p1.png"))) 
# p1b <-png::readPNG(here::here(paste0("data2/p1b.png"))) 
# p1c <-png::readPNG(here::here(paste0("data2/p1c.png"))) 
# p0 <-png::readPNG(here::here(paste0("data2/p0.png"))) 
# p2a <-png::readPNG(here::here(paste0("data2/p2a.png"))) 
# p2b <-png::readPNG(here::here(paste0("data2/p2b.png")))    
# p2c <-png::readPNG(here::here(paste0("data2/p2c.png")))    
# p3 <-png::readPNG(here::here(paste0("data2/p3.png")))    
# pixels:  410x380 - change format to expand whole width?
      
#which(BackText$label ==  "Glen Affric\n")  
#which(BackText$label == "River Beauly")  
#which(BackText$label == "BEAULY FIRTH") 
#BackText[243,] # rerun with coords ~ 258952 847767

labs_df <- data.frame(x=c(218593,258952),y=c(822545,847767),labs=c( "Glen Affric" ,"Beauly Firth")  )
 
  
   


################# adding latest GIS layers from NatureScot  
################# catchments, beaver dam capacity
################# stored in data2

recompute_catchdata <- FALSE
  if(recompute_catchdata==TRUE) {
    
################# catchment maps - precut to extent beauly affric
    intcatch <- st_sf(geometry=  st_read( paste0("./data2/intcatch.shp") , quiet=TRUE)$geometry   , layer="water body intercatchments") 
    catch <- st_sf(geometry=    st_read( paste0("./data2/catch.shp")) $geometry , layer="main catchments")
    catch <- st_transform(catch, mercproj)  
    intcatch<- st_transform(intcatch, mercproj) 

################# beaver dam capacity map - many pars in original shp, used categorical var because I dont know what they want
    damcap_cat <- st_read(  paste0("./data2/damcap_cat.shp")  , quiet=TRUE)
    damcap_hi<- st_sf(geometry= st_union(damcap_cat[damcap_cat$BDC_cat %in% c( "Pervasive",  "Frequent"),]), layer="frequent to pervasive")
    damcap_occ<- st_sf(geometry= st_union(damcap_cat[damcap_cat$BDC_cat %in% c( "Occasional"),]), layer="occasional")
  # damcap_lo<- st_sf(geometry= st_union(damcap_cat[damcap_cat$BDC_cat %in% c( "Rare"  ,     "None"),]), layer="none to rare")

    damcap_hi <- st_transform(damcap_hi, mercproj)   
    damcap_occ <- st_transform(damcap_occ, mercproj)   
  # damcap_lo <- st_transform(damcap_lo, mercproj)   
    extra_HabLayers_catch   <- rbind(catch,intcatch )
          
  # extra_HabLayers_damcap_occtohi <- rbind( damcap_hi,damcap_occ)#,damcap_lo)
  # ggplot()+geom_sf(data=extra_HabLayers_damcap, aes(col=layer))
         
  #  st_write(damcap_lo, here::here("data2/extra_HabLayers_damcap_lo.shp") , append=FALSE) ## nope- omitted rare/none because it is huge and mostly useless 
  st_write(damcap_occ, here::here("data2/extra_HabLayers_damcap_occ.shp") , append=FALSE)  
  st_write(damcap_hi, here::here("data2/extra_HabLayers_damcap_hi.shp") , append=FALSE)  
  st_write(extra_HabLayers_catch, here::here("data2/extra_HabLayers_catch.shp") , append=FALSE)
  extra_HabLayers <- rbind(st_read( here::here("data2/extra_HabLayers_damcap_hi.shp")),
                           st_read( here::here("data2/extra_HabLayers_damcap_occ.shp")),
                           st_read(here::here("data2/extra_HabLayers_catch.shp")))
  extra_HabLayers   <- st_transform( extra_HabLayers,mercproj)
  extra_HabLayers_l <-   st_cast(extra_HabLayers,"MULTILINESTRING")
  extra_HabLayers_l <- extra_HabLayers_l %>% group_by(layer) %>% summarise()
  st_write(extra_HabLayers_l,  here::here("data2/extra_HabLayers_l.shp"), append=FALSE)

}

#### load layers (prepared with code above) 
  intcatch <- st_sf(geometry=  st_read( paste0("./data2/intcatch.shp") , quiet=TRUE)$geometry, layer="water body intercatchments") 
  catch    <- st_sf(geometry=  st_read( paste0("./data2/catch.shp"), quiet=TRUE)$geometry,     layer="main catchments")
  catch    <- st_transform(catch, mercproj)  
  intcatch <- st_transform(intcatch, mercproj)  
  extra_HabLayers_l <- st_read( here::here("data2/extra_HabLayers_l.shp") , quiet=TRUE)
  st_crs(extra_HabLayers_l ) <- mercproj
   
 