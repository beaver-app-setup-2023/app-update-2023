
################################################################################    
################      BEAVER TRANSLOCATION SIMULTATION APP      ################
################        for NatureScot Nov. 2022                ################
################                                                ################
################      GIS data prep - large shapefiles          ################
################   run before app code to generate layers       ################
################          (not sourced in app)                  ################
################################################################################  

### NOTE SomE/A lot OF THIS code is no longer used 
### some of the code features in script appSepExtents - updated
### some of it was for testing/at first





############# interactive graphical interface for visualizing data when out of RAM issues
#install.packages("profvis")
#library(profvis)
# profvis::profvis({shiny::runApp()}) 
# sounds useful, cant get it to run (crashes all things - old laptop issue could be?)



 
#################### original GIS data
#################### larger shapefiles were moved from \data to data_init\
#################### so they are not loaded within app (RAM limits etc)


 # UKbound  <-  st_transform(st_read("./data/GBR_adm/GBR_adm0.shp" )$geometry , mercproj)
 
 ## ireland shp from OSi Open Data Portal
 ## https://data-osi.opendata.arcgis.com/datasets/8dbdeabbeedc489a9db5117bbcf51d56_0/explore?location=53.307147%2C-8.082126%2C8.43
 #  IRLbound <-  st_read("./data/GBR_adm/Province_Boundaries_Ungeneralised_-_OSi_National_Administrative_Boundaries_-_2015.shp" )$geometry   #  ireland too
 #  t <- IRLbound %>%   st_make_valid()  %>%  st_union()
 #  IRLbound <-   st_transform(t, mercproj) 
      
   
   
 #    IRLbound_l <-  st_cast(IRLbound,"MULTILINESTRING")
 #    UKbound_l <-  st_cast(UKbound,"MULTILINESTRING")
 #    IRLbound_l <-st_simplify(IRLbound_l) 
 #    UKbound_l <- st_simplify(UKbound_l) 
 #    plot(UKbound_l )
     
 #    UKbound_l <-  st_union(UKbound_l,IRLbound_l)
 #    UKbound <- st_polygonize(UKbound_l) %>% st_collection_extract("POLYGON")
 #    UKbound <- st_union(UKbound)
 #    UKbound
 #    plot( UKbound )
    
 #    st_write(UKbound, here("data/UKbound.shp") , append=FALSE)


  
 UKbound  <-  st_read(paste0(here::here(),"/data_init/UKbound.shp" ))$geometry  
 st_crs(UKbound)  <- mercproj 
 
 scotbound  <- st_crop(UKbound, scotcrop)
 st_crs(scotbound)  <- mercproj
 st_write(scotbound, here("data/scotbound.shp") , append=FALSE)
 scotbound <-  st_read("./data/scotbound.shp" )$geometry  

 
 
 #rivlines  <- st_read("D:/Beavers 2022 App/data_init/WatercourseLink.shp")$geometry
 #rivlines  <- st_crop(st_transform(rivlines, mercproj), scotcrop)
 #st_write(rivlines, here("data/rivlines.shp") , append=FALSE)
 rivlines <-  st_read("./data/rivlines.shp" )  
 ## add lakes st_write(BackLayers[BackLayers$layer == "lakes region",],"./data/lakes.shp")
  





 

### hab layer - recoded that in appSepExtents 
### rerun with replot == TRUE 
  
 ggplot()+
     geom_sf(data=scotcrop,  fill="lightseagreen" ) +
     geom_sf(data=scotbound, fill="beige" )+ 
     geom_sf(data=SDMmap, aes(fill=layer), col=NA, show.legend=FALSE) +
     scale_fill_manual(values=c("suitable"=alpha("blue",.7), "dispersal"=alpha("olivedrab4",.6), "unsuitable"=alpha("yellow",.4)))+
     geom_sf(data=rivlines, col="lightseagreen", size=.9, alpha=.6)+
     geom_sf(data=rivlines, col="steelblue", size=.5)+
        coord_sf(crs = mercproj,expand=FALSE) + 
         theme_bw(base_size=8) +
         theme(   panel.background= element_rect(fill =alpha("lightseagreen",.4)), 
                  panel.ontop=FALSE, axis.text=element_blank(),axis.ticks =element_blank(),
                  plot.background = element_rect(fill = NA, colour = NA)) 
 #      ggsave( here::here(paste0("data/HabMap.png")), 
 #                  width = 4, height = 4, dpi = 200, units = "in", device='png')
 # st_write(SDMmap,  here::here(paste0("data/HabMapLayers.shp")))
   
st_point(c(240000,835000))
 ###?
 ###
## reduced window where ACM pointed - for modelling
 
 plot(scotcrop)
 plot(scotbound, add=T)
 
  map <- readGDAL(here("data/bvr_rc_100"))
 #window <- cbind(c(120000,450000  ),c( 590000  ,   1000000 )) # scotland

 window <- cbind(c(215000 ,  265000  ),c( 815000, 865000 ))
 

window <- cbind(window[c(1,1,2,2,1),1], window[c(1,2,2,1,1),2])
window <- matrix.to.SpatPolys(window, proj=map@proj4string)
map.r <- raster(extent(map))
res(map.r) <- map@grid@cellsize
map.r[] <- map@data$band1
proj4string(map.r) <- CRS(proj4string(window))

mapr.mask <- mask(map.r, window) 

hab2 <- as(mapr.mask, "SpatialPointsDataFrame")
hab2 <- hab2[!is.na(hab2@data$layer),]
gridded(hab2) <- TRUE
hab <- as(hab2, "SpatialGridDataFrame")



 
  
 UKbound  <-  st_read("./data/UKbound.shp" )$geometry  
 st_crs(UKbound)  <- mercproj 
 #gp0<- 
   ggplot()+  
   geom_sf(data=UKbound, fill= "burlywood3", col= "burlywood3")+ 
   geom_sf(data=scotcrop,  fill="yellow" , col=0) + 
        coord_sf(crs = mercproj ) + 
        theme_bw(base_size=8) +
         theme(   panel.background= element_rect (fill=alpha("lightseagreen",.4)), panel.grid.major = element_line(colour = "beige", size=.7),
                  panel.ontop=FALSE, axis.text=element_blank(),axis.ticks =element_blank(),
                  plot.background = element_rect(fill = NA, colour = NA)) 
       
 #       ggsave( here::here(paste0("data/GBMap.png")), 
#                 width = 10, height = 20, dpi = 400, units = "in", device='png')
        ggsave( here::here(paste0("www/GBMap_reduced200.png")), 
                   width = 10, height = 20, dpi = 200, units = "in", device='png')
  
    ggsave( here::here(paste0("www/GBMap_reduced400.png")), 
                   width = 10, height = 20, dpi = 400, units = "in", device='png')
  
   
   
   
   
# gp1<-
 ggplot()+
    scale_y_continuous(expand=c(0,0))+  scale_x_continuous(expand=c(0,0))+ 
    geom_sf(data=scotcrop,  fill="deepskyblue4" , col="steelblue") +
   
    geom_sf(data=scotbound, col=alpha("beige",.3) , fill=NA, size=3)+ 
    geom_sf(data=scotbound, col=alpha("beige",.4) , fill=NA, size=1)+ 
    geom_sf(data=scotbound, fill="beige" , col=NA)+ 
    geom_sf(data=rivlines, col="steelblue", size=.9, alpha=.5 )+
    geom_sf(data=rivlines, col="steelblue", size=.5)+
    geom_sf(data=lakes, col=NA, fill="steelblue")+
        coord_sf(crs = mercproj,expand=FALSE) + 
        theme_bw(base_size=8) +
         theme(   panel.background= element_rect(fill =alpha("lightseagreen",.4)), 
                  panel.ontop=FALSE, axis.text=element_blank(),axis.ticks =element_blank(),
                  plot.background = element_rect(fill = NA, colour = NA)) 
        ggsave( here::here(paste0("www2/ScotMap.png")), 
               dpi = 400, units = "in", device='png')
 
 
 #    width = 4, height = 4,
 
 ggplot()+
  geom_sf(data=SuitLayer, fill=alpha("deeppink",.5),  col=alpha("deeppink",.5), size=1)+
  coord_sf(crs = mercproj,expand=FALSE) + 
        theme_bw(base_size=0) +
         theme(   panel.background= element_blank() ,
                  panel.ontop=FALSE,
                  plot.background = element_blank())   


 ggplot()+
    geom_sf(data=scotcrop,  fill=NA,col=NA ) +
    geom_sf(data=scotbound, fill=NA,col=NA)+  
    geom_sf(data=SuitLayer, fill=alpha("turquoise",.9),  col=alpha("turquoise",.9), size=1)+
     coord_sf(crs = mercproj,expand=FALSE) + 
        theme_bw(base_size=8) +
          theme_bw(base_size=0) +
         theme(   panel.background= element_blank() ,axis.ticks=element_blank(),axis.text=element_blank(),
                  panel.ontop=FALSE,
                   plot.background = element_rect(fill = NA, colour = NA))  
  
  # ggsave( here::here(paste0("data/SuitLayer.png")), 
  #                      width = 4, height = 4, dpi = 400, units = "in", device='png')
#imgsuit   <- png::readPNG(here::here(paste0("data/SuitLayer.png"))) ## maybe use as overlay on small / whole extent intro map?
# grid::grid.raster(imgsuit)
      
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

 #### recode HABITAT MAP in sf - for plotting
  SDMmap_r <- read_stars( "./data/bvr_rc_100/w001001.adf") 
  tempras <- st_warp(SDMmap_r, crs=mercproj) 
 
  tempras<- st_crop(tempras, scotcrop)
  tempras<- st_as_sf(tempras)
  names(tempras) <- c("SDMvalue", "geometry")
  tempras$layer <- "unsuitable"
  tempras$layer[tempras$SDMvalue==2] <- "suitable"
  tempras$layer[tempras$SDMvalue==1] <- "dispersal"
  SDMmap <- subset(tempras, select= c(layer))
  names(SDMmap) <- c("layer", "geometry")
  SDMmap <- SDMmap %>% group_by(layer) %>% summarize()
   
  
  
         
temp_bbox00 <-  st_bbox(SDMmap)    ## 3km buffer around init point location to stat with? is that enough
st_crs(temp_bbox0) <- mercproj  
 window 
map2.r <- raster(extent(map))
res(map2.r) <- map@grid@cellsize
map2.r[] <- map@data$band1
proj4string(map.r) <- CRS(proj4string(window))

  
r_masked = mask(map2.r, intcatch_bound2 , inverse = T )
 `
plot(r_masked, add = T)
plot(intcatch_bound2$geometry , col=2,add=T)  
  
  `
  
  
  ### temp solution to partition the catchments for now (later do several polygons distinct with option to release at different times)
  ### make boundary unsuitable nd let run random
  