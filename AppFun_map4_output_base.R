
# new_ext_sim = rvsim_out$new_ext_sim  nb value of new_ext_sim comes from <<
cat("mapsim_output_function()  -  ")  

############ FUN: generate final map  (background mapsim_output0)
############ 
FUN_mapsim_output_function <-function(region_box, new_ext_sim, rivlines, HabMapLayers, BackLayers, BackPoints, BackText){ ##  
    cat("\nfunction: mapsim_output_function\n")    
         ggplot() + 
                  geom_sf(data=st_crop(region_box, new_ext_sim), col=NA, fill="white")+
                  geom_sf(data=st_crop(region_box, new_ext_sim), col=NA, fill="steelblue", alpha=.9)+
                  geom_sf(data=st_crop(coastline, new_ext_sim) ,  fill="beige", col="steelblue", alpha=.9, size=1)   +
                  geom_sf(data= st_crop(rivlines, new_ext_sim), col=alpha("skyblue",.7),fill=alpha("steelblue",.8), size=2)+ 
                  geom_sf(data=st_crop(rivlines, new_ext_sim), col=alpha("steelblue",.9),fill=alpha("skyblue",.7), size=1)+ 
                  geom_sf(data=st_crop(HabMapLayers [ HabMapLayers$layer %in% c("suitable","dispersal") ,], new_ext_sim) , mapping = aes(fill= layer  ),col=NA , alpha=.3, size=0 )+
                  geom_sf(data=st_crop(HabMapLayers [HabMapLayers$layer %in% c("suitable" ) ,], new_ext_sim) , mapping = aes(  col= layer), fill=NA   )+
                  geom_sf(data=st_crop(BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , new_ext_sim), mapping = aes( col= layer), size=1,alpha=.8 )+
                  geom_sf(data=st_crop(BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,], new_ext_sim), col= "yellow", size=.8,alpha=.8 )+
                  geom_sf(data=st_crop(BackLayers [ BackLayers$layer %in% c("railway line")  ,], new_ext_sim), linetype="dotted", col= 1, size=3  )+
                  geom_sf(data=st_crop(BackPoints , new_ext_sim) , aes(col=layer,  shape=layer), size=3, stroke=1 )+
                  geom_sf_text(data=st_crop(BackText [  BackText$layer2 != "Railway Station",], new_ext_sim), aes(label=label, col=layer2  )  )+
                  geom_sf_label(data=st_crop(BackText [ BackText$layer2  == "Railway Station",], new_ext_sim), aes(label=label),fill=alpha(1,.4), col="beige")+
                                        
                 scale_fill_manual(values=c(   "suitable"=alpha("turquoise3",.3), "dispersal"=alpha("olivedrab4",.3), "unsuitable"=alpha("honeydew3",.4), "river"="steelblue",
                                               "a road"="brown", "b road"="orange", "admin line"="black",
                                               "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=alpha("lightseagreen",.7),
                                               "marsh"="green","woodland region"=alpha("olivedrab4",.7),"urban region"="purple","rivers line"=alpha("skyblue",.8),"lakes region"=alpha("steelblue",.9),
                                               "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                               "Loch"=0, "Forest"=0,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                 
                 scale_colour_manual(values=c( "suitable"="turquoise3", "dispersal"="olivedrab4", "unsuitable"=0, "river"="steelblue","a road"="brown", "b road"="orange", "admin line"="black",
                                               "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=0,
                                               "marsh"="green","woodland region"=0,"urban region"=0,"rivers line"=alpha("skyblue",.8),"lakes region"= 0,
                                               "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                               "Loch"="steelblue4","Forest"="green4",  "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                
                 scale_shape_manual(values=c( "TV or radio mast or tower"=13,"Wind powered generator"=8,"foreshor region"=0,
                                              "marsh"=8,"woodland region"=0,"urban region"=0,"rivers line"=0,"lakes region"= 0,
                                              "minor road"=0,"primary road"=0,"railway line"=0,
                                              "Loch"=0,"Forest"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
              
                 scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1,"admin seed"=5,
                                             "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                             "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                             "minor road"=1,"primary road"=1.5,"railway line"=2,
                                             "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )+  
                  annotation_scale(location = "bl")+
                  coord_sf(crs = mercproj, expand=F) +
                  theme(legend.position = "none", axis.title.x = element_blank(), 
                  axis.title.y = element_blank(),   
                  panel.border = element_rect(fill =NA) ,
                  plot.background = element_rect(fill = "transparent", colour = NA)) 
}

 