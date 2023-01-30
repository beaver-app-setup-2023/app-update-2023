cat("load region ggmaps\n")
 


#### pre-run maps in ggplot for visualisation in side bar
#### could do with those as images too for faster processing?
  
# just water  
 p0 <- ggplot() +  geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
                   annotation_custom(grid::rasterGrob(imgScotMap, interpolate = F)) +
                    scale_y_continuous(expand=c(0,0))+  scale_x_continuous(expand=c(0,0))+  
                    coord_sf(crs = mercproj,expand=F) +
                    annotation_scale(location = "bl") +
                    theme(legend.position = "none", axis.title.x = element_blank(),  axis.title.y = element_blank(), panel.border = element_rect(fill =NA),  axis.text = element_blank(), plot.background = element_rect(fill = "transparent", colour = NA),plot.margin=grid::unit(c(0,0,0,0), "mm"))
 # ggsave( here::here(paste0("data2/p0.png")))
  
  
# suit hab  
 p1 <- ggplot() +   geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
 p1b <-   ggplot() +  geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
 p1c <-    ggplot() +   geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
 p2a <-       ggplot() +  geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
 p2b <-       ggplot() +  geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
 p2c <-  ggplot() +  geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
 p3 <-       ggplot() +  geom_sf(data= coastline, col=3, fill=3, alpha=.2)+ 
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
   