cat("mapzoom_function()  -  ")
 

  

#  ext_region_box=ldat$ext_region_box
#  ext_UKbound=ldat$ext_UKbound
#  ext_area_sel=ldat$ext_area_sel
#  rivers=ldat$riv 
  
  
  ############################################################################################ FUN - mapzoom_function - generate map for main tab candidate release site 
mapzoom_function <-function(ext_region_box, ext_UKbound, ext_area_sel, rivers,nonsel ){
  cat("\nfunction: mapzoom_function\n")
       
    
 ggplot() + 
          geom_sf(data =  ext_region_box, col=NA, fill="white")+
          geom_sf(data =  ext_region_box, col=NA, fill="steelblue", alpha=.9)+
          geom_sf(data =  ext_UKbound ,  fill="beige", col="steelblue", alpha=.9, size=1)   +
          geom_sf(data=   rivers, col=alpha("skyblue",.7),fill=alpha("steelblue",.8), size=2)+ 
          geom_sf(data=   rivers, col=alpha("steelblue",.9),fill=alpha("skyblue",.7), size=1)+ 
          geom_sf(data=   nonsel,col=NA,fill=alpha("grey",.7))+
          scale_fill_manual(values=c( "suitable"=alpha("turquoise3",.3), "dispersal"=alpha("olivedrab4",.3), "unsuitable"=alpha("honeydew3",.4), "river"="steelblue",
                                      "a road"="brown", "b road"="orange", "admin line"="black",
                                      "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=alpha("lightseagreen",.7),
                                      "marsh"="green","woodland region"=alpha("olivedrab4",.7),"urban region"="purple","rivers line"=alpha("skyblue",.8),"lakes region"=alpha("steelblue",.9),
                                      "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                      "Loch"=0, "Forest"=0,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
         
          scale_colour_manual(values=c(  "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="brown", "b road"="orange", "admin line"="black",
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
              axis.title.y = element_blank(),   axis.text = element_blank(),   
              panel.border = element_rect(fill =NA) ,
              plot.background = element_rect(fill = "transparent", colour = NA)) 
}     
  
   