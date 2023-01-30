## once mapsim_output0 generated, add sim layers
## may have to optimise to not run code for years other than that selected
## OR - al changes on output maps at once?
cat("map_output_yALL() for now ALL YEARS  -  ")
 

 
 
  

FUN_map_output_yALL <-function(start_terr, sim_terr3,sim_terr, sim_terr10, lay_release_pts ,mgmt.years,  mapsim_output0,
                               plot_probability_range_y3_d_,plot_probability_range_y5_d_,plot_probability_range_y10_d_,
                               chull_y3_all ,chull_y5_all, chull_y10_all,
                               show_chull_y3,show_chull_y5, show_chull_y10)  { 
 cat("\nfunction: map_output_yALL ALL YEARS for now.. \n")
          start_terr$family <-  as.factor(start_terr$family)
 
          ## y5
           sim_terr$dens <- as.numeric(as.character(sim_terr$dens)) 
           sim_terr      <- st_buffer(st_centroid(st_cast(sim_terr, "POLYGON") ) %>% group_by(Nruns) %>% summarize (geometry=st_union(geometry) ),40, byfeature=TRUE) # t11
     
          ## y3
          sim_terr3$dens <- as.numeric(as.character(sim_terr3$dens))#t0
          sim_terr3      <-  st_buffer(st_centroid(st_cast(sim_terr3, "POLYGON") ) %>% group_by(Nruns) %>% summarize (geometry=st_union(geometry) ),40, byfeature=TRUE) # t44
 
                  
           start_terr$family      <-  as.factor(start_terr$family) 
           lay_release_pts$family <- as.factor(lay_release_pts$family)
    
          
  
          if( mgmt.years == 10){ 
            print(sim_terr10)
          sim_terr10$dens <- as.numeric(as.character(sim_terr10$dens)) 
          sim_terr10 <-  st_buffer(st_centroid(st_cast(sim_terr10, "POLYGON") ) %>% group_by(Nruns) %>% summarize (geometry=st_union(geometry) ),40, byfeature=TRUE) # t11
          }
         
              ## to save in rvsim and reuse instead of rerun
              sim_terr3_bufpts  <- sim_terr3
              sim_terr_bufpts   <- sim_terr
              sim_terr10_bufpts <- sim_terr10
     
          
                   
              
   mapsim10  <- NULL
   if( mgmt.years == 10){
          mapsim10  <-mapsim_output0  +
                                       new_scale_fill() + new_scale_color()+  
                                       scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                       breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                       scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                       breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                              #         geom_sf(data=sim_terr10, aes(fill=Nruns,col=Nruns), alpha=1)+
                              #          geom_sf(data=chull_y10_all, fill=alpha("magenta",.1),col=1)+
                                       coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                       theme(axis.text=element_blank())
          }
   
          mapsim  <- mapsim_output0  +
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                        geom_sf(data=sim_terr, aes(fill=Nruns,col=Nruns), alpha=1)+
                                 #      geom_sf(data=chull_y5_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
              
          mapsim3  <- mapsim_output0  +
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="brown2"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="brown2"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                       geom_sf(data=sim_terr3, aes(fill=Nruns,col=Nruns), alpha=.8)+
                                   #    geom_sf(data=chull_y3_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
              
return(list(mapsim,mapsim3,mapsim10,
            mapsim_output0,mapsim_output0, # to put something in need updating indexing of returned output without that value now
            sim_terr3_bufpts ,sim_terr_bufpts , sim_terr10_bufpts  
       ))
} 


 
