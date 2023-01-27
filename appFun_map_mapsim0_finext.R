cat("mapsim0_finext()  -  ")

#######################################################################################################
FUN_mapsim0_finext <- function(start_terr, mapsim_output0 ){ ## only start terr but on final extent
    cat("\nfunction: mapsim0_finext\n")      
          start_terr$family <-  as.factor(start_terr$family)  
          
          mapsim0_finext <-   mapsim_output0 + geom_sf(data=start_terr ,  fill="magenta" ,  col=1 )+  
                                                 coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                                 theme(axis.text=element_blank())
   return(mapsim0_finext)    
}






 
   