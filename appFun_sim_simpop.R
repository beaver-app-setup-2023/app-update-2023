cat("sim_pop()  -  ")

################################################################################################################ FUN: simpop_function - simulate pop growth - 
simpop_function<-function(hab, fam.start , ter.start, mgmt.years, mgmt.reps ){
cat("\nfunction: sim_pop\n")
      
  for(rep in 1:mgmt.reps) {  
            update_busy_bar(10) 
            families <-  fam.start  
            ter <-  ter.start
            out <- data.frame( year=1:mgmt.years, num.fam=0, 
                              num.adt=0, deaths.adt=0, 
                              num.juv=0, juv.born=0, deaths.juv=0, 
                              num.sub=0, deaths.sub=0, 
                              sub.recruit=0, sub.disp=0, sub.fail=0 )
            out$num.fam[out$year==0] <- NROW(subset(families, num.m+num.f>0))
            out$num.adt[out$year==0] <- sum(families$num.m, families$num.f)
              
            routes <- data.frame(bvr=0, cell=0)
           
              if( mgmt.years==5) {updval <- 16}
              if( mgmt.years==10){updval <- 8}
            
            yearcount <-0
            for (year in 1: mgmt.years) {
                                 yearcount <- yearcount+1
                                 removeUI('#text', immediate = T)
                                 insertUI('#placeholder', ui = tags$p(id = 'text', paste('( computing simulation run ', rep,"/15    -    year ",yearcount," )")), immediate = T)
                                 cat(paste0('\nrep ', rep,"/15 | year ",yearcount, "  ")) 
                                 disp.fail <<- deaths.adt <<- deaths.sub <<- deaths.juv <<- births <<- disp.succ <<- 0
                                 updval2 <- 10+yearcount*updval
                                 update_busy_bar(updval2) 
                                 hab2 <- matrix(hab@data[,1], byrow=FALSE, ncol=hab@grid@cells.dim["x"])
                                
                                #cat("subadult survival  -  ")
                                cat(".")
                                last.years.young <- sapply(families$young, survival, mort=mort.sub)
                                deaths.sub <- sum(families$young) - sum(last.years.young)
                                
                               # cat ("production of young  -  ")
                                cat(".")
                                this.years.young <- sapply(families$fam.id, breeding2, fam=families, litter.size=litter.size, breed.prob=breed.prob)
                                births <- this.years.young
                                this.years.young <- sapply(this.years.young, survival, mort=mort.juv)
                                deaths.juv <- sum(births)-sum(this.years.young)
                                families$young <- this.years.young
                              
                              # cat ("adult survival  -  ")
                                cat(".")
                               mal.surv <- sapply(families$num.m, survival, mort=mort.adt)
                               fem.surv <- sapply(families$num.f, survival, mort=mort.adt)
                               deaths.adt <- sum( (families$num.m - mal.surv) + (families$num.f - fem.surv) )
                               families$num.m <- mal.surv
                               families$num.f <- fem.surv
                              
                              # cat ("recruitment  -  ")
                                cat(".")
                               recruit <- t(sapply(families$fam.id, recruitment2, young=last.years.young, fam=families, famsize.max=famsize.max))
                               sub.disp <- recruit[,1]
                               families$num.m <- families$num.m + recruit[,2]
                               families$num.f <- families$num.f + recruit[,3]
                              # cat (" dispersing  -  ")
                                cat(".")
                        
                               disp <- unlist(apply(data.frame(fam.id=1:length(sub.disp), sub.disp), MARGIN=1,function(x) rep(x[1], times=x[2])))
                               
                              if(length(disp)>0) {
                                 disp <- data.frame(fam.id=disp, ind.id=1:length(disp)) 
                                 for (id in psample(disp$fam.id)) {
                                    disp_compute <- dispersal2(id, fam=families, ter, hab2 , famsize.max, move.max, hab.tot.quality, routes)  
                                     
                          families <- disp_compute [[1]]
                          ter <- disp_compute [[2]] 
                        }
                      }
                      
                       
                      # cat ("territorial release\n")
                                cat(".")
                        
                            out$num.fam[out$year==year] <- NROW(subset(families, num.m+num.f>0))
                            out$num.adt[out$year==year] <- sum(families$num.m, families$num.f)  
                            out$propfem[out$year==year] <- sum(families$num.f)#/out$num.adt[out$year==year]
                            out$deaths.adt[out$year==year] <- deaths.adt
                            out$juv.born[out$year==year] <- sum(births)
                            out$num.juv[out$year==year] <- sum(births) - deaths.juv
                            out$deaths.sub[out$year==year] <- deaths.sub
                            out$deaths.juv[out$year==year] <- deaths.juv
                            out$num.sub[out$year==year] <- sum(recruit[,1:3])
                            out$sub.recruit[out$year==year] <- sum(recruit[,2:3])
                            out$sub.disp[out$year==year] <- disp.succ
                            out$sub.fail[out$year==year] <- disp.fail 
                             
                              ter.all2 <- rbind(ter.all2, data.frame(cell=which(values(ter)>0), ter=values(ter)[values(ter)>0], rep.id=rep, year=year ))
                              fam.all2 <- rbind(fam.all2, data.frame(families, rep.id=rep, year=year))
                         # keep only years of interest? NatureScot to say which
                 
 
            }# year loop
                                            cat("| ")
 
             out.all    <- rbind(out.all, data.frame(rep.id=rep(rep, NROW(out)), out)) 
             routes.all <- rbind(routes.all, data.frame(rep.id=rep(rep, NROW(routes)), routes)) 
          
          
              update_busy_bar(100)
             
        }#rep loop 
                                             cat("\n") 
        removeUI('#text', immediate = T)
        insertUI('#placeholder', ui = tags$p(id = 'text', paste("( gathering simulation output as GIS layers )")), immediate = T)
  
    update_busy_bar(10)
   
    
     ## pdm 5yrs
     cat("year 5  -  ")
     fam5 <- fam.all2[fam.all2$year ==  5  ,] %>%   group_by(rep.id,fam.id) %>% summarise(num.adt=num.m +  num.f)
     fam5 <- fam5[fam5$num.adt>0,]
     ter.all2$count <- 1 
    
     ter_occ5<- NULL
     for (repnum in 1:mgmt.reps){
        occfam <- unique(fam5$fam.id[fam5$rep.id == repnum ])
        ter_occ5 <- rbind(ter_occ5,ter.all2[ter.all2$year ==  5 & ter.all2$rep.id ==repnum & ter.all2$ter %in% occfam,])
       }
    
     ter_occ5 <- ter_occ5 %>% group_by(cell) %>% summarise(dens=sum(count))
       
          rter2 <- raster(extent(hab)) # empty raster
          res(rter2 ) <-  100 
          crs(rter2) <- mercproj
          values(rter2)[ter_occ5$cell] <-  ter_occ5$dens 
        
    update_busy_bar(30) 
    fm2 <- as(rasterToPolygons(rter2, dissolve = F),"sf") # was T -to id the families but not relevant when several reps
       names(fm2)[1] <- "dens" 
    fm2$Nruns <- "1 to 3 runs" 
    fm2$Nruns[fm2$dens>2] <- "3 to 5 runs"  
    fm2$Nruns[fm2$dens>4] <- "5 to 10 runs"
    fm2$Nruns[fm2$dens>9] <- "10 to 15 runs"
     
    
    
    ## pdm 3yrs
     
     cat("year 3  -  ") 
     fam3 <- fam.all2[fam.all2$year ==  3  ,] %>%   group_by(rep.id,fam.id) %>% summarise(num.adt=num.m +  num.f)
     fam3 <- fam3[fam3$num.adt>0,]
     ter.all2$count <- 1 
    
     ter_occ3<- NULL
    for ( repnum in 1:mgmt.reps){
        occfam <- unique(fam3$fam.id[fam3$rep.id == repnum ])
        ter_occ3 <- rbind(ter_occ3,ter.all2[ter.all2$year ==  3 & ter.all2$rep.id ==repnum & ter.all2$ter %in% occfam,])
    }
    update_busy_bar(50)
    ter_occ3 <- ter_occ3 %>% group_by(cell) %>% summarise(dens=sum(count))
      
            rter3 <- raster(extent(hab)) # empty raster
         res(rter3 ) <- 100 
         crs(rter3) <- mercproj
        values(rter3)[ter_occ3$cell] <-  ter_occ3$dens 
         
     
    fm3 <- as(rasterToPolygons(rter3, dissolve = F),"sf") # was T -to id the families but not relevant when several reps
     names(fm3)[1] <- "dens" 
    fm3$Nruns <- "1 to 3 runs" 
    fm3$Nruns[fm3$dens>2] <- "3 to 5 runs"  
    fm3$Nruns[fm3$dens>4] <- "5 to 10 runs"
    fm3$Nruns[fm3$dens>9] <- "10 to 15 runs"
    
    fm10 <- NULL
    if( mgmt.years == 10){
    ## pdm 10yrs
    
     cat("year 10  -  ")
     update_busy_bar(70) 
     fam10 <- fam.all2[fam.all2$year == 10  ,] %>%   group_by(rep.id,fam.id) %>% summarise(num.adt=num.m +  num.f)
     fam10 <- fam10[fam10$num.adt>0,]
     ter.all2$count <- 1 
    
     ter_occ10<- NULL
    for ( repnum in 1:mgmt.reps){
        occfam <- unique(fam10$fam.id[fam10$rep.id == repnum ])
        ter_occ10 <- rbind(ter_occ10,ter.all2[ter.all2$year ==  10 & ter.all2$rep.id ==repnum & ter.all2$ter %in% occfam,])
    }
    
    ter_occ10 <- ter_occ10 %>% group_by(cell) %>% summarise(dens=sum(count))
          rter10 <- raster(extent(hab)) # empty raster
          res(rter10 ) <-  100 
          crs(rter10) <- mercproj
          values(rter10)[ter_occ10$cell] <-  ter_occ10$dens 
       
     
    fm10 <- as(rasterToPolygons(rter10, dissolve = F),"sf")  
       names(fm10)[1] <- "dens" 
    fm10$Nruns <- "1 to 3 runs" 
    fm10$Nruns[fm10$dens>2] <- "3 to 5 runs"  
    fm10$Nruns[fm10$dens>4] <- "5 to 10 runs"
    fm10$Nruns[fm10$dens>9] <- "10 to 15 runs"
    fm10$layer <- "probabilities over all reps"
    }
     
    fm2$layer <- "probabilities over all reps"
    fm3$layer <- "probabilities over all reps"
     update_busy_bar(80)
     
     
     cat("stats  -  ")
    ## recap tables
        out.all$propfem  <- out.all$propfem/out.all$num.adt 
        out.all$meanfamsize  <-  out.all$num.adt /out$num.fam 
            
    out1 <- out.all  %>% group_by(year) %>% 
                        summarise(meanfam=round(mean(num.fam),1),minfam=min(num.fam),maxfam=max(num.fam),
                                  meanadt=round(mean(num.adt),1),minadt=min(num.adt),maxadt=max(num.adt),
                                  meanpropfem=round(mean(propfem),1),minpropfem=round(min(propfem),1),maxpropfem=round(max(propfem),1),
                                  meanmeanfamsize=round(mean(meanfamsize),1),minmeanfamsize=min(meanfamsize),maxmeanfamsize=max(meanfamsize),
                                  meanjb=round(mean(juv.born),1),minjb=min(juv.born),maxjb=max(juv.born),
                                  meansub=round(mean(num.sub),1),minsub=min(num.sub),maxsub=max(num.sub),
                                  meandisp=round(mean(sub.disp),1),mindisp=min(sub.disp),maxdisp=max(sub.disp),
                                  meandispf=round(mean(sub.fail),1),mindispf=min(sub.fail),maxdispf=max(sub.fail))
     
       cat("summary table\n")
    
     
    out2    <- with(out1, data.frame("Year"=year,
                                     "Number of families"= paste0(meanfam,"(",minfam,"-",maxfam,")"),  
                                     "Number of adults"= paste0(meanadt,"(",minadt,"-",maxadt,")"), 
                                     "Proportion of females"= paste0(meanpropfem,"(",minpropfem,"-",maxpropfem,")"),
                                      "Number of juveniles born"= paste0(meanjb,"(",minjb,"-",maxjb,")"), 
                                     "Number of sub-adults"= paste0(meansub,"(",minsub,"-",maxsub,")"), 
                                     "Number dispersing"= paste0(meandisp,"(",mindisp,"-",maxdisp,")"), 
                                     "Number failing dispersal"= paste0(meandispf,"(",mindispf,"-",maxdispf,")") )) 
      
    update_busy_bar(90)
    ## prettier table?
    out3 <- with(out1, data.frame("simulated output on year 5"=c("Number of families", "Number of adults"),
                                  "average (min - max)"=c(paste0(meanfam,"(",minfam,"-",maxfam,")")[6],paste0(meanadt,"(",minadt,"-",maxadt,")")[6]),
                                  "median"=c(median(meanfam)[6],median(meanadt)[6])
    ))
    
    threshold_Nfams <- out1$maxfam[1]-.1
    threshold_Ninds <- out1$maxadt[1]-.1
    threshold_Nindsdble <- out1$maxadt[1]*2-.1
    threshold_Nfamsdle <- out1$maxfam[1]*2-.1
     out2 <- out.all %>% group_by(year) %>% summarise(
                                  meanfam=paste0(round(mean(num.fam),1),"(", min(num.fam),"-", max(num.fam),")"), # change to Nfams rather than 2 fams?
                                  medfam=round(median(num.fam),1) ,                              
                                  prob_fam2plus = paste0(round(length(which(num.fam>threshold_Nfams))/mgmt.reps*100,0),"%"),
                                  prob_famdble = paste0(round(length(which(num.fam>threshold_Nfamsdle))/mgmt.reps*100,0),"%"), 
                                   meanadt=paste0(round(mean(num.adt),1) ,"(", min(num.adt),"-", max(num.adt),")"), 
                                  medadt=round(median(num.adt),1) ,
                                  prob_adt4lus = paste0(round(length(which(num.adt>threshold_Ninds))/mgmt.reps*100,0),"%") ,
                                  prob_adtdble = paste0(round(length(which(num.adt>threshold_Nindsdble))/mgmt.reps*100,0),"%") )
                     
    colnames(out2) <- c("year after release", "number of families\naverage (min-max)", "number of families\nmedian",paste0("predicted probability of\nat least ",out1$maxfam[1]," families"),
                        paste0("predicted probability of number of territories having at least doubled"),
                        "number of adults\naverage (min-max)", "number of adults\nmedian",paste0("predicted probability of\nat least ",out1$maxadt[1]," adults"),
                         paste0("predicted probability of abundance having at least doubled"))  
       
 
     
 update_busy_bar(100)
    simpop <- list(ter.all2,fm2,out2, fm3, fm10  )
    return(simpop)
    }
 