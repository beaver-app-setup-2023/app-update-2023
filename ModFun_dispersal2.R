cat("dispersal2()  -  ")



dispersal2 <- function(id, fam, ter, hab, famsize.max, move.max, hab.tot.quality, routes) {
   bvr <- max(routes$bvr)+1
  
  #made a change here, indicated by lines marked ###. Seems to work
  startf <- adjacent.id(ter, cells=which(terra::values(ter)==id), id=id, directions=8)
  startf.natal <- startf
  test1 <- find.best.hab(startf, hab) 
   
  start.here <- psample(find.best.hab(startf, hab ), 1)  ###
  # wander around
  path <- wander2(id, hab=hab, origin=start.here)  ### Added Mar 2020
 
  if(length(path)==0) path <- start.here           ### Added Mar 2020
  
  #now start looking around destination for territory. 
  start.here <- path[length(path)]
  startf <- adjacent.id(ter, cells=which(terra::values(ter)==id), id=id, directions=8)
  startf.natal <- startf
  
  if(length(start.here)==0) {disp.fail <<- disp.fail + 1; 
   disp_return <- list(fam,ter)
   return(disp_return)
   }  
  
  routes.loc <- data.frame(bvr=rep(bvr, length(path)), cell=path)
  
  verbose=FALSE
  
   if(verbose) cat ("\tbeaver from", id, ": natal=", sort(startf.natal), "\n")
  
  success <- 0
  moves   <- 0
  while (success == 0 & moves < move.max) {
     if(length(startf)==0) {success <- -1; next;}
    ter.here <- ter[start.here]
    hab.here <- hab[start.here]
    if(verbose) cat ("\tbeaver from", id, ": starting at", start.here, ", hab here =", hab.here, "\n")
    
    #what do I do?
    if (is.na(hab.here) | hab.here==0) {
       cat(".")
      #choose again from start
      if(verbose) cat ("\tbeaver from", id, ": no go, starting again\n")
      success <- 0
      start.here <- psample(find.best.hab(startf, hab, start.here), 1) ###
      startf <- startf[-which(startf==start.here)] ###
      if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here))
    } else if (hab.here==1) {
       #choose again from here
      startf <- terra::adjacent(terra::rast(hab), cells=start.here, directions=8, pairs=FALSE)
      startf <- startf[-which(terra::values(ter)[startf]==id)] #exclude cells of same family ## values
      if(length(startf) == 0) {
        if (verbose) cat("\tbeaver from", id, ": can't settle at", start.here, ", returning to natal\n")
        startf <- startf.natal
        start.here <- psample(find.best.hab(startf, hab, NA), 1) ###
        startf <- startf[-which(startf==start.here)] ###
        startf.natal <- startf.natal[-which(startf.natal==start.here)] ###
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here))
         if(verbose) cat("\t\tchoices (natal):", startf, "\n")
      } else {
        if (verbose) cat("\tbeaver from", id, ": can't settle at", start.here, ", moving on\n") 
         if(verbose) cat("\t\tchoices:", startf, "\n") 
        start.here <- psample(find.best.hab(startf, hab, start.here), 1) ### 
        startf <- startf[-which(startf==start.here)] ### 
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here))   
      }   
      success <- 0
    } else if (ter.here > 0) {
      
      #try to join this territory
      if (verbose) cat("\tbeaver from", id, ": discovered territory", ter.here, "\n")
      famsize <- fam$num.m[ter.here] + fam$num.f[ter.here] 
      if (famsize < famsize.max & famsize > 0) {
        if(fam$num.m[ter.here] < fam$num.f[ter.here]) {fam$num.m[ter.here] <- fam$num.m[ter.here] +1} else {fam$num.f[ter.here] <- fam$num.f[ter.here] +1}
        success <- 1
        if (verbose) cat("\tbeaver from", id, ": joined territory", ter.here, "\n")
      } else {
        success <- 0
        if (verbose) cat("\tbeaver from", id, ": too big or empty\n")
        start.here <- psample(find.best.hab(startf, hab, start.here), 1) ###
        startf <- startf[-which(startf==start.here)] ###
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here))
      }
      
    } else {
       #try to form own territory
      if (verbose) cat("\tbeaver from", id, ": found empty space", start.here, "\n")
      fam.id.new <- NROW(fam)+1
      ter.temp <- ter ### added <<  here?
      ter.temp[start.here] <- fam.id.new
      qual <- hab[start.here]
        tries <- 0
      while (qual < hab.tot.quality & tries<50) {
         cat("-")
        ter.temp <- expand.territory2(ter.temp, fam.id.new, hab) ####<<
        qual <- sum(hab[which(values(ter.temp)==fam.id.new)])
         
        tries <- tries+1
      }
      if (qual<hab.tot.quality) {
        success <- 0
        if (verbose) cat("\tbeaver from", id, ": failed to find enough space, qual =", qual, "\n")
        start.here <- psample(find.best.hab(startf, hab, start.here), 1) ###
        startf <- startf[-which(startf==start.here)] ###
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here))
      } else { #add new family
        success <- 2
        num.m <- num.f <- young <- 0
        if(runif(1) < 0.5) {num.m <- 1} else {num.f <- 1}
        col <- rainbow(30)[fam.id.new]
        fam <- rbind(fam, data.frame(fam.id=fam.id.new, num.m, num.f, young,  qual, stringsAsFactors=FALSE))
        ter  <- ter.temp ####removed <<
         
        if (verbose) cat("\tbeaver from", id, ": formed new family", fam.id.new, "\n")
      }
           
    }
    if(success==2) { ter  <<- ter.temp} 
    
    #do I stop?
    if (length(startf)==0 & length(startf.natal)==0) {   #if all forward spaces are gone and all natal territory is exhausted
      if (verbose) cat("\tbeaver from", id, ": run out of space, fails\n")
      success <- -1
    } 
    moves <- moves + 1
    if(moves>=move.max) success <- -1
  } 
  
  if(success==-1) {
    if (verbose) cat("\tbeaver from", id, ": unable to settle, dies\n")
    disp.fail <<- disp.fail + 1
  } else {
    disp.succ <<- disp.succ + 1
  }
  
  if (NROW(routes.loc)>0) routes <<- rbind(routes, routes.loc)
   
  disp_return <- list(fam,ter)
  return( disp_return  )
} 

