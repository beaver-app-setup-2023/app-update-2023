cat("find.best.hab()  -  ")



find.best.hab <- function(choices.in, hab, origin=NA) {
   # this function finds the best habitat from the choices given in choices.in
   # include all cells of habitat2
  ch <- which(hab[choices.in]==2)
    # include the four cells furthest away from the supplied origin
  if(is.na(origin)) {
    
    s <- ifelse(sum(!is.na(hab[choices.in]))<3,sum(!is.na(hab [choices.in])),3)#now hab$layer?
    ch <- c(ch, psample(which(!is.na(hab [choices.in])), s))
    
  } else {
    dist <- sqrt((col(hab)[origin] - col(hab)[choices.in])^2 + (row(hab)[origin] - row(hab)[choices.in])^2 )
    dist[is.na(hab[choices.in])] <- -999
    dist.ord <- order(dist, decreasing=TRUE)    

    ch <- c(ch, dist.ord[1:3])
  }  

  return(choices.in[unique(ch)])
}

