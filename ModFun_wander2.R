cat("wander2()  -  ")



wander2 <- function(id, hab, origin) {
  r_id <- row(hab)[origin]
  c_id <- col(hab)[origin]
  
  otr <- 12; inr <- 5
  
  hab_outer <- which(row(hab)>(r_id-otr) & row(hab)<(r_id+otr) & col(hab)>(c_id-otr) & col(hab)<(c_id+otr))
  hab_inner <- which(row(hab)>(r_id-inr) & row(hab)<(r_id+inr) & col(hab)>(c_id-inr) & col(hab)<(c_id+inr))
  hab_ring <- hab_outer[hab_outer %nin% hab_inner]
  
  data.frame(index=hab_ring, hab=hab[hab_ring]) %>% 
    filter(hab==2) %>% 
    sample_frac() %>% 
    pull(index) ->
    path
  return(path)
}

