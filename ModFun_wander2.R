cat("wander2()  -  ")



wander2 <- function(id, hab, origin) {
  r_id <- floor(terra::rowFromCell(terra::rast(hab), origin))
  c_id <- floor(terra::colFromCell(terra::rast(hab), origin))
  
  otr <- 12; inr <- 5
  
  hab_outer <- which(terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(r_id-otr) & 
                       terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(r_id+otr) & 
                       terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(c_id-otr) & 
                       terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(c_id+otr)) #Upper distance
  hab_inner <- which(terra::rowFromCell(rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(r_id-inr) & 
                       terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(r_id+inr) & 
                       terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(c_id-inr) & 
                       terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(c_id+otr)) #Lower distance
  hab_ring <- hab_outer[hab_outer %nin% hab_inner] #Habitable area
  
  data.frame(index=hab_ring, hab=terra::rast(hab)[hab_ring]) %>% 
    rename("hab" = "lyr.1") %>% 
    filter(hab==2) %>% 
    slice_sample(prop = 1) %>% 
    pull(index) -> path
  
  return(path)
}

