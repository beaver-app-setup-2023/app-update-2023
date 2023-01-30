
### box = extent, spatial, polygon is a rectangle (region_box)
### poly= land covered within extent, polygon is contained by geographical boundaries (coastline)


insertUI('#placeholder', ui = tags$p(id = 'text', paste('( reading GIS layers )')), immediate = T)

org_name    <- "NatureScot"  
region_name <- "Glen Affric to Beauly Firth"

cat("retrieve GIS data for region: ")

if (org_name == "NatureScot"){
  if (region_name == "Glen Affric to Beauly Firth") {
#### NatureScot first extent (Nov 202) - "from Glen Affric to Beauly Firth" <- jan save and store extents instead of loading when more than 1
   cat(region_name, "(", org_name,")\n")
   beauly_box    <- as(as(extent(st_bbox(st_sfc(st_buffer(st_point(c(255000,845000) ), 20000) ))), "SpatialPolygons"), "sf")    ## init relevant loc point mentioned
   affric_box    <- as(as(extent(st_bbox(st_sfc(st_buffer(st_point(c(209793 , 820544) ), 20000) ))), "SpatialPolygons"), "sf")  ## init relevant loc point mentioned
   region_box    <- as(as(extent(st_bbox( st_union( beauly_box, affric_box) )), "SpatialPolygons"), "sf") ## extent incl. both extents
cat("spatial bounding box  -  ")
}}

  st_crs(region_box) <- mercproj
 labs_df <- data.frame(x=c(218593,258952),y=c(822545,847767),labs=c( "Glen Affric" ,"Beauly Firth")  )
 
 
## index by region/org? 
source(here::here("initLoad_GIS_beaverHabitat.R"))  
 