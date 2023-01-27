cat("init reactive df  -  ")


############################# init reactive df & vals

 val <- reactiveValues(clickx =NULL, clicky =NULL,clickx2 =rep(NULL,3), clicky2 =rep(NULL,3) ) 
 rv <- reactiveValues(siminfo =paste0('\n'))     
 
 triggers <- reactiveValues(new_extent=0,new_sim=0, new_simfeat=0) 
 ldat <- reactiveValues(ext_scotcrop=NULL,  ext_UKbound= NULL,ext_area=NULL, LocalHab=NULL)
  
 plot_data <- reactiveValues(Nfams=NULL, x =NULL, y=NULL, buffer_m=2000,tdat=NULL  ) # static buffer now - 2km?
 plot_data2 <- reactiveValues( x =NULL, y=NULL ) 
 
 savedat <- reactiveValues( table0=NULL, table=NULL, tdat=NULL, tdat0=NULL, tablepts0=NULL, tablepts=NULL,p1= NULL, ptstp_nro= 0  )
 reac_lay <- reactiveValues(OS = 0, OSall=0,hab = 0,  selected_OS=character(0))
 rvsim <- reactiveValues(trigsim=0,mapsim=NULL,mapsim0=NULL,start_pop=NULL, sim_pop=NULL,lay_output=NULL,lay_release_pts=NULL, trig=0, temp_bbox=NULL, lay_release_pts_trig=0, linelength=NULL, Nfams_w2Young=0,Nfams_wYoung=0, Nads=0,Nyoungs =0)
   
   
 rvtext1 <-reactiveValues(extent_infotext=NULL,  famsettled_text=NULL,init_terr_idtext=NULL,init_terr_counter=0,extent_counter=0,sim_counter=0,init_terr_idtext_y0=NULL)         
 rvplot1 <-  reactiveValues(new_ext0=NULL, mapsim_init=NULL, trigger_init_terr =NULL)       
 rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)

 dl_data <- reactiveValues(out_Nruns=NULL,out_chulls=NULL,locname=format(Sys.time(), "%d%b%y_%H%M") )  

 update_busy_bar(100)    