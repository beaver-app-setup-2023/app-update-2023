
################################################################################    
################      BEAVER TRANSLOCATION SIMULTATION APP      ################
################        for NatureScot Nov. 2022                ################
################        shiny code - ui+server                  ################
################################################################################        
 


 shiny::addResourcePath("www", paste0(here::here(),"/www"))
 shiny::addResourcePath("www2", paste0(here::here(),"/www2")) 
 
 
ui <-function(request) { 
 #,width=90,height=90
navbarPage(
  title=div(div(img(src="www/BeaverLogoTeal.png",width=90,height=75 ), style="margin-left:20px;padding-top:0px; margin-top:0px;position: fixed;text-align:left;display:flex;"),
           div(style="margin-left:900px;position:fixed; display:flex;padding-top:20px;font-size:12px;", bookmarkButton("" , id = "bookmarkBtn", class = "btn-success", title = "Bookmark this application's state to get a URL and return to the current session.")),
           div(style="color:#ffff00;margin-left:180px;font-size:22px;", "A Beaver Translocation Simulation App") ,
           div(style="padding-bottom: 5px; padding-top:10px;margin-left:130px;color:turquoise;font-size:13px;white-space:pre-wrap;", 
                                  '\na Shiny App developped by .zelda. at the University of Newcastle (UK) for NatureScot.'),
           div(style="margin-left:130px;color:teal;font-size:13px; white-space: pre-wrap;", 
                  'contact:       zeldavdwaal@gmail.com      aileen.mill@newcastle.ac.uk     steven.rushton@newcastle.ac.uk '),style="margin-bottom:0px,padding-bottom:0px;")  , 
          id = 'PageID',
          windowTitle='Beaver Translocation App', 
  
  fluidRow( style="background:black;",
            column(5,div("another paceholder",style="padding:0;margin:0;color:black;font-size:13px;")), 
            column(6,div(id = 'placeholder' ),
                    tags$head(tags$style("#text{padding:0;margin:0;font-size:12px;color:yellow;font-family:monospace;text-align:right;white-space:pre-wrap;}")) )
             ),
 br(),
  
  
  
  
  
             
####################################################################### tab 1 
tabPanel( value="intro_tab",
           div("Select release site",style="font-size:14px;"),
           shinyjs::useShinyjs(),
           shinybusy::use_busy_bar(color = "#01DF01", height = "10px"),                                                                        
           shinyWidgets::chooseSliderSkin(skin="Modern" ),
          
sidebarLayout(position = "left", 
              
sidebarPanel(  id="sidebarPanel1",  width = 3,  
 
       fluidRow(column(2, div(img(src="www/GBMap.png",width=80), 
                          style="margin-left:0px;padding:0px;margin-top:0px;margin-right:0px;text-align:left;font-size:13px;")  ),
                
                column(9,offset=1,   
                         div("Select a candidate release site within the area.",style="font-size:23px; padding-left:90;color:beige;text-align:left;padding-bottom:20px "),
                      
                       
                         div("Double-click on the map to select a candidate release site within the area: ",
                              span("Glen Affric to Beauly Firth, Scotland.", style="font-style:italic;font-face:bold;color:turquoise;"),
                              "Click on ",span(style="color:orange",icon("binoculars")," explore")," to assess the location as a potential release site." ,
                              style="font-size:13px; text-align:left;padding-bottom:0px;color:beige;margin-left:90;") )) ,
      fluidRow(  column(4, 
                        div("Buffer size", style="font-size:13px; padding-top:12px;color:orange;text-align:right;margin-right:0;font-face:bold;")) ,  
                 column(8, 
                        div(shinyWidgets::prettyRadioButtons(inputId ='buffer_sel', label=NULL, choiceNames=c("2km", "5km", "10km"), choiceValues=c(2000,5000,10000),
                                                      selected=2000, width="100%", inline=TRUE, status = "warning", thick = FALSE,bigger=TRUE, shape="round"),
                                                      style="font-size:12px; padding-top:12px;color:orange;text-align:left;margin-right:0;padding-right:0;")) 
             ),       
            
      fluidRow(  shinyWidgets::addSpinner(plotOutput("plotmap1",    dblclick =  "plot_click", 
                                 width="100%" , 
                                 hover = hoverOpts("plotmap1plot_hover",  nullOutside=TRUE)) ,color = "purple"),
                                 tags$head(tags$style("#plotmap1{max-height:1000px !important;}")),
                                 tags$style(HTML('#mplotmap1 {  margin:auto;text-align:center; }')) 
                                ), 
      
      tags$style( '#plotmap1{ width:200px; transform:  scale(.9) ; transition: all 0.3s;}
                    #plotmap1:hover{ transform: translateX(+10%) translateY(+10%) scale(1.2) ; transform-origin:bottom; z-index: 10; }'), 

      fluidRow( 
               div(strong("Display landscape features:"),style="padding-left:20px;font-size:14px;text-align:left;padding-bottom:6px;color:turquoise;font-face:bold;"),
               div(shinyWidgets::prettyCheckboxGroup(inputId = "show_suit" ,  label = NULL, 
                                                     choices = c( "habitat suitable for beaver settlement","main catchments","water body intercatchments", "main roads","named places","urban regions"),
                                                     selected="habitat suitable for beaver settlement",
                                                     status = "info",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round"), 
                                                     style="color:turquoise;padding-left:40px;width:90%;margin-bottom:0px;padding-top:0px;font-size:14px;") 
             ), 
      br(), 
      fluidRow(
      column(1),
      column(3, 
              div(uiOutput("plotmap1hover_info",style="font-size:14px; white-space: pre-wrap;color:beige; text-align:left;margin-bottom:0px;margin-left:0px;"))),
      column(3,
             div(uiOutput("plotmap1_infosel",style="font-size:14px;white-space: pre-wrap;color:orange; text-align:left;margin-right:0px;")) ),
      column(5, 
             actionButton("pick_extent",  div(style="margin-left:10px;","explore"),  icon("binoculars" ),
                            class = "btn-warning",style="border-radius:50px !important;display:inline-block;float:right; width:120px;" ) )) 
      ), # end sidebarPanel tab1
  


mainPanel(  shinyjs::useShinyjs(), width = 8,  
         verticalLayout(
              
               fluidRow(column(12, offset=1,
                        fluidRow(  column(9,  div("Simulate a beaver translocation within the candidate release site.",style="font-size:23px;text-align:center; color:beige;padding-bottom:20px") ,
                                   column(3,   p( span( style="color:yellow;",icon("cog"),"Translocation Parameters."),code("  1. Adjust the translocation parameters for the session.", style="font-size:13px;padding-left:2px; color:beige;font-family: Helvetica;"))),
                                   column(3,   p( span( style="color:turquoise",icon("palette"),"Visualise Local Landscape."),code("  2. Display and assess local landscape and habitat suitability.", style="font-size:13px;padding-left:2px; color:beige;font-family: Helvetica;"))),
                                   column(3,   p( span( style="color:yellowgreen",icon("map"),"Candidate Release Site."),code("  3. Locate chosen release points on the map.", style="font-size:13px;padding-left:2px;  color:beige;font-family: Helvetica;"))), 
                                   column(3,   p( span( style="color:red",icon("home"),"Simulate Settlement."),code("  4. Click on the button to simulate initial settlement.", style="font-size:13px;padding-left:2px; color:beige;font-family: Helvetica;")))),
                                          
                                   column(3,
                                           br(), 
                                           actionButton("start_sim", div(style="margin-left:5px;","simulate settlement"),  icon("home"), 
                                                                         class = "btn-danger",style="border-radius:50px !important;display:inline-block;float:right; width:200px;"))),
                hr(), 
            
             fluidRow( 
                     column(8,  absolutePanel(   fixed = TRUE ,draggable = FALSE, top = 300,   left ="27%",  right = "25%", bottom=0,# width ="auto", height = "auto",
                                              shinyWidgets::addSpinner(plotOutput("mapzoom",  dblclick = "plot_click2",  #height='60vh'
                                              height="100%" ,  hover = hoverOpts("mapzoom_hover", delay = 100, delayType = "debounce", nullOutside=TRUE)),color = "yellowgreen"),
                                              tags$head(tags$style("#mapzoom{max-height:10000px !important;}")),
                                              tags$style(HTML('#mapzoom {  margin:auto;}')) ,
                                              br(),br() ,hr())  ,
               
                                absolutePanel(   fixed = TRUE ,draggable = FALSE, top = 320,   left ="33%",  right = "75%",  
                                      uiOutput("map_logo")) 
                             ), 
    
       
    ##         
                     column(4,  
                             fluidRow(style = 'border-left: 1px solid; color:yellow',
                                           div( icon("cog") , "Translocation Parameters" ,style="padding-left:10px;font-size:23px;text-align:left;padding-top:10px;color:yellow;")  ,
                                           div("Inform parameters for a single release session including up to 10 beaver families. Notes that coordinates will be amended automatically for any point located less than 300m from any other release location.", style="padding-left:10px;font-size:13px;color:beige;width:95%;"),          
                                br(),   
                            
                             fluidRow( column(5,  
                                           div("Number of groups released: ",style="padding-left:0px;font-size:15px;text-align:right;padding-top:24px;color:yellow;font-face:bold;")),
                                       column(3,
                                           div("target",style="padding-left:0px;font-size:13px;text-align:right;padding-top:4px;color:yellow;"),
                                           numericInput(inputId="Nfams_init", label=NULL, value=3, min = 1,  max = 10, step = 1, width = "100%"),
                                           tags$style(HTML('#Nfams_init{background-color:black; color:beige;text-align:center;height:33px;}'))),
                                       column(3,
                                           div("actual",style="padding-left:0px;font-size:13px;text-align:right;padding-top:4px;color:orange;"),
                                           textOutput( "Nfams_init_actual"  ),
                                           tags$style(HTML('#Nfams_init_actual{background-color:black; color:orange;text-align:center;height:33px;padding-top:6px;}')))
                                      ),
                            
                             
                            fluidRow( column(12, 
                                           div(strong("Release protocol:"),
                                               style="padding-left:20px;font-size:16px;text-align:left;padding-top:12px;padding-bottom:4px;color:yellow;font-face:bold;"))),
                            fluidRow( column(11, offset=1,
                                           tags$style(HTML("input[type='radio']  {background:black;accent-color:orange;padding-left:20px;transform: scale(1.5); }")),
                                           radioButtons( inputId = "Nsites",label=NULL, 
                                                    choiceNames =  list(  p(span("automated release locations ",style = "color:yellow;padding-right:200px;"),span("double-click on the map to sample (or resample) randomly distributed release locations within any suitable habitat within the extent.", style = "color:beige")),  
                                                                          p(span("specify each release point ",style = "color:yellow;padding-right:400px;"),span("locate each release point on the map with one double-click per location.", style = " color:beige")) ,
                                                                           p(span("start from a single release point ",style = "color:yellow;padding-right:200px;"),span("\nlocate one starting point on the map with a double-click to concentrate starting territories around the location.", style = " color:beige")) ),
                                                     choiceValues= c("random_location_across" ,"each_pt_on_map","single_release_pt"  ),
                                                     selected =  "random_location_across") 
                                      )), 
                                   
                                   tags$head(tags$script(HTML("
                                     $(document).ready(function(e) {
                                       $('input[value=\"random_location_across\"]').parent().parent().after('<hr>');
                                       $('input[value=\"each_pt_on_map\"]').parent().parent().after('<hr>');}) "))),
                                    tags$style(HTML('#Nsites{ width:95%;}')),
                                  br(),  
                              
                            fluidRow( column(4,  
                                             div("Demographics:",
                                                 style="padding-left:20px;font-size:16px;text-align:left;padding-top:12px;padding-bottom:4px;color:yellow;font-face:bold;"))
                                      ),
                            
                            fluidRow( column(4, offset=1,
                                             radioButtons( inputId = "demog",label=NULL, 
                                                              choices = c("all adult pairs","some adult pairs, some families","all families"), 
                                                              selected = "some adult pairs, some families" ) ) ,
                                  
                                      column(7,
                                             textOutput("Nyoung_text"), tags$style("#Nyoung_text{white-space: pre-wrap;padding-left:50px;font-size:13px;text-align:left;padding-top:0px;padding-bottom:4px;color:orange;font-face:bold;width:80%;")   ,
                                             shinyWidgets::setSliderColor(c("#FF4500"), c(1)),
                                             div(style="width:70%;padding-left:50px;font-size:13px;color:black;",
                                                 sliderInput(inputId="Nyoungs_perfam", label=NULL, min=1, max=4, value=c(1,2), step = 1, round = FALSE, ticks=FALSE )    )  )
                                      ),
                            br()   
                      
                      
                     ), # fluidRow transloc pars
                     
                     
                            hr(),
                     
                     fluidRow( style = 'border-left: 1px solid; color:turquoise',
                            br(),
                            fluidRow(column(12,
                                         column(9,     div(icon("palette"), "Visualise local landscape" ,style="font-size:23px;text-align:left;padding-top:2px;color:turquoise;")),
                                         column(3,                                       
                                            div(style="display:inline-block;float:center;", actionButton("update_mapzoom_hab", "show",   class = "btn-success", style="border-radius:50px !important;")),
                                            shinyBS::bsTooltip("update_mapzoom_hab", "Display habitat and OS features on map.", "top", options = list(container = "body")) )                         
                                      )),
                                            
                                            br(), 
                            
                            fluidRow( column(12,
                                       column(6,
                                            shinyWidgets::prettyCheckboxGroup(inputId = "selected_layer_all" , label = "Habitat Suitability", choices = c("all") ,status = "primary",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round"),
                                            shinyWidgets::prettyCheckboxGroup(inputId = "selected_layer" ,  label = NULL, choices = c( "suitable","dispersal","unsuitable"), selected="suitable" ,status = "primary",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round" ) ),
                                      column(6,  
                                            shinyWidgets::prettyCheckboxGroup(inputId = "selected_OS_all" , label = "OS Features", choices = c("all","named places","all roads", "OS regions"), selected="all roads" ,status = "primary",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round")) ) ),
                                      br(),
                                     fluidRow( column(12, column(3,column(6,br())))),
                                                      
                                                      
                            fluidRow( column(12, column(6,
                                            shinyWidgets::prettyCheckboxGroup(inputId = "selected_OS1" , label=NULL,  
                                                               choices = c("named places", "local altitude",  "a road", "b road", "primary road","minor road","road names","railway line" ),status = "primary",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round" ) ),  
                                      column(6,
                                            shinyWidgets::prettyCheckboxGroup(inputId = "selected_OS2" , label=NULL,  
                                                               choices = c(  "woodland region","urban region","rivers line","lakes region","foreshor region", "marsh" , "TV or radio mast or tower","Wind powered generator"  
                                                               ),status = "primary",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round" ) ))),
                         tags$style(HTML('#selected_OS1{ color:turquoise}')),
                         tags$style(HTML('#selected_OS2{ color:turquoise}')),
                         tags$style(HTML('#selected_OS_all{ color:turquoise}')),
                         tags$style(HTML('#selected_layer{ color:turquoise}')),
                         tags$style(HTML('#selected_layer_all{ color:turquoise}')), 
                            br() 
                     ),
                            hr() 
                            
                     ))) #vertical panel            
  ))) #   mainPanel tab1
)), #   sidebarLayout,  tabPanel 1
        
   
 
####################################################################### tab 2       

tabPanel( value="sim_tab" ,
          div("Simulate range expansion",style="font-size:14px;"),
          shinybusy::use_busy_bar(color = "yellow", height = "10px"),
         

 fluidPage(     
 
##################### title row tab 2 
fluidRow(column(12, 
                
        column(4,   style = "background-color:black;",
            
          div( "Initial population settlement.",style="color:orange;padding-top:5px;padding-bottom:15px;text-align:center;padding-left:0px;font-size:22px;font-face:bold;"),
          br(),
          fluidRow( column(4,  
                           div('The simulation of initial settlement given the selected translocation parameters has generated a potential set of starting territories, displayed on the map.',style="font-size:13px;color:beige;text-align:left;")),
                    column(3,
                           div(style="font-size:13px;color:tomato;text-align:left;", icon("redo"),' "re-run"' ),
                           div('will generate an alternative simulation of starting territories.',  style= "font-size:13px;color:beige;text-align:left;")),
                    column(5, 
                           div( style="font-size:13px;color:tomato;text-align:left;", icon("play"),' "simulate growth"'),
                           div( 'will simulate 15 runs of population growth and range expansion over 5 or 10 years based on the current simulated starting territories.', style="font-size:13px;color:beige;text-align:left;"))
                    ),
            hr(),
          
          fluidRow(column(5, 
                         textOutput("candsite_idtext"),
                         tags$head(tags$style("#candsite_idtext{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:yellow")),
                         textOutput("init_terr_idtext"),
                         tags$head(tags$style("#init_terr_idtext{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:turquoise")),
                         br(),   
                            textOutput("extent_infotext" ),
                            tags$head(tags$style("#extent_infotext{color:beige;font-size:14px;white-space: pre-wrap;text-align:left;padding-left:20px;max-height:400px; }")) , 
                           br(),  br(),  br(), 
                    div(style="display:inline-block;float:center;",actionButton("start_reruninit",   div(style="margin-left:10px;","re-run?"),   icon("redo"),  class = "btn-danger", 
                              style="text-align:center;width:200px; border-radius:50px !important;"))
                              ),
                     
                   column(7,  
                     
                            shinyWidgets::addSpinner(plotOutput("mapsim_init", width="100%",  
                                            height="300px"  ),color = "turquoise"),
                            shinyWidgets::prettyCheckboxGroup(inputId ='sim_objective', '',  choices=c( "release points (initial)", "release points (relocated)", 'starting territories' ),  selected='starting territories', width="90%",
                                            status = "primary",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round"),
                                            tags$head(tags$style("#sim_objective{color: turquoise;font-size:13px;background:black;text-align:left;padding-left:50px;}"))
                        )) , 
    br()
    ),
           
###############################     
###############################  output tabs / main tab 2     
  
  
     column( 8,
              column(11,offset=1, # to add haldf a col as sep no more..
                 
              fluidRow(
                   column(4,
                          column(6,    actionButton("start_sim_growth",   div(style="margin-left:10px;","simulate growth") ,  
                                                                                  icon("play"), class = "btn-danger",style="display:inline-block;float:right;border-radius:50px !important;text-align:center;width:200px;")  ),
                          column(6,   shinyWidgets::prettyRadioButtons(inputId ='Nyears_sim', label=NULL,  choiceNames=c("5 years", "10 years" ),choiceValues=c(5,10),
                                                                        selected=5, width="90%", status = "danger",inline=FALSE,thick = FALSE,bigger=TRUE, shape="round"),
                                  
                          tags$head(tags$style("#Nyears_sim{color: beige; text-align:left;padding-left:6px;padding-top:14px;display:inline-block;float:left;}")))
                          ),
                   column(8,
                         div( "Simulated range expansion.",style="color:magenta;padding-top:5px;text-align:center;padding-right:0px;font-size:22px;font-face:bold;"),
                         br(), br(), br(), br(),
             tags$head( tags$style(HTML("
             .tabbable ul li:nth-child(1) { float: left;padding-left:370px;}
             .tabbable ul li:nth-child(2) { float: left;}
             .tabbable ul li:nth-child(3) { float: left;}
             .tabbable ul li:nth-child(4) { float: left;} 
             .tabbable ul li:nth-child(5) { float: right;}"
             )))
            )),
           
            
            
         tabsetPanel(  id="resTabs",  
                        
################ tab0
               tabPanel(div('initial settlement',style="color:royalblue;font-size:14px; "),     id="tab_y0",
                         br(),             
                         fluidRow(  
                                   column(5, uiOutput("init_sett_title1"),#    
                                          tags$head(tags$style("#init_sett_title1{color:beige;text-align:right;font-size:15px;}")), 
                                          textOutput("candsite_idtext_y0") ,
                                          textOutput("init_terr_idtext_y0"),
                                          textOutput("sim_idtext_y0"),
                                          tags$head(tags$style("#candsite_idtext_y0{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:yellow}")),    
                                          tags$head(tags$style("#init_terr_idtext_y0{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:turquoise}")) ,
                                          tags$head(tags$style("#sim_idtext_y0{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:beige}"))  
                                        ),
                                   column(5, 
                                          textOutput("init_sett_title2"),
                                          tags$head(tags$style("#init_sett_title2{color:beige;text-align:left;font-size:15px;}")))
                                        ),
                                    
                    column(4,      
                                    br(),br(),br(),
                    column(11,
                    conditionalPanel( condition="output.showExtraLayers == 1",style = "background:#464646", 
                                      div('Download simulation outputs',style="color:orange;font-size:14px; padding-bottom:4px;") ,
                                      fluidRow( 
                                            column(2, uiOutput("save_out_button")),
                                            column(10, uiOutput("save_out_customID"))),    
                                      tags$head(tags$style("#location{white-space:pre-wrap;padding-bottom:15px;color:yellowgreen;width:100%;font-size: 14px;text-align:center;height:50px;background:black; font-style:italic}")) 
                    ),       
                    br(),br(),   
                                   
                    conditionalPanel(condition="output.showExtraLayers1 == true",style = "background:#464646", 
                              div('Visualize over output maps',style="color:orange;font-size:14px; padding-bottom:4px;") ,
                              div("beaver dam building capacity:" ,style="color:beige;padding-left:26px;text-align:left;padding-bottom:6px;font-size:14px;"),
                              shinyWidgets::prettyCheckboxGroup ("show_extra_lay1",label=NULL,  status = "default",  plain=TRUE,
                                         choices = c("frequent to pervasive","occasional"),
                                         selected=character(0),width="100%", inline=FALSE,  thick = FALSE,bigger=TRUE ) ,
                           
                           hr(style="border-color: beige;width:60%") ,
                           div("catchments boundaries:" ,style="color:beige;padding-left:26px;text-align:left;padding-bottom:6px;font-size:14px;"),

                              shinyWidgets::prettyCheckboxGroup ("show_extra_lay2", label=NULL, 
                                                              choices=c("main catchments","water body intercatchments"), 
                                                              selected=character(0),status = "default",  plain=TRUE,width="90%"  ),  
                      
                               tags$head(tags$style("#show_extra_lay1{color:beige;text-align:left;font-size:14px;padding-left:46px;padding-bottom:0px;}")) ,
                               tags$head(tags$style("#show_extra_lay2{color:beige;text-align:left;font-size:14px;margin-top:0px;padding-left:46px;padding-bottom:15px;}")) 
                                    
 ),# cond panel 2 
   div(p( ".",style="color:black;padding-top:500px;font-size:13px;"))
     
 )  #col11         
 
 
 ),#col 4 
                              
         column(8,   # map0 col
               br(),   
               absolutePanel(   fixed = TRUE ,draggable = FALSE, top = 400,   left ="60%",  right = "1%", bottom=0,# width ="auto", height = "auto",
                                                    shinyWidgets::addSpinner(plotOutput("mapsim0_finext",    
                                                    height="100%" ),color = "blue"),
                                                    tags$head(tags$style("#mapsim0_finext{max-height:30000px !important;}")),
                                                    tags$style(HTML('#mapsim0_finext{  margin:auto; }'))
                            ),
                            br(),br()
                ),
                br(),br(),br(),br(),br(),br() #col8
        ), #tabpanel 0
                                         
          
  ################ tab1
               tabPanel(div('year 3',style="color:magenta;font-size:14px; ") , id="tab_y3",
                         br(),
                     fluidRow( column(5,
                                      tags$div( "Probability density map:" ,style="color:beige;text-align:right;font-size:15px;"),
                                      textOutput("candsite_idtext_y3") ,
                                      textOutput("init_terr_idtext_y3"),
                                      textOutput("sim_idtext_y3"),
                                      tags$head(tags$style("#candsite_idtext_y3{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:yellow}")),    
                                      tags$head(tags$style("#init_terr_idtext_y3{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:turquoise}")) ,
                                      tags$head(tags$style("#sim_idtext_y3{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:beige}")) 
                                     ),
                               column(5, tags$div( "simulated occupancy 3 years after translocation." ,style="color:beige;text-align:left;font-size:15px;"))),
                      
                    column(4, 
                       br(), br(), br(), br(), 
                       div("Display simulated occupancy*" , style="color:beige;font-size:14px;padding-left:30px;font-face:bold;margin-bottom:0px;" ),
                       br(),
                       shinyWidgets::prettyCheckboxGroup ("plot_probability_range_y3",label=NULL, icon = icon("ok", lib = "glyphicon"), status = "default",  plain=TRUE,
                                    choiceValues=c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),
                                    choiceNames =list(HTML('<div style="display:flex"><i class="fa fa-circle"
                                               style="color:lightblue; padding-top:2px;"></i><div style="color:white;">1 to 3 runs</div></div>'),
                                                            HTML('<div style="display:flex"><i class="fa fa-circle"
                                               style="color:yellow;"></i><div style="color:yellow; ">3 to 5 runs</div></div>'),
                                                            HTML('<div style="display:flex"><i class="fa fa-circle"
                                               style="color:orange;"></i><div style="color:orange;">5 to 10 runs</div></div>'),
                                                            HTML('<div style="display:flex"><i class="fa fa-circle"
                                               style="color:red;"></i><div style="color:red;">10 to 15 runs</div></div>')),
                                    selected=c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),width="90%", inline=FALSE,  thick = FALSE,bigger=TRUE ),
                      div("*as the number of runs predicting", tags$br(),  "that a cell is occupied", tags$br(),  "amongst a total of 15 runs."  , style="color:beige;font-size:13px;padding-left:30px;" ),
                             br(), br(), br(), 
                             shinyWidgets::prettyCheckboxGroup ("show_chull_y3", label=NULL,choices="Show convex hull", selected=character(0),  width="90%", inline=FALSE,status = "danger", thick = FALSE,bigger=TRUE, shape="round"),
                                   div("draw a convex hull around the cells",tags$br(),
                                       "predicted to be occupied by at least",tags$br(),
                                       "the number of runs selected."  , style="color:beige;font-size:13px;padding-left:30px;padding-top:0px;" ),
                      br(),br(),br(),  div(p( "Summary values include 15 simulations\nconsidering similar initial territories.",style="padding-bottom:500px;padding-left:30px;max-width:400px;color:turquoise;text-align:left;font-size:13px;white-space:pre-wrap")),
                       #oct17           
                      tags$head(tags$style("#show_chull_y3{color:beige;font-size:14px;padding-left:30px;margin-bottom:0px;}")),  
                                    tags$head(tags$style("#plot_probability_range_y3{color:beige;border-color:black;font-size:13px;padding-left:40px;margin-top:0px;}"))  
                       ),
                    
                    
                   column(8,
                        br(),
                        absolutePanel(   fixed = TRUE ,draggable = FALSE, top = 400, # if top too low then cant click on tabs between top and xpx below..
                                         left ="60%",  right = "1%", bottom=0,# width ="auto", height = "auto", 
                                         shinyWidgets::addSpinner(plotOutput("mapsim3",    
                                         height="100%" ),color = "purple"),
                                         tags$head(tags$style("#mapsim3{max-height:30000px !important;}")),
                                         tags$style(HTML('#mapsim3{  margin:auto; }')),
                                         br(),br()  )  
                        ), 
                        br(),br(),br(),  br(),br(),br()#col8
                          ),
################ tab2
               tabPanel(div('year 5',style="color:hotpink;font-size:14px; ") ,id="tab_y5", 
                        br(),
                       
                     fluidRow( column(5,
                                      tags$div( "Probability density map:" ,style="color:beige;text-align:right;font-size:15px;"),
                                      textOutput("candsite_idtext_y5") ,
                                      textOutput("init_terr_idtext_y5"),
                                      textOutput("sim_idtext_y5"),
                                      tags$head(tags$style("#candsite_idtext_y5{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:yellow}")),    
                                      tags$head(tags$style("#init_terr_idtext_y5{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:turquoise}")) ,
                                      tags$head(tags$style("#sim_idtext_y5{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:beige}"))
                                     ),
                               column(4, tags$div( "simulated occupancy 5 years after translocation." ,style="color:beige;text-align:left;font-size:15px;"))),
                      
                        
                    column(4, 
                       br(), br(),  br(), br(),   
                       div("Display simulated occupancy*", style="color:beige;font-size:14px;padding-left:30px;font-face:bold;margin-bottom:0px;" ),
                       br(),
                       shinyWidgets::prettyCheckboxGroup ("plot_probability_range_y5",label=NULL, icon = icon("ok", lib = "glyphicon"), status = "default",  plain=TRUE,
                                    choiceValues=c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),
                                    choiceNames =list(HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:lightblue; padding-top:2px;"></i><div style="color:white;">1 to 3 runs</div></div>'),
                                                      HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:yellow;"></i><div style="color:yellow; ">3 to 5 runs</div></div>'),
                                                      HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:orange;"></i><div style="color:orange;">5 to 10 runs</div></div>'),
                                                      HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:red;"></i><div style="color:red;">10 to 15 runs</div></div>')),
                                     selected=c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),width="90%", inline=FALSE,  thick = FALSE,bigger=TRUE ),
                       div("*as the number of runs predicting", tags$br(),  "that a cell is occupied", tags$br(),  "amongst a total of 15 runs."  , style="color:beige;font-size:13px;padding-left:30px;" ),
                       br(),br(),br(),
                       shinyWidgets::prettyCheckboxGroup ("show_chull_y5",label=NULL, choices="Show convex hull", selected=character(0),  width="90%", inline=FALSE,status = "danger", thick = FALSE,bigger=TRUE, shape="round"),
                       div("draw a convex hull around the cells",tags$br(),
                                       "predicted to be occupied by at least",tags$br(),
                                       "the number of runs selected."  , style="color:beige;font-size:13px;padding-left:30px;padding-top:0px;" ),
                              br(),br(),br(),  div(p( "Summary values include 15 simulations\nconsidering similar initial territories.",style="padding-bottom:500px;padding-left:30px;max-width:400px;color:turquoise;text-align:left;font-size:13px;white-space:pre-wrap")),
               
                     tags$head(tags$style("#show_chull_y5{color:beige;font-size:14px;padding-left:30px;margin-bottom:0px;}")),  
                     tags$head(tags$style("#plot_probability_range_y5{color:beige;border-color:black;font-size:13px;padding-left:40px;margin-top:0px;}"))  ,
                     br(),br(),br(),br(),br(),br()
                     ),  
                
                 column(8,
                        br(),
                        absolutePanel(   fixed = TRUE ,draggable = FALSE, top = 400,   left ="60%",  right = "1%", bottom=0,# width ="auto", height = "auto",
                               shinyWidgets::addSpinner(plotOutput("mapsim",    
                                  height="100%" ),color = "red"),
                               tags$head(tags$style("#mapsim{max-height:30000px !important;}")),
                              tags$style(HTML('#mapsim{  margin:auto; }')),
                                br(),br()  ) 

                      
                      
                      
                      )
                   ,br(),br(),br(),br(),br(),br()
                 ),
          
################ tab3 -
               tabPanel(div('year 10',style="color:pink;font-size:14px; ") , id="tab_y10" ,   
                         br(),
                             
                     fluidRow( column(5,
                                      tags$div( "Probability density map:" ,style="color:beige;text-align:right;font-size:15px;"),
                                      textOutput("candsite_idtext_y10") ,
                                      textOutput("init_terr_idtext_y10"),
                                      textOutput("sim_idtext_y10"),
                                      tags$head(tags$style("#candsite_idtext_y10{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:yellow}")),    
                                      tags$head(tags$style("#init_terr_idtext_y10{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:turquoise}")) ,
                                      tags$head(tags$style("#sim_idtext_y10{text-align:left;font-size:14px;font-face:bold;padding-left:20px;color:beige}"))      
                                      ),
                                column(4, 
                                      tags$div( "simulated occupancy 10 years after translocation." ,style="color:beige;text-align:left;font-size:15px;"))
                               ),
                      
                    column(4, 
                                      br(),br(),br(),br(), 
                       div("Display simulated occupancy*", style="color:beige;font-size:14px;padding-left:30px;font-face:bold;margin-bottom:0px;" ),
                       br(),
                       shinyWidgets::prettyCheckboxGroup ("plot_probability_range_y10",label=NULL, icon = icon("ok", lib = "glyphicon"), status = "default",  plain=TRUE,
                                    choiceValues=c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),
                                    
                                    choiceNames =list(HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:lightblue; padding-top:2px;"></i><div style="color:white;">1 to 3 runs</div></div>'),
                                         HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:yellow;"></i><div style="color:yellow; ">3 to 5 runs</div></div>'),
                                         HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:orange;"></i><div style="color:orange;">5 to 10 runs</div></div>'),
                                         HTML('<div style="display:flex"><i class="fa fa-circle"
                                         style="color:red;"></i><div style="color:red;">10 to 15 runs</div></div>')),
                                    selected=c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),width="90%", inline=FALSE,  thick = FALSE,bigger=TRUE ),
                      
                       div("*as the number of runs predicting", tags$br(),  "that a cell is occupied", tags$br(),  "amongst a total of 15 runs."  , style="color:beige;font-size:13px;padding-left:30px;" ),
                       br(), br(), br(), 
                       
                       shinyWidgets::prettyCheckboxGroup ("show_chull_y10", label=NULL,choices="Show convex hull", selected=character(0),  width="90%", inline=FALSE,status = "danger", thick = FALSE,bigger=TRUE, shape="round"),
                            div("draw a convex hull around the cells",tags$br(),
                                       "predicted to be occupied by at least",tags$br(),
                                       "the number of runs selected."  , style="color:beige;font-size:13px;padding-left:30px;padding-top:0px;" ),
                              br(),br(),br(),  div(p( "Summary values include 15 simulations\nconsidering similar initial territories.",style="padding-bottom:500px;padding-left:30px;max-width:400px;color:turquoise;text-align:left;font-size:13px;white-space:pre-wrap")),
                                   tags$head(tags$style("#show_chull_y10{color:beige;font-size:14px;padding-left:30px;margin-bottom:0px;}")),
                                   tags$head(tags$style("#plot_probability_range_y10{color:beige;border-color:black;font-size:13px;padding-left:40px;margin-top:0px;}")) 
                       ) ,
                 column(8,
                        br(),
                      
                         absolutePanel(   fixed = TRUE ,draggable = FALSE, top = 400,   left ="60%",  right = "1%", bottom=0,# width ="auto", height = "auto",
                                          shinyWidgets::addSpinner(plotOutput("mapsim10",    
                                          height="100%" ),color = "pink"),
                                          tags$head(tags$style("#mapsim10{max-height:30000px !important;}")),
                                          tags$style(HTML('#mapsim10{  margin:auto; }')),
                                          br(),br()  )
                      ),
                br(),br(),br(),br(),br(),br() 
),
 
################ tab4
                tabPanel(div('summary table', style="color:turquoise;font-size:14px; "), id="tab_table",  
                        br(),
                        br(),
                        tableOutput("res_table" ) ,
                        div(p( "Summary values include 15 simulations considering similar initial territories.",style="color: turquoise;text-align:right;font-size:13px;"))
                        ) 
 
)# tabsetPanel
)#col7 of analysis tab 
) # col11 for half offset 
)# col 12
)# fluirow 
)), #fluidpage #tabpanel  #2    
 
 
tabPanel(value="refs_tab",
         div("About",style="font-size:14px;"),   
         
   fluidRow(column(10, offset=1, 
                
              navlistPanel(   
                
                    tabPanel(div("Assumptions and Parameters",style='color:beige;text-align:right;'), 
                              fluidRow(style="background-color:#464646;padding:70px;", 
                                   div("Assumptions and Parameters in the Simulation Model",style='color:turquoise;text-align:center;'),  
                                   hr(style="border-color:turquoise;width:80%") ,
                                   div('The App largely uses the spatially-explicit process-based model detailed in Shirley',
                                   span(" et al. ", style='font-style:italic'),
                                   '2015, initially developped and later validated using beaver population data collated Tay and Earn catchments. The model used for simulated range expansion of translocated beaver populations includes two main components: a Geographical Information System storing habitat suitability values, and an individual-based population dynamics module simulating life histories and individual dispersal within the GIS-held landscape.
                                       Important nodes, processes and parameter values included in the model are described below.', 
                                     style='font-size: 13px;color:beige;margin-bottom:20px;text-align:justify;') , 
                              
                                   div('Overview of the Simulation Model', style='font-size: 14px;color:turquoise;padding-top:11px;') ,
                                   textOutput("AboutTheModel"), 
                                   tags$head(tags$style("#AboutTheModel{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")), 
                                   br(),
                                   div('Habitat Quality Values', style='font-size: 14px;color:turquoise;padding-top:11px;'),
                                   textOutput("AboutPars_suit"), 
                                   tags$head(tags$style("#AboutPars_suit{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")), 
                                   br(),
                                   div('Beaver families', style='font-size: 14px;color:turquoise;padding-top:11px;') ,
                                   textOutput("AboutPars_fams0"), 
                                   tags$head(tags$style("#AboutPars_fams0{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")), 
                                   fluidRow( column(10,offset=1,
                                                   column(6,
                                                          textOutput("AboutPars_fams1"), 
                                                          tags$head(tags$style("#AboutPars_fams1{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")) ,
                                                          br(),
                                                          textOutput("AboutPars_fams2"), 
                                                          tags$head(tags$style("#AboutPars_fams2{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}"))),
                                                  column(6,
                                                         textOutput("AboutPars_fams3"), 
                                                         tags$head(tags$style("#AboutPars_fams3{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")) ,
                                                         br(),
                                                         textOutput("AboutPars_fams4"), 
                                                         tags$head(tags$style("#AboutPars_fams4{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")))
                                         )),
                                   div('Dispersal', style='font-size: 14px;color:turquoise;padding-top:30px;') ,
                                   textOutput("AboutPars_disp"), 
                                   tags$head(tags$style("#AboutPars_disp{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")), 
                                   br(),
                                   div('Survival & Reproduction', style='font-size: 14px;color:turquoise;padding-top:11px;') ,
                                   textOutput("AboutPars_surv"), 
                                   tags$head(tags$style("#AboutPars_surv{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}"))
                                   
                        )),
                      
                    tabPanel(div("Computation Time", style='color:beige;text-align:right;'),
                              fluidRow(style="background-color:#464646;padding:70px;", 
                                       div("Computation Time", style='color:yellow;text-align:center;'),  
                                       hr(style="border-color:yellow;width:80%") ,
                                       div("The App allows the execution of complex model processes within a user-friendly interface, which require time for computation. Progress is illustrated by a progress bar at the top of the page, and major stages are displayed under the title as the program executes each step of the algorithm. The App is hosted on a server that may be slow performance when busy with other processes. Factors that may affect computing time include:",
                                                                      style="color:beige;font-size:13px; white-space: pre-wrap;text-align:justify;"),  
                                       textOutput("AboutUsability_comput"), 
                                       tags$head(tags$style("#AboutUsability_comput{color:beige;padding-left:40px;padding-right:40px;padding-top:10px;font-size:13px; white-space:pre-wrap;text-align:justify;}"))  ,
                                       div("Note that the program execution cannot be interrupted until the current computation is completed; calling more processes while the program is running (e.g. clicking to execute simulation while still running) will either not be considered or lead to their subsequent execution once the current process is complete.",
                                                                     style="color:beige;font-size:13px; white-space: pre-wrap;text-align:justify;")   
                        )),
                       
                    tabPanel(div("Main R Packages", style='color:beige;text-align:right;'), 
                              fluidRow(style="background-color:#464646;padding:70px;",  
                                       div("Main R Packages", style='color:pink;text-align:center;'),  
                                       hr(style="border-color:pink;width:80%") ,
                                       textOutput("RPackages") ,
                                       tags$head(tags$style("#RPackages {color:beige;font-size: 13px; white-space: pre-wrap;text-align:center; }"))
                        )),
                    
                    tabPanel(div("References", style='color:beige;text-align:right;'), 
                             fluidRow(style="background-color:#464646;padding:70px;", 
                                      div("References", style='color:yellowgreen;text-align:center;'),
                                      hr(style="border-color:yellowgreen;width:80%") ,
                                      textOutput("refs"),
                                      tags$head(tags$style("#refs{color:beige;font-size:13px; white-space: pre-wrap;text-align:justify; }"))
                        )),
                             
            
                    tabPanel(div("Simulation Outputs", style='color:beige;text-align:right;'),  
                             fluidRow(style="background-color:#464646;padding:70px;", 
                                     div("Simulation Outputs", style='color:orange;text-align:center;'),  
                                     hr(style="border-color:orange;width:80%") , 
                                     div('Output values', style='font-size: 14px;color:orange') ,
                                     textOutput("AboutOutVals"), 
                                     tags$head(tags$style("#AboutOutVals{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")), 
                                     br(),    
                                     div('Bookmarking', style='font-size: 14px;color:orange;padding-top:11px;') ,
                                     textOutput("AboutBookmarking"), 
                                     tags$head(tags$style("#AboutBookmarking{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")), 
                                     div("Note that closing the browser window of the Shiny App will cause the program to end without the possibility to return to the current session status.",
                                          style="color:beige;font-size:13px; white-space: pre-wrap;text-align:justify;"),  
                                     br(),    
                                     div('Downloading Outputs', style='font-size: 14px;color:orange;padding-top:11px;') ,
                                     textOutput("AboutDownloading"), 
                                     tags$head(tags$style("#AboutDownloading{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;}")) ,
                                     br(),   
                                     div('Exported files', style='font-size: 14px;color:orange;padding-top:11px;') ,
                                     textOutput("AboutOutput0"), 
                                     tags$head(tags$style("#AboutOutput0{color:beige;font-size:13px;margin-bottom:10px;white-space:pre-wrap;text-align:left;}")),
                                        fluidRow( column(9,offset=1,
                                                  column(6,
                                                         textOutput("AboutOutput1b"), 
                                                         tags$head(tags$style("#AboutOutput1b{color:beige;font-size:13px; white-space:pre-wrap;text-align:left;}"))   ),  
                                                  column(4,offset=1,
                                                         textOutput("AboutOutput1a"), 
                                                         tags$head(tags$style("#AboutOutput1a{color:beige;font-size:13px; white-space:pre-wrap;text-align:left;}"))   ) )),    
                                        fluidRow(), 
                                        br(),
                                        div('Georeferenced data are compiled into five ESRI shapefiles, each storing the geometric locations and attributes of one of the following outputs:', 
                                             style='font-size: 13px;color:beige;margin-bottom:6px;margin-top:6px;') , 
                                       
                                        fluidRow(column(9,offset=1,
                                               column(6,
                                               textOutput("AboutOutput2a"), 
                                               tags$head(tags$style("#AboutOutput2a{color:beige;font-size:13px; white-space:pre-wrap;text-align:left;}"))  ),
                                               column(4, offset=1,
                                                textOutput("AboutOutput2b"), 
                                                tags$head(tags$style("#AboutOutput2b{color:beige;font-size:13px; white-space:pre-wrap;text-align:left;}"))  ))),
                                        fluidRow(), 
                                        textOutput("AboutOutput_f"),
                                        tags$head(tags$style("#AboutOutput_f{color:beige;font-size:13px; white-space:pre-wrap;text-align:justify;margin-top:6px;}"))  
                                  
                       ))    
 )
))), 


 tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),




              
 tags$head( tags$style(HTML("code {
                display:block;
                padding:0px; 
                font-size:13px; 
            #    word-break:break-all;
             #   word-wrap:break-word;
                #white-space:pre-wrap;
                white-space:  wrap;
                background-color:black;
                 border:0px solid black;
                border-radius:4px; 
                font-family: Helvetica ;
            }"))), 


 
theme = shinythemes::shinytheme("darkly"),
tags$head(tags$style(type = 'text/css', 
                        HTML(  'body {padding-top: 104px;}',
                           '.navbar { background-color: black; 
                           font-size: 13px; height:100px !important; }
                           .tab-panel{ background-color: red; color: white}
                           .navbar-default .navbar-nav > .active > a , 
                           .navbar-default .navbar-nav > .active > a:focus,
                           .navbar-default .navbar-nav > .active > a:hover { color:beige;  background-color: green; }' ))),  
position = "fixed-top")
}
    
 
  


   
 
 
server <-function(input, output, session ) { 

insertUI('#placeholder', ui = tags$p(id = 'text', paste('( reading GIS layers )')), immediate = T)
labs_df <- data.frame(x=c(218593,258952),y=c(822545,847767),labs=c( "Glen Affric" ,"Beauly Firth")  )
 

 source(here::here("appFunctions.R") )
 # source(here::here("appStart.R") )     # initial extent (smaller) and early version - no longer used
 source(here::here("appSepExtents.R") )  # includes beualy to affric extent and major updates
  
#  To mute warnings of possible GDAL/OSR exportToProj4() degradation,
#  use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.
  
 shinyjs::addClass(id = "PageID", class = "navbar-right")
 update_busy_bar(10) 
 print("reactive values init")
 update_busy_bar(50)
 val <- reactiveValues(clickx =NULL, clicky =NULL,clickx2 =rep(NULL,3), clicky2 =rep(NULL,3) ) 
 rv <- reactiveValues()     
 rv$siminfo <- paste0('\n')
 rv$listseglength <- rv$listNsegments <- NA  
 rv$Ndisping <- rv$Msel <- 0
 triggers <- reactiveValues(new_extent=0,new_sim=0, new_simfeat=0) 
 ldat <- reactiveValues()
 ldat$ext_scotcrop <-   ldat$ext_UKbound <- NULL
 ldat$ext_area <- ldat$LocalHab <-  NULL
 plot_data <- reactiveValues(Nfams=NULL, x =NULL, y=NULL, buffer_m=2000,tdat=NULL  ) # static buffer now - 2km?
 plot_data2 <- reactiveValues( x =NULL, y=NULL ) 
 plot_data$legend_obs <- NULL  
 savedat <- reactiveValues( table0=NULL, table=NULL, tdat=NULL, tdat0=NULL, tablepts0=NULL, tablepts=NULL,p1= p0, ptstp_nro= 0  )
 reac_lay <- reactiveValues(OS = 0, OSall=0,hab = 0,  selected_OS=character(0))
 rvsim <- reactiveValues(trigsim=0,mapsim=NULL,mapsim0=NULL,start_pop=NULL, sim_pop=NULL,lay_output=NULL,lay_release_pts=NULL, trig=0, temp_bbox=NULL, lay_release_pts_trig=0, linelength=NULL, Nfams_w2Young=0,Nfams_wYoung=0, Nads=0,Nyoungs =0)
 update_busy_bar(100)     
 print("init 0/4")
  
 rvtext1 <-reactiveValues(extent_infotext=NULL,  famsettled_text=NULL,init_terr_idtext=NULL,init_terr_counter=0,extent_counter=0,sim_counter=0,init_terr_idtext_y0=NULL)         
 rvplot1 <-  reactiveValues(new_ext0=NULL, mapsim_init=NULL, trigger_init_terr =NULL)       
  
 
 rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)

 
 
 
 
 
  
################################################################################################################  init terr re-run
 observeEvent(input$start_reruninit ,{
   rvsim$mapsim <- rvsim$mapsim3 <- rvsim$mapsim10 <- NULL  
   print("trig init sim rerun")
 rvplot1$new_ext0 <- rvplot1$mapsim_init<-rvplot1$mapsim_init0<- rvsim_out$mapsim_output0 <- rvsim$mapsim0_finext<- NULL # added  because weird extent on rerun
 if(is.null(rvplot1$trigger_init_terr) ) {rvplot1$trigger_init_terr <-  2} else {
 if(rvplot1$trigger_init_terr==1) {rvplot1$trigger_init_terr <-  2} else {
 if(rvplot1$trigger_init_terr==2) {rvplot1$trigger_init_terr <-  1} } }
    } )  


 
################################################################################################################ RESET on selecting new extent
observeEvent(input$pick_extent,{ ## reset hab layers and extent point data on selecting new extent
    print("pick extent")
    shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer_all", selected = character(0) )
    shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer", selected = "suitable")  
    shinyWidgets::updatePrettyCheckboxGroup(session, "selected_OS_all", selected = "all roads")
    rvsim <- reactiveValues(trigsim=0, mapsim0=NULL,start_pop=NULL, sim_pop=NULL,lay_output=NULL,lay_release_pts=NULL, trig=0, temp_bbox=NULL, lay_release_pts_trig=0, linelength=NULL)
    rvplot1 <-  reactiveValues(new_ext0=NULL, mapsim_init=NULL, trigger_init_terr =NULL) #sep16       
    rvsim$mapsim <-   rvsim$mapsim3 <-  rvsim$mapsim10 <- NULL#sep17
} , ignoreInit = FALSE)
    



################################################################################################################ RESET point coords when selecting new release protocol
observeEvent(input$Nsites,{ val$clickx2 <- val$clicky2 <-    NULL
}, ignoreInit = TRUE) 
 

 
################################################################################################################  init terr - trigger
 observeEvent(input$start_sim ,{
 rvsim$mapsim <- rvsim$mapsim3 <- rvsim$mapsim10 <- NULL #sep17
 rvplot1$new_ext0 <- rvplot1$mapsim_init<-rvplot1$mapsim_init0<- rvsim_out$mapsim_output0 <- rvsim$mapsim0_finext<- NULL 
 print("trig init sim") 
   if(is.null(rvplot1$trigger_init_terr) ) {rvplot1$trigger_init_terr <-  1} else {
         if(rvplot1$trigger_init_terr==1) {rvplot1$trigger_init_terr  <-  2} else {
                 if(rvplot1$trigger_init_terr==2) {rvplot1$trigger_init_terr <-  1} }}
} )  
 
####################### start the sim - produce release points df
observeEvent(rvplot1$trigger_init_terr,{  
   
  rvtext1$init_terr_counter <-  rvtext1$init_terr_counter+1
  print(" rvtext1$init_terr_counter")
  rvplot1 <-  reactiveValues(new_ext0=NULL, mapsim_init=NULL)    #reset     
      
  print("1- points")
  pts0 <-  savedat$ptstp #  used to be savedat$tablepts when no choice of extent size - now retrieves only pts overlapping etc # oc19
   
  pts0$x <- round(pts0$X,-2)
  pts0$y  <- round(pts0$Y,-2)
 
 pts0 <-   st_as_sf(data.frame(x=pts0$x, y=pts0$y, family=pts0$family), coords=c("x","y"),crs = st_crs(mercproj)) 
    print(pts0)
    rvsim$lay_release_pts <- pts0 #  st_as_sf(data.frame(pts0), coords=c("x","y"), crs=mercproj)
    rvsim$lay_release_pts$layer <- "release points" 
    print("rvsim$lay_release_pts 1")
    rvsim$lay_release_pts_trig <- rvsim$lay_release_pts_trig+1
    print("trigger *")
  } )

   



################################################################################################################ startpop_function - trigger 1 - generate reloc points
observeEvent(rvsim$lay_release_pts_trig,{
   print("release pts ")
   removeUI('#text', immediate = T)
   insertUI('#placeholder', ui = tags$p(id = 'text', paste('( preparing GIS layers )')), immediate = T)
   print("* trigger")
   shinyWidgets::updatePrettyCheckboxGroup(session, "sim_objective", selected = character(0))        
   print("2- trigger simulation")
   return_list_start <- startpop_function()  
   rvsim$start_pop <- return_list_start[[2]]
   rvsim$temp_bbox <- return_list_start[[1]]
   rvsim$suitable<- return_list_start[[3]]
   rvsim$dispersal<- return_list_start[[4]]
   rvsim$lay_release_pts_new <- return_list_start[[5]]
   rvsim$reloc_text <- return_list_start[[6]]
   rvsim$fam.start <- return_list_start[[7]]
   rvsim$ter.start <-  return_list_start[[8]]
   
   rvsim$matNrows <-  return_list_start[[9]]
   rvsim$matNcols <-  return_list_start[[10]]
   rvsim$hab <- return_list_start[[11]]
   rvsim$Ngroups <-   return_list_start[[12]]
   rvsim$Nfams_wYoung <- return_list_start[[13]]  
   rvsim$Nfams_w2Young <- return_list_start[[14]]
   rvsim$Nads <- return_list_start[[15]]   
   rvsim$Nyoungs <- return_list_start[[16]] 
   ## lines between old/new locs  (not used in the end)
    new_pts <- rvsim$lay_release_pts_new      
    old_pts <- rvsim$lay_release_pts[rvsim$lay_release_pts$family %in% new_pts$family,]
    old_pts <- old_pts[order(old_pts$family),]
    new_pts <- new_pts[order(new_pts$family),] 
    fam_pts <- new_pts$family 
    old_pts <- st_cast(st_geometry(old_pts))
    new_pts <- st_cast(st_geometry(new_pts))
    print("***") 
  } , ignoreInit=TRUE, ignoreNULL=TRUE )  
 


################################################################################################################ startpop_function - trigger 2  - output text + update maps mapsim_update_function_init
observeEvent( rvsim$start_pop ,{ 
    Nfams_init <-  plot_data$Nfams 
    fams <-  seq(1,Nfams_init)
    start_terr0  <-  rvsim$start_pop 
    print("**")
    Nfams_actual <- length(start_terr0$layer)
    if (Nfams_actual ==0) { rvtext1$famsettled_text <- "No group could settle."}
    if (Nfams_actual != Nfams_init) { 
      if (Nfams_actual==1) {
        rvtext1$famsettled_text <- paste0( length(start_terr0$layer),   " group settled but ", Nfams_init-length(start_terr0$layer), " could not settle.")   } else {
        rvtext1$famsettled_text <- paste0( length(start_terr0$layer),   " groups settled but ", Nfams_init-length(start_terr0$layer), " could not settle.")   }}
    if (Nfams_actual == Nfams_init) { rvtext1$famsettled_text <- "All groups settled."} 
    shinyWidgets::updatePrettyCheckboxGroup(session, "sim_objective", selected = "starting territories")  # update map initi conditions
    ## end init map here
    ## trigger next computations
 mapsim_update_function_init_out <- mapsim_update_function_init()
 rvplot1$mapsim_init <- mapsim_update_function_init_out [[1]]
 rvplot1$mapsim_init0 <- mapsim_update_function_init_out [[2]]
 removeUI('#text', immediate = T)
}, ignoreInit=TRUE ) 
 
  
 
 


################################################################################################################ reactive/debounced params/values
mgmt.years <- reactive(input$Nyears_sim)   #observe ({ rvsim$mgmt.years <- mgmt.years() }) 
mgmt.reps  <- 15

options(ggrepel.max.overlaps = Inf)

observeEvent(plot_data$Nfams,{ rvsim$Nfams <- plot_data$Nfams }, ignoreInit=FALSE)

dl_data <- reactiveValues(out_Nruns=NULL,out_chulls=NULL,locname=format(Sys.time(), "%d%b%y_%H%M") )  

show_extra_lay <- reactive( c(input$show_extra_lay1,input$show_extra_lay2) )
show_extra_lay_d <- show_extra_lay %>%  debounce(700)

# debounce box ticking
show_suit   <- reactive( input$show_suit)
show_suit_d <- show_suit  %>%  debounce(1200)
 
Nfams_init  <- reactive( input$Nfams_init)
Nfams_init_d<- Nfams_init  %>%  debounce(2000)

observe ({ plot_data$Nfams <- Nfams_init_d() }) 

plot_probability_range_y3 <- reactive( input$plot_probability_range_y3 )
plot_probability_range_y3_d <- plot_probability_range_y3 %>%  debounce(500)
plot_probability_range_y5 <- reactive( input$plot_probability_range_y5 )
plot_probability_range_y5_d <- plot_probability_range_y5 %>%  debounce(500)
plot_probability_range_y10 <- reactive( input$plot_probability_range_y10 )
plot_probability_range_y10_d <- plot_probability_range_y10 %>%  debounce(500) 

  
  
  




 
################################################################################################################ simulate pop growth - trigger 
observeEvent(input$start_sim_growth,{  
   update_busy_bar(20)
      rvsim$mapsim  <-  rvsim$mapsim3  <-  rvsim$mapsim10 <- rvsim$mapsim0_finext <- NULL
      rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)
      rvsim$mgmt.years <- mgmt.years() 
      print(paste0("mgmt years = ",rvsim$mgmt.years))
      rvsim$sim_pop <- simpop_function()
  removeUI('#text', immediate = T)
  insertUI('#placeholder', ui = tags$p(id = 'text', paste('( computing summary statistics )')), immediate = T)
      rvsim$ter_sim  <- rvsim$sim_pop[[1]]
      rvsim$fam_sim  <- rvsim$sim_pop[[2]] 
      rvsim$fam_sim3 <- rvsim$sim_pop[[4]]   
      rvsim$chull_y3_all <- list (st_convex_hull(st_union(rvsim$fam_sim3)), st_convex_hull(st_union(rvsim$fam_sim3[rvsim$fam_sim3$dens> 2,])), st_convex_hull(st_union(rvsim$fam_sim3[rvsim$fam_sim3$dens>4,])), st_convex_hull(st_union(rvsim$fam_sim3[rvsim$fam_sim3$dens>9,])))
      rvsim$chull_y5_all <- list (st_convex_hull(st_union(rvsim$fam_sim )), st_convex_hull(st_union(rvsim$fam_sim[rvsim$fam_sim$dens> 2,])), st_convex_hull(st_union(rvsim$fam_sim[rvsim$fam_sim$dens>4,])), st_convex_hull(st_union(rvsim$fam_sim[rvsim$fam_sim$dens>9,])))
    
          x <- character(0) 
   shinyWidgets::updatePrettyCheckboxGroup(session, "show_chull_y3", selected = x)
   shinyWidgets::updatePrettyCheckboxGroup(session, "show_chull_y5", selected = x)
   shinyWidgets::updatePrettyCheckboxGroup(session, "show_chull_y10", selected = x)
    
      x <- c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") 
   shinyWidgets::updatePrettyCheckboxGroup(session, "plot_probability_range_y3", selected = x)
   shinyWidgets::updatePrettyCheckboxGroup(session, "plot_probability_range_y5", selected = x)
   shinyWidgets::updatePrettyCheckboxGroup(session, "plot_probability_range_y10", selected = x)

   shinyWidgets::updatePrettyCheckboxGroup(session, "show_extra_lay1",  selected=character(0) )
   shinyWidgets::updatePrettyCheckboxGroup(session, "show_extra_lay2",  selected=character(0) )    
     
   rvsim$fam_sim10 <-NULL
   if(!is.null (rvsim$sim_pop[[5]])) {
   rvsim$fam_sim10 <- rvsim$sim_pop[[5]]
   print("rvsim$fam_sim10")
   print(rvsim$fam_sim10)
    rvsim$chull_y10_all <- list (st_convex_hull(st_union(rvsim$fam_sim10)), st_convex_hull(st_union(rvsim$fam_sim10[rvsim$fam_sim10$dens> 2,])), st_convex_hull(st_union(rvsim$fam_sim10[rvsim$fam_sim10$dens>4,])), st_convex_hull(st_union(rvsim$fam_sim10[rvsim$fam_sim10$dens>9,])))
  } 
    print("rvsim$sim_pop") 
    update_busy_bar(30) 
    ## trigger next computations
    rvsim$trig <- rvsim$trig+1   
    print(paste0("trig ", rvsim$trig) ) 
   
}, ignoreInit=TRUE)  
 




 

################################################################################################################ UI stuff

observeEvent(savedat$p1,{ removeUI('#text', immediate = T)})

output$selected_group2 <- renderUI({
  req(rvsim$Nfams)
  if(rvsim$Nfams ==1) return()
  if(rvsim$Nfams ==2)     {
  div (style="color:beige;font-size:12px;",
    shinyWidgets::prettyCheckboxGroup(inputId = "selected_group2" , label=NULL,  choices= c(  "adult female","adult male","subadult"), selected= c(  "adult female","adult male"), 
                         status = "warning",inline=TRUE,thick = FALSE,bigger=TRUE, shape="round")  
    ) }    
}) 

output$Nfams_init_actual<- renderText({paste0(savedat$ptstp_nro) }) # fams actually located within candiate release site
 
output$init_sett_title1 <- renderUI({ if(is.null(rvsim$mapsim))   { HTML(paste0("",br()))} else {HTML(paste0("Probability density map:"))} })
output$init_sett_title2 <- renderText({ if(is.null(rvsim$mapsim))   {paste0("\n \n")} else {" initial settlement on year 0."} })
output$Nyoung_text <- renderText({
       if(input$demog =="all adult pairs") {paste0("Groups released\ndon't include any young.")} else {
            if(input$Nyoungs_perfam[1] == input$Nyoungs_perfam[2]) {Ny_txt<- input$Nyoungs_perfam[1]
                                                                    paste0("Families released all include\n",Ny_txt, " young.")} else {
                                                                         Ny_txt<- paste0(input$Nyoungs_perfam[1]," to ", input$Nyoungs_perfam[2])}
                                                                         paste0("Families released include\n",Ny_txt, " young.") }
})
    
output$extent_infotext <-  renderText({  if(is.null(rvsim$Ngroups)) return() ## oc20 add Ngroups within selected extent
  actualNfams_txt <- "."
    if (plot_data$Nfams != savedat$ptstp_nro) {actualNfams_txt <- paste0("; including ",savedat$ptstp_nro," located within the selected extent and included in the simulation.")}
  
      Nfamstext0 <- paste0(plot_data$Nfams, " initial groups")
      if(plot_data$Nfams<2) {Nfamstext0 <- paste0(plot_data$Nfams, " initial group")}
      if(plot_data$Nfams == Nfams_init_d()) {Nfamstext <-  paste0(Nfamstext0," of beavers (")} else {
      Nfamstext <- paste0(Nfamstext0," of beavers, reduced from ",Nfams_init_d()," due to lack of suitable space (") }
      Ngroups_txt <- paste0(rvsim$Ngroups ," groups")
      if(rvsim$Ngroups<2) {Ngroups_txt <- paste0(rvsim$Ngroups ," group")}
      NgroupswYoung_txt <- paste0(rvsim$Nfams_w2Young+ rvsim$Nfams_wYoung ," groups are")
      if(rvsim$Nfams_w2Young+ rvsim$Nfams_wYoung<2) {NgroupswYoung_txt <- paste0(rvsim$Nfams_w2Young+ rvsim$Nfams_wYoung," group is")}
  paste0(
"The simulated release included ",Nfamstext , input$demog,")",actualNfams_txt ,"\n\n", rvtext1$famsettled_text, 
"\n\nThe initial (settled) population contains ", Ngroups_txt ,", or a total of ", rvsim$Nads+rvsim$Nyoungs ," beavers (", rvsim$Nads, " adults + ",rvsim$Nyoungs," young).\n\n",
NgroupswYoung_txt  ," with young (",rvsim$Nfams_w2Young, " with 2 young and ", rvsim$Nfams_wYoung, " with 1 young)."
) 
})

output$siminfo <- renderText ({ rv$sim_info  }) 
   
############## reac vals for conditional panel
output$showExtraLayers <- reactive( as.numeric(length(rvsim$ter.start)>0)  ) # i.e. when sim_pop exists, value is 1 
outputOptions(output, "showExtraLayers", suspendWhenHidden = FALSE)
output$showExtraLayers1 <- reactive( !is.null(rvsim$sim_pop)  ) 
outputOptions(output, "showExtraLayers1", suspendWhenHidden = FALSE)
   
   
############## saving  
output$save_out_customID <- renderUI({ textInput("location",label=NULL ,placeholder= "Custom output ID (optional)" ) })
output$save_out_button   <- renderUI({  downloadButton("save_out", "" ,class = "btn-success",style = "height:50px;") })
   

############## ABOUT tab text      
ref_SNHReport2020 <- paste0("Beaver Management Report for 2020, Published by NatureScot, Scotland's Nature Agency, in August 2021. Available from www.nature.scot/doc/beaver-management-report-2020\n\n")
ref_SNHReport2015 <- paste0('Shirley, M.D.F., Harrington, L.A. & Mill, A.C. 2015. A model simulating potential colonisation by Eurasian beaver (Castor fiber) following reintroduction to Scotland. Scottish Natural Heritage Commissioned Report No. 814.\n\n')
ref_SNHReport1997 <- paste0("Macdonald, D, Maitland, P, Rao, S, Rushton, S, Strachan, R, and Tattersall, F (1997). Development of a protocol for identifying beaver release sites. SNH Research, Survey & Monitoring 93, Battleby.\n\n")
ref_SNHReport_fromRep1 <- paste0("Beavers in Scotland report to Scottish Government, and other reports are available from: www.nature.scot/professional-advice/safeguarding-protected-areas-and-species/protected-species/protected-species-z-guide/protected-species-beaver/beavers-scotland")
ref_SNHReport_fromRep2 <- paste0("Management Framework for Beavers in Scotland: www.nature.scot/professional-advice/safeguarding-protected-areas-and-species/protected-species/protected-species-z-guide/protected-species-beaver/management")
output$refs <-  renderText({paste0( ref_SNHReport2020,ref_SNHReport2015, ref_SNHReport1997,ref_SNHReport_fromRep1,ref_SNHReport_fromRep2 )})

output$RPackages  <- renderText({
paste0( 
'App building.
shiny; shinyEffects; shinyBS; shinybusy; shinyjs; shinythemes; shinyWidgets;  

Data formatting and export.
Rfast; dplyr; grid; stringr; zip; png; here; 

Geometrical operations and spatial data encoding.
sf; raster; spatstat; lwgeom; rgdal; rgeos; stars; nngeo; 

Graphics and maps creation.
ggplot2; ggspatial; viridis; plotly; ggnewscale; ggrepel; rgee;') }) 
            

## info tab
output$AboutOutVals<- renderText({ paste0('A simulation run includes the step-by-step execution of the population expansion model from year 0 (initial territories) to year 5 (or year 10 if selected). Here, simulation outputs include 15 runs starting from a similar set of initial population territories. 
Once all simulation runs are complete, the number of runs predicting occupancy on a given year is summed for each cell. The total number of runs predicting occupancy is then partitioned into pre-determined categories to facilitate summary mapping: 1 to 3 runs, 3 to 5 runs, 5 to 10 runs, 10 to 15 runs. Outputs can also be visualised and exported as the convex hulls for the set of cells included at a given level of simulated occupancy.
Each cell is then coloured according to the number of runs associated with it. Note that due to the stochasticity in dispersal routes and choice of habitat for building territories, low beaver densities in relation to suitable habitat area will often be associated with low numbers of runs per cell.'
)})
  
output$AboutBookmarking <- renderText({ paste0('Unlike web sites, this Shiny App does not contain the current state of the app in its URL, so the current state of the App can not be shared or accessed again using its URL. A bookmark button was added in the navigation bar to allow returning to a specific release location. Clicking the download button will generate a URL that stores the coordinates for the current candidate release site location. Copy that link and paste it in your browser to reaccess or share the App with focus at the given location.')   
})

output$AboutDownloading <- renderText({ paste0("Simulation outputs can be downloaded at once. When all the simulation runs are completed, click on the download button located in the 'Simulate range expansion' tab, next to the map displaying initial settlement under the simulated range expansion output. Downloaded output files will be referenced using the date at time of download, unless a customised reference is entered in the adjacent text box (optional).")
})

output$AboutOutput_f <- renderText({ paste0("Note that downloading output data after simulating starting territories and before simulating population growth will generate a .zip file containing only the currently existing output.")  })
output$AboutOutput0 <- renderText({ paste0("Simulation outputs are downloaded as a .zip file containing the following data compiled during the session:")  })
output$AboutOutput1a <- renderText({ paste0("- a text document summarising the selected release parameters")  })
output$AboutOutput1b <- renderText({ paste0("- a summary table of annual statistics about the estimated beaver population abundance as number of individuals and territories ")  })
output$AboutOutput2a <- renderText({ paste0("- number of simulation runs estimating cell occupancy on years 3, 5 and 10 (if selected) after release
- convex hulls associated with all relevant combinations of the above")  })
output$AboutOutput2b <- renderText({ paste0("- release points
- initial territories
- extent of the initial release site")  })

output$AboutPars_fams0 <- renderText({ paste0("Each territory block is modelled as supporting one family of beavers. Families can include up to four age classes, each associated with stage-specific properties dictating how the individuals move and contribute to population growth:")  }) 
output$AboutPars_fams1 <- renderText({ paste0("adults:
all individuals older than 2; they are capable of breeding and have settled into their territory - they do not disperse.")  }) 
 output$AboutPars_fams2 <- renderText({ paste0("sub-adults:
1 to 2 year old; they do not breed but will disperse in order to settle into their own territory where they will start the following year as adults.")  })
output$AboutPars_fams3 <- renderText({ paste0("juveniles:
1 year of age, do not disperse nor breed yet, and share the parental territory until the following year.")  })
 output$AboutPars_fams4 <- renderText({ paste0("new young:
individuals born on a given year (under 1 year old) and who share the parental territory.")  })
        
output$AboutPars_suit<- renderText({ paste0("Spatial dynamics of the beavers integrate with an underlying habitat map provided by NatureScot, used to describe territories and movement. The habitat map is divided into three categories of habitat: ‘suitable’ habitat which can support a beaver territory; dispersal habitat which cannot support a territory but can be used by beavers for dispersal; and ‘unsuitable habitat’ which can neither support a territory nor be used by beavers. The minimum total value allowing settlement of a beaver into their new territory block is 8ha of suitable habitat.")  }) 
output$AboutPars_disp<- renderText({ paste0("Dispersal occurs annually when sub-adults leave the family territory block. The maximum length of the annual dispersal route is 21km. When the dispersing sub-adult cannot gather the minimum area of suitable habitat required for its settlement, the individual is considered stranded and removed from the population.")  })
output$AboutPars_surv<- renderText({ paste0("Survival is quantified annually as function of mortality separately for juvenile (0.92) is and adults and sub-adults (0.87). Failed dispersal, which occurs when settlement into a new territory was not completed, also results in the death of the sub-adult. The average breeding probability is 0.6; the average litter size is 1.95, with a maximum value of 9 young produced annually per adult pair.")  })
output$AboutTheModel <- renderText({paste0("The landscape population model of Shirley et al. (2015) predicts the spatial spread of beavers from spatially defined territories and a habitat map. The model is described in detail in the 2015 report but for summary the main processes are outlined here. The population model is dynamic and runs on an annual timestep, the spatial dynamics of the beavers integrate with an underlying habitat map used to describe territories and movement.  The habitat map is divided into three categories of habitat: ‘suitable’ habitat which can support a beaver territory; dispersal habitat which cannot support a territory but can be used by beavers; and ‘unsuitable habitat’ which can neither support a territory nor be used by beavers. The population dynamics are based around individual beaver families. 
This is a process-based model, meaning that life-history processes (population dynamics) of mortality, mating, birth, and dispersal are explicitly modelled at the family level on a yearly timestep. Each family consists of a pair of breeding beavers plus zero or more non-breeding adults, subadults, and juveniles. Each family also has a spatially-referenced territory containing sufficient suitable habitat to support the family. Each year, each beaver has a probability of dying equal to the sex-specific mortality rate. Each beaver family with both a breeding male and a breeding female has a probability of producing a litter of kits. Some of these kits will join the family as subadults, but others will disperse away from the natal territory. 
Dispersal is the most complex process to model since it subsumes a multitude of behaviours. In the model each disperser leaves the natal territory and initially travels through beaver-permeable habitat without attempting to settle (up to a maximum dispersal distance). It then attempts to find or establish a territory. If it finds another beaver territory that it can join as a breeding adult then it does so, otherwise it searches for suitable habitat that falls outside another territory. If it locates sufficient contiguous habitat then it establishes a territory. This beaver must be joined by a dispersing mate before it can breed.")
})
  
output$AboutPars_uncert1  <- renderText({  
 "The distribution of simulated beaver populations is very much dependent on the habitat map supplied by NatureScot. The map categorises habitat quality into three distinct habitat types which may not account for local, small-scale variations in geomorphology that might otherwise limit or help dispersal and settlement. Suggested improvements that may enhance the representativeness of beaver movement and growth within a landscape include:
  - assess the existence and quantify the suspected levels of dispersal potential within the landscape (e.g. between vs. within catchments, along rivers vs. not)
  - inform the range of dispersal abilities between individuals (e.g. reports of individuals dispersing up to x km)
  - estimate the variations in suitability requirements between individuals and given existing options within a given landscape (e.g. territory size depends on surroundings?)
" })
  
output$AboutPars_uncert2  <- renderText({  
 "- components of carrying capacity may change in time and hard to quantify (e.g. water quality, adjacent land use)
  - incorporate an option for lags between simulated releases
  - select release sites according to catchment or ecologically relevant geometries" })

output$AboutPars_uncert3  <- renderText({ "The population model is dynamic and runs on an annual timestep, the Uncertainty vs. added stochastiticy etc" })
 
output$AboutUsability_comput  <- renderText({ 
"
Release protocol 
Only one single group of beavers can be release within a habitat cell; it is currently a model requirement and is representative of practice in the field. The habitat map resolution is of 1ha. The algorithm generating automated release point coordinates aims for the target number of released groups to be distributed 300m apart within the extent and all within suitable habitat, ensuring only one beaver group can be attributed per cell. In other protocols, points are located manually or concentrated around a single starting point. In those cases, the algorithm verifies that the assumptions of distance, suitability and extent boundaries are respected for the set of points, and relocates points to a nearest adequate location when they are not. This process may take time as it is both iterative and considers the changing distribution of all points.

Translocated population size 
Given the individual-based nature of the algorithm, the number of individuals in the simulation affects the amount of computations required to account for all the beaver population. Individual dispersal routes in particular may demand longer computation time when the number of dispersers (sub-adults) increase or when dispersal routes become longer (due to reduced options for instance).

Landscape and habitat
The distribution of habitat quality patches will influence dispersal routes. Failed dispersal may take longer to simulate as each attempt is computed even if unproductive, before the individual is considered stranded (and removed from the simulated population). Dispersal routes will also increase in length when animals have to travel far to find a patch for settlement, or test then leave sub-optimal patches, or have to move within large dispersal-only patches.

Duration of simulation runs 
Computation time will increase with the number of individuals alive in the simulation; in positive growth, the difference in overall computation time between a 5 and 10 years simulation for each run may be significant.
   " })

output$map_logo  <- renderUI({ if( is.null(plot_data$mapzoom0))  return( )
  div(style = "color:yellowgreen;",icon("map")) 
}) 
  
output$plotmap1_infosel <- renderUI({   
   if( is.null(plot_data$x))  return( paste0("\n\n\n")  )
   paste0( "selected\n    X: ",round(as.numeric(plot_data$x),-1),"\n    Y: ",round(as.numeric(plot_data$y),-1))  
}) 
 
output$plotmap1hover_info <- renderUI({ 
  if( is.null(input$plotmap1plot_hover$x))  return( paste0("\n\n\n")  )
  if(!is.null(input$plotmap1plot_hover$x)) #  return(  )
  paste0("current\n    X: ",round(as.numeric(input$plotmap1plot_hover$x),-1),"\n    Y: ",round(as.numeric(input$plotmap1plot_hover$y ),-1))  
}) 
  
 




 


################################################################################################################ indexing of extent/init terr sim/pop growth sim 
observeEvent( input$pick_extent,{ 
    removeUI('#text', immediate = T) 
    rvtext1$extent_counter <- rvtext1$extent_counter+1
    rvtext1$sim_counter <- 0
    rvtext1$init_terr_counter <- 0}, ignoreInit=TRUE, ignoreNULL=FALSE)

observeEvent( input$start_sim_growth,{
    rvtext1$sim_counter <-rvtext1$sim_counter+1
    rvtext1$sim_idtext_output <- rvtext1$sim_idtext 
    }, ignoreInit=TRUE, ignoreNULL=FALSE)
  

observeEvent(rvtext1$extent_counter,{rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)} )
observeEvent(rvtext1$sim_counter,{rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)} ) 
observeEvent(rvtext1$init_terr_counter,{rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)} )

observe({rvtext1$candsite_idtext <- paste0("Site #", rvtext1$extent_counter )})
observe({rvtext1$init_terr_idtext <-  paste0("Initial territories simulation #",  rvtext1$init_terr_counter)}) 
observe({rvtext1$sim_idtext <- paste0("Population growth simulation #", rvtext1$sim_counter )})
  
output$candsite_idtext    <-  renderText({rvtext1$candsite_idtext })
output$candsite_idtext_y0 <-  output$candsite_idtext_y3 <-output$candsite_idtext_y5 <-output$candsite_idtext_y10 <-renderText({rvtext1$candsite_idtext })
output$init_terr_idtext  <-  renderText({  rvtext1$init_terr_idtext })
output$init_terr_idtext_y0 <- output$init_terr_idtext_y3 <- output$init_terr_idtext_y5 <- output$init_terr_idtext_y10 <-  renderText({ rvtext1$init_terr_idtext  })
output$sim_idtext_y0 <-  output$sim_idtext_y3 <-output$sim_idtext_y5 <-output$sim_idtext_y10 <-renderText({rvtext1$sim_idtext }) 



 




################################################################################################################ OUTPUT data
output$reloc_text_fam <-renderText({  rvsim$reloc_text_fam  })
output$reloc_text  <-renderText({  rvsim$reloc_text   })

    
 
output$res_table <- renderTable({if(is.null(rvsim$mapsim3)) return()
  rvsim$sim_pop[[3]]},align = 'c', striped=T , spacing = 'xs',  hover = TRUE, width="100%",colnames = TRUE , rownames = FALSE,digits=0   )
 
output$mapsim_init  <- renderPlot({  if(is.null(rvplot1$mapsim_init)) {return()} else{
  print("map init terr render")
  update_busy_bar(100)
  rvplot1$mapsim_init }
  }  ,  bg="transparent")

output$mapsim10  <- renderPlot({ if(is.null(rvsim$mapsim10) ) {return()} else{ 
  print("map sim 10 render")
  update_busy_bar(100)
  rvsim$mapsim10    } }  ,  bg="transparent")

output$mapsim  <- renderPlot({if(is.null(rvsim$mapsim) ){return()} else{
  print("map sim render")
  update_busy_bar(100)
  rvsim$mapsim  }  }   ,  bg="transparent")

output$mapsim3  <- renderPlot({ if(is.null(rvsim$mapsim3) ){return()} else{
  print("map sim 3 render")
  update_busy_bar(100)
  rvsim$mapsim3  
  }  }   ,  bg="transparent")











 
################################################################################################################ FUN: simpop_function - simulate pop growth - 
simpop_function<-function(){
    print("run sim ")
    hab <- rvsim$hab
    print("hab")
  for(rep in 1:mgmt.reps) {
            cat("  rep",rep , "/",mgmt.reps," ##\n")   
            update_busy_bar(10) 
            families <- rvsim$fam.start 
            print("families")
            #print(families)
            ter <- rvsim$ter.start
            out <- data.frame( year=0:rvsim$mgmt.years, num.fam=0, 
                              num.adt=0, deaths.adt=0, 
                              num.juv=0, juv.born=0, deaths.juv=0, 
                              num.sub=0, deaths.sub=0, 
                              sub.recruit=0, sub.disp=0, sub.fail=0 )
            out$num.fam[out$year==0] <- NROW(subset(families, num.m+num.f>0))
            out$num.adt[out$year==0] <- sum(families$num.m, families$num.f)
              
            routes <- data.frame(bvr=0, cell=0)
           
              if(rvsim$mgmt.years==5) {updval <- 16}
              if(rvsim$mgmt.years==10){updval <- 8}
            
            yearcount <-0
            for (year in 1: rvsim$mgmt.years) {
                                 yearcount <- yearcount+1
                                 removeUI('#text', immediate = T)
                                 insertUI('#placeholder', ui = tags$p(id = 'text', paste('( computing simulation run ', rep,"/15    -    year ",yearcount," )")), immediate = T)
                                 print(paste0('simulation run:', rep,"/15  -  year ",yearcount))
                                 cat("# year", year,"/",rvsim$mgmt.years, " ~ ")
                                 disp.fail <<- deaths.adt <<- deaths.sub <<- deaths.juv <<- births <<- disp.succ <<- 0
                                 updval2 <- 10+yearcount*updval
                                 update_busy_bar(updval2)
                                 print("upd test")
                                 hab2 <- matrix(hab@data[,1], byrow=FALSE, ncol=hab@grid@cells.dim["x"])
                                
                                cat(" subadult survival\n")
                                last.years.young <- sapply(families$young, survival, mort=mort.sub)
                                deaths.sub <- sum(families$young) - sum(last.years.young)
                                
                                cat (" production of young\n")
                                this.years.young <- sapply(families$fam.id, breeding2, fam=families, litter.size=litter.size, breed.prob=breed.prob)
                                births <- this.years.young
                                this.years.young <- sapply(this.years.young, survival, mort=mort.juv)
                                deaths.juv <- sum(births)-sum(this.years.young)
                                families$young <- this.years.young
                              
                               cat (" adult survival\n")
                               mal.surv <- sapply(families$num.m, survival, mort=mort.adt)
                               fem.surv <- sapply(families$num.f, survival, mort=mort.adt)
                               deaths.adt <- sum( (families$num.m - mal.surv) + (families$num.f - fem.surv) )
                               families$num.m <- mal.surv
                               families$num.f <- fem.surv
                              
                               cat ("recruitment\n")
                               recruit <- t(sapply(families$fam.id, recruitment2, young=last.years.young, fam=families, famsize.max=famsize.max))
                               #print(recruit)
                               sub.disp <- recruit[,1]
                               families$num.m <- families$num.m + recruit[,2]
                               families$num.f <- families$num.f + recruit[,3]
                               cat (" dispersing\n")
                       
                               print("ter")
                               disp <- unlist(apply(data.frame(fam.id=1:length(sub.disp), sub.disp), MARGIN=1,function(x) rep(x[1], times=x[2])))
                               
                              if(length(disp)>0) {
                                 disp <- data.frame(fam.id=disp, ind.id=1:length(disp))
                                 print("families") 
                                 for (id in psample(disp$fam.id)) {
                                    disp_compute <- dispersal2(id, fam=families, ter, hab2 , famsize.max, move.max, hab.tot.quality, routes) ##iof hab2
                                    print("disp_compute" )
                          
                          families <- disp_compute [[1]]
                          ter <- disp_compute [[2]]
                          print(families) 
                        }
                      }
                      
                       
                       cat (" territorial release\n")
                       
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
                             #print(out)
                             
                              ter.all2 <- rbind(ter.all2, data.frame(cell=which(values(ter)>0), ter=values(ter)[values(ter)>0], rep.id=rep, year=year ))
                              fam.all2 <- rbind(fam.all2, data.frame(families, rep.id=rep, year=year))
                         
                 
            }# year loop
             # last year values
             out.all    <- rbind(out.all, data.frame(rep.id=rep(rep, NROW(out)), out))#t11
             routes.all <- rbind(routes.all, data.frame(rep.id=rep(rep, NROW(routes)), routes)) 
          
              print("ter.all2")
              #print(ter.all2)
              update_busy_bar(100)
             
        }#rep loop 
  
        removeUI('#text', immediate = T)
        insertUI('#placeholder', ui = tags$p(id = 'text', paste("( gathering simulation output as GIS layers )")), immediate = T)
  
    update_busy_bar(10)
   
    
     ## pdm 5yrs
    fam5 <- fam.all2[fam.all2$year ==  5  ,] %>%   group_by(rep.id,fam.id) %>% summarise(num.adt=num.m +  num.f)
    fam5 <- fam5[fam5$num.adt>0,]
     ter.all2$count <- 1 
    
     ter_occ5<- NULL
    for ( repnum in 1:mgmt.reps){
        occfam <- unique(fam5$fam.id[fam5$rep.id == repnum ])
        ter_occ5 <- rbind(ter_occ5,ter.all2[ter.all2$year ==  5 & ter.all2$rep.id ==repnum & ter.all2$ter %in% occfam,])
    }
    
     ter_occ5 <- ter_occ5 %>% group_by(cell) %>% summarise(dens=sum(count))
      
          print("ter_occ5 ")
          rter2 <- raster(extent(rvsim$hab)) # empty raster
          res(rter2 ) <-  100 
          crs(rter2) <- mercproj
          values(rter2)[ter_occ5$cell] <-  ter_occ5$dens 
        
    update_busy_bar(30) 
    fm2 <- as(rasterToPolygons(rter2, dissolve = F),"sf") # was T -to id the families but not relevant when several reps
    print("fm2")
    #print(fm2) 
     names(fm2)[1] <- "dens" 
    fm2$Nruns <- "1 to 3 runs" 
    fm2$Nruns[fm2$dens>2] <- "3 to 5 runs" ## t44
    fm2$Nruns[fm2$dens>4] <- "5 to 10 runs"
    fm2$Nruns[fm2$dens>9] <- "10 to 15 runs"
     
    
    
    ## pdm 3yrs
      
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
      
        print("ter_occ3 ")
           rter3 <- raster(extent(rvsim$hab)) # empty raster
         res(rter3 ) <- 100 
         crs(rter3) <- mercproj
        values(rter3)[ter_occ3$cell] <-  ter_occ3$dens 
         
     
    fm3 <- as(rasterToPolygons(rter3, dissolve = F),"sf") # was T -to id the families but not relevant when several reps
    print("fm3")
    names(fm3)[1] <- "dens" 
    fm3$Nruns <- "1 to 3 runs" 
    fm3$Nruns[fm3$dens>2] <- "3 to 5 runs"  
    fm3$Nruns[fm3$dens>4] <- "5 to 10 runs"
    fm3$Nruns[fm3$dens>9] <- "10 to 15 runs"
    
    fm10 <- NULL
    if(rvsim$mgmt.years == 10){
    ## pdm 10yrs
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
          rter10 <- raster(extent(rvsim$hab)) # empty raster
         res(rter10 ) <-  100 
         crs(rter10) <- mercproj
         values(rter10)[ter_occ10$cell] <-  ter_occ10$dens 
       
     
    fm10 <- as(rasterToPolygons(rter10, dissolve = F),"sf")  
    print("fm10")
      names(fm10)[1] <- "dens" 
    fm10$Nruns <- "1 to 3 runs" 
    fm10$Nruns[fm10$dens>2] <- "3 to 5 runs"  
    fm10$Nruns[fm10$dens>4] <- "5 to 10 runs"
    fm10$Nruns[fm10$dens>9] <- "10 to 15 runs"
    fm10$layer <- "probabilities over all reps"
    }
     
    fm2$layer <-  "probabilities over all reps"
    fm3$layer <- "probabilities over all reps"
     update_busy_bar(80)
     
     
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
     
      print("out1")
     
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
    print("out.all")
    print(out.all)
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
      
     print("out 2 df ok")  
      
 update_busy_bar(100)
    simpop <- list(ter.all2,fm2,out2, fm3, fm10  )
    return(simpop)
    }
 
   

observeEvent(rvsim$sim_pop,{
        print("summaries post simulated pop")
        req(!is.null(rvsim$sim_pop[[3]])) # result table
          ####Nruns 
      out5 <- rvsim$sim_pop[[2]]  
      out3 <- rvsim$sim_pop[[4]]
      out5$year <- 5
      out3$year <- 3
      print("compiled years 3 + 5")
       out_Nruns <- rbind(out3,out5)
      if(!is.null(rvsim$sim_pop[[5]]))  {
       out10 <- rvsim$sim_pop[[5]]
       out10$year <- 10
       out_Nruns <-  rbind(out_Nruns,out10)  }
       
       dl_data$out_Nruns <- out_Nruns %>% group_by(Nruns,layer,year) %>% summarise()
       print("download Nruns ok")
       print( dl_data$out_Nruns)
       
       ####convex hulls
       runs_cats <- c("1 to 15 runs","3 to 15 runs","5 to 15 runs"  , "10 to 15 runs")
       notNA_y3 <- which(!is.na(do.call("c",rvsim$chull_y3_all)))
       notNA_y5 <- which(!is.na(do.call("c",rvsim$chull_y5_all)))
         
       chull3 <-  st_as_sf(do.call("c",rvsim$chull_y3_all)[notNA_y3])   ## rvsim$chull_y3_all class logical??
       chull3$year <- 3
       chull3$Nruns <-  c("1 to 15 runs","3 to 15 runs","5 to 15 runs"  , "10 to 15 runs") [notNA_y3]
       chull5 <-   st_as_sf( do.call("c",rvsim$chull_y5_all) [notNA_y5])
       chull5$year <- 5
       chull5$Nruns <-  c("1 to 15 runs","3 to 15 runs","5 to 15 runs"  , "10 to 15 runs") [notNA_y5]
       out_chulls <- rbind(chull3,chull5)
       
       if(!is.null(rvsim$chull_y10_all ))  {
        notNA_y10 <- which(!is.na(do.call("c",rvsim$chull_y10_all)))
       chull10 <-  st_as_sf(do.call("c",rvsim$chull_y10_all)[notNA_y10])   ## rvsim$chull_y3_all class logical??
       chull10$year <- 10
       chull10$Nruns <-  c("1 to 15 runs","3 to 15 runs","5 to 15 runs"  , "10 to 15 runs") [notNA_y10] 
        out_chulls <-  rbind(out_chulls,chull10)  }
       
        dl_data$out_chulls <-   out_chulls
        print("download convex hulls ok")
        print(dl_data$out_chulls) 
 
 }, ignoreNULL=FALSE, ignoreInit=FALSE)
  
 
  


 
################################################################################################################ Download output - all at once / zipped 
filelist <-NULL
output$save_out = downloadHandler(
          filename = function(){ paste0("SimOutputs_",dl_data$locname, ".zip") },
          content = function(file){
          # temp  wdir
        owd <- setwd( tempdir())
        on.exit( setwd(owd))
        #unlink(tempdir(), recursive = T)
        txt_sim <- paste0("Population growth was not simulated.\n")
           if(!is.null(rvsim$sim_pop)) {
             txt_sim <- paste0("Population growth was simulated over ",rvsim$mgmt.years, " years for 15 runs considering similar starting territories.\n")
             print("dl_data$locname")
             print( dl_data$locname )
        # result table
          write.csv(rvsim$sim_pop[[3]], paste0("SimTable_",dl_data$locname,".csv"), row.names = FALSE ) 
       
        # shapefiles - Nruns
         shp_loc <- paste0(file.path(tempdir()) , "/SimNruns_",dl_data$locname)
         my_dsn <- paste0( shp_loc,".shp")
         sf::st_write(dl_data$out_Nruns, dsn =  my_dsn, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
        # shapefiles - conv hulls
         shp_loc2 <- paste0(file.path(tempdir()) , "/SimChulls_",dl_data$locname)
         my_dsn2 <- paste0( shp_loc2,".shp")
         sf::st_write(dl_data$out_chulls, dsn =  my_dsn2, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
          }  
        if(!is.null(rvsim$start_pop)){ 
           # result text
            print("text")
             NsitesChoices <- c(" automated release locations."="random_location_across.",
                           " locate each release point."="each_pt_on_map.",
                           " start from a single release point."="single_release_pt" ) 
        
              if(plot_data$Nfams == Nfams_init_d()) {Nfamstext <- paste0(Nfams_init_d(), " initial groups of beavers (")} else {
               Nfamstext <- paste0(plot_data$Nfams, " initial groups of beavers, reduced from ",Nfams_init_d()," due to lack of suitable space (") }
               txt_prot <- paste0("The simulated release included ",Nfamstext , input$demog,").\n\nThe release point distribution protocol was:", names(NsitesChoices[NsitesChoices==input$Nsites]) )
               txt_site <- paste0("The candidate release site was a ", round(as.numeric(input$buffer_sel)/1000,1),"km buffer around centroid point located at x:", 
                                  round(as.numeric(plot_data$x),-1),"  y:",round(as.numeric(plot_data$y),-1),".\nCoordinate reference system: +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs\n\n")
                txt_init <-  paste0("\n",rvtext1$famsettled_text,  
                        "\n\nThe initial (settled) population contains ",rvsim$Ngroups ," groups, or a total of ", rvsim$Nads+rvsim$Nyoungs ," beavers (", rvsim$Nads, " adults + ",rvsim$Nyoungs," young).\n\n",
                        rvsim$Nfams_w2Young+ rvsim$Nfams_wYoung  ," groups are with young (",rvsim$Nfams_w2Young, " with 2 young and ", rvsim$Nfams_wYoung, " with 1 young).") 
                txt_intro <- paste0( "Outputs downloaded on ",format(as.Date(Sys.Date(), "%y-%m-%d"), "%d %B %Y"),".")
               txt_intro2 <- paste0("Simulations generated by a beaver individual-based model within a shiny App available from ")
               txt_intro3 <- paste0(" https://naturalandenvironmentalscience.shinyapps.io/ScotBeaverTranslocation/")
               txt_intro4 <- paste0(" developed by .zelda. at the University of Newcastle for NatureScot (2022)." )     
               txt_sep <- c("__________________________________________________________________________________________")
               txt_out <-   c(txt_sep,txt_intro,txt_intro2,txt_intro3,txt_intro4, txt_sep, txt_prot, txt_site, txt_sep, txt_init,txt_sep,txt_sim,txt_sep)
          print("txt_out") 
          write.csv(txt_out, paste0("SimText_",dl_data$locname,".txt"),  row.names = FALSE,    quote=FALSE)
          # shapefiles - release points
          shp_loc3 <- paste0(file.path(tempdir()) , "/ReleasePoints_",dl_data$locname)
          my_dsn3 <- paste0( shp_loc3,".shp")
          sf::st_write(rvsim$lay_release_pts_new, dsn =  my_dsn3, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE) 
          # shapefiles - init terrs
          shp_loc4 <- paste0(file.path(tempdir()) , "/InitTerrs_",dl_data$locname)
          my_dsn4 <- paste0( shp_loc4,".shp")
          sf::st_write(st_union(rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,]), dsn =  my_dsn4, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE) 
          # shapefiles - init extent
          shp_loc5 <- paste0(file.path(tempdir()) , "/ReleaseSite_",dl_data$locname)
          my_dsn5 <- paste0( shp_loc5,".shp")
          sf::st_write(ldat$ext_area_sel, dsn =  my_dsn5, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)   
          }   
  print("zip files")
      filelist <-   list.files(tempdir(),pattern=dl_data$locname)    
    # Zip them up
      zip( file, filelist)  
      },
      contentType = "application/zip"
  ) 
   
 # unlink(tempdir(), recursive = T)
 #       print(list.files(tempdir(),pattern=dl_data$locname))
 



################################################################################################################ Download output - file name
observeEvent(input$location,{ 
    req(!is.null(input$location))
    if( input$location=="") {dl_data$locname <- format(Sys.time(), "%d%b%y_%H%M")  } else { 
        dl_data$locname <- str_replace_all(input$location, "[^[A-Za-z1-9,]]", "") %>% str_squish(.) 
       }
   print("stored name")
   print(dl_data$locname)
   },ignoreNULL=FALSE, ignoreInit=FALSE) 




 
################################################################################################################ FUN: startpop_function - simulate init terrs - 
startpop_function <-function(){ 
         print("crop hab map for init terrs")
         print(mercproj)
         pts  <- rvsim$lay_release_pts
         pts  <-  st_transform(pts,mercproj)
         print("0 - pts")
         update_busy_bar(5)      
         temp_bbox0 <-  st_bbox(st_as_sf(st_buffer(   st_union(pts)   , 5000)   ))    ## 3km buffer around init point location to stat with? is that enough
         st_crs(temp_bbox0) <- mercproj 
        
        window <- cbind(c(temp_bbox0$xmin, temp_bbox0$xmax ),c(temp_bbox0$ymin, temp_bbox0$ymax )) 
        window <- cbind(window[c(1,1,2,2,1),1], window[c(1,2,2,1,1),2])
        window <- matrix.to.SpatPolys(window, proj= CRS(mercproj)) #hab@proj4string)
        update_busy_bar(10) 
        mapr.mask <-  mask(map.r, window) # was mask
        
        #mapr.mask[is.na(mapr.mask)] <- 0       #oc6 - longer to process
         removeUI('#text', immediate = T)
         insertUI('#placeholder', ui = tags$p(id = 'text', paste('( extracting habitat quality values for the extent )')), immediate = T)
         
        update_busy_bar(15) 
        temp_bbox0 <- as(as(extent(alignExtent(temp_bbox0, mapr.mask, snap='near')), "SpatialPolygons"), "sf")
        st_crs(temp_bbox0) <- mercproj
        print("temp_bbox0")
        update_busy_bar(20) 
        hab2 <- as(mapr.mask, "SpatialPointsDataFrame")
        print("hab2")
        #print(summary(hab2))
        hab2 <- hab2[hab2@data$layer>0,] 
        #   hab2 <- hab2[!is.na(hab2@data$layer),]                # leave in or not??  
        gridded(hab2) <- TRUE
        hab <- as(hab2, "SpatialGridDataFrame") # just grids
        update_busy_bar(40)    
        
        matNrows <- hab@grid@cells.dim[1] 
        matNcols <- hab@grid@cells.dim[2] 
        
        ## create territories
        ter <- ter0 <- matrix (0, ncol=matNcols , nrow=matNrows, byrow=F)
        rter <- raster(extent(hab)) # empty raster
        res(rter ) <- c(100,100)
        crs(rter)  <- mercproj
        cellnum    <- matrix (seq(1:length(ter)),  ncol=matNcols , nrow=matNrows, byrow=F)  
           
        ## release pts 
        tay <- SpatialPointsDataFrame(coords=cbind(x=as.numeric(st_coordinates(pts)[,1]),y=as.numeric(st_coordinates(pts)[,2])), data=data.frame(family=seq(1:nrow(pts) ), num.m =1,num.f = 1))
        crs(tay)  <-  hab@proj4string
        tay$index <- getGridIndex(tay@coords, map@grid)# getGridIndex(tay@coords, hab@grid)
        tay0      <- tay
        
       removeUI('#text', immediate = T)
       insertUI('#placeholder', ui = tags$p(id = 'text', paste('( validating release point coordinates )')), immediate = T)
            
        
           taysf <- as(tay,"sf")
           st_crs(taysf)   <- mercproj
           ## distance between pts
           too_close <- NULL
           if( input$Nsites ==   "single_release_pt") {
             too_close <- tay$family 
           } else {
            if( input$Nsites !=  "random_location_across") {
              for (id in 1:nrow(tay)){
              too_close <- unique(c(too_close,which(as.numeric(st_distance(taysf)[id,])<300 & as.numeric(st_distance(taysf)[id,])>0) ) )} } }#t00 was 800m
           
           print("too_close?")
           print(too_close ) 
           print("tay")
           print(tay)
        
        suitable  <- st_union(st_as_sf(hab2[hab2$layer ==2,]))
        dispersal <- st_union(st_as_sf(hab2[hab2$layer ==1,]))
        pts_options <- st_union(dispersal, suitable)
        print("pts otions")
        st_crs(suitable) <- st_crs(dispersal) <- mercproj
        
        ptronews <-    NULL
        to_suit <- which( mapr.mask[tay] <2)
        
        
        reloc_text1 <- reloc_text2 <- NULL
        if(length(too_close)>1) { reloc_text1 <-  paste0("Families ",  paste0(as.character(too_close), collapse=" & ")   ," were initially located <1km apart.")}
        if(length(to_suit)>1)   { reloc_text2 <- paste0("Families ",paste0(as.character(to_suit), collapse=" & ")," were not initially located in suitable habitat.")}
        if(length(to_suit)==1)   { reloc_text2 <- paste0("Family ", to_suit ," was not initially located in suitable habitat.")}
        
        reloc_text <-  paste0(reloc_text1,"\n",reloc_text2)
        print(reloc_text)
        
        
        ### relocate pts
       if( input$Nsites !=  "random_location_across") {
                  reloc_vec <- seq(-800,800,200)
                  prev_moved <- NULL
                  while( any(mapr.mask[tay] <2) | length(too_close)>1){
                             print("moving release pt")
                             print( too_close) 
                             print(length(too_close))
                             relocdf <- data.frame(family = tay$family ,movex =0,movey=0) 
                               
                              if(length(too_close)>1){
                                  print("move further") 
                                  too_close1 <- NULL
                                   if(length(to_suit)>0 ){ # move in priority the points in suit hab - will only apply for the first round
                                          print("to_suit1")         
                                          #print(to_suit)
                                          too_close1 <- too_close[too_close %in% to_suit]
                                          to_suit  <- to_suit[!to_suit %in% too_close1 ] 
                                          print("too_close1")
                                          #print(too_close1)
                                          print("to_suit2")         
                                          #print(to_suit)
                                          }  
                                  if(!is.null( too_close1) ) {print("too close is to suit") # shoud do only first time
                                                    too_close <- too_close1 }   else { # move pts previously moved in priority
                                                                                                              if( !is.null(prev_moved) ){
                                                                                                                    too_close  <- too_close[too_close %in% prev_moved]    }  # move pt previously moved again
                                                                                                              if(  is.null(prev_moved) ){
                                                                                                                    too_close <- too_close[sample(length(too_close),length(too_close)-1)] } } # random pick one of two pts too near
                                
                                
                                
                                    relocdf$movex[too_close] <-  sample(reloc_vec,length(too_close) )
                                    relocdf$movey[too_close] <-  sample(reloc_vec ,length(too_close) )
                              }
                              
                             if(length(to_suit)>0 ){
                                        print("move to suit")
                                        relocdf$movex[to_suit] <-  sample(reloc_vec ,length(to_suit) )
                                        relocdf$movey[to_suit] <-  sample(reloc_vec ,length(to_suit) )
                              }
                            
                             ros <-  relocdf$family[ which( relocdf$movex!=0 | relocdf$movey !=0) ]
                             prev_moved <- ros
                             upd <-0
                             upd_pt <-  100/length(ros) # tot Npts
                            
                             for(ro in  ros){
                                       ptronews <- NULL
                                       ptro0 <- st_as_sf(tay[tay$family == ro,])
                                       ptro <-   st_as_sf(st_sfc(  st_point(c( st_coordinates(ptro0)[,1]+relocdf$movex[ro], st_coordinates(ptro0)[,2]+relocdf$movey[ro])), crs=mercproj ) ,ptro0 %>% st_drop_geometry() )
                                       st_crs(ptro) <- st_crs(ptro0) <- mercproj 
                                       print("ptro") 
                                       rad  <- 300
                                       add_radius <- 200 
                                       ptronew <- st_intersection(st_buffer(ptro,rad )  , st_cast(st_as_sf(suitable),"POINT"))
                                         
                                        nro=nrow(ptronew)
                                        while(nro==0){ # i.e. didnt find an intersection
                                               rad  <- rad + add_radius
                                               print("larger radius required")
                                                       if(rad  <  1500){
                                                            ptronew <- st_intersection(st_buffer(ptro,rad)  , st_cast(st_as_sf(suitable),"POINT"))
                                                            nro <- nrow(ptronew)
                                                            print("ptronew loop")
                                                         }
                                                         
                                                       if( rad > 1500){ 
                                                             print("cant find hab")
                                                             rvsim$reloc_text_init <- ("Selected point location too far from enough suitable habitat - review coordinates.")
                                                             ptronew <- ptro 
                                                             nro <- 1} 
                                           }
                                       
                                       ptronew <- ptronew [sample(seq(1,nrow(ptronew)),1),] # pick one point in suit hab
                                       print("ptronew")
                                       #print(ptronew) 
                                       ptronews <- rbind(ptronew,ptronews) 
                                      
                                       tay_reloc <- SpatialPointsDataFrame(coords=cbind(x=as.numeric( st_coordinates(ptronews)[,1]),y=as.numeric( st_coordinates(ptronews)[,2])), data=ptronews%>% st_drop_geometry())
                                       tay_reloc$index  <- getGridIndex(tay_reloc@coords, hab@grid)
                                       crs(tay_reloc) <-  crs(tay) <-  mercproj
                                         
                                       print("new tay")
                                       tay <- tay[!tay$family %in% tay_reloc$family,] 
                                       tay <- rbind(tay,tay_reloc)
                                    
                                       taysf <- as(tay,"sf")
                                       st_crs(taysf)   <- mercproj
                                     
                                       too_close <- NULL
                                       for (id in 1:nrow(tay)){
                                         too_close <- unique(c(too_close,which(as.numeric(st_distance(taysf)[id,])<800 & as.numeric(st_distance(taysf)[id,])>0) ) )}
                                       
                                       print("too_close?")
                                       print(too_close )
                                       to_suit <- which( mapr.mask[tay] <2)
                          } ## here
                  } 
        }
        print("post loop")
        tay@data$index <-  getGridIndex(tay@coords, hab@grid)
        print(tay)  
            
         
          ## move pts to new locations here 
          num.fam <- nrow(tay@data)
          families <- data.frame(fam.id=tay$family, num.m=tay$num.m, num.f=tay$num.f, young=0,   qual=0, stringsAsFactors=FALSE)
          
          removeUI('#text', immediate = T)
          insertUI('#placeholder', ui = tags$p(id = 'text', paste('( simulating initial settlement )')), immediate = T)
         
          if(input$demog =="some adult pairs, some families") {
             if(num.fam<4) {num_with <- 1} else {
                num_with <- round(nrow(families)/2)}
        
          families$young[1:num_with] <- sample(seq(input$Nyoungs_perfam[1], input$Nyoungs_perfam[2]) ,num_with, replace=TRUE)}
        
         if(input$demog =="all families") {families$young <- sample(seq(input$Nyoungs_perfam[1], input$Nyoungs_perfam[2]) ,nrow(families), replace=TRUE)}
          
        start   <-   tay$index 
        print("start cells")
        update_busy_bar(50)
          
          
        adjacent(raster(ter0), start, directions=8 ) %>% ### initial - locate points within suit of disp
          data.frame() %>% 
          mutate(id=sapply(from,function(x) which(x == start)),
                 hab=as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE) [to]) %>% 
        #  filter(hab >0) %>% #sep20
          distinct(to, .keep_all=TRUE) ->
          start_df
        print("start_df")
        print(start_df)
        update_busy_bar(60)
        ## note the adjacent()function omits NA cells? if NA in map inds can jump over..
        ## sample one start point per family within disp or suit, and distinct
        sd1 <- start_df %>% group_by(id  ) %>% summarise(maxhab=max(hab, na.rm=TRUE))#cell= to[ hab==max(hab)])
        print("sd1 -1")
        #print(sd1)
        sd1 <- merge(sd1, start_df)
        print("sd1 -2")
        #print(sd1)
        sd1 <- sd1[sd1$hab == sd1$maxhab,]
        print("sd1 -3")
        #print(sd1) #sep20
        startcells <- sd1[!is.na(sd1$id),] %>% group_by(id ) %>% summarise(cell= unique(to)[sample(1:length(unique(to)),1)] )
          
         
        startid_suboptimal <- which(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE) [startcells$cell]<2)
         
        
        #ter[startcells$cell] <- startcells$id
        ter[unique(start_df$from)] <- unique(start_df$id) # start from relocated point?
        print("startcells")
        
        
        ## store in families and ter
        families$qual <- sapply(families$fam.id,function(x, hab, ter) sum(hab[ter==x]), hab=as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE) , ter=ter)
        families$qual[families$qual==1] <- 0
        update_busy_bar(70) 
        Ntries <- 0  
         
        print("families init") 
        
          families <- families[order(families$fam.id),]
          #start_dfsum <- start_df %>% group_by(id) %>% summarise(habqual= sum(hab) ) 
           grow <- families$fam.id[which(families$qual < hab.tot.quality) ] ## id ## , pairs=FALSE = no col from/to
        if(length(grow)>0) {  
                 ids <-  sample(grow,length(grow))
                 for(id in ids) {
                     Ntries <- famqual <- 0 
                           
                     while ( Ntries<30    ) {   
                          
                         while(famqual<hab.tot.quality){
                              
                              families$qual[id] <- famqual <- sum(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id][as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id]>1], na.rm=T) # only count suitable hab
                              print(paste0("family # ",id))
                              current <- which(ter==id)
                              adj0 <- adj1 <- adj2 <-   NULL
                                                
                              adj0 <- adjacent(rter, current, directions=8, pairs=FALSE)
                              adj2 <-  adj0[which(ter[adj0]==0 & as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[adj0]==2)] #exclude occupied territory and null/dispersal habitat
                              adj1 <-  adj0[which(ter[adj0]==0 & as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[adj0]==1)] #exclude occupied territory and null/suitable habitat
                                       
                             if(length(adj2)>0 & Ntries<30) { 
                                       adj1 <- NULL
                                       Ntries<- Ntries+1
                                       print("adj2")
                                       best <-  adj2[sample(length(adj2),1)] #randomly pick one of the best adjacent pixels
                                       ter[best] <- id
                                  #print("best")
                                  #print(best)
                                  #print(ter[best])
                                  #print("qual")
                                  #print(sum(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id][as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id]>1]), na.rm=TRUE   )
                                  #print(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id])
                                  ## here - erase extrxa solo patches if
                                  
                                   
                                          families$qual[id] <- famqual <- sum(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id][as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id]>1], na.rm=TRUE) # only count suitable hab
                                          patches <- NULL
                                          xy <- xyFromCell(rter, which(ter==id & as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)==2)  )
                                          patches <- st_cast(  st_as_sf ( st_buffer(st_sfc(  st_multipoint(x = matrix(xy, ncol=2))) ,75)), "POLYGON"  )
                                     
                                          if(nrow(patches)>1) { 
                                                   print("split territories")           ## several territories split = keep going
                                                   larger_patch_cells <-  unique(cellFromXY(rter,data.frame(st_coordinates(patches[which(st_area(patches)==max(st_area(patches))),])[,c(1,2)]) ))
                                                   larger_patch_cells <- larger_patch_cells[larger_patch_cells+1 %in% which(ter==id)] #### +1?????
                                                            if(length(larger_patch_cells)>7)  { 
                                                                  Ntries <- 31
                                                                  ter[ter==id] <- 99     
                                                                  famqual <- 99                     ## keep tried terr unoccupiable for that round?
                                                                  ter[larger_patch_cells] <- id
                                                                  families$qual[id] <-  sum(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id][as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id]>1], na.rm=TRUE) # only count suitable hab
                                                             } else { 
                                                                   families$qual[id] <- famqual <- length(larger_patch_cells)*2
                                                                    }
                                                }
                                            
                                        
                                          if(families$qual[id] > hab.tot.quality & nrow(patches)==1) { ## one territory not split = win
                                                               Ntries <- 31
                                                               famqual <- 99
                                                               print("settled")
                                                               grow <- grow[grow!=id]              }
                              } 
                                 
                            
                           if(length(adj2)==0 & length(adj1)>0 & Ntries<30) {
                                     Ntries<- Ntries+1
                                     famqual <- 0
                                     secbest <- adj1[sample(length(adj1),1)]
                                     ter[secbest] <- id           # id territory for next try but it is disp not suit so dont count in famdf
                            }
                                      
                            if(length(adj1)==0 & length(adj2)==0 |  length(adj1)>0 & Ntries>29 ) {
                                                   print("no option")
                                                   Ntries <- 31
                                                   famqual <- 99
                                                       #   if(length(adj)==0) {
                                                                grow <- grow[grow!=id]
                                                              #  families$qual[id] <- sum(as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)[ter==id])
                                                                ter[ ter==id ] <- 0 # free territory
                             } 
                        print(paste0("try #",Ntries) )
                                 } # while
                           } #if N<30next try
                    #  else { next }
                            print("next")
                  } #id
                      
         } #if(length(grow)>0) 
         
          
        ## omit starting points within dispersal from territories and moves into dispersal previsouy recorded
        ter[startcells$cell[startid_suboptimal] ] <- 0 
        ter[ter==99] <- 0
        ter[ as.matrix(hab, ncol=matNcols , nrow=matNrows, byrow=FALSE)<2] <- 0
        
          update_busy_bar(80) 
        
        print("families post")
        #print(families)
        #if(any(families$qual < hab.tot.quality)) stop("Some territories are not complete")
        families$num.m[which(families$qual < floor(hab.tot.quality/2))] <- 0
        families$num.f[which(families$qual < floor(hab.tot.quality/2))] <- 0
        families$young[which(families$qual < floor(hab.tot.quality/2))] <- 0
        # sapply(families$fam.id, release.territory, fam=families, ter=ter)
         
        not_settling <- length(families$fam.id[which(families$num.m+families$num.f==0)])
        print("not_settling") # is it foolproof here ?
        print(not_settling)
         
        update_busy_bar(90) 
           
        values(rter) <- c(matrix(c(ter), ncol=matNcols , nrow=matNrows, byrow=FALSE))
          
        tay <- tay[order(tay$family),]
        newpts <- st_as_sf ( st_cast(st_sfc( st_multipoint(x = matrix(xyFromCell(rter, tay$index), ncol=2)) ) ,"POINT"),family=   tay$family)   # removed rev()
        st_crs(newpts) <- mercproj       
         
             removeUI('#text', immediate = T)
             insertUI('#placeholder', ui = tags$p(id = 'text', paste('( gathering GIS layers for initial territories )')), immediate = T)
      
         rter_sf <-  rter
         rter_sf[rter==0] <- NA # faster to omit all empty cells..
         
         ### foolproof if none settle
     if (length(which(!is.na(values(rter)))) >0){
              fm <- as(rasterToPolygons( rter_sf , dissolve = T),"sf") #  TRUE, takes longerbut useful  
              fm <- fm[fm$layer>0,]
              if(nrow(fm)>0){
                  names(fm)[1] <- "family"
                  fm$layer <- "starting territories"
                }
               famgrid <- fm
               taysf <- as(tay,"sf")
               st_crs(taysf)   <- mercproj  
      }
           
        if(nrow(famgrid)>0){ temp_bbox <- st_as_sfc( st_bbox( st_buffer( st_union(famgrid,taysf) ,  1500)  )) } else { temp_bbox <- st_as_sfc(temp_bbox0)}
        startpop <- famgrid
         
        Nfam_settled <- nrow(families[which(families$num.f+families$num.m>1),])
        Nfam_youngs <- length(which(families$young==1) )
        Nfam_2youngs<- length(which(families$young==2) )
         
        print("fam check")
        Nads <- sum(families$num.f,families$num.m)
        Nyoungs<- sum(families$young) 
        
         

return_list_start  <- list(temp_bbox,startpop,suitable,dispersal, newpts,reloc_text, 
                           families,rter,matNrows,matNcols,
                           hab ,Nfam_settled,Nfam_youngs,Nfam_2youngs,Nads,Nyoungs)
  
print("output ok")
update_busy_bar(100)
return(return_list_start)
} 
  
 
 
 
observeEvent(rvsim$trig,{   # once simulations completed
  print("cut to new bbox?")
  removeUI('#text', immediate = T)
  insertUI('#placeholder', ui = tags$p(id = 'text', paste('( preparing GIS layers for simulated population range )')), immediate = T)
 
  print("N years")
  print(rvsim$mgmt.years)
    
 if(!is.null(rvsim$temp_bbox)) {  
           update_busy_bar(40)
           rvsim$LocalBackLayers <- rvsim$LocalBackText <-rvsim$LocalBackPoints <- rvsim$riv <- NULL
          
       # recompute extent that contains all simulated terr + recrop etc with buffer #oc23
       buf_y3_y5 <- st_buffer(st_union(st_union(rvsim$fam_sim3),st_union(rvsim$fam_sim )   ),1400 )
            if(rvsim$mgmt.years == 5) { sim_bbox <-   st_as_sfc(  st_bbox ( buf_y3_y5))} #st_buffer(st_union(st_as_sfc(st_bbox(rvsim$fam_sim),st_as_sfc(st_bbox(rvsim$fam_sim3)))) ,400) }
            if(rvsim$mgmt.years == 10) {  sim_bbox <-   st_as_sfc(  st_bbox ( st_buffer(st_union(buf_y3_y5,st_union(rvsim$fam_sim10)  ),1400 )))} #st_buffer(st_union(st_as_sfc(st_bbox(rvsim$fam_sim),st_as_sfc(st_bbox(rvsim$fam_sim3)),st_as_sfc(st_bbox(rvsim$fam_sim10)))) ,400) }
             st_crs(sim_bbox) <- mercproj
             rvsim$LocalHab <- st_crop(HabMapLayers,sim_bbox) 
      rvsim$ext_UKbound <- st_crop(UKbound , sim_bbox)  
      rvsim$ext_scotcrop <- st_crop(scotcrop,sim_bbox) 
      update_busy_bar(50) 
      rvsim$riv  <- st_union( st_crop(rivlines,sim_bbox) ) 
      rvsim$LocalHab <- rbind(rvsim$LocalHab, st_sf(geometry= rvsim$riv , layer="river") )  # **               
      rvsim$LocalBackLayers  <- st_crop(BackLayers,sim_bbox) 
      rvsim$LocalBackText    <- st_intersection(BackText,sim_bbox) 
      rvsim$LocalBackPoints  <- st_intersection(BackPoints,sim_bbox)  
      print("background layers sim  map")
      update_busy_bar(70)
         
            
         rvsim_out$new_ext_sim <-  sim_bbox # rvsim$mapsim_update_function_y5_out[[4]]
         rvsim_out$mapsim_output0 <-  mapsim_output_function() # rvsim$mapsim_update_function_y5_out[[5]] uses new extent new_ext_sim 
      
         rvsim$mapsim_update_function_y5_out <- mapsim_update_function_y5()   
         
         rvsim$sim_terr3_bufpts  <- rvsim$mapsim_update_function_y5_out [[6]]
         rvsim$sim_terr_bufpts   <- rvsim$mapsim_update_function_y5_out [[7]]
         rvsim$sim_terr10_bufpts <- rvsim$mapsim_update_function_y5_out [[8]]
           print("rvsim$sim_terr3_bufpts")
           #print(rvsim$sim_terr3_bufpts)
         rvsim$mapsim <- rvsim$mapsim_update_function_y5_out[[1]] # t44
         update_busy_bar(80)
         rvsim$mapsim3 <- rvsim$mapsim_update_function_y5_out [[2]]
         update_busy_bar(90)
         rvsim$mapsim10 <- rvsim$mapsim_update_function_y5_out [[3]]
         rvsim$mapsim0_finext <- rvsim$mapsim0_finext0 <-  mapsim0_finext() # nb- is built with rvsim_out$mapsim_output0 + init terrs  
         print("pre crop extra layers")
            extra_HabLayers_c <- st_crop(extra_HabLayers_l ,  sim_bbox )   ## here can be topo issue with original shp cant use intersection() so using crop but mh
          
        catch_polys  <- st_crop(intcatch$geometry,  sim_bbox )
        catch_polys0 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$lay_release_pts_new ))) ==0]),year=0)
        catch_polys3 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$sim_terr3_bufpts     ))) ==0]),year=3)   ### here could vary covered polygon with Nruns level BUT requires computing for each level as in chulls not just nruns - just copy computation structure here and change to select only relevant level for mapout per yr and level- no time now
        catch_polys5 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$sim_terr_bufpts     ))) ==0]),year=5)
            rvsim$catch_polys <- rbind( catch_polys0, catch_polys3, catch_polys5 ) 
           
         if(rvsim$mgmt.years == 10) { 
                  catch_polys10 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$sim_terr10_bufpts   ))) ==0]),year=10)
                  rvsim$catch_polys <- rbind( rvsim$catch_polys, catch_polys10) } 
            extra_HabLayers_c <- st_crop(extra_HabLayers_c , st_buffer(sim_bbox,-50))   ## here can be topo issue with original shp cant use intersection()?.
            freqdam <-  extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "frequent to pervasive" ]
         if(length(freq) >0) { extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "frequent to pervasive" ] <- st_union(st_line_sample( st_cast(freqdam, "LINESTRING"),  density = 1/150, type = "regular") ) }
            occasdam <- extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "occasional" ] 
         if(length(occasdam) >0) { extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "occasional" ] <- st_union(st_line_sample( st_cast(occasdam, "LINESTRING"),  density = 1/150, type = "regular") ) }
      
          rvsim$extra_HabLayers <- extra_HabLayers_c #  
          rm(extra_HabLayers_c)
          print("extra catchments etc layers cropped")
          update_busy_bar(100)
 }
 removeUI('#text', immediate = T)
  })

 
####################################################################################################### FUN: mapsim_output_function - generate final map  
mapsim_output_function <-function(){
         # nb value of new_ext_sim comes from <<
         new_ext_sim <- rvsim_out$new_ext_sim 
         ggplot() + 
                  geom_sf(data=st_crop(scotcrop, new_ext_sim), col=NA, fill="white")+
                  geom_sf(data=st_crop(scotcrop, new_ext_sim), col=NA, fill="steelblue", alpha=.9)+
                  geom_sf(data=st_crop(UKbound, new_ext_sim) ,  fill="beige", col="steelblue", alpha=.9, size=1)   +
                  geom_sf(data= st_crop(rivlines, new_ext_sim), col=alpha("skyblue",.7),fill=alpha("steelblue",.8), size=2)+ 
                  geom_sf(data=st_crop(rivlines, new_ext_sim), col=alpha("steelblue",.9),fill=alpha("skyblue",.7), size=1)+ 
                  geom_sf(data=st_crop(HabMapLayers [ HabMapLayers$layer %in% c("suitable","dispersal") ,], new_ext_sim) , mapping = aes(fill= layer  ),col=NA , alpha=.3, size=0 )+
                  geom_sf(data=st_crop(HabMapLayers [HabMapLayers$layer %in% c("suitable" ) ,], new_ext_sim) , mapping = aes(  col= layer), fill=NA   )+
                  geom_sf(data=st_crop(BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , new_ext_sim), mapping = aes( col= layer), size=1,alpha=.8 )+
                  geom_sf(data=st_crop(BackLayers [ BackLayers$layer %in% c("b road","a road","minor road","primary road") ,], new_ext_sim), col= "yellow", size=.8,alpha=.8 )+
                  geom_sf(data=st_crop(BackLayers [ BackLayers$layer %in% c("railway line")  ,], new_ext_sim), linetype="dotted", col= 1, size=3  )+
                  geom_sf(data=st_crop(BackPoints , new_ext_sim) , aes(col=layer,  shape=layer), size=3, stroke=1 )+
                  geom_sf_text(data=st_crop(BackText [  BackText$layer2 != "Railway Station",], new_ext_sim), aes(label=label, col=layer2  )  )+
                  geom_sf_label(data=st_crop(BackText [ BackText$layer2  == "Railway Station",], new_ext_sim), aes(label=label),fill=alpha(1,.4), col="beige")+
                                        
                 scale_fill_manual(values=c(   "suitable"=alpha("turquoise3",.3), "dispersal"=alpha("olivedrab4",.3), "unsuitable"=alpha("honeydew3",.4), "river"="steelblue",
                                               "a road"="brown", "b road"="orange", "admin line"="black",
                                               "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=alpha("lightseagreen",.7),
                                               "marsh"="green","woodland region"=alpha("olivedrab4",.7),"urban region"="purple","rivers line"=alpha("skyblue",.8),"lakes region"=alpha("steelblue",.9),
                                               "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                               "Loch"=0, "Forest"=0,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                 
                 scale_colour_manual(values=c( "suitable"="turquoise3", "dispersal"="olivedrab4", "unsuitable"=0, "river"="steelblue","a road"="brown", "b road"="orange", "admin line"="black",
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
                  axis.title.y = element_blank(),   
                  panel.border = element_rect(fill =NA) ,
                  plot.background = element_rect(fill = "transparent", colour = NA)) 
}


mapsim0_function <-function(){
    ext_scotcrop <- rvsim$ext_scotcrop
    ext_UKbound  <- rvsim$ext_UKbound
    sim_bbox     <- rvsim_out$new_ext_sim  
    rivers       <- rvsim$riv   
    LocalHab     <- rvsim$LocalHab
    LocalBackLayers <- rvsim$LocalBackLayers
    LocalBackPoints <- rvsim$LocalBackPoints
    LocalBackText <- rvsim$LocalBackText
     
       ggplot() + 
              geom_sf(data =  ext_scotcrop, col=NA, fill="white")+
              geom_sf(data =  ext_scotcrop, col=NA, fill="steelblue", alpha=.9)+
              geom_sf(data =  ext_UKbound ,  fill="beige", col="steelblue", alpha=.9, size=1)   +
              geom_sf(data=   rivers, col=alpha("skyblue",.7),fill=alpha("steelblue",.8), size=2)+ 
              geom_sf(data=   rivers, col=alpha("steelblue",.9),fill=alpha("skyblue",.7), size=1)+ 
              geom_sf(data= LocalHab [LocalHab$layer %in% c("suitable","dispersal") ,] , mapping = aes(fill= layer  ),col=NA , alpha=.3, size=0 )+
              geom_sf(data= LocalHab [LocalHab$layer %in% c("suitable" ) ,] , mapping = aes(  col= layer), fill=NA   )+
                                      geom_sf(data=LocalBackLayers [LocalBackLayers$layer %in% c("b road","a road","minor road","primary road") ,] , mapping = aes( col= layer), size=1,alpha=.8 )+
                                      geom_sf(data=LocalBackLayers [LocalBackLayers$layer %in% c("b road","a road","minor road","primary road") ,], col= "yellow", size=.8,alpha=.8 )+
                                      geom_sf(data=LocalBackLayers [LocalBackLayers$layer %in% c("railway line")  ,], linetype="dotted", col= 1, size=3  )+
                                      geom_sf(data=LocalBackPoints  , aes(col=layer,  shape=layer), size=3, stroke=1 )+
                                      geom_sf_text(data=LocalBackText [ LocalBackText$layer2 != "Railway Station",], aes(label=label, col=layer2  )  )+
                                      geom_sf_label(data=LocalBackText [LocalBackText$layer2  == "Railway Station",], aes(label=label),fill=alpha(1,.4), col="beige")+
                                      
              scale_fill_manual(values=c(  "suitable"=alpha("turquoise3",.3), "dispersal"=alpha("olivedrab4",.3), "unsuitable"=alpha("honeydew3",.4), "river"="steelblue",
                                            "a road"="brown", "b road"="orange", "admin line"="black",
                                            "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=alpha("lightseagreen",.7),
                                            "marsh"="green","woodland region"=alpha("olivedrab4",.7),"urban region"="purple","rivers line"=alpha("skyblue",.8),"lakes region"=alpha("steelblue",.9),
                                            "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                            "Loch"=0, "Forest"=0,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
             
               scale_colour_manual(values=c(   "suitable"="turquoise3", "dispersal"="olivedrab4", "unsuitable"=0, "river"="steelblue","a road"="brown", "b road"="orange", "admin line"="black",
                                               "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=0,
                                               "marsh"="green","woodland region"=0,"urban region"=0,"rivers line"=alpha("skyblue",.8),"lakes region"= 0,
                                               "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                               "Loch"="steelblue4","Forest"="green4",  "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
            
               scale_shape_manual(values=c( "TV or radio mast or tower"=13,"Wind powered generator"=8,"foreshor region"=0,
                                            "marsh"=8,"woodland region"=0,"urban region"=0,"rivers line"=0,"lakes region"= 0,
                                            "minor road"=0,"primary road"=0,"railway line"=0,
                                            "Loch"=0,"Forest"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
          
               scale_size_manual(values=c("a road"=2, "b road"=1.5, "admin line"=1,"admin seed"=5,
                                          "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                          "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                          "minor road"=1,"primary road"=1.5,"railway line"=2,
                                          "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )+  
              annotation_scale(location = "bl")+
              coord_sf(crs = mercproj, expand=F) +
              theme(legend.position = "none", axis.title.x = element_blank(), 
              axis.title.y = element_blank(),   
              panel.border = element_rect(fill =NA) ,
              plot.background = element_rect(fill = "transparent", colour = NA)) 
}     





####################################################################################################### switch tab once init terrs simulation is complete
observeEvent( rvplot1$mapsim_init,{         
   updateTabsetPanel(session, "PageID", selected = "sim_tab") ## switch panel tab on start_sim - after computation
}, ignoreInit=TRUE, ignoreNULL=TRUE)

 

  

 

####################################################################################################### show_extra_lay_d() - generate final maps with hab+catch+dam cap layers as required  
### catch intcatch and dm cap layers  to plot
observeEvent( show_extra_lay_d()  ,{
  req(!is.null(rvsim$extra_HabLayers) )
  print("add layers:")
  print(show_extra_lay_d())

    show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
    print(nrow(show_lays))   
    catch_polys <- NULL
    if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
    
           rvsim$mapsim0_finext  <-  rvsim$mapsim0_finext0   + new_scale_colour() +  new_scale("shape") + 
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      geom_sf(data= catch_polys[catch_polys$year == 0,] ,  col=NA, fill=alpha("brown",.4) )+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
             print("show y0 ")  
 
    
 chull_y3_all <- chull_y5_all <- chull_y10_all <- NULL   
       
         if(!is.null(rvsim$mapsim10)) {
           if( "Show convex hull" %in% input$show_chull_y10 ) {
             chull_y10_all <- rvsim$chull_y10_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y10_d()), na.rm=TRUE) ]] }
             sim_terr10 <- rvsim$sim_terr10_bufpts[which(rvsim$sim_terr10_bufpts$Nruns %in% plot_probability_range_y10_d()),]
             rvsim$mapsim10  <-  rvsim_out$mapsim_output0  + new_scale_colour() +   new_scale("shape") +
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr10, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 10,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y10_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
         print("show y10") }  
       
           if( "Show convex hull" %in% input$show_chull_y3 ) {         
             chull_y3_all <- rvsim$chull_y3_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y3_d()), na.rm=TRUE) ]] }
             sim_terr3  <- rvsim$sim_terr3_bufpts[which(rvsim$sim_terr3_bufpts$Nruns %in% plot_probability_range_y3_d()),]
            
            rvsim$mapsim3  <-  rvsim_out$mapsim_output0  + new_scale_colour() +   new_scale("shape") +
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="brown2"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="brown2"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr3, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 3,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y3_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
       print("show y3")
              
                
           if( "Show convex hull" %in% input$show_chull_y5 ) {
              chull_y5_all <- rvsim$chull_y5_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y5_d()), na.rm=TRUE) ]] }
              sim_terr  <- rvsim$sim_terr_bufpts[which(rvsim$sim_terr_bufpts$Nruns %in% plot_probability_range_y5_d()),]
         
          rvsim$mapsim  <- rvsim_out$mapsim_output0  + new_scale_colour() +  new_scale("shape") +   
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 5,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y5_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
          print("show y5 - add extra layers with ")
          print(plot_probability_range_y5_d())
             
  }, ignoreInit=TRUE, ignoreNULL=FALSE)
  
      
      

####################################################################################################### base (final extent) map 
output$mapsim0_finext  <- renderPlot({ if(is.null(rvsim$mapsim3) ){ return() } else{ 
   print("map sim 0 render")
   update_busy_bar(100) 
  rvsim$mapsim0_finext  
 } }  ,  bg="transparent")
 
  # run on init to update with checked boxes but after maps are created a first time
observeEvent(input$sim_objective,{
   req(!is.null(rvplot1$mapsim_init0)) 
   print("sim obj update")
   rvplot1$mapsim_init <- mapsim_update_function_init()[[1]] 
},ignoreInit=TRUE, ignoreNULL=FALSE) 

 

####################################################################################################### update 5y final output map with GIS (user-selected) layers - trigger = probability range
observeEvent(plot_probability_range_y5_d(),{
     req(!is.null(rvsim$mapsim) )
     chull_y5_all <- NULL   
     show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
     catch_polys <- NULL
     if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
       
     if( "Show convex hull" %in% input$show_chull_y5 ) {
             chull_y5_all <- rvsim$chull_y5_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y5_d()), na.rm=TRUE) ]] }
             sim_terr  <- rvsim$sim_terr_bufpts[which(rvsim$sim_terr_bufpts$Nruns %in% plot_probability_range_y5_d()),]
             rvsim$mapsim   <- rvsim_out$mapsim_output0  + new_scale_colour() +  new_scale("shape") +   
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr , aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 5,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y5_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
         print("show y5 - show Nruns")
         print(show_extra_lay_d())
},ignoreInit=TRUE, ignoreNULL=FALSE)  


####################################################################################################### update 3y final output map with GIS (user-selected) layers - trigger = probability range    
observeEvent( plot_probability_range_y3_d(),{
   req(!is.null(rvsim$mapsim3) )          ### recode to update just one map at a time not 3
   chull_y3_all <- NULL   
   show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
   catch_polys <- NULL
   if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
         
           if( "Show convex hull" %in% input$show_chull_y3 ) {
             chull_y3_all <- rvsim$chull_y3_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y3_d()), na.rm=TRUE) ]] }
             sim_terr3 <- rvsim$sim_terr3_bufpts[which(rvsim$sim_terr3_bufpts$Nruns %in% plot_probability_range_y3_d()),]
             print("check y3 chulls")
              
             rvsim$mapsim3  <- rvsim_out$mapsim_output0  + new_scale_colour() +  new_scale("shape") +   
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr3, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 5,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y3_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
         print("show y3")    
   },ignoreInit=TRUE, ignoreNULL=FALSE)  


####################################################################################################### update 10y final output map with GIS (user-selected) layers - trigger = probability range  
observeEvent( plot_probability_range_y10_d(),{
   req(!is.null(rvsim$mapsim10) )
   chull_y10_all <- NULL   
   show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
   catch_polys <- NULL
    if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
         
           if( "Show convex hull" %in% input$show_chull_y10 ) {
            chull_y10_all <- rvsim$chull_y10_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y10_d()), na.rm=TRUE) ]] }
             sim_terr10 <- rvsim$sim_terr10_bufpts[which(rvsim$sim_terr10_bufpts$Nruns %in% plot_probability_range_y10_d()),]
             rvsim$mapsim10  <-rvsim_out$mapsim_output0  + new_scale_colour() +  new_scale("shape") +   
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr10, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 5,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y10_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
         print("show y10")    
    },ignoreInit=TRUE, ignoreNULL=FALSE)  
   



####################################################################################################### FUN: mapsim_update_function_init - generate side panel map on TAB 2 incl. release points and init terrs
mapsim_update_function_init <-function(){ ## update with initial conds # small side map
          update_busy_bar(10)
          mapsim_init0 <-   rvplot1$mapsim_init0
          start_terr <- rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,]       
          print(" retrieving init computations")
              lay_release_pts <- lay_release_pts_new <- NULL
              lay_release_pts_new <- rvsim$lay_release_pts_new 
              lay_release_pts <- rvsim$lay_release_pts   
       
            print("new ext?")  
            print(is.null(mapsim_init0))
           
       if( is.null(mapsim_init0)) {
                        print(" -yes new extent")
                        start_terr <- rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,]       
                        print(" retrieving start_terr ")
                        lay_release_pts <- lay_release_pts_new <- NULL
                        lay_release_pts_new <- rvsim$lay_release_pts_new #t55
                        lay_release_pts <- rvsim$lay_release_pts  ## text layer - LOCAL
                  
                        print("recompute extent init terrs map")
                       if(nrow(start_terr)>0) {    mapped_features <-   st_union(st_union(start_terr),st_union(lay_release_pts)  )
                                                       mapped_bbox <- st_bbox(mapped_features) }
                       if(nrow(start_terr)==0) {   mapped_features <-    st_union(lay_release_pts)     
                                                       mapped_bbox <- st_bbox(mapped_features) }
                        update_busy_bar(20)   
                                
                            new_ext0 <-  st_as_sfc(  st_bbox (st_buffer(mapped_features,3000) ) )  
                            st_crs(new_ext0) <- mercproj 
                            rvplot1$LocalHab <- st_crop(HabMapLayers,new_ext0) 
                            rvplot1$ext_UKbound  <- st_crop(UKbound , new_ext0)  
                            rvplot1$ext_scotcrop <-  st_crop(scotcrop,new_ext0) 
                            rvplot1$riv  <- st_union( st_crop( rivlines,new_ext0) ) 
                            print("rvplot1$riv")
                            #print(rvplot1$riv)
                            update_busy_bar(30) 
                            if(length(rvplot1$riv)>0){ rvplot1$LocalHab <- rbind(rvplot1$LocalHab, st_sf(geometry= rvplot1$riv , layer="river") )   }               
                            rvplot1$LocalBackLayers  <- st_crop(BackLayers,new_ext0) 
                            rvplot1$LocalBackText    <- st_intersection(BackText,new_ext0) 
                            rvplot1$LocalBackPoints    <- st_intersection(BackPoints,new_ext0) 
                                    
                       ## {} was here
                        mapsim_init0 <-  ggplot( ) + 
                                            geom_sf(data =  rvplot1$ext_scotcrop, col=NA, fill="white")+
                                            geom_sf(data =  rvplot1$ext_scotcrop, col=NA, fill="steelblue", alpha=.9)+
                                            geom_sf(data =  rvplot1$ext_UKbound ,  fill="beige", col="steelblue", alpha=.9, size=1)   +
                                            geom_sf(data= rvplot1$LocalHab [rvplot1$LocalHab$layer %in% c("suitable","dispersal"),] ,  mapping = aes(fill= layer , col= layer), alpha=.4, size=0 ) + 
                                            geom_sf(data=   rvplot1$riv , col=alpha("skyblue",.7),fill=alpha("steelblue",.8), size=2)+ 
                                            geom_sf(data=   rvplot1$riv , col=alpha("steelblue",.9),fill=alpha("skyblue",.7), size=1)+  
                                            geom_sf(data=rvplot1$LocalBackLayers [rvplot1$LocalBackLayers$layer %in% c("b road","a road","minor road","primary road")  ,], col= "yellow", size=.8,alpha=.8 )+
                                            geom_sf(data=rvplot1$LocalBackLayers [rvplot1$LocalBackLayers$layer %in% c("railway line")  ,], linetype="dotted", col= 1, size=3  )+
                                            geom_sf(data=rvplot1$LocalBackPoints  , aes(col=layer,  shape=layer), size=3, stroke=1 )+
                                            geom_sf_text (data=rvplot1$LocalBackText [ rvplot1$LocalBackText$layer2 != "Railway Station",], aes(label=label, col=layer2, size=layer2 )   )+
                                            geom_sf_label(data=rvplot1$LocalBackText [rvplot1$LocalBackText$layer2  == "Railway Station",], aes(label=label),fill=alpha(1,.4), col="beige")+ 
                        
                                            scale_fill_manual(values=c(   "suitable"=alpha("turquoise3",.3), "dispersal"=alpha("olivedrab4",.3), "unsuitable"=alpha("honeydew3",.4), "river"="steelblue",
                                                                           "a road"="brown", "b road"="orange", "admin line"="black",
                                                                           "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=alpha("lightseagreen",.7),
                                                                           "marsh"="green","woodland region"=alpha("olivedrab4",.7),"urban region"="purple","rivers line"=alpha("skyblue",.8),"lakes region"=alpha("steelblue",.9),
                                                                           "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black" ,
                                                                            "Loch"=0, "Forest"=0,  "named places"=0,  "road names"=0, "local altitude"="purple"   ))+
                                         
                                             scale_colour_manual(values=c( "suitable"=0, "dispersal"=0, "unsuitable"=0, "river"="steelblue","a road"="brown", "b road"="orange", "admin line"="black",
                                                                           "TV or radio mast or tower"="blue","Wind powered generator"="blue","foreshor region"=0,
                                                                           "marsh"="green","woodland region"=0,"urban region"=0,"rivers line"=alpha("skyblue",.8),"lakes region"= 0,
                                                                           "minor road"=alpha("grey",.6),"primary road"="red","railway line"="black",
                                                                           "Loch"="steelblue4","Forest"="green4",  "named places"=1, "road names"="grey30", "local altitude"="purple"   )  )+  
                                           
                                             scale_shape_manual(values=c( "TV or radio mast or tower"=13,"Wind powered generator"=8,"foreshor region"=0,
                                                                           "marsh"=8,"woodland region"=0,"urban region"=0,"rivers line"=0,"lakes region"= 0,
                                                                           "minor road"=0,"primary road"=0,"railway line"=0,
                                                                            "Loch"=0,"Forest"=0,  "named places"=0, "road names"=0, "local altitude"=17)  )+  
                                      
                                             scale_size_manual(values=c( "a road"=2, "b road"=1.5, "admin line"=1, "admin seed"=5,
                                                                            "TV or radio mast or tower"=3,"Wind powered generator"=3,"foreshor region"=0,
                                                                            "marsh"=3,"woodland region"=0,"urban region"=0,"rivers line"=2,"lakes region"= 0,
                                                                            "minor road"=1,"primary road"=1.5,"railway line"=2,
                                                                            "Loch"=4.5,"Forest"=4, "named places"=4, "road names"=3, "local altitude"=3)  )+  
                                              annotation_scale(location = "bl")+
                                              coord_sf(crs = mercproj, expand=F) +
                                              theme(legend.position = "none", axis.title.x = element_blank(), 
                                              axis.title.y = element_blank(),  axis.text = element_blank(),    
                                              panel.border = element_rect(fill =NA) ,
                                              plot.background = element_rect(fill = "transparent", colour = NA)) 
       }
               
               
              print("new ext 1")        
              update_busy_bar(60)               
                       
       if(length(input$sim_objective)==0) {mapsim_init  <-  mapsim_init0 } else {
         
               req(!is.null(mapsim_init0)  & length(input$sim_objective)>0  )
               print("sim objective")
               print(input$sim_objective) 
               plotfills <- rep("magenta",Nfams_init_d()) 
              
               mapsima <- mapsimb <- mapsimc <-  mapsim_init0
                       
               if("starting territories" %in% input$sim_objective) {
                     print("start")
                     start_terr$family <-  as.factor(start_terr$family)
                     mapsima <-mapsimb<- mapsimc + new_scale_fill() +  scale_fill_manual("", values=plotfills, drop=FALSE)+
                                                   geom_sf(data=start_terr ,  fill= "magenta" ,col=1)
                     mapsim_init <-  mapsima +  coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")
               }
               
              update_busy_bar(80) 
              print("new ext 4")
              if("release points (initial)" %in% input$sim_objective) {
                    lay_release_pts$family <- as.factor(lay_release_pts$family)
                    mapsima  <- mapsimb  + new_scale_fill() +  scale_fill_manual("", values=rainbow(10)[seq(1,Nfams_init_d()   )], drop=FALSE)+ 
                                           geom_sf(data=lay_release_pts, aes(fill= family  ), stroke=2,   size=3,  shape=21)#+  
                    mapsim_init <-  mapsima +  coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")
               } 
               update_busy_bar(90) 
               if("release points (relocated)" %in% input$sim_objective) {
                    lay_release_pts_new$family <- as.factor(lay_release_pts_new$family)
                     mapsima  <- mapsima + geom_sf(data=lay_release_pts_new,   size=5, stroke=2,  shape=4)  
                     mapsim_init <-  mapsima +  coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")
                           }
       } 
 
 update_busy_bar(100) 
 return(list(mapsim_init,mapsim_init0))
}
  



#######################################################################################################
mapsim0_finext <-function(){ ## only start terr but on final extent
          start_terr <- rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,]      
          start_terr$family <-  as.factor(start_terr$family) 
          lay_release_pts <- rvsim$lay_release_pts  
          start_terr$family <-  as.factor(start_terr$family) 
          lay_release_pts$family <- as.factor(lay_release_pts$family)
           
          mapsim0_finext <-  rvsim_out$mapsim_output0 + geom_sf(data=start_terr ,  fill="magenta" ,  col=1 )+ ##oct15
                                                 coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                                 theme(axis.text=element_blank())
   return(mapsim0_finext)    
}

 
        
 


mapsim_update_function_y5 <-function(){ 
          start_terr <- rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,]      
          start_terr$family <-  as.factor(start_terr$family)

          print("N years")
          print(input$Nyears_sim)
           
          ## y5
          sim_terr      <- rvsim$fam_sim
          sim_terr$dens <- as.numeric(as.character(sim_terr$dens))#t0
          sim_terr      <- st_buffer(st_centroid(st_cast(sim_terr, "POLYGON") ) %>% group_by(Nruns) %>% summarize (geometry=st_union(geometry) ),40, byfeature=TRUE) # t11
          print("y5") 
       
          ## y3
          sim_terr3      <- rvsim$fam_sim3
          sim_terr3$dens <- as.numeric(as.character(sim_terr3$dens))#t0
          sim_terr3      <-  st_buffer(st_centroid(st_cast(sim_terr3, "POLYGON") ) %>% group_by(Nruns) %>% summarize (geometry=st_union(geometry) ),40, byfeature=TRUE) # t44
          print("y3")

                  
          lay_release_pts        <- rvsim$lay_release_pts  
          start_terr$family      <-  as.factor(start_terr$family) 
          lay_release_pts$family <- as.factor(lay_release_pts$family)
    
          
          sim_terr10 <- NULL
          if(rvsim$mgmt.years == 10){  
          sim_terr10 <- rvsim$fam_sim10
          sim_terr10$dens <- as.numeric(as.character(sim_terr10$dens))#t0
          print("sim_terr10 ")
          sim_terr10 <-  st_buffer(st_centroid(st_cast(sim_terr10, "POLYGON") ) %>% group_by(Nruns) %>% summarize (geometry=st_union(geometry) ),40, byfeature=TRUE) # t11
          #print(sim_terr10)
          }
         
              ## to save in rvsim and reuse instead of rerun
              sim_terr3_bufpts <- sim_terr3
              sim_terr_bufpts <- sim_terr
              sim_terr10_bufpts <- sim_terr10
     
      mapsim_output0   <- rvsim_out$mapsim_output0  
      if(is.null( mapsim_output0)) {  mapsim_output0 <- mapsim_output_function() } # is this required? - redraw with new extent inc all terrs
      chull_y3_all <- chull_y5_all <- chull_y10_all <- NULL 
         
            
   if(length(plot_probability_range_y5_d())==0)   {sim_terr$Nruns  <- NA  } else {
         sim_terr <-  sim_terr[ sim_terr$Nruns %in%  plot_probability_range_y5_d(),]  
         if( "Show convex hull" %in% input$show_chull_y5 ) {
            chull_y5_all <- rvsim$chull_y5_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y5_d()), na.rm=TRUE) ]]} }
         
          
   if(length(plot_probability_range_y3_d())==0)   {sim_terr3$Nruns <- NA } else {
        sim_terr3 <-  sim_terr3[ sim_terr3$Nruns %in%  plot_probability_range_y3_d(),]
        if( "Show convex hull" %in% input$show_chull_y3 ) {
           chull_y3_all <- rvsim$chull_y3_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y3_d()), na.rm=TRUE) ]]}  }
    
                   
              
   mapsim10  <- NULL
   if(rvsim$mgmt.years == 10){
       if(length(plot_probability_range_y10_d())==0)   {sim_terr10$Nruns <- NA } else {
         sim_terr10 <-  sim_terr10[ sim_terr10$Nruns %in%  plot_probability_range_y10_d(),]
           if( "Show convex hull" %in% input$show_chull_y10 ) {
              chull_y10_all <- rvsim$chull_y10_all[[min(which(c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs") %in%  plot_probability_range_y10_d()), na.rm=TRUE) ]]}  }
              print("y10")
              mapsim10  <-mapsim_output0  +
                                       new_scale_fill() + new_scale_color()+  
                                       scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                       breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                       scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                       breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                       geom_sf(data=sim_terr10, aes(fill=Nruns,col=Nruns), alpha=1)+
                                       geom_sf(data=chull_y10_all, fill=alpha("magenta",.1),col=1)+
                                       coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                       theme(axis.text=element_blank())
          }
  
  #y5
          mapsim  <- mapsim_output0  +
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data=chull_y5_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
              print("y5")
  #y3
          mapsim3  <- mapsim_output0  +
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="brown2"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="brown2"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr3, aes(fill=Nruns,col=Nruns), alpha=.8)+
                                      geom_sf(data=chull_y3_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
              print("y3")
return(list(mapsim,mapsim3,mapsim10,
            mapsim_output0,mapsim_output0, # to put something in need updating indexing of returned output without that value now
            sim_terr3_bufpts ,sim_terr_bufpts , sim_terr10_bufpts  
       ))
} 
    


####################################################################################################### show convex hull on final output map 3yr  
observeEvent(input$show_chull_y3,{
        req(!is.null(rvsim$mapsim3))
        catch_polys <- NULL
  if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
   
   show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
   chull_y3_all <- NULL
   if( "Show convex hull" %in% input$show_chull_y3 ) { 
             chull_y3_all <-  rvsim$chull_y3_all[[ min(which( c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs")   %in%  plot_probability_range_y3_d()),na.rm=TRUE)  ]] }
             sim_terr3  <- rvsim$sim_terr3_bufpts[which(rvsim$sim_terr3_bufpts$Nruns %in% plot_probability_range_y3_d()),]
             rvsim$mapsim3   <-  rvsim_out$mapsim_output0  + new_scale_colour() +   new_scale("shape") +
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr3, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 3,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y3_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())                                      
}, ignoreInit=TRUE, ignoreNULL=FALSE)
   



####################################################################################################### show convex hull on final output map 5yr
observeEvent(input$show_chull_y5,{
       req(!is.null(rvsim$mapsim ))
       catch_polys <- NULL
  if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
  
    show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
    chull_y5_all <- NULL
    if( "Show convex hull" %in% input$show_chull_y5 ) { 
             chull_y5_all <-  rvsim$chull_y5_all[[ min(which( c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs")   %in%  plot_probability_range_y5_d()),na.rm=TRUE)  ]] }
             sim_terr  <- rvsim$sim_terr_bufpts[which(rvsim$sim_terr_bufpts$Nruns %in% plot_probability_range_y5_d()),]
             rvsim$mapsim   <-  rvsim_out$mapsim_output0  + new_scale_colour() +   new_scale("shape") +
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr , aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 5,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y5_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
   }, ignoreInit=TRUE, ignoreNULL=FALSE)
  


####################################################################################################### show convex hull on final output map 10yr      
observeEvent(input$show_chull_y10,{
       req(!is.null(rvsim$mapsim10 ))
       catch_polys <- NULL
   if ("water body intercatchments" %in% show_extra_lay_d()) { catch_polys <- rvsim$catch_polys } 
   
   show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
   chull_y10_all <- NULL
   if( "Show convex hull" %in% input$show_chull_y10 ) { 
             chull_y10_all <-  rvsim$chull_y10_all[[ min(which( c( "1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs")   %in%  plot_probability_range_y10_d()),na.rm=TRUE)  ]] }
             sim_terr10 <- rvsim$sim_terr10_bufpts[which(rvsim$sim_terr10_bufpts$Nruns %in% plot_probability_range_y10_d()),]
             rvsim$mapsim10  <-  rvsim_out$mapsim_output0  + new_scale_colour() +   new_scale("shape") +
                                      scale_colour_manual("", values=c("main catchments"=1, "water body intercatchments"="brown","frequent to pervasive"="blue","occasional"="purple"  ) )+
                                      scale_shape_manual("", values=c("main catchments"=NA, "water body intercatchments"=NA,"frequent to pervasive"=8,"occasional"=8 ) )+
                                      scale_size_manual("", values=c("main catchments"=1, "water body intercatchments"=.7,"frequent to pervasive"=3,"occasional"=5 ) )+
                                      geom_sf(data= show_lays , aes(col=layer, shape=layer))+
                                      new_scale_fill() + new_scale_color()+  
                                      scale_fill_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      scale_color_manual("",values=c("1 to 3 runs"="white","3 to 5 runs"="yellow" ,"5 to 10 runs"="orange","10 to 15 runs"="red"),
                                      breaks=c("1 to 3 runs","3 to 5 runs","5 to 10 runs"  , "10 to 15 runs"),drop = FALSE ) +
                                      geom_sf(data=sim_terr10, aes(fill=Nruns,col=Nruns), alpha=1)+
                                      geom_sf(data= catch_polys[catch_polys$year == 10,] ,  col=NA, fill=alpha("brown",.4) )+
                                      geom_sf(data=chull_y10_all, fill=alpha("magenta",.1),col=1)+
                                      coord_sf(crs = mercproj, expand=F) + annotation_scale(location = "bl")+
                                      theme(axis.text=element_blank())
   }, ignoreInit=TRUE, ignoreNULL=FALSE)
  
        
      

     
   
 
####################################################################################################### always - click on first map = store coords

## if click on map
sel_pars <- reactive({  list(#input$sel_type,
      input$plot_click$x)
})

# store coords to plot on wee init map 
observeEvent(sel_pars(),{
       req(!is.null(input$plot_click$x )) 
       print("compute bbox centroid")
       plot_data$x <- round(as.numeric(input$plot_click$x),-2)
       plot_data$y <- round(as.numeric(input$plot_click$y) ,-2) 
}, ignoreInit=FALSE) 



####################################################################################################### always - plot release points when clicking on candidate release site map
observe({
  print("input$plot_click2")
      req(!is.null(input$plot_click2) )
      plot_data2$x <- round(as.numeric(input$plot_click2$x),-2)
      plot_data2$y <- round(as.numeric(input$plot_click2$y) ,-2)
     # print("plot_data2$y ")
     # print(plot_data2$y )
}) 



####################################################################################################### compute release points coords
####################################################################################################### triggered when bbox centroid, Nfams or protocol changes
toListen <- reactive({
 list(plot_data2$x, plot_data$Nfams , input$Nsites)
  })
 
observeEvent(toListen()  ,{  
   insertUI('#placeholder', ui = tags$p(id = 'text', paste('( computing coordinates )')), immediate = T)
    
   print("isolate click 2 coords") 
   req(plot_data2$x & plot_data2$y & nrow(ldat$LocalHab[ldat$LocalHab$layer == "suitable",])>0)   
   if( input$Nsites ==  "each_pt_on_map") {     p2x <- plot_data2$x[1]
                                                p2y <- plot_data2$y[1]
                                                #print(paste(length(unique(p2x)) , "pts") )
     }
    if( input$Nsites ==   "single_release_pt"  ) { p2x <- rep(plot_data2$x[1],as.numeric(as.character(plot_data$Nfams)))
                                                   p2y <- rep(plot_data2$y[1],as.numeric(as.character(plot_data$Nfams)))
                                                   #print(paste0(length(unique(p2x)) , "pt"))
     }
    if( input$Nsites == "random_location_across"){ # sample suitable hab layer 
          p2coords <- st_coordinates( st_sample(  x = st_erase(ldat$LocalHab[ldat$LocalHab$layer == "suitable",],ldat$nonsel),   ## sample only within selected catchments
                                                  type = "SSI",
                                                  r = 400 , # threshold distance (in metres)
                                                  n =as.numeric(as.character(plot_data$Nfams)) # number of points
                                                ))
                                                #https://stackoverflow.com/questions/64847597/how-do-i-generate-data-where-points-are-repelled-if-they-land-within-a-certain/64851194#64851194
                                                ##rMaternI: the points all arrive at the same time. Then any point which lies too close to another point, is deleted. (Matérn inhibition model 1)
                                                #rSSI: the points arrive one-by-one. Each point is placed randomly, subject to the condition that it does not lie too close to an existing point. The game stops when it is impossible to place a new point anywhere ("simple sequential inhibition")
          plot_data$Nfams <- nrow(p2coords)
          p2x  <- c( as.numeric(p2coords[,1]) )
          p2y  <- c( as.numeric(p2coords[,2]) )
     }
  removeUI('#text', immediate = T)
  
 isolate({
      val$clickx2 =    c( p2x,val$clickx2) [1:as.numeric(as.character(plot_data$Nfams))] 
      val$clicky2 =    c( p2y, val$clicky2 )  [1:as.numeric(as.character(plot_data$Nfams))]  
      # print("isolate vals")
             }) 
 }, ignoreInit=TRUE) 
   



####################################################################################################### isolate bbox centroid coords
####################################################################################################### (nb this was useful when storing several centroids, not currently used but will be useful in future)
observe({  
  plot_data$x;plot_data$y 
  isolate({
      val$clickx =   c( val$clickx, plot_data$x)
      val$clicky =   c(val$clicky,plot_data$y)   
       }) 
 }) 

observeEvent(val$clicky2, {  ### table storing consecutive centroid values
     req(!is.null(val$clickx2) & !is.null(val$clicky2)   ) 
     isolate({ 
       savedat$tablepts0 = rbind( na.exclude(savedat$tablepts0), data.frame(  x= val$clickx2,y=val$clicky2))# plot_data2$x , y=plot_data2$y   )  )
       savedat$tablepts = data.frame(family = seq(1:plot_data$Nfams), X=NA, Y=NA)
       savedat$tablepts$X  <- c(NA,NA,NA, savedat$tablepts0$x) [ (length(savedat$tablepts0$x)-as.numeric(as.character(plot_data$Nfams))+1+3) : (length(savedat$tablepts0$x)+3)]
       savedat$tablepts$Y  <- c(NA,NA,NA, savedat$tablepts0$y)  [ (length(savedat$tablepts0$x)-as.numeric(as.character(plot_data$Nfams))+1+3) : (length(savedat$tablepts0$x)+3)]
       })
 }, ignoreInit = FALSE)

 



  

#######################################################################################################  foolproofing: 
#######################################################################################################  disable/enable buttons depending on vals
observe ({
  if ( is.null(plot_data$mapzoom)) {shinyjs::disable("save_famreleased") } else {shinyjs::enable("save_famreleased") }  })
observe ({ 
if(input$demog =="all adult pairs") {shinyjs::disable("Nyoungs_perfam") } else {shinyjs::enable("Nyoungs_perfam") }  })   
observe ({
  if (  is.na(sum(savedat$tablepts$X)) ) {shinyjs::disable("save_pt") } else {shinyjs::enable("save_pt") }  })
observe ({
  if ( is.null(savedat$tablepts)  | is.na(sum( savedat$tablepts$Y)) | is.na(sum(savedat$tablepts$X))) { shinyjs::disable("start_sim")} else { shinyjs::enable("start_sim") }  })
observe ({
  if ( is.null(savedat$tablepts)  | is.na(sum( savedat$tablepts$Y)) | is.na(sum(savedat$tablepts$X))) { shinyjs::disable("start_reruninit")} else { shinyjs::enable("start_reruninit") }  })
observe ({
  if ( length(rvsim$ter.start)==0 ) { shinyjs::disable("start_sim_growth")} else { shinyjs::enable("start_sim_growth") }  })  
observe ({
#  print("how many pts ready?")
#  print(savedat$ptstp_nro)
  if (  savedat$ptstp_nro==0 ) { shinyjs::disable("start_sim")} else  {shinyjs::enable("start_sim") }   })  
observe ({
  if( is.null(plot_data$x)) {shinyjs::disable("pick_extent")} else {shinyjs::enable("pick_extent") }  })
observe ({
  if( is.null(plot_data$x)) {shinyjs::disable("bookmarkBtn")} else {shinyjs::enable("bookmarkBtn") }  })
observe ({
  if ( is.null(plot_data$x)) {shinyjs::disable("update_mapzoom_hab") } else {shinyjs::enable("update_mapzoom_hab") }  })
observe ({  
  if(is.null(plot_data$x)) {shinyjs::disable("save_extent")} else {shinyjs::enable("save_extent") }  })





 
 
 

#######################################################################################################  generate candidate release site map (TaAB1 main panel) with initial release points 
#######################################################################################################  and catchments if selected 
observe(  { # ldat$ext_area is generated after all layers are cropped etc
  req(ldat$LocalHab$layer) # i.e. a candidate rel site bbox
  if(nrow(ldat$LocalHab[ldat$LocalHab$layer == "suitable",])==0) {
    shinyjs::disable( "Nsites")
    shinyjs::disable( "Nfams_init" )
    shinyjs::disable( "demog") 
    tcoords <- st_coordinates(st_centroid(ldat$ext_area)) # this is why it updates before ready pick ext? oc19
    plot_data$mapzoom_final <- plot_data$mapzoom +   annotate("text", x = tcoords[,1] , y =  tcoords[,2],  size=5, col="steelblue",
                                                               label = "The extent contains no habitat suitable for beaver settlement.\nSelect an alternative location to start the simulation.")
    
    } else {
    # note plot_data$Nfams is reactive debounced from input
    Npts_col <- rainbow(10)[seq(1,plot_data$Nfams)]   
    ptstptest <- "see"
    ptstp <- savedat$tablepts
    mapzoom <- plot_data$mapzoom 
            if("main catchments" %in% show_suit_d()  )   { mapzoom <- mapzoom  +  geom_sf(data= ldat$catch ,  col=1, size=1.5,fill=NA) }
       
            if( !is.null(savedat$tablepts$Y)) { ptstp <-st_as_sf(x = data.frame(X=savedat$tablepts$X[!is.na(savedat$tablepts$X)],Y=savedat$tablepts$Y[!is.na(savedat$tablepts$Y)]),   coords = c("X", "Y"), crs = mercproj) 
                                                ptstp0 <- st_intersection(ptstp, ldat$ext_area_sel)
                                                Npts_col <-NA
                                                ptstp<-data.frame(X=NA,Y=NA, family=NA)
                                                print("ptstp0")
                                                #print(ptstp0)
                                                #print(nrow(ptstp0 ))
                                                if(nrow(ptstp0)>0){
                                                      ptstptest <- "OK"
                                                      Npts_col <- rainbow(10)[seq(1,nrow(ptstp0))]
                                                      #print(Npts_col)
                                                 ptstp <-data.frame(X=st_coordinates(ptstp0)[,1],Y=st_coordinates(ptstp0)[,2], family=seq(1:nrow(ptstp0)))
                                                }
                                             #savedat$tablepts dont modify here or infinite loop
                                                }
       
            if("water body intercatchments" %in% show_suit_d()  )   {
                       plot_intcatch <- plot_intcatch_nosel <- ldat$intcatch  
                       if(length( ptstp$Y)>0) { plot_intcatch_nosel <- ldat$intcatch [lengths(st_intersects(ldat$intcatch ,ptstp0 ))==0]   }
                       mapzoom  <-  mapzoom  + 
                                            geom_sf(data= plot_intcatch ,  col="brown", fill=NA)+ 
                                            geom_sf(data= plot_intcatch_nosel ,  col=NA, fill=alpha("brown", .2)) + 
                                            coord_sf(crs = mercproj, expand=F) 
            }    

     savedat$ptstp_nro <- length(which(!is.na(ptstp$Y)) )
     savedat$ptstp <- ptstp 
    if( ptstptest == "OK" ) {   mapzoom <- mapzoom + 
                                               geom_point(aes(x= ptstp$X , y= ptstp$Y), col=1 ,fill=Npts_col   , stroke=2,  shape=21, size=7  ) +#Npts_col
                                               coord_sf(crs = mercproj, expand=F)
    }
   
    plot_data$mapzoom_final <-  mapzoom  
    shinyjs::enable( "Nsites")
    shinyjs::enable( "Nfams_init" )
    shinyjs::enable( "demog") 
      }
})

 
output$mapzoom  <-  renderPlot({
      print("mapping")
      plot_data$mapzoom_final }   ,  bg="transparent")
    
  
 

############################################################################################ was useful when storing consecutive bbox centroids
############################################################################################ formatting of table when switching to single point 
observeEvent(input$Nsites,{
  print("single_release_pt")
  if( input$Nsites ==  "single_release_pt") {if(length(which(!is.na(savedat$tablepts0$x)))>0 ) { 
                                         savedat$tablepts$X <- savedat$tablepts0$x[!is.na(savedat$tablepts0$x)][1]
                                         savedat$tablepts$Y <- savedat$tablepts0$y[!is.na(savedat$tablepts0$y)][1] } else { savedat$tablepts <- NULL} }
  print("--") 
}, ignoreInit = FALSE)

output$table_saved_extents <- renderTable({if(is.null(savedat$table)) return(   )
  savedat$table },align = 'c', striped=T , spacing = 'xs',  hover = TRUE, width="100%",colnames = TRUE , rownames = FALSE   )
 
   

################################################################################################################ add potential candidate release site bbox onto init map
################################################################################################################ (side bar map Tab1 - dets stored into plot_data$tdat before into ldat)
output$plotmap1 <- renderPlot({ 
  print("map p1")
  savedat$p1 +       geom_sf(data= plot_data$tdat  , fill="orange", alpha=.5,col=1 )+
                     coord_sf(crs = mercproj,expand=F) +
                     annotation_scale(location = "bl") +
                     theme(legend.position = "none", axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(),#text(colour="beige",family="sans", size=12, face='plain' ) ,
                     plot.background = element_rect(fill = "transparent", colour = NA),plot.margin = margin(0,0,0,0, "cm"))
} , bg="transparent")  

  



################################################################################################################ display selected GIS layers (or saved as images) on initial extent map 
################################################################################################################ (side-bar TAB 1 Glen Affric to Beauly)
observeEvent( show_suit_d(),{
   print(show_suit_d())
   len_showsuit <- length(show_suit_d())
   len2 <- sum(c("main catchments", "water body intercatchments", "urban regions") %in% show_suit_d() ) 
    if( len2 >0 )   {len_showsuit <- length(show_suit_d())-len2} 
    if( len_showsuit==0) { p1  <-  p0 }  
    if( len_showsuit==1) {
      if( "habitat suitable for beaver settlement" %in% show_suit_d()   ) { p1  <-  p1}
      if( "main roads" %in% show_suit_d()  ) { p1  <-  p1b } 
      if( "named places" %in% show_suit_d()    ) { p1  <-  p1c } }
    if( len_showsuit==2) {
      if( !("named places" %in% show_suit_d()  )    )    { p1  <-  p2a}
      if( !("main roads" %in% show_suit_d() ) ){ p1  <-  p2b } 
      if( !("habitat suitable for beaver settlement" %in% show_suit_d() ))   { p1  <-  p2c } }
    if( len_showsuit==3) { p1  <-  p3 }
  # with data
     if( "urban regions" %in% show_suit_d()  )   { p1  <- p1  +
       geom_sf(data= BackLayers[ BackLayers$layer == "urban region",]   ,  col=NA, fill=alpha("purple",.6)) }
     if("main catchments" %in% show_suit_d()  )   { p1  <- p1  +  geom_sf(data= catch ,  col=1, size=1.5,fill=NA) }
     if("water body intercatchments" %in% show_suit_d()  )   { p1  <- p1  + geom_sf(data= intcatch ,  col="brown", fill=NA) }  
        p1 <- p1 + geom_label_repel(data=labs_df,mapping=aes(x=x,y=y, label=labs) , fontface = "bold", nudge_y=c(15000,2000),nudge_x=c(-8000,-28000), col="white", fill=1, alpha=.8, segment.size = .8,segment.colour="black") 
    savedat$p1 <- p1
}, ignoreInit=FALSE, ignoreNULL=FALSE) 

     


############################################################################################ always = candidate release site plotted in side-bar map 
############################################################################################ (needs to be selected in ui to then be incorporated as ldat$ext_area_sel below)    
observe ({
  req(!is.null(plot_data$x))
  print("  buffer dis ")
  print( as.numeric(input$buffer_sel))
  plot_data$tdat <-  st_as_sfc( st_bbox( st_buffer(st_sfc( st_point(c( plot_data$x,plot_data$y )), crs=mercproj),  as.numeric(input$buffer_sel) )  ))
} ) # for recovering coords in bookmked session
      
    



############################################################################################  trigger = input$pick_extent - make plotted bbox the new bbox for the candidate release site
observeEvent (input$pick_extent,{  
   insertUI('#placeholder', ui = tags$p(id = 'text', paste('( cropping GIS layers to extent )')), immediate = T)
   req(!is.null(plot_data$tdat)) # to work extent first - required for when returning to session with x and y coords saved
   print("trigger - selected extent x,y,buf")
       x <- plot_data$x 
       y <- plot_data$y   
  
   ldat$ext_area_sel <-   st_as_sfc( st_bbox(st_as_sf(st_buffer(  plot_data$tdat  ,1 )   )))
   #st_union(plot_data$tdat) # incl the shapes not their bbox - useful later when using catchments as geoms
})  
 
 



############################################################################################  trigger = input$pick_extent - activate trigger: triggers$new_extent
observeEvent(input$pick_extent,{ 
    triggers$new_extent <- isolate(triggers$new_extent+1)
    triggers$new_sim <- 0
    print("ping - new_extent")
    print(triggers$new_extent)
   }) 
 



############################################################################################  trigger = triggers$new_extent - crop all GIS layer for candidate release site
observeEvent(triggers$new_extent,{       # not on init 
     print(paste0("pong - new_extent ",  triggers$new_extent))
     ldat$ext_area <-  ldat$ext_area_sel # geom is a st_as_sfc poly (not a bbox) here 
     update_busy_bar(30)
     ldat$LocalHab <-  st_intersection(HabMapLayers,ldat$ext_area) 
     ldat$ext_UKbound <- st_crop(UKbound , ldat$ext_area)  
     update_busy_bar(50)
     ldat$ext_scotcrop <- st_crop(scotcrop,ldat$ext_area) 
     update_busy_bar(70) 
     ldat$riv  <- st_union( st_crop( rivlines,ldat$ext_area) )
     ldat$LocalHab <- rbind(ldat$LocalHab, st_sf(geometry=  ldat$riv , layer="river") )                  
     ldat$LocalBackLayers  <- st_crop(BackLayers,ldat$ext_area) 
     ldat$LocalBackText    <- st_intersection(BackText,ldat$ext_area) 
     ldat$LocalBackPoints    <- st_intersection(BackPoints,ldat$ext_area) 
     ldat$nonsel <- NULL 
  # if (input$sel_type=="catch") { # later use to sample only on selected catchments - will work for square sel too
     ldat$nonsel <- st_erase(ldat$ext_area,ldat$ext_area_sel)#plot_data$tdat)
  # } 
## generate cropped layers to show catchments on zoomed map  at same time    
     ldat$catch <- st_crop(catch,ldat$ext_area) 
     ldat$intcatch <- st_intersection(intcatch$geometry ,ldat$ext_area) 
     plot_data$mapzoom0 <- mapzoom_function()  + geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.7))+ 
                                                 coord_sf(crs = mercproj, expand=F)
 update_busy_bar(100)
     plot_data$mapzoomb <- plot_data$mapzoomc <- plot_data$mapzoom <- plot_data$mapzoom0
     print('init layers ready')
     removeUI('#text', immediate = T)
}, ignoreInit=TRUE)   

  
  





############################################################################################ filter/aggregate GIS layers to display on candidate release site map
############################################################################################ (categories like "all roads" "OS regions" "named places"
observeEvent (input$selected_layer_all, { reac_lay$hab <- isolate(reac_lay$hab + 1) # just a trigger 
}, ignoreNULL = FALSE, ignoreInit=TRUE)


observeEvent (reac_lay$hab,  {  
        x <- input$selected_layer
        if("all" %in% input$selected_layer_all  ) { x <- c("suitable", "dispersal", "unsuitable") } else { x <- character(0) }
        shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer", selected = x)
}, ignoreInit=TRUE)  
   
observe({ reac_lay$selected_OS <- c(input$selected_OS1, input$selected_OS2)  })

observeEvent (input$selected_OS_all,  {   
       x1 <- input$selected_OS1
       x2 <- input$selected_OS2  
       x0 <- input$selected_OS_all
             
  if("all roads" %in% x0 )  { x1 <- unique(c(x1, "a road", "b road", "minor road", "primary road", "road names", "railway line") )  } else { x1 <-  x1[!x1 %in% c("a road", "b road", "minor road", "primary road", "road names", "railway line")]  }
  if("OS regions" %in% x0 ) { x2 <- unique(c(x2, "woodland region","urban region","rivers line","lakes region","foreshor region", "marsh", "TV or radio mast or tower", "Wind powered generator") ) } else { x2 <-  x2[!x2 %in% c( "woodland region","urban region","rivers line","lakes region","foreshor region", "marsh", "TV or radio mast or tower", "Wind powered generator")]  }
  if("named places" %in% x0){ x1 <- unique(c(x1,  "named places", "local altitude") ) }else { x1 <-  x1[!x1 %in% c("named places", "local altitude")]  }
  if("all" %in%  x0)  {
      x1 <- unique(c(x1, "a road", "b road", "minor road", "primary road", "road names", "railway line", "named places", "local altitude") )   
      x2 <- unique(c(x2, "woodland region","urban region","rivers line","lakes region","foreshor region", "marsh", "TV or radio mast or tower", "Wind powered generator") ) 
   }
   shinyWidgets::updatePrettyCheckboxGroup(session, "selected_OS1", selected = x1)
   shinyWidgets::updatePrettyCheckboxGroup(session, "selected_OS2", selected = x2)
}, ignoreNULL=FALSE, ignoreInit=FALSE)  
  

  
                             

############################################################################################ update candidate release site map with GIS layers - trigger = beaver habitat map
observeEvent(input$update_mapzoom_hab,{      # when clicked, crop OS map otherwise just hab is ready
    LocalHab <-  ldat$LocalHab
    reac_lay$selected_OS <- c(input$selected_OS1, input$selected_OS2)
    print("plot mapzoom layers:")
    #print(reac_lay$selected_OS)
    selected_OS <- reac_lay$selected_OS
    
    show_railstations <- NULL ## to display labels only when lines are displayed
    if ( "railway line"  %in%  selected_OS) {show_railstations <- "Railway Station"}
        update_busy_bar(10)
        plot_data$mapzoomOS <- plot_data$mapzoom0 +  
                                geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% selected_OS,], aes(col=layer, fill=layer, size=layer ), size=2,alpha=.7 )+
                                geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% c("b road","a road","minor road","primary road") & ldat$LocalBackLayers$layer %in% selected_OS,], col= "yellow", size=.8,alpha=.8 )+
                                geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% c("railway line") & ldat$LocalBackLayers$layer %in% selected_OS,], linetype="dotted", col= 1, size=3  )+
                                geom_sf(data=ldat$LocalBackPoints [ldat$LocalBackPoints$layer %in% selected_OS,], aes(col=layer,  shape=layer), size=3, stroke=1 )+
                                geom_sf_text (data=ldat$LocalBackText [ldat$LocalBackText$layer  %in% selected_OS & ldat$LocalBackText$layer2 != "Railway Station",], aes(label=label, col=layer2, size=layer2 )  )+
                                geom_sf_label(data=ldat$LocalBackText [ldat$LocalBackText$layer2  == show_railstations,], aes(label=label),fill=alpha(1,.4), col="beige")+
                                geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.5))+
                                coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") 
       
    update_busy_bar(60)
           plot_data$mapzoomc <- plot_data$mapzoom0  +  
                                             geom_sf(data= LocalHab [LocalHab$layer %in% input$selected_layer ,] ,                    
                                             mapping = aes(fill= layer , col= layer), alpha=.7, size=0 ) +
                                             coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl")
    update_busy_bar(100)
        plot_data$mapzoom <-  plot_data$mapzoomc  +  
                                 geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% selected_OS,], aes(col=layer, fill=layer, size=layer ), size=2,alpha=.7 )+
                                 geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% c("b road","a road","minor road","primary road") & ldat$LocalBackLayers$layer %in% selected_OS,], col= "yellow", size=.8,alpha=.8 )+
                                 geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% c("railway line") & ldat$LocalBackLayers$layer %in% selected_OS,], linetype="dotted", col= 1, size=3  )+
                                 geom_sf(data=ldat$LocalBackPoints [ldat$LocalBackPoints$layer %in% selected_OS,], aes(col=layer,  shape=layer), size=3, stroke=1 )+
                                 geom_sf_text(data=ldat$LocalBackText [ldat$LocalBackText$layer  %in% selected_OS& ldat$LocalBackText$layer2 != "Railway Station",], aes(label=label, col=layer2, size=layer2 )  )+
                                 geom_sf_label(data=ldat$LocalBackText [ldat$LocalBackText$layer2  == show_railstations,], aes(label=label),fill=alpha(1,.4), col="beige")+
                                 geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.5))+
                                 coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl")  
    
      if(any(!(c("suitable", "dispersal", "unsuitable","river")  %in% input$selected_layer  )) )  { # if any layer missing unselected "all" box
               x <- input$selected_layer_all  
               x <- character(0) 
               shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer_all", selected = x)
      }
        
      if(length( selected_OS) >0) {plot_data$legend_OSmap <- paste0("\nOS Features with Beaver Habitat Suitability overlay") }
      if(length(input$selected_layer) >0) {plot_data$legend_OSmap <- paste0("\nBeaver Habitat Suitability") }
 }, ignoreInit=FALSE)  


 

############################################################################################ update candidate release site map with GIS layers - trigger = OS layer
observeEvent(input$update_mapzoomOS,{
    reac_lay$selected_OS <- c(input$selected_OS1, input$selected_OS2)
    LocalHab <-  ldat$LocalHab
    selected_OS <- reac_lay$selected_OS
    update_busy_bar(10)
    show_railstations <- NULL ## to display labels only when lines are displayed
    if ( "railway line"  %in%  selected_OS) {show_railstations <- "Railway Station"}
     plot_data$mapzoomOS <- plot_data$mapzoom0 +  
                            geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% selected_OS,], aes(col=layer, fill=layer, size=layer ), size=2,alpha=.7 )+
                            geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% c("b road","a road","minor road","primary road") & ldat$LocalBackLayers$layer %in% selected_OS,], col= "yellow", size=.8,alpha=.8 )+
                            geom_sf(data=ldat$LocalBackLayers [ldat$LocalBackLayers$layer %in% c("railway line") & ldat$LocalBackLayers$layer %in% selected_OS,], linetype="dotted", col= 1, size=3  )+
                            geom_sf(data=ldat$LocalBackPoints [ldat$LocalBackPoints$layer %in% selected_OS,], aes(col=layer,  shape=layer), size=3, stroke=1 )+
                            geom_sf_text(data=ldat$LocalBackText [ldat$LocalBackText$layer  %in% selected_OS& ldat$LocalBackText$layer2 != "Railway Station",], aes(label=label, col=layer2, size=layer2 )  )+
                             geom_sf_label(data=ldat$LocalBackText [ldat$LocalBackText$layer2  == show_railstations,], aes(label=label),fill=alpha(1,.4), col="beige")+
                          geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.5))+
                               coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") 
   
    update_busy_bar(60)
    plot_data$mapzoomc <- plot_data$mapzoom0  +  
                                         geom_sf(data= LocalHab [LocalHab$layer %in% input$selected_layer ,] ,                    
                                         mapping = aes(fill= layer , col= layer), alpha=.7, size=0 ) + 
                                         coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl")
    update_busy_bar(100)
    plot_data$mapzoom <-  plot_data$mapzoomOS  +   
                                         geom_sf(data= LocalHab [LocalHab$layer %in% input$selected_layer ,] ,                    
                                         mapping = aes(fill= layer , col= layer), alpha=.7, size=0) + 
                                         geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.5))+
                                         coord_sf(crs = mercproj, expand=F)+ annotation_scale(location = "bl") 
 
    
    if(length( selected_OS) >0) {plot_data$legend_OSmap <- paste0("\nOS Features with Beaver Habitat Suitability overlay") }
    if(length(input$selected_layer) >0) {plot_data$legend_OSmap <- paste0("\nBeaver Habitat Suitability") }
  
    if(any(!(c("suitable", "dispersal", "unsuitable","river")  %in% input$selected_layer  )) )  { # if any layer missing unselected "all" box
           x <- input$selected_layer_all  
           x <- character(0) 
           shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer_all", selected = x) }
}, ignoreInit=FALSE) 
 
 
  

############################################################################################ FUN - mapzoom_function - generate map for main tab candidate release site 
mapzoom_function <-function(){
    print("run mapzoom function") 
    ext_scotcrop <- ldat$ext_scotcrop
    ext_UKbound <- ldat$ext_UKbound
    ext_area_sel <- ldat$ext_area_sel
    rivers <- ldat$riv   
    
 ggplot() + 
          geom_sf(data =  ext_scotcrop, col=NA, fill="white")+
          geom_sf(data =  ext_scotcrop, col=NA, fill="steelblue", alpha=.9)+
          geom_sf(data =  ext_UKbound ,  fill="beige", col="steelblue", alpha=.9, size=1)   +
          geom_sf(data=   rivers, col=alpha("skyblue",.7),fill=alpha("steelblue",.8), size=2)+ 
          geom_sf(data=   rivers, col=alpha("steelblue",.9),fill=alpha("skyblue",.7), size=1)+ 
          geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.7))+
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
  
   

  
 
    
    

################################################################################################################ BOOKMARKING  
bookmarkingWhitelist <- c( "plot_click$x","plot_click$y" )

observeEvent(input$bookmarkBtn, {  session$doBookmark()                                })  
 
observe({       toExclude <- setdiff(names(input), bookmarkingWhitelist)
                setBookmarkExclude(toExclude)                                          })

onBookmark(function(state) {      state$values$x <- round(as.numeric(plot_data$x),-1)
                                  state$values$y <- round(as.numeric(plot_data$y),-1)  })

onRestored(function(state) {   plot_data$y <- state$values$y 
                               plot_data$x <- state$values$x                           })
 
  
# kill processes when closing browser # doesnae work does it?!
session$onSessionEnded(function() { })
# onStop(function() cat("Session stopped\n"))
  
 
  
}
 
enableBookmarking(store = "url")
shinyApp(ui, server)  

 
     
