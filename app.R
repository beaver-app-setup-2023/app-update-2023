
################################################################################    
################      BEAVER TRANSLOCATION SIMULTATION APP      ################
################        for NatureScot Nov. 2022                ################
################        shiny code - ui+server                  ################
################                                                ################
################          UPDATING - 2023                       ################
################################################################################        
 


 shiny::addResourcePath("www", paste0(here::here(),"/www"))
 shiny::addResourcePath("www2", paste0(here::here(),"/www2")) 
 
 
ui <-function(request) { 
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
                                                                          p(span("specify each release point ",style = "color:yellow;padding-right:400px;"),span("locate each release point on the map with one double-click per location.", style = " color:beige"))  ),
                                                     choiceValues= c("random_location_across" ,"each_pt_on_map" ),
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

 shinyjs::addClass(id = "PageID", class = "navbar-right")
 
  

#### source scripts
#### 
#  AppFun_mapX_    functions to generate maps in app (numbers chronological not in code)
#  AppFun_sim      functions to run sim in app
#  AppFun_misc_    other functions relating to app
#  initLoad_val_   envir, data, maps etc loaded on init 
#  ModDat_         values relating to simulation code
#  ModFun_         functions relating to simluation code
 
     source_R <- unlist(list.files(here::here(),pattern="initLoad_Rlibs"))
       cat("load envir\n")  
     for(sc in source_R) { source(sc)}

     update_busy_bar(10)    
 
     source_IBMcode      <- unlist(list.files(here(),pattern="Mod"))
     source_App_Functions  <- unlist(list.files(here(),pattern="AppFun_"))
     source_App_initVals <- unlist(list.files(here(),pattern="initLoad_val")) 
     
  cat("\n\nload init reactives\n")  
     for(sc in source_App_initVals) { source(sc)}    
 
  cat("\n\nload data\n")
     for(sc in source_IBMcode) { source(sc) }
     
  cat("\n\nload beaver pop simulation functions and params\n")
     for(sc in source_App_Functions ) { source(sc)  }
       
     update_busy_bar(40)      
  cat("\n\nload region GIS data\n")  
     source(here::here("initLoad_GIS_regionbbox.R") )  # source GIS stuff files in cascade - may use to filter file loading/region
  update_busy_bar(90) 
     savedat$p1= p0 
  update_busy_bar(100)
 
 
 
 
 
 
  
################################################################################################################  init terr re-run
 observeEvent(input$start_reruninit ,{
   rvsim$mapsim <- rvsim$mapsim3 <- rvsim$mapsim10 <- NULL  
   cat("\ntrigger re-run of start pop simulation\n")
 rvplot1$new_ext0 <- rvplot1$mapsim_init<-rvplot1$mapsim_init0<- rvsim_out$mapsim_output0 <- rvsim$mapsim0_finext<- NULL # added  because weird extent on rerun
 if(is.null(rvplot1$trigger_init_terr) ) {rvplot1$trigger_init_terr <-  2} else {
 if(rvplot1$trigger_init_terr==1) {rvplot1$trigger_init_terr <-  2} else {
 if(rvplot1$trigger_init_terr==2) {rvplot1$trigger_init_terr <-  1} } }
    } )  


 
################################################################################################################ RESET on selecting new extent
observeEvent(input$pick_extent,{ ## reset hab layers and extent point data on selecting new extent
    cat("selected extent\n")
    shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer_all", selected = character(0) )
    shinyWidgets::updatePrettyCheckboxGroup(session, "selected_layer", selected = "suitable")  
    shinyWidgets::updatePrettyCheckboxGroup(session, "selected_OS_all", selected = "all roads")
    rvsim <- reactiveValues(trigsim=0, mapsim0=NULL,start_pop=NULL, sim_pop=NULL,lay_output=NULL,lay_release_pts=NULL, trig=0, temp_bbox=NULL, lay_release_pts_trig=0, linelength=NULL)
    rvplot1 <-  reactiveValues(new_ext0=NULL, mapsim_init=NULL, trigger_init_terr =NULL)         
    rvsim$mapsim <-   rvsim$mapsim3 <-  rvsim$mapsim10 <- NULL 
} , ignoreInit = FALSE)
    



################################################################################################################ RESET point coords when selecting new release protocol
observeEvent(input$Nsites,{ 
    cat("\nupdate number of released families\n")
  val$clickx2 <- val$clicky2 <-    NULL
}, ignoreInit = TRUE) 
 

 
################################################################################################################  init terr - trigger
 observeEvent(input$start_sim ,{
 rvsim$mapsim <- rvsim$mapsim3 <- rvsim$mapsim10 <- NULL  
 rvplot1$new_ext0 <- rvplot1$mapsim_init<-rvplot1$mapsim_init0<- rvsim_out$mapsim_output0 <- rvsim$mapsim0_finext<- NULL 
 cat("\ntrigger initial start pop simulation") 
   if(is.null(rvplot1$trigger_init_terr) ) {rvplot1$trigger_init_terr <-  1} else {
         if(rvplot1$trigger_init_terr==1) {rvplot1$trigger_init_terr  <-  2} else {
                 if(rvplot1$trigger_init_terr==2) {rvplot1$trigger_init_terr <-  1} }}
} )  
 
####################### start the sim - produce release points df
observeEvent(rvplot1$trigger_init_terr,{  
    rvtext1$init_terr_counter <-  rvtext1$init_terr_counter+1
    rvplot1 <-  reactiveValues(new_ext0=NULL, mapsim_init=NULL)    #reset     
    # points 
    pts0   <-  savedat$ptstp #  used to be savedat$tablepts when no choice of extent size - now retrieves only pts overlapping etc  
    pts0$x <- round(pts0$X,-2)
    pts0$y <- round(pts0$Y,-2)
    pts0   <- st_as_sf(data.frame(x=pts0$x, y=pts0$y, family=pts0$family), coords=c("x","y"),crs = st_crs(mercproj)) 
    rvsim$lay_release_pts <- pts0 #  st_as_sf(data.frame(pts0), coords=c("x","y"), crs=mercproj)
    rvsim$lay_release_pts$layer <- "release points"  
    rvsim$lay_release_pts_trig <- rvsim$lay_release_pts_trig+1
    cat("\nrelease points ok  -  ")
  } )

    


################################################################################################################ startpop_function - trigger 1 - generate reloc points
observeEvent(rvsim$lay_release_pts_trig,{
   removeUI('#text', immediate = T)
   insertUI('#placeholder', ui = tags$p(id = 'text', paste('( preparing GIS layers )')), immediate = T)
   shinyWidgets::updatePrettyCheckboxGroup(session, "sim_objective", selected = character(0))        
   cat("trigger simulation of starting territories")
   return_list_start <- startpop_function(pts=rvsim$lay_release_pts, habmap_raster=map.r, Nsites=input$Nsites, demog=input$demog, Nyoungs_perfam=input$Nyoungs_perfam)  # jan
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
   ## lines between old/new locs  (not used in the end, have to remove!)
    new_pts <- rvsim$lay_release_pts_new      
    old_pts <- rvsim$lay_release_pts[rvsim$lay_release_pts$family %in% new_pts$family,] # jan check here
    old_pts <- old_pts[order(old_pts$family),]
    new_pts <- new_pts[order(new_pts$family),] 
    fam_pts <- new_pts$family 
    old_pts <- st_cast(st_geometry(old_pts))
    new_pts <- st_cast(st_geometry(new_pts))
    cat(paste0("output data compiled  -  ")) 
  } , ignoreInit=TRUE, ignoreNULL=TRUE )  
 


################################################################################################################ startpop_function - trigger 2  - output text + update maps FUN_map_output_startingterr
observeEvent( rvsim$start_pop ,{ 
  cat(paste0("generate mapped output \n"))
    Nfams_init <-  plot_data$Nfams 
    fams <-  seq(1,Nfams_init)
    start_terr0  <-  rvsim$start_pop  
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
 FUN_map_output_startingterr_out <- FUN_map_output_startingterr( mapsim_init0=rvplot1$mapsim_init0, start_pop=rvsim$start_pop,lay_release_pts=rvsim$lay_release_pts,lay_release_pts_new=rvsim$lay_release_pts_new, rvplot1=rvplot1, sim_objective= input$sim_objective, Nfams_init_d_=Nfams_init_d())
 rvplot1$mapsim_init <- FUN_map_output_startingterr_out [[1]]
 rvplot1$mapsim_init0 <- FUN_map_output_startingterr_out [[2]]
 removeUI('#text', immediate = T)
}, ignoreInit=TRUE ) 
 
  
 
 


################################################################################################################ reactive/debounced params/values
mgmt.years <- reactive(input$Nyears_sim)   #observe ({ rvsim$mgmt.years <- mgmt.years() }) 
 
# intermediary storage for Nfam val - debounced/to keep for comparison with actual 
Nfams_init  <- reactive( input$Nfams_init)
Nfams_init_d<- Nfams_init  %>%  debounce(2000)
observe ({ plot_data$Nfams <- Nfams_init_d() })  
observeEvent(plot_data$Nfams,{ rvsim$Nfams <- plot_data$Nfams }, ignoreInit=FALSE)

# debounce all box ticking 
show_extra_lay   <- reactive( c(input$show_extra_lay1,input$show_extra_lay2) )
show_extra_lay_d <- show_extra_lay %>%  debounce(700)

show_suit   <- reactive( input$show_suit)
show_suit_d <- show_suit  %>%  debounce(1200)
  
plot_probability_range_y3   <- reactive( input$plot_probability_range_y3 )
plot_probability_range_y3_d <- plot_probability_range_y3 %>%  debounce(500)
plot_probability_range_y5   <- reactive( input$plot_probability_range_y5 )
plot_probability_range_y5_d <- plot_probability_range_y5 %>%  debounce(500)
plot_probability_range_y10  <- reactive( input$plot_probability_range_y10 )
plot_probability_range_y10_d<- plot_probability_range_y10 %>%  debounce(500) 

  
  
  




 
################################################################################################################ simulate pop growth - trigger 
observeEvent(input$start_sim_growth,{  
   update_busy_bar(20)
      rvsim$mapsim  <-  rvsim$mapsim3  <-  rvsim$mapsim10 <- rvsim$mapsim0_finext <- NULL
      rvsim_out <- reactiveValues(new_ext_sim=NULL, mapsim_output0=NULL)
      rvsim$mgmt.years <- mgmt.years() 
      cat(   rvsim$mgmt.years," mgmt years  -  ",mgmt.reps," reps" )
      rvsim$sim_pop <- simpop_function(hab=rvsim$hab, fam.start=rvsim$fam.start, ter.start=rvsim$ter.start, mgmt.years=rvsim$mgmt.years, mgmt.reps=mgmt.reps  )
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
       rvsim$chull_y10_all <- list (st_convex_hull(st_union(rvsim$fam_sim10)), st_convex_hull(st_union(rvsim$fam_sim10[rvsim$fam_sim10$dens> 2,])), st_convex_hull(st_union(rvsim$fam_sim10[rvsim$fam_sim10$dens>4,])), st_convex_hull(st_union(rvsim$fam_sim10[rvsim$fam_sim10$dens>9,])))
  } 
    cat("pop growth simulated  - ") 
    update_busy_bar(30) 
    ## trigger next computations
    rvsim$trig <- rvsim$trig+1   
    cat(paste0("trigger GIS comp  -  ") ) 
   
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
    
output$extent_infotext <-  renderText({  if(is.null(rvsim$Ngroups)) return()  
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
  
   
############## reac vals for conditional panel
output$showExtraLayers <- reactive( as.numeric(length(rvsim$ter.start)>0)  ) # i.e. when sim_pop exists, value is 1 
outputOptions(output, "showExtraLayers", suspendWhenHidden = FALSE)
output$showExtraLayers1 <- reactive( !is.null(rvsim$sim_pop)  ) 
outputOptions(output, "showExtraLayers1", suspendWhenHidden = FALSE)
   
   
############## saving  
output$save_out_customID <- renderUI({ textInput("location",label=NULL ,placeholder= "Custom output ID (optional)" ) })
output$save_out_button   <- renderUI({  downloadButton("save_out", "" ,class = "btn-success",style = "height:50px;") })
   

############## ABOUT tab text      
output$refs         <- renderText({paste0( ref_SNHReport2020,ref_SNHReport2015, ref_SNHReport1997,ref_SNHReport_fromRep1,ref_SNHReport_fromRep2 )})
output$RPackages    <- renderText({ AboutText_R }) 
output$AboutOutVals <- renderText({About_OutputValues})
output$AboutUsability_comput  <- renderText({ AboutUsability_computation })
 
output$AboutBookmarking <- renderText({AboutBookmarking})
output$AboutDownloading <- renderText({ AboutDownloading})
output$AboutOutput_f    <- renderText({ AboutOutput_f  })
output$AboutOutput0     <- renderText({ AboutOutput0  })
output$AboutOutput1a    <- renderText({ AboutOutput1a })
output$AboutOutput1b    <- renderText({AboutOutput1b } )
output$AboutOutput2a    <- renderText({ AboutOutput2a  })
output$AboutOutput2b    <- renderText({ AboutOutput2b })

## description demog
output$AboutPars_fams0  <- renderText({ AboutPars_fams0  }) 
output$AboutPars_fams1  <- renderText({ AboutPars_fams1  }) 
output$AboutPars_fams2  <- renderText({ AboutPars_fams2  })
output$AboutPars_fams3  <- renderText({ AboutPars_fams3  })
output$AboutPars_fams4  <- renderText({ AboutPars_fams4  })
        
output$AboutPars_suit <- renderText({ AboutPars_suit }) 
output$AboutPars_disp <- renderText({ AboutPars_disp })
output$AboutPars_surv <- renderText({ AboutPars_surv })
output$AboutTheModel  <- renderText({ AboutTheModel  })
   


### Text reg sim info
output$siminfo <- renderText ({ rv$sim_info  })

 
output$map_logo  <- renderUI({ if( is.null(plot_data$mapzoom0))  return( )
  div(style = "color:yellowgreen;",icon("map")) 
}) 
  
output$plotmap1_infosel <- renderUI({   
   if( is.null(plot_data$x))  return( paste0("\n\n\n")  )
   paste0( "selected\n    X: ",round(as.numeric(plot_data$x),-1),"\n    Y: ",round(as.numeric(plot_data$y),-1))  
}) 
 
output$plotmap1hover_info <- renderUI({ 
  if( is.null(input$plotmap1plot_hover$x))  return( paste0("\n\n\n")  )
  if(!is.null(input$plotmap1plot_hover$x))  
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
  cat("output table\n") 
  rvsim$sim_pop[[3]] },align = 'c', striped=T , spacing = 'xs',  hover = TRUE, width="100%",colnames = TRUE , rownames = FALSE,digits=0   )
 
output$mapsim_init  <- renderPlot({  if(is.null(rvplot1$mapsim_init)) {return()} else{
  cat("generate starting terrs map  -  ")
  update_busy_bar(100)
  rvplot1$mapsim_init }
  }  ,  bg="transparent")

output$mapsim10  <- renderPlot({ if(is.null(rvsim$mapsim10) ) {return()} else{ 
   cat("generate output map y10  -  ")
  update_busy_bar(100)
  rvsim$mapsim10    } }  ,  bg="transparent")

output$mapsim  <- renderPlot({if(is.null(rvsim$mapsim) ){return()} else{
   cat("generate output map y5  -  ")
  update_busy_bar(100)
  rvsim$mapsim  }  }   ,  bg="transparent")

output$mapsim3  <- renderPlot({ if(is.null(rvsim$mapsim3) ){return()} else{
   cat("generate output map y3  -  ")
  update_busy_bar(100)
  rvsim$mapsim3  
  }  }   ,  bg="transparent")











  
   

observeEvent(rvsim$sim_pop,{
         req(!is.null(rvsim$sim_pop[[3]])) # result table
      out5 <- rvsim$sim_pop[[2]]  
      out3 <- rvsim$sim_pop[[4]]
      out5$year <- 5
      out3$year <- 3
      cat(" gather output data for dl: ")
       out_Nruns <- rbind(out3,out5)
      if(!is.null(rvsim$sim_pop[[5]]))  {
       out10 <- rvsim$sim_pop[[5]]
       out10$year <- 10
       out_Nruns <-  rbind(out_Nruns,out10)  }
       
       dl_data$out_Nruns <- out_Nruns %>% group_by(Nruns,layer,year) %>% summarise()
       cat("Nruns layers + ") 
       
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
        cat("convex hulls layer \n")  
 
 }, ignoreNULL=FALSE, ignoreInit=TRUE) #jan26
  
 
  
 
################################################################################################################ Download output - all at once / zipped 
filelist <-NULL
output$save_out = downloadHandler(
          filename = function(){ paste0("SimOutputs_",dl_data$locname, ".zip") },
          content = function(file){
                        FUN_dl_output_content( filename =filename ,file=file,sim_pop = rvsim$sim_pop,start_pop=rvsim$start_pop,mgmt.years = rvsim$mgmt.years,locname = dl_data$locname, out_chulls =dl_data$out_chulls,out_Nruns = dl_data$out_Nruns,Nfams_init_d_ =Nfams_init_d(),Nfams=plot_data$Nfams,
                                               lay_release_pts_new=rvsim$lay_release_pts_new,Nfams_wYoung=rvsim$Nfams_wYoung,Nsites=input$Nsites,demog = input$demog,buffer_sel = input$buffer_sel,y=plot_data$y,x=plot_data$x, Nads = rvsim$Nads,Nyoungs = rvsim$Nyoungs,
                                               Nfams_w2Young = rvsim$Nfams_w2Young, famsettled_text=rvtext1$famsettled_text,ext_area_sel=ldat$ext_area_sel,  Ngroups=rvsim$Ngroups)},
          contentType = "application/zip"
  ) # rvsim$mgmt.years / mgmt.years() 
    


################################################################################################################ Download output - file name
observeEvent(input$location,{ 
    req(!is.null(input$location))
    if( input$location=="") {dl_data$locname <- format(Sys.time(), "%d%b%y_%H%M")  } else { 
        dl_data$locname <- str_replace_all(input$location, "[^[A-Za-z1-9,]]", "") %>% str_squish(.) 
       }
   cat(paste0("stored simulation ref: ",dl_data$locname,"  -  "))
   },ignoreNULL=FALSE, ignoreInit=FALSE) 




  
  
 
 
 
observeEvent(rvsim$trig,{   # once simulations completed
  cat("crop GIS layers to new bbox: ")
  removeUI('#text', immediate = T)
  insertUI('#placeholder', ui = tags$p(id = 'text', paste('( preparing GIS layers for simulated population range )')), immediate = T)
     
 if(!is.null(rvsim$temp_bbox)) {  
           update_busy_bar(40)
           rvsim$LocalBackLayers <- rvsim$LocalBackText <-rvsim$LocalBackPoints <- rvsim$riv <- NULL
     # recompute extent that contains all simulated terr + recrop etc with buffer  
       buf_y3_y5 <- st_buffer(st_union(st_union(rvsim$fam_sim3),st_union(rvsim$fam_sim )   ),1400 )
            if(rvsim$mgmt.years == 5)  { sim_bbox <-   st_as_sfc(  st_bbox ( buf_y3_y5))} #st_buffer(st_union(st_as_sfc(st_bbox(rvsim$fam_sim),st_as_sfc(st_bbox(rvsim$fam_sim3)))) ,400) }
            if(rvsim$mgmt.years == 10) { sim_bbox <-   st_as_sfc(  st_bbox ( st_buffer(st_union(buf_y3_y5,st_union(rvsim$fam_sim10)  ),1400 )))} #st_buffer(st_union(st_as_sfc(st_bbox(rvsim$fam_sim),st_as_sfc(st_bbox(rvsim$fam_sim3)),st_as_sfc(st_bbox(rvsim$fam_sim10)))) ,400) }
             st_crs(sim_bbox) <- mercproj
      rvsim$LocalHab <- st_crop(HabMapLayers,sim_bbox) 
      cat("habitat  -  ")
      rvsim$ext_coastline <- st_crop(coastline , sim_bbox)  
      rvsim$ext_region_box <- st_crop(region_box,sim_bbox) 
      cat("land  -  ")
      update_busy_bar(50) 
      rvsim$riv  <- st_union( st_crop(rivlines,sim_bbox) ) 
      cat("rivers  -  ")
      rvsim$LocalHab <- rbind(rvsim$LocalHab, st_sf(geometry= rvsim$riv , layer="river") )  # **               
      rvsim$LocalBackLayers  <- st_crop(BackLayers,sim_bbox) 
      rvsim$LocalBackText    <- st_intersection(BackText,sim_bbox) 
      rvsim$LocalBackPoints  <- st_intersection(BackPoints,sim_bbox)  
      cat("text\n")
      update_busy_bar(70)
          
      cat("generate output maps  - ")  
         rvsim_out$new_ext_sim    <-  sim_bbox # rvsim$FUN_map_output_yALL_out[[4]]
         rvsim_out$mapsim_output0 <-  FUN_mapsim_output_function( region_box, new_ext_sim = rvsim_out$new_ext_sim, rivlines, HabMapLayers, BackLayers, BackPoints, BackText) #  uses new extent new_ext_sim ## jan* partition
      
         rvsim$FUN_map_output_yALL_out <- FUN_map_output_yALL(start_terr=rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,],      
                                                              sim_terr3=rvsim$fam_sim3,sim_terr= rvsim$fam_sim, sim_terr10 = rvsim$fam_sim10 ,
                                                              lay_release_pts =rvsim$lay_release_pts ,mgmt.years =rvsim$mgmt.years,mapsim_output0 = rvsim_out$mapsim_output0,
                                                              plot_probability_range_y3_d_=plot_probability_range_y3_d(),plot_probability_range_y5_d_=plot_probability_range_y5_d(),plot_probability_range_y10_d_=plot_probability_range_y10_d(),
                                                              chull_y3_all=rvsim$chull_y3_all,chull_y5_all=rvsim$chull_y5_all,chull_y10_all=rvsim$chull_y10_all,
                                                              show_chull_y3=input$show_chull_y3,show_chull_y5=input$show_chull_y5, show_chull_y10=input$show_chull_y10)   
         
         rvsim$sim_terr3_bufpts  <- rvsim$FUN_map_output_yALL_out [[6]]
         rvsim$sim_terr_bufpts   <- rvsim$FUN_map_output_yALL_out [[7]]
         rvsim$sim_terr10_bufpts <- rvsim$FUN_map_output_yALL_out [[8]]
        rvsim$mapsim <- rvsim$FUN_map_output_yALL_out[[1]]  
         update_busy_bar(80)
         rvsim$mapsim3 <- rvsim$FUN_map_output_yALL_out [[2]]
         update_busy_bar(90)
         rvsim$mapsim10 <- rvsim$FUN_map_output_yALL_out [[3]]
         
         
         rvsim$mapsim0_finext <- rvsim$mapsim0_finext0 <-  FUN_mapsim0_finext(start_terr= rvsim$start_pop[rvsim$start_pop$layer =="starting territories" ,], mapsim_output0=rvsim_out$mapsim_output0 )  # nb- is built with rvsim_out$mapsim_output0 + init terrs  
         cat("crop extra hab layers  - ")
      #      extra_HabLayers_c <- st_crop(extra_HabLayers_l ,  sim_bbox )   ## here can be topo issue with original shp cant use intersection() so using crop but mh
         cat("catchments layers  - ")  
        catch_polys  <- st_crop(intcatch$geometry,  sim_bbox )
        catch_polys0 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$lay_release_pts_new ))) ==0]),year=0)
        catch_polys3 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$sim_terr3_bufpts     ))) ==0]),year=3)   ### here could vary covered polygon with Nruns level BUT requires computing for each level as in chulls not just nruns - just copy computation structure here and change to select only relevant level for mapout per yr and level- no time now
        catch_polys5 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$sim_terr_bufpts     ))) ==0]),year=5)
            rvsim$catch_polys <- rbind( catch_polys0, catch_polys3, catch_polys5 ) 
           
         if(rvsim$mgmt.years == 10) { 
                  catch_polys10 <- st_sf(geometry=st_union(catch_polys[lengths(st_intersects(catch_polys, st_union( rvsim$sim_terr10_bufpts   ))) ==0]),year=10)
                  rvsim$catch_polys <- rbind( rvsim$catch_polys, catch_polys10) } 
            extra_HabLayers_c <- st_crop(extra_HabLayers_l , st_buffer(sim_bbox,-50))   ## here can be topo issue with original shp cant use intersection()?.
                  cat("dam building capacity layer\n")
               freqdam <-  extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "frequent to pervasive" ]
         if(length(freq) >0) { extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "frequent to pervasive" ] <- st_union(st_line_sample( st_cast(freqdam, "LINESTRING"),  density = 1/150, type = "regular") ) }
            occasdam <- extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "occasional" ] 
         if(length(occasdam) >0) { extra_HabLayers_c$geometry[extra_HabLayers_c$layer == "occasional" ] <- st_union(st_line_sample( st_cast(occasdam, "LINESTRING"),  density = 1/150, type = "regular") ) }
      
          rvsim$extra_HabLayers <- extra_HabLayers_c   
          rm(extra_HabLayers_c) 
          update_busy_bar(100)
 }
 removeUI('#text', immediate = T)
  }, ignoreInit=TRUE)
##jan26 was not init=T# need foolproofing here ensure all layers of extra_HabLayers_c$layer are there even if NULL for final extent
 
 




####################################################################################################### switch tab once init terrs simulation is complete
observeEvent( rvplot1$mapsim_init,{         
   updateTabsetPanel(session, "PageID", selected = "sim_tab") ## switch panel tab on start_sim - after computation
}, ignoreInit=TRUE, ignoreNULL=TRUE)

 

  

 

####################################################################################################### show_extra_lay_d() - generate final maps with hab+catch+dam cap layers as required  
### catch intcatch and dm cap layers  to plot
observeEvent( show_extra_lay_d()  ,{
  req(!is.null(rvsim$extra_HabLayers) )
  cat(paste0("add layers: ", show_extra_lay_d()))
     show_lays <- rvsim$extra_HabLayers[rvsim$extra_HabLayers$layer %in% show_extra_lay_d(),]
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
             cat("\ny0 map  -  ")  
 
    
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
             cat("y10 map  -  ")   }  
       
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
             cat("y3 map  -  ")  
              
                
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
             cat("y5 map  -  ")  
            cat(paste0("show ",plot_probability_range_y5_d(),"\n"))
             
  }, ignoreInit=TRUE, ignoreNULL=FALSE)
  
      
      

####################################################################################################### base (final extent) map 
output$mapsim0_finext  <- renderPlot({ if(is.null(rvsim$mapsim3) ){ return() } else { 
   cat("generate output map y0  -  ")
   update_busy_bar(100) 
  rvsim$mapsim0_finext  
 } }  ,  bg="transparent")
 
  # run on init to update with checked boxes but after maps are created a first time
observeEvent(input$sim_objective,{ 
     req(!is.null(rvplot1$mapsim_init0)) 
    req(!is.null(rvsim$startpop))
        
     cat(paste("update with ",input$sim_objective ,"  -  "))
 
   rvplot1$mapsim_init <- FUN_map_output_startingterr()[[1]] # jan shouldnt rerun whole code to add a layer here - correct
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
          cat(paste0("update map y5 with ", show_extra_lay_d()))
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
         cat("update map y3  -  ")    
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
         cat("update map y10  -  ")
    },ignoreInit=TRUE, ignoreNULL=FALSE)  
   

 

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
       cat("click 1:  coords  -  ")
       plot_data$x <- round(as.numeric(input$plot_click$x),-2)
       plot_data$y <- round(as.numeric(input$plot_click$y) ,-2) 
}, ignoreInit=FALSE) 



####################################################################################################### always - plot release points when clicking on candidate release site map
observe({
      req(!is.null(input$plot_click2) )
      plot_data2$x <- round(as.numeric(input$plot_click2$x),-2)
      plot_data2$y <- round(as.numeric(input$plot_click2$y) ,-2)
  cat("click2  -  ")

}) 



####################################################################################################### compute release points coords
####################################################################################################### triggered when bbox centroid, Nfams or protocol changes
toListen <- reactive({
 list(plot_data2$x, plot_data$Nfams , input$Nsites)
  })
 
observeEvent(toListen()  ,{  
   insertUI('#placeholder', ui = tags$p(id = 'text', paste('( computing coordinates )')), immediate = T)
    
   cat("isolate click2 coords  -  ") 
   req(plot_data2$x & plot_data2$y & nrow(ldat$LocalHab[ldat$LocalHab$layer == "suitable",])>0)   
   if( input$Nsites ==  "each_pt_on_map") {     p2x <- plot_data2$x[1]
                                                p2y <- plot_data2$y[1] 
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
  req(!is.null(ldat$LocalHab$layer)) #jan26 # i.e. a candidate rel site bbox
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
                                                cat("stored\n") 
                                                if(nrow(ptstp0)>0){
                                                      ptstptest <- "OK"
                                                      Npts_col <- rainbow(10)[seq(1,nrow(ptstp0))] 
                                                 ptstp <-data.frame(X=st_coordinates(ptstp0)[,1],Y=st_coordinates(ptstp0)[,2], family=seq(1:nrow(ptstp0)))
                                                } 
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
 
 
output$mapzoom  <-  renderPlot({  if(is.null(plot_data$mapzoom_final)) {return()} else {
      cat("display candidate site map\n")
      plot_data$mapzoom_final  }
},  bg="transparent")
    
  
 

############################################################################################ was useful when storing consecutive bbox centroids
############################################################################################ formatting of table when switching to single point 
 

output$table_saved_extents <- renderTable({if(is.null(savedat$table)) return(   )
  savedat$table },align = 'c', striped=T , spacing = 'xs',  hover = TRUE, width="100%",colnames = TRUE , rownames = FALSE   )
 
   

################################################################################################################ add potential candidate release site bbox onto init map
################################################################################################################ (side bar map Tab1 - dets stored into plot_data$tdat before into ldat)
output$plotmap1 <- renderPlot({ 
  cat("display map p1 (region)")
  savedat$p1 +       geom_sf(data= plot_data$tdat  , fill="orange", alpha=.5,col=1 )+
                      coord_sf(crs = mercproj,expand=F) +
                     annotation_scale(location = "bl") +
                     theme(legend.position = "none", axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     panel.border = element_rect(fill =NA),  axis.text = element_blank(), 
                     plot.background = element_rect(fill = "transparent", colour = NA),plot.margin = margin(0,0,0,0, "cm"))
} , bg="transparent")  

  



################################################################################################################ display selected GIS layers (or saved as images) on initial extent map 
################################################################################################################ (side-bar TAB 1 Glen Affric to Beauly)
observeEvent( show_suit_d(),{
   cat( "  -  display layers for: ",show_suit_d(),"\n") 
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
    savedat$p1 <- p1+coord_sf(crs = mercproj,expand=F) 
}, ignoreInit=FALSE, ignoreNULL=FALSE) 

     


############################################################################################ always = candidate release site plotted in side-bar map 
############################################################################################ (needs to be selected in ui to then be incorporated as ldat$ext_area_sel below)    
observe ({
  req(!is.null(plot_data$x))
  cat("buffer of", as.numeric(input$buffer_sel), "m  -  ")
   plot_data$tdat <-  st_as_sfc( st_bbox( st_buffer(st_sfc( st_point(c( plot_data$x,plot_data$y )), crs=mercproj),  as.numeric(input$buffer_sel) )  ))
 cat("candidate extent (temp)\n")
 } ) # for recovering coords in bookmked session
      
    



############################################################################################  trigger = input$pick_extent - make plotted bbox the new bbox for the candidate release site
observeEvent (input$pick_extent,{  
   insertUI('#placeholder', ui = tags$p(id = 'text', paste('( cropping GIS layers to extent )')), immediate = T)
   req(!is.null(plot_data$tdat)) # to work extent first - required for when returning to session with x and y coords saved
   cat("candidate site extent from  x,y,buff  -  ")
       x <- plot_data$x 
       y <- plot_data$y   
  
   ldat$ext_area_sel <-   st_as_sfc( st_bbox(st_as_sf(st_buffer(  plot_data$tdat  ,1 )   )))
   #st_union(plot_data$tdat) # incl the shapes not their bbox - useful later when using catchments as geoms
})  
 
 



############################################################################################  trigger = input$pick_extent - activate trigger: triggers$new_extent
observeEvent(input$pick_extent,{ 
    triggers$new_extent <- isolate(triggers$new_extent+1)
    triggers$new_sim <- 0
    cat("trigger GIS layers prep for selected extent:\n") 
   }) 
 



############################################################################################  trigger = triggers$new_extent - crop all GIS layer for candidate release site
observeEvent(triggers$new_extent,{       # not on init 
      ldat$ext_area <-  ldat$ext_area_sel # geom is a st_as_sfc poly (not a bbox) here 
     update_busy_bar(30)
         cat( "habitat  -  ")
     ldat$LocalHab <-  st_intersection(HabMapLayers,ldat$ext_area) 
         cat( "land  -  ")
     ldat$ext_coastline <- st_crop(coastline , ldat$ext_area)  
     update_busy_bar(50)
     ldat$ext_region_box <- st_crop(region_box,ldat$ext_area) 
     update_busy_bar(70) 
         cat( "rivers -  ")
     ldat$riv  <- st_union( st_crop( rivlines,ldat$ext_area) )
     ldat$LocalHab <- rbind(ldat$LocalHab, st_sf(geometry=  ldat$riv , layer="river") )      
         cat( "text  -  ")
     ldat$LocalBackLayers  <- st_crop(BackLayers,ldat$ext_area) 
     ldat$LocalBackText    <- st_intersection(BackText,ldat$ext_area) 
     ldat$LocalBackPoints    <- st_intersection(BackPoints,ldat$ext_area) 
     ldat$nonsel <- NULL 
  # if (input$sel_type=="catch") { # later use to sample only on selected catchments - will work for square sel too
     ldat$nonsel <- st_erase(ldat$ext_area,ldat$ext_area_sel)#plot_data$tdat)
  # } 
## generate cropped layers to show catchments on zoomed map  at same time    

         cat( "catchments\n")
     ldat$catch <- st_crop(catch,ldat$ext_area) 
     ldat$intcatch <- st_intersection(intcatch$geometry ,ldat$ext_area) 
     
     
         cat( "\ngenerate map for candidate release site")
     plot_data$mapzoom0 <- mapzoom_function(ext_region_box=ldat$ext_region_box,ext_coastline=ldat$ext_coastline,ext_area_sel=ldat$ext_area_sel,rivers=ldat$riv,nonsel=ldat$nonsel)  + 
                                            geom_sf(data=ldat$nonsel,col=NA,fill=alpha("grey",.7))+ coord_sf(crs = mercproj, expand=F)
 update_busy_bar(100)
     plot_data$mapzoomb <- plot_data$mapzoomc <- plot_data$mapzoom <- plot_data$mapzoom0
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
    cat("display layers on site map  -  ") 
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

 
     
