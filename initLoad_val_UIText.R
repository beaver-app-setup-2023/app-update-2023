
############################ ABOUT tab text  
############## refs
ref_SNHReport2020 <- paste0("Beaver Management Report for 2020, Published by NatureScot, Scotland's Nature Agency, in August 2021. Available from www.nature.scot/doc/beaver-management-report-2020\n\n")
ref_SNHReport2015 <- paste0('Shirley, M.D.F., Harrington, L.A. & Mill, A.C. 2015. A model simulating potential colonisation by Eurasian beaver (Castor fiber) following reintroduction to Scotland. Scottish Natural Heritage Commissioned Report No. 814.\n\n')
ref_SNHReport1997 <- paste0("Macdonald, D, Maitland, P, Rao, S, Rushton, S, Strachan, R, and Tattersall, F (1997). Development of a protocol for identifying beaver release sites. SNH Research, Survey & Monitoring 93, Battleby.\n\n")
ref_SNHReport_fromRep1 <- paste0("Beavers in Scotland report to Scottish Government, and other reports are available from: www.nature.scot/professional-advice/safeguarding-protected-areas-and-species/protected-species/protected-species-z-guide/protected-species-beaver/beavers-scotland")
ref_SNHReport_fromRep2 <- paste0("Management Framework for Beavers in Scotland: www.nature.scot/professional-advice/safeguarding-protected-areas-and-species/protected-species/protected-species-z-guide/protected-species-beaver/management")

##############RPackages
AboutText_R <- paste0( 
'App building.
shiny; shinyEffects; shinyBS; shinybusy; shinyjs; shinythemes; shinyWidgets;  

Data formatting and export.
Rfast; dplyr; grid; stringr; zip; png; here; 

Geometrical operations and spatial data encoding.
sf; raster; spatstat; lwgeom; rgdal; rgeos; stars; nngeo; 

Graphics and maps creation.
ggplot2; ggspatial; viridis; plotly; ggnewscale; ggrepel; rgee;')


About_OutputValues <- paste0('A simulation run includes the step-by-step execution of the population expansion model from year 0 (initial territories) to year 5 (or year 10 if selected). Here, simulation outputs include 15 runs starting from a similar set of initial population territories. 
Once all simulation runs are complete, the number of runs predicting occupancy on a given year is summed for each cell. The total number of runs predicting occupancy is then partitioned into pre-determined categories to facilitate summary mapping: 1 to 3 runs, 3 to 5 runs, 5 to 10 runs, 10 to 15 runs. Outputs can also be visualised and exported as the convex hulls for the set of cells included at a given level of simulated occupancy.
Each cell is then coloured according to the number of runs associated with it. Note that due to the stochasticity in dispersal routes and choice of habitat for building territories, low beaver densities in relation to suitable habitat area will often be associated with low numbers of runs per cell.'
)



   
AboutUsability_computation  <- paste0(
"
Release protocol 
Only one single group of beavers can be release within a habitat cell; it is currently a model requirement and is representative of practice in the field. The habitat map resolution is of 1ha. The algorithm generating automated release point coordinates aims for the target number of released groups to be distributed 300m apart within the extent and all within suitable habitat, ensuring only one beaver group can be attributed per cell. In other protocols, points are located manually or concentrated around a single starting point. In those cases, the algorithm verifies that the assumptions of distance, suitability and extent boundaries are respected for the set of points, and relocates points to a nearest adequate location when they are not. This process may take time as it is both iterative and considers the changing distribution of all points.

Translocated population size 
Given the individual-based nature of the algorithm, the number of individuals in the simulation affects the amount of computations required to account for all the beaver population. Individual dispersal routes in particular may demand longer computation time when the number of dispersers (sub-adults) increase or when dispersal routes become longer (due to reduced options for instance).

Landscape and habitat
The distribution of habitat quality patches will influence dispersal routes. Failed dispersal may take longer to simulate as each attempt is computed even if unproductive, before the individual is considered stranded (and removed from the simulated population). Dispersal routes will also increase in length when animals have to travel far to find a patch for settlement, or test then leave sub-optimal patches, or have to move within large dispersal-only patches.

Duration of simulation runs 
Computation time will increase with the number of individuals alive in the simulation; in positive growth, the difference in overall computation time between a 5 and 10 years simulation for each run may be significant.
   "  )



AboutDownloading <- paste0("Simulation outputs can be downloaded at once. When all the simulation runs are completed, click on the download button located in the 'Simulate range expansion' tab, next to the map displaying initial settlement under the simulated range expansion output. Downloaded output files will be referenced using the date at time of download, unless a customised reference is entered in the adjacent text box (optional).")

AboutBookmarking <- paste0('Unlike web sites, this Shiny App does not contain the current state of the app in its URL, so the current state of the App can not be shared or accessed again using its URL. A bookmark button was added in the navigation bar to allow returning to a specific release location. Clicking the download button will generate a URL that stores the coordinates for the current candidate release site location. Copy that link and paste it in your browser to reaccess or share the App with focus at the given location.')   





 
### about model
 AboutOutput_f <-  paste0("Note that downloading output data after simulating starting territories and before simulating population growth will generate a .zip file containing only the currently existing output.")   
 AboutOutput0 <-   paste0("Simulation outputs are downloaded as a .zip file containing the following data compiled during the session:")    
 AboutOutput1a <-   paste0("- a text document summarising the selected release parameters")    
 AboutOutput1b <-   paste0("- a summary table of annual statistics about the estimated beaver population abundance as number of individuals and territories ")    
 AboutOutput2a <-   paste0("- number of simulation runs estimating cell occupancy on years 3, 5 and 10 (if selected) after release
- convex hulls associated with all relevant combinations of the above")   
 AboutOutput2b <-   paste0("- release points
- initial territories
- extent of the initial release site")    


### about demog params
AboutPars_fams0 <-  paste0("Each territory block is modelled as supporting one family of beavers. Families can include up to four age classes, each associated with stage-specific properties dictating how the individuals move and contribute to population growth:")   
AboutPars_fams1 <-  paste0("adults:
all individuals older than 2; they are capable of breeding and have settled into their territory - they do not disperse.")   
 AboutPars_fams2 <-  paste0("sub-adults:
1 to 2 year old; they do not breed but will disperse in order to settle into their own territory where they will start the following year as adults.")  
AboutPars_fams3 <-  paste0("juveniles:
1 year of age, do not disperse nor breed yet, and share the parental territory until the following year.")  
 AboutPars_fams4 <-  paste0("new young:
individuals born on a given year (under 1 year old) and who share the parental territory.")  
   
 
### about params 
AboutPars_suit<-  paste0("Spatial dynamics of the beavers integrate with an underlying habitat map provided by NatureScot, used to describe territories and movement. The habitat map is divided into three categories of habitat: ???suitable??? habitat which can support a beaver territory; dispersal habitat which cannot support a territory but can be used by beavers for dispersal; and ???unsuitable habitat??? which can neither support a territory nor be used by beavers. The minimum total value allowing settlement of a beaver into their new territory block is 8ha of suitable habitat.")   
AboutPars_disp<-  paste0("Dispersal occurs annually when sub-adults leave the family territory block. The maximum length of the annual dispersal route is 21km. When the dispersing sub-adult cannot gather the minimum area of suitable habitat required for its settlement, the individual is considered stranded and removed from the population.")  
AboutPars_surv<-  paste0("Survival is quantified annually as function of mortality separately for juvenile (0.92) is and adults and sub-adults (0.87). Failed dispersal, which occurs when settlement into a new territory was not completed, also results in the death of the sub-adult. The average breeding probability is 0.6; the average litter size is 1.95, with a maximum value of 9 young produced annually per adult pair.")  
AboutTheModel <- paste0("The landscape population model of Shirley et al. (2015) predicts the spatial spread of beavers from spatially defined territories and a habitat map. The model is described in detail in the 2015 report but for summary the main processes are outlined here. The population model is dynamic and runs on an annual timestep, the spatial dynamics of the beavers integrate with an underlying habitat map used to describe territories and movement.  The habitat map is divided into three categories of habitat: ???suitable??? habitat which can support a beaver territory; dispersal habitat which cannot support a territory but can be used by beavers; and ???unsuitable habitat??? which can neither support a territory nor be used by beavers. The population dynamics are based around individual beaver families. 
This is a process-based model, meaning that life-history processes (population dynamics) of mortality, mating, birth, and dispersal are explicitly modelled at the family level on a yearly timestep. Each family consists of a pair of breeding beavers plus zero or more non-breeding adults, subadults, and juveniles. Each family also has a spatially-referenced territory containing sufficient suitable habitat to support the family. Each year, each beaver has a probability of dying equal to the sex-specific mortality rate. Each beaver family with both a breeding male and a breeding female has a probability of producing a litter of kits. Some of these kits will join the family as subadults, but others will disperse away from the natal territory. 
Dispersal is the most complex process to model since it subsumes a multitude of behaviours. In the model each disperser leaves the natal territory and initially travels through beaver-permeable habitat without attempting to settle (up to a maximum dispersal distance). It then attempts to find or establish a territory. If it finds another beaver territory that it can join as a breeding adult then it does so, otherwise it searches for suitable habitat that falls outside another territory. If it locates sufficient contiguous habitat then it establishes a territory. This beaver must be joined by a dispersing mate before it can breed.")
 