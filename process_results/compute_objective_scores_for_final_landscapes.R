compute.objective.scores <- function(landscape, wedRaster, arRaster) {
  
  
  options("rgdal_show_exportToProj4_warnings"="none")
  library(raster)
  library(plyr)
  
  ###################################################################
  ###################################################################
  
  #Set grid square folder name
  fol_gs = "data/SK86"
  
  #Set input param folder name
  fol_ip = "data/fitness/input_parameters"
  
  #Set parameters
  cellsize = 25
  objectives = c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees", "Farmer")
  
  #Set edge widths 
  #Must use this order: (wed,wlf,ar)
  edgecodes = c(24,12,11)  #ExpOp_GFS codes
  edgewidths = c(4.0,2.0,1.0)
  
  ###################################################################
  ###################################################################
  #LOAD PARAMETER FILES
  
  #Read in parameter csvs
  poll_names = read.csv(paste(fol_ip,"/poll_names.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  av = read.csv(paste(fol_ip,"/av.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  distances = read.csv(paste(fol_ip,"/distances.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  growth = read.csv(paste(fol_ip,"/growth.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  attract = read.csv(paste(fol_ip,"/attract.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  floralCover_earlylate = read.csv(paste(fol_ip,"/floralCover_earlylate.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  floralCover = read.csv(paste(fol_ip,"/floralCover.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
  
  # Read in optim landcover file for later
  optimLandcovers <- read.csv("data/optim_landcovers.csv")
  
  #Replace any NAs in floralCover and attract with 0
  floralCover_earlylate[is.na(floralCover_earlylate)==TRUE] = 0.0
  floralCover[is.na(floralCover)==TRUE] = 0.0
  attract[is.na(attract)==TRUE] = 0.0
  #Replace any negative values in floralCover and attract with 0
  floralCover_earlylate[floralCover_earlylate<0.0] = 0.0
  floralCover[floralCover<0.0] = 0.0
  attract[attract<0.0] = 0.0
  
  #Make parameter list
  parameters_BB = list(poll_names=poll_names,distances=distances,av=av,growth=growth,florNestInfo=list(floralCover=floralCover_earlylate,attract=attract))
  parameters_SB = list(poll_names=poll_names,distances=distances,av=av,growth=growth,florNestInfo=list(floralCover=floralCover,attract=attract))
  
  #Make parameter list for summer-flying solitaries
  poll_names2 = poll_names
  ind = which(poll_names2$solitary_social=="solitary")
  poll_names2$foraging_P1[ind] = "none"
  poll_names2$foraging_P2[ind] = "q"
  parameters_SB2 = parameters_SB
  parameters_SB2$poll_names = poll_names2
  
  ###################################################################
  #SOURCE FUNCTIONS
  
  source("code/fitness/computeFloralNesting_specifycolumnletter.R")
  source("code/fitness/getsteadystatepop.R")
  source("code/fitness/runpoll_3seasons.R")
  source("code/fitness/kerncalc.R")
  source("code/fitness/latfordisp.R")
  source("code/fitness/growth.func.R")
  source("code/fitness/getcropyieldsandincome_edited.R")
  
  ###################################################################
  #RUN SIMULATIONS
  
  ###########################
  #LOAD LANDCOVER RASTERS
  
  #Read in landscape raster
  landscape0 = landscape
  
  ###########################
  #LOAD EDGECOVER RASTERS
  
  #Read in edge rasters
  wed = wedRaster
  wlf = raster(paste(fol_gs,"/edgecover_rasters/hedgerows.tif",sep=""))
  ar = arRaster
  
  ###########################
  #PROCESS EDGECOVER RASTERS
  
  #Replace NAs with 0
  values(wed) = replace(values(wed),is.na(values(wed))==TRUE,0)
  values(wlf) = replace(values(wlf),is.na(values(wlf))==TRUE,0)
  values(ar) = replace(values(ar),is.na(values(ar))==TRUE,0)
  
  #Convert edge rasters to sqm
  edges_wed = wed*cellsize*edgewidths[1]
  edges_wlf = wlf*cellsize*edgewidths[2]
  edges_ar = ar*cellsize*edgewidths[3]
  
  #Conflate edge codes
  edgecodes_confl = c(24,12,11) #,0)
  
  #Make stack of edge rasters
  edgesstack = stack(edges_wed,edges_wlf,edges_ar) #,edges_us)
  
  ###########################
  #CALCULATE TOTAL EDGECOVER AREA AND REMAINING LANDCOVER AREA
  
  #Calculate total edge area
  edgearea = sum(edgesstack)
  
  #Calculate remaining landcover area
  lcarea = edgearea
  values(lcarea) = cellsize*cellsize-values(edgearea)
  values(lcarea) = ifelse((values(lcarea)<0.0),0.0,values(lcarea))
  
  
  ###########################
  #RUN POLL4POP
  

  # Initialise output vector
  scores <- data.frame(matrix(ncol=4, nrow=0, dimnames=list(NULL, c("gnb", "tnb", "gns", "farmer"))))
  output <- c()
  
  # Initialise spring visitation rate vector for crop pollination service function 
  springVisRaster <- landscape
  values(springVisRaster) <- 0
  
  #Assess each objective
  nbees = 1
  for (s in 1:length(objectives))
  { #Get guild name
    obj = objectives[s] # EDIT: Changed 'beesp' to 'obj' to reflect arguments of optimisation routine. Subsequent object calls of 'beesp' also changed.
    
    #If guild is bumblebee
    if (grepl("Bumble",obj)==TRUE)
    {nfloral = 3
    #Compute nesting and foraging resource maps
    nf<-computeFloralNesting_specifycolumnletter(landuseMap=landscape,edgesMap=edgesstack,unitEdges="sqm",widthEdges=1,landuseCodes,bees=obj,num_floral=nfloral,florNestInfo=parameters_BB$florNestInfo,codeEdges=edgecodes_confl,cell.size=cellsize,"b",paramList=parameters_BB)
    #Compute population growth
    poll<-getsteadystatepop(nf,parameters_BB,obj,nbees,nfloral,cellsize)
    
    #Get total visitation rate for spring
    spring_visrate_beesp = sum(values(poll$flowvis[[1]][[1]]))+sum(values(poll$flowvis[[1]][[2]]))
    #Extract visitation rate for summer 
    summer_visrate_beesp = sum(values(poll$flowvis[[1]][[3]]))
    
    # Add sping visrate values to raster for crop pollination service function
    springVisRaster <- springVisRaster + poll$flowvis[[1]][[2]] # (just late spring as this is when OSR/ field beans are in flower)
    }
    
    #If guild is solitary bee
    if (grepl("Solitary",obj)==TRUE) 
    {nfloral = 2
    #Compute nesting and foraging resource maps
    nf<-computeFloralNesting_specifycolumnletter(landuseMap=landscape,edgesMap=edgesstack,unitEdges="sqm",widthEdges=1,landuseCodes,bees=obj,num_floral=nfloral,florNestInfo=parameters_SB$florNestInfo,codeEdges=edgecodes_confl,cell.size=cellsize,"b",paramList=parameters_SB)
    
    #Do spring-flying solitaries
    #Compute population growth
    poll<-getsteadystatepop(nf,parameters_SB,obj,nbees,nfloral,cellsize)
    
    #Extract visitation rate for spring
    spring_visrate_beesp = sum(values(poll$flowvis[[1]][[1]]))
    
    # Add sping visrate values to raster for crop pollination service function
    springVisRaster <- springVisRaster + poll$flowvis[[1]][[1]]#Do summer-flying solitaries
    
    #Compute population growth
    poll<-getsteadystatepop(nf,parameters_SB2,obj,nbees,nfloral,cellsize)
    
    #Extract visitation rate for summer
    summer_visrate_beesp = sum(values(poll$flowvis[[1]][[2]]))
    }
    
    if (grepl("Farmer",obj)==FALSE && any(grepl("bees",objectives))){
      total_visrate_beesp <- (-1) * spring_visrate_beesp * summer_visrate_beesp # *(-1) as the nsga2r minimises the function and we want to maximise the function. 
      output <- c(output, total_visrate_beesp)
    }
  }
  
  # Add crop.pollination function for if grepl "Pollserv"(e.g) = TRUE
  for (s in 1:length(objectives)){
    obj = objectives[s]
    if (grepl("Farmer",obj)==TRUE) { 
      visitationtoyield <- read.csv("data/fitness/input_parameters/VisitationYieldRelationship.csv")
      yieldfactors_temporal <- read.csv("data/fitness/input_parameters/NationalCropProductivityperYear.csv")
      
      farmerReturns <- getcropyieldsandincome(visrate=springVisRaster,cropcode=optimLandcovers$Code_ExpOp_GFS,landscape,lcarea,refyr=2020,cellsize,visitationtoyield,yieldfactors_temporal, objectives)
      income <- sum(values(farmerReturns), na.rm=T)
      output <- c(output, -income)
    }
  }
  
  scores[nrow(scores)+1,]<-output
  return(scores)
  
}