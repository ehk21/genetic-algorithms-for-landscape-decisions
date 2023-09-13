################################################################################
################################################################################
#' ehk21@ic.ac.uk - May 2022
#' 
#' Objective function consists of: maximising floral visitation rate for ground-nesting bumblebees; maximising floral visitation rate for tree-nesting bumblebees; 
#' maximising floral visitation rate for ground nesting solitary bees; maximising crop-pollination services (economic benefits)
#'
#' @param x vector of shuffled numbers between upper bound and lower bound whih correspond to row values in optim_landcovers data frame (for assigning new landcover during optimisation) 
#' @param objectives vector of objectives to be optimised (i.e. whose fitness will be evaluated during the fitness function)... options are: c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees", "CropPollinationService")
#' @param optimLandcovers data frame of land covers to be shuffled during optimisation and their respective expOp codes.
#' @param optimFields data frame consisting of current landcover for each field included in the optimisation (and column for new landcover as defined by shuffled values in x)
#' @param optimRaster &
#' @param otherRaster rasters of landscape separated by fields whose landcover will and won't be shuffled
#' @param boundaries vector of boundaries separated by field ID, as generated in prepare landscape function
#' @param otherMargins raster of arable field margins in fixed portion of the landscape, as generated in prepare landscape function
#' @param otherWoodland raster of woodland boundaries in fixed portion of the landscape, as generated in prepare landscape function
#' @param optimPixels by-pixel dataframe of landcovers of fields to be optimised
#' @param gridSquare vector containing details of specified grid square (used for creating reference raster & calling relevent rasters in fitness function).
#'                  format: c(gridsquare name, xmin, xmax, ymin, ymax)
#' 
#' @return vector of final fitness scores for each objective
#' 
#' Related functions: new.landscape, computeFloralNesting_specifycolumnletter, getsteadystatepop, runpoll_3seasons, kerncalc, latfordisp, growth.func
#' 
################################################################################
################################################################################

fitness<- function(x, objectives, optimLandcovers, optimFields, optimRaster, 
                   otherRaster, boundaries, otherMargins, otherWoodland, optimPixels, gridSquare){
  
  #library(raster)
  
  # #Set working dir
  # dir = getwd()
  
  #Set input param folder name
  fol_ip = "data/fitness/input_parameters"
  
  #Set grid square folder name
  fol_gs = "data/SK86"
  
  #Set parameters
  cellsize = 25
  
  # EDIT: Changed 'guilds' to 'objectives' to reflect arguments of optimisation 
  # routine. Subsequent object calls of 'guilds' also changed. 'objectives' is 
  # now called as an argument.
  
  #Set edge widths 
  #Must use this order: (wed,wlf,ar) 
  edgecodes = c(24,12,11) #,0) #ExpOp_GFS codes
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
  #parameters = list(guild_params=guild_params,habitat_params=habitat_params)
  #print(parameters)
  
  #Make parameter list for summer-flying solitaries
  poll_names2 = poll_names
  ind = which(poll_names2$solitary_social=="solitary")
  poll_names2$foraging_P1[ind] = "none"
  poll_names2$foraging_P2[ind] = "q"
  parameters_SB2 = parameters_SB
  parameters_SB2$poll_names = poll_names2
  #print(parameters_SB2$poll_names)
  
  ###################################################################
  #SOURCE FUNCTIONS
  
  source("code/fitness/computeFloralNesting_specifycolumnletter.R")
  source("code/fitness/getsteadystatepop.R")
  source("code/fitness/runpoll_3seasons.R")
  source("code/fitness/kerncalc.R")
  source("code/fitness/latfordisp.R")
  source("code/fitness/growth.func.R")
  source("code/fitness/new_landscape.R")
  source("code/fitness/getcropyieldsandincome_edited.R")
  
  ###################################################################
  #RUN SIMULATIONS
  
  ###########################
  #LOAD LANDCOVER RASTER
  
  # Create new landscape raster
  landscapeRasters <- new.landscape(x, optimLandcovers=optimLandcovers, optimFields=optimFields, optimRaster=optimRaster, 
                                    otherRaster=otherRaster, otherMargins=otherMargins, boundaries=boundaries, otherWoodland=otherWoodland, optimPixels=optimPixels, gridSquare=gridSquare)
  landscape0 <- landscapeRasters$landcoverRaster
  landscape = landscape0
  
  #Read in edge rasters
  wed = landscapeRasters$woodlandRaster
  wlf = raster(paste(fol_gs,"/edgecover_rasters/hedgerows.tif",sep=""))
  ar = landscapeRasters$marginsRaster
  
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
  edgecodes_confl = c(24,12,11)
  
  #Make stack of edge rasters
  edgesstack = stack(edges_wed,edges_wlf,edges_ar) 
  
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
  
  return(output)
  }


