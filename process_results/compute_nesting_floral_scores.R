# Poll4pop function used to compute floral and nesting scores for optimised landscapes during analysis

compute.nesting.floral.scores <- function(landscape, wedRaster, arRaster, objectives) {


      options("rgdal_show_exportToProj4_warnings"="none")
      library(raster)
      library(plyr)
      
      ###################################################################
      ###################################################################
      
      #Set input param folder name
      fol_ip = "data/fitness/input_parameters"
      
      #Set parameters
      cellsize = 25
      guilds = c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees")
      
      #Set edge widths 
      #Must use this order: (wed,wlf,ar)
      edgecodes = c(24,12,11)  #ExpOp_GFS codes
      edgewidths = c(4.0,2.0,1.0)
      
      ###################################################################
      ###################################################################
      #LOAD PARAMETER FILES
      
      #Read in parameter csvs
      poll_names = read.csv(paste(dir,fol_ip,"/poll_names.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      av = read.csv(paste(dir,fol_ip,"/av.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      distances = read.csv(paste(dir,fol_ip,"/distances.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      growth = read.csv(paste(dir,fol_ip,"/growth.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      #guild_params = read.csv(paste(dir,fol_ip,"/poll_guild_params.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      attract = read.csv(paste(dir,fol_ip,"/attract.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      floralCover_earlylate = read.csv(paste(dir,fol_ip,"/floralCover_earlylate.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      floralCover = read.csv(paste(dir,fol_ip,"/floralCover.csv",sep=""),header=TRUE,stringsAsFactors=FALSE,na=c("",NA))
      
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
      
      source(paste(dir,"code/fitness/computeFloralNesting_specifycolumnletter.R",sep=""))
      source(paste(dir,"code/fitness/getsteadystatepop.R",sep=""))
      source(paste(dir,"code/fitness/runpoll_3seasons.R",sep=""))
      source(paste(dir,"code/fitness/kerncalc.R",sep=""))
      source(paste(dir,"code/fitness/latfordisp.R",sep=""))
      source(paste(dir,"code/fitness/growth.func.R",sep=""))
      
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
      wlf = readRDS("data/WLF2km")
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
      
      #Do each guild
      nbees = 1
      
      nfScores <- data.frame(matrix(ncol=5, nrow=0, dimnames=list(NULL, c("guild", "spring1f", "spring2f", "summerf", "nesting"))))
      
      for (s in 1:length(unique(guilds)))
      {#Get guild name
        beesp = guilds[s]
        
        #If guild is bumblebee
        if (grepl("Bumble",beesp)==TRUE)
        {nfloral = 3
        #Compute nesting and foraging resource maps
        # assign(beesp, computeFloralNesting_specifycolumnletter(landuseMap=landscape,edgesMap=edgesstack,unitEdges="sqm",widthEdges=1,landuseCodes,bees=beesp,num_floral=nfloral,florNestInfo=parameters_BB$florNestInfo,codeEdges=edgecodes_confl,cell.size=cellsize,"b",paramList=parameters_BB)
        nf<-computeFloralNesting_specifycolumnletter(landuseMap=landscape,edgesMap=edgesstack,unitEdges="sqm",widthEdges=1,landuseCodes,bees=beesp,num_floral=nfloral,florNestInfo=parameters_BB$florNestInfo,codeEdges=edgecodes_confl,cell.size=cellsize,"b",paramList=parameters_BB)
        scores <- list("guild"=beesp, "spring1f"=sum(values(nf[["floral"]][[1]]@layers[[1]])),"spring2f"=sum(values(nf[["floral"]][[1]]@layers[[2]])), "summerf"=sum(values(nf[["floral"]][[1]]@layers[[3]])), "nesting"= sum(values(nf[["nest"]][[1]])))
        nfScores <- rbind(nfScores, scores)
        }
        
        #If guild is solitary bee
        if (grepl("Solitary",beesp)==TRUE)
        {nfloral = 2
        #Compute nesting and foraging resource maps
        nf<-computeFloralNesting_specifycolumnletter(landuseMap=landscape,edgesMap=edgesstack,unitEdges="sqm",widthEdges=1,landuseCodes,bees=beesp,num_floral=nfloral,florNestInfo=parameters_SB$florNestInfo,codeEdges=edgecodes_confl,cell.size=cellsize,"b",paramList=parameters_SB)
        scores <- list("guild"=beesp, "spring1f"=sum(values(nf[["floral"]][[1]]@layers[[1]])),"spring2f"=NA, "summerf"=sum(values(nf[["floral"]][[1]]@layers[[2]])), "nesting"= sum(values(nf[["nest"]][[1]])))
        nfScores <- rbind(nfScores, scores)
        
        }
      }
      
      return(nfScores)

  }