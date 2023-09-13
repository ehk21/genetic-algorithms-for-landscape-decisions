################################################################################
################################################################################
#' 
#' Updated nsga2R function to suit landscape optimisation. Now has new input arguments according to fitness function... 
#' 
#' @param objectives vector of objectives to be optimised (i.e. whose fitness will be evaluated during the fitness function)
#' @param optimLandcovers data frame of land covers to be shuffled during optimisation and their respective expOp codes.
#' @param optimFields data frame consisting of current landcover for each field included in the optimisation (and column for new landcover as defined by shuffled values in x)
#' @param optimRaster &
#' @param otherRaster rasters of landscape separated by fields whose landcover will and won't be shuffled
#' @param optimPixels by-pixel dataframe of landcovers of fields to be optimised
#' @param boundaries vector of boundaries separated by field ID, as generated in prepare landscape function
#' @param otherMargins raster of arable field margins in fixed portion of the landscape, as generated in prepare landscape function
#' @param otherWoodland raster of woodland boundaries in fixed portion of the landscape, as generated in prepare landscape function
#' @param optimPixels by-pixel dataframe of landcovers of fields to be optimised
#' @param gridSquare vector containing details of specified grid square (used for creating reference raster & calling relevent rasters in fitness function).
#'                  format: c(gridsquare name, xmin, xmax, ymin, ymax)
#'
#' Related functions: fitness, boundedPolyMutation_edited, boundedSBXover_edited
#' 
################################################################################
################################################################################

nsga2R.edited <-
  function(fn, varNo, objDim, lowerBounds=rep(-Inf,varNo), upperBounds=rep(Inf,varNo),
           popSize=100, tourSize=2, generations=20, cprob=0.7, XoverDistIdx=5, mprob=0.2, MuDistIdx=10, 
           objectives, optimLandcovers, optimFields, optimRaster, otherRaster, boundaries, otherMargins, otherWoodland, optimPixels, gridSquare) {
    
    # Load packages
    # library(plyr)
    # library(nsga2R)
    
    # Source functions
    source("code/fitness/fitness_crop_poll.R")
    source("code/nsga2r_edited/boundedPolyMutation_edited.R")
    source("code/nsga2r_edited/boundedSBXover_edited.R")

    cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
    cat("\n")
    cat("initializing the population")
    print(Sys.time())
    cat("\n")
    parent <- t(sapply(1:popSize, function(u) array(round(runif(length(lowerBounds),(lowerBounds - 0.5), (upperBounds + 0.5)))))); # EDITED added round() function
    parent <- cbind(parent, t(apply(parent,1,fn, objectives=objectives, optimLandcovers=optimLandcovers, 
                                    optimFields=optimFields, optimRaster=optimRaster, otherRaster=otherRaster, boundaries=boundaries, otherMargins=otherMargins, otherWoodland=otherWoodland, optimPixels=optimPixels, gridSquare=gridSquare))); # EDITED: added additional arguments from fitness function
    
    cat("ranking the initial population")
    cat("\n")  
    ranking <- fastNonDominatedSorting(parent[,(varNo+1):(varNo+objDim)]);
    # Rank index for each chromosome
    rnkIndex <- integer(popSize);
    i <- 1;
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i;
      i <- i + 1;
    }
    parent <- cbind(parent,rnkIndex);
    initialPop <- parent # EDIT save  configuration of initial population for use in analyses

    if(length(unique(objectives))==1){ # EDIT: added scores object to track how the fitness changes over each generation
      rank=c(1,2,3,4,5)
      } else {rank = 1}
    
    if(length(which(parent[,ncol(parent)] %in% rank))==1){
      scores <- t(as.data.frame(c(0, parent[which(parent[,ncol(parent)] %in% rank),(ncol(parent)-(objDim)):ncol(parent)]))) 
    } else {
      scores <- data.frame(0, parent[which(parent[,ncol(parent)] %in% rank),(ncol(parent)-(objDim)):ncol(parent)]) 
    }
    colnames(scores) <- c("gen", objectives, "rank") # EDIT ends
    
    cat("crowding distance calculation")
    cat("\n")
    objRange <- apply(parent[,(varNo+1):(varNo+objDim)], 2, max) - apply(parent[,(varNo+1):(varNo+objDim)], 2, min);
    cd <- crowdingDist4frnt(parent,ranking,objRange);
    parent <- cbind(parent,apply(cd,1,sum));
    for (iter in 1: generations){
    cat("---------------generation---------------",iter,"starts")
      print(Sys.time())
      cat("\n")
      cat("tournament selection")
      cat("\n")
      matingPool <- tournamentSelection(parent,popSize,tourSize);
      cat("crossover operator")
      cat("\n")
      childAfterX <- boundedSBXover_edited(matingPool[,1:varNo],cprob,optimLandcovers, objectives); #EDITED: now sources edited function which rounds values # Only design parameters are input as the first argument
      cat("mutation operator")
      cat("\n")
      childAfterM <- boundedPolyMutation_edited(childAfterX,lowerBounds,upperBounds,mprob,MuDistIdx); #EDITED substituted in edited boundedPolyMutation function
      cat("evaluate the objective fns of childAfterM")
      cat("\n")
      childAfterM <- cbind(childAfterM, t(apply(childAfterM,1,fn, objectives=objectives, optimLandcovers=optimLandcovers, 
                                                optimFields=optimFields, optimRaster=optimRaster, otherRaster=otherRaster, boundaries=boundaries, otherMargins=otherMargins, otherWoodland=otherWoodland, optimPixels=optimPixels, gridSquare=gridSquare))); # EDITED: added in additional arguments from fitness function
     
      
      # Consider use child again and again ...
      cat("Rt = Pt + Qt")
      cat("\n")
      # Combine the parent with the childAfterM (No need to retain the rnkIndex and cd of parent)
      parentNext <- rbind(parent[,1:(varNo+objDim)],childAfterM)
      cat("ranking again")
      cat("\n")
      ranking <- fastNonDominatedSorting(parentNext[,(varNo+1):(varNo+objDim)]);
      i <- 1;
      while (i <= length(ranking)) {
        rnkIndex[ranking[[i]]] <- i;
        i <- i + 1;
      }
      parentNext <- cbind(parentNext,rnkIndex);
      
      
      if(length(which(parentNext[,ncol(parentNext)] %in% rank))==1){ # EDIT: added scores object to track how the fitness changes over each generation
        genScores <- t(as.data.frame(c(iter, parentNext[which(parentNext[,ncol(parentNext)] %in% rank),(ncol(parentNext)-(objDim)):ncol(parentNext)])))
      } else {
        genScores <- data.frame(iter, parentNext[which(parentNext[,ncol(parentNext)] %in% rank),(ncol(parentNext)-(objDim)):ncol(parentNext)]) 
      }
      colnames(genScores) <- c("gen", objectives, "rank")
      
      scores <- rbind(scores, genScores) # EDIT ends
      
      cat("crowded comparison again")
      cat("\n")
      objRange <- apply(parentNext[,(varNo+1):(varNo+objDim)], 2, max) - apply(parentNext[,(varNo+1):(varNo+objDim)], 2, min);
      cd <- crowdingDist4frnt(parentNext,ranking,objRange);
      parentNext <- cbind(parentNext,apply(cd,1,sum));
      parentNext.sort <- parentNext[order(parentNext[,varNo+objDim+1],-parentNext[,varNo+objDim+2]),];
      cat("environmental selection")
      cat("\n")
      # choose the first 'popSize' rows for next generation
      parent <- parentNext.sort[1:popSize,]
      cat("---------------generation---------------",iter,"ends")
      cat("\n")
      if (iter != generations) {
        cat("\n")
        cat("********** new iteration *********")
        cat("\n")
      } else {
        cat("********** stop the evolution *********")
        cat("\n")
      }
    }
    # report on nsga2 settings and results
  result = list(functions=fn, parameterDim=varNo, objectiveDim=objDim, lowerBounds=lowerBounds,
                upperBounds=upperBounds, popSize=popSize, tournamentSize=tourSize,
                generations=generations, XoverProb=cprob, XoverDistIndex=XoverDistIdx,
                mutationProb=mprob, mutationDistIndex=MuDistIdx, parameters=parent[,1:varNo],
                objectives=parent[,(varNo+1):(varNo+objDim)], paretoFrontRank=parent[,varNo+objDim+1],
                crowdingDistance=parent[,varNo+objDim+2], scores=as.data.frame(scores), initialPop=initialPop);
  class(result)="nsga2R";
  return(result)
  }
