# save pdf of rasters for rank 1 landscapes in final populations of specified optimisations.

rm(list=ls())

library(sf)
library(rasterVis)
library(dplyr)
library(gridExtra)

source("code/process/output_optim_rasters.R")

optimsDF <- read.csv("data/optims.csv")

# format dataframe
optimsDF$complete[which(optimsDF$complete == "t")] <- TRUE
optimsDF$complete[which(optimsDF$complete == "f")] <- FALSE
optimsDF$complete <- as.logical(optimsDF$complete)

# initiate pdf...
pdf("results/optimisations/TNB_rasters", width=20, height=55)

for(j in c(201:226)){
  if(isTRUE(optimsDF$complete[j])){
    
    # load results
    result <- readRDS(paste0("results/optimisations/results/", optimsDF$optim[j], optimsDF$number[j]))
    scores <- result[["optim"]][["scores"]]
    finalPop <- data.frame(result[["optim"]][["parameters"]])
    optimLandcovers <- result[["optimLandcovers"]]
    radius <- result[["radius"]]
    pareto <- result[["optim"]][["paretoFrontRank"]]
    objectives <- result[["objectives"]]
    initialPop <- data.frame(result[["optim"]][["initialPop"]])
    shuffleCodes <- result$shuffleCodes
    gridSquare <- c("SK86", 480000, 490000 ,360000, 370000)
    landcoverData <- read_sf("data/SK86/lcm-2020-vec_4558456.gpkg")
    
    name <- paste0(optimsDF$optim[j], "_", radius, "_", optimsDF$number[j])
    
    # subset populations to just rank1 (multi-objective) or rank 1-3(single-objective)
    if(length(unique(objectives))==1){
      finalPop <- finalPop[which(c(1,2,3) %in% pareto),]
      colnames(finalPop) <- c(1:ncol(finalPop))
    } else{
      finalPop <- finalPop[which(pareto==1),]
      colnames(finalPop) <- c(1:ncol(finalPop))
    }
    
      optimRasters <- list()
      for (i in 1:nrow(finalPop)){
        optimRasters[[i]] <- output.raster(finalPop, rowNo=i, radius, optimLandcovers)
      }
      
  do.call(grid.arrange,list(grobs=optimRasters, ncol=3, top=name))
      
  }}

dev.off()    
