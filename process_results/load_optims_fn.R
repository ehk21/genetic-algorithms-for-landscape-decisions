# function to process and compile results of landscape optimisations, including: number of pixels of each landcover, 
# nesting/floral scores for landscapes, final population assemblages, fitness scores of optimisations per generation, 
# landscape metrics e.g. patch density of each landscape

load.optims <- function(optimsDF, rows, pix, pop, fit, nf, dens){

  # load packages
  library(reshape2)
  library(sf)
  library(raster)
  library(rgdal)
  library(sp)
  library(landscapemetrics)
  
  # source functions
  source("code/process/output_rasters.R")
  source("code/process/compute_nesting_floral_scores.R")
  
  # format dataframe
  optimsDF$complete[which(optimsDF$complete == "t")] <- TRUE
  optimsDF$complete[which(optimsDF$complete == "f")] <- FALSE
  optimsDF$complete <- as.logical(optimsDF$complete)
  
  # generate dataframes
  pixels <- data.frame(matrix(ncol=12, nrow=0, dimnames=list(NULL, c( "Code_ExpOp_GFS", "pixels", "optim", "radius", "rep", "pop", "landscape", "clumpiness", "patchDensity", "patchNo", "patchArea", "classArea"))))
  floralNestingScores <- data.frame(matrix(ncol=10, nrow=0, dimnames=list(NULL, c("optim", "radius", "rep", "pop", "landscape","guild", "spring1f", "spring2f", "summerf", "nesting"))))
  populations <- data.frame(matrix(ncol=7, nrow=0, dimnames=list(NULL, c("landscape", "optim", "radius", "rep", "pop", "field", "Code_ExpOp_GFS"))))
  fitnessScores <- data.frame(matrix(ncol=7, nrow=0, dimnames=list(NULL, c("gen", "rank", "optim", "radius", "rep", "objective", "score"))))
  patchDensity <- data.frame(matrix(ncol=9 ,nrow=0, dimnames=list(NULL, c("optim", "radius", "rep", "pop", "landscape", "patchDensity", "patchNo", "patchArea", "totalArea"))))

  for(j in rows){
    if(isTRUE(optimsDF$complete[j])){
      
      print(j)
      print(paste0(optimsDF$optim[j], optimsDF$number[j]))
  
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
    
      # subset populations to just rank1 (multi-objective) or rank 1-3(single-objective)
      if(length(unique(objectives))==1){
        finalPop <- finalPop[which(c(1,2,3) %in% pareto),]
        colnames(finalPop) <- c(1:ncol(finalPop))
        initialPop <- initialPop[which(c(1,2,3) %in% initialPop$rnkIndex), 1:(ncol(initialPop)-(length(objectives)+1))]
        colnames(initialPop) <- c(1:ncol(initialPop))
      } else{
        finalPop <- finalPop[which(pareto==1),]
        colnames(finalPop) <- c(1:ncol(finalPop))
        initialPop <- initialPop[which(initialPop$rnkIndex==1), 1:(ncol(initialPop)-(length(objectives)+1))]
        colnames(initialPop) <- c(1:ncol(initialPop))
      }
      

      if(isTRUE(pix) || isTRUE(nf) || isTRUE(dens)){
        initRasters <- list()
        for (i in 1:nrow(initialPop)){
          initRasters[[i]] <- output.raster(initialPop, rowNo=i, radius, optimLandcovers)
        }
        
        finalRasters <- list()
        for (i in 1:nrow(finalPop)){
          finalRasters[[i]] <- output.raster(finalPop, rowNo=i, radius, optimLandcovers)
        }
      }
      
      if(isTRUE(pix)){
        
        # initial landscape pixels
        for(i in 1:length(initRasters)){
          raster <- initRasters[[i]][["optimRaster"]]
          pixelTally <- data.frame(table(factor(values(raster), levels=optimLandcovers$Code_ExpOp_GFS)))
          colnames(pixelTally) <- c("Code_ExpOp_GFS", "pixels")
          pixelTally$optim <- optimsDF$optim[j]
          pixelTally$radius <- optimsDF$radius[j]
          pixelTally$rep <- optimsDF$rep[j]
          pixelTally$pop <- "initial"
          pixelTally$landscape <- i
          
          clumps <- as.data.frame(lsm_c_clumpy(raster))
          pixelTally$clumpiness <- NA
          for(i in 1:nrow(clumps)){
            pixelTally$clumpiness[which(pixelTally$Code_ExpOp_GFS==clumps$class[i])] <- clumps$value[i]
          }
            
          density <- as.data.frame(lsm_c_pd(raster))
          pixelTally$patchDensity <- NA
          for(i in 1:nrow(density)){
            pixelTally$patchDensity[which(pixelTally$Code_ExpOp_GFS==density$class[i])] <- density$value[i]
          }
          
          no <- as.data.frame(lsm_c_np(raster))
          pixelTally$patchNo <- NA
          for(i in 1:nrow(no)){
            pixelTally$patchNo[which(pixelTally$Code_ExpOp_GFS==no$class[i])] <- no$value[i]
          }
          
          pArea <- as.data.frame(lsm_c_area_mn(raster))
          pixelTally$patchArea <- NA
          for(i in 1:nrow(pArea)){
            pixelTally$patchArea[which(pixelTally$Code_ExpOp_GFS==pArea$class[i])] <- pArea$value[i]
          }
          
          cArea <- as.data.frame(lsm_c_ca(raster))
          pixelTally$classArea <- NA
          for(i in 1:nrow(cArea)){
            pixelTally$classArea[which(pixelTally$Code_ExpOp_GFS==cArea$class[i])] <- cArea$value[i]
          }
          
          pixels <- rbind(pixels, pixelTally)
        }
          
        # final landscape pixels
        for(i in 1:length(finalRasters)){
          raster <- finalRasters[[i]][["optimRaster"]]
          pixelTally <- data.frame(table(factor(values(raster), levels=optimLandcovers$Code_ExpOp_GFS)))
          colnames(pixelTally) <- c("Code_ExpOp_GFS", "pixels")
          pixelTally$optim <- optimsDF$optim[j]
          pixelTally$radius <- optimsDF$radius[j]
          pixelTally$rep <- optimsDF$rep[j]
          pixelTally$pop <- "final"
          pixelTally$landscape <- i
          
          clumps <- as.data.frame(lsm_c_clumpy(raster))
          pixelTally$clumpiness <- NA
          for(i in 1:nrow(clumps)){
            pixelTally$clumpiness[which(pixelTally$Code_ExpOp_GFS==clumps$class[i])] <- clumps$value[i] 
          }
          
          density <- as.data.frame(lsm_c_pd(raster))
          pixelTally$patchDensity <- NA
          for(i in 1:nrow(density)){
            pixelTally$patchDensity[which(pixelTally$Code_ExpOp_GFS==density$class[i])] <- density$value[i]
          }
          
          no <- as.data.frame(lsm_c_np(raster))
          pixelTally$patchNo <- NA
          for(i in 1:nrow(no)){
            pixelTally$patchNo[which(pixelTally$Code_ExpOp_GFS==no$class[i])] <- no$value[i]
          }
          
          pArea <- as.data.frame(lsm_c_area_mn(raster))
          pixelTally$patchArea <- NA
          for(i in 1:nrow(pArea)){
            pixelTally$patchArea[which(pixelTally$Code_ExpOp_GFS==pArea$class[i])] <- pArea$value[i]
          }
          
          cArea <- as.data.frame(lsm_c_ca(raster))
          pixelTally$classArea <- NA
          for(i in 1:nrow(cArea)){
            pixelTally$classArea[which(pixelTally$Code_ExpOp_GFS==cArea$class[i])] <- cArea$value[i]
          }
          
          pixels <- rbind(pixels, pixelTally)
        }
      }
      
      
      if(isTRUE(nf)){
        
        # initial landscape nesting/floral scores
        for(i in 1:length(initRasters)){
          landcoverRaster <- initRasters[[i]][["landcoverRaster"]]
          marginsRaster <- initRasters[[i]][["marginsRaster"]]
          woodlandRaster <- initRasters[[i]][["woodlandRaster"]]
          
          # # Create 2km buffer template to use for cropping rasters:
          # ref <-raster(xmn=as.numeric(gridSquare[2]), xmx=as.numeric(gridSquare[3]), 
          #              ymn=as.numeric(gridSquare[4]),ymx= as.numeric(gridSquare[5]), 
          #              res=c(25,25), crs='+init=EPSG:27700')
          # midpoint <- c((as.numeric(gridSquare[2])+as.numeric(gridSquare[3]))/2, 
          #               (as.numeric(gridSquare[4])+as.numeric(gridSquare[5]))/2)
          # midpointSFC <- st_sfc(st_point(midpoint), crs='+init=EPSG:27700')
          # midpointBuffer <- st_buffer(midpointSFC, 2000)
          # landscapeSubset <- st_intersects(midpointBuffer, landcoverData)
          # buffer <- landcoverData[landscapeSubset[[1]],]
          # buffer$gid <- as.double(buffer$gid)
          # bufferRaster <- rasterize(as(buffer, 'Spatial'), ref, field='gid')
          # saveRDS(bufferRaster, "cluster/data/buffer2km")
          
          buffer <- readRDS("data/buffer2km")
          values(landcoverRaster) <- replace(values(landcoverRaster), is.na(values(buffer)==TRUE), 0)
          values(marginsRaster) <- replace(values(marginsRaster), is.na(values(buffer)==TRUE), NA)
          values(woodlandRaster) <- replace(values(woodlandRaster), is.na(values(buffer)==TRUE), NA)
          
          nfScores <- compute.nesting.floral.scores(landscape=landcoverRaster, wedRaster=woodlandRaster, arRaster=marginsRaster, objectives=objectives)
          nfScores$optim <- optimsDF$optim[j]
          nfScores$radius <- radius
          nfScores$rep <- optimsDF$number[j]
          nfScores$pop <- "initial"
          nfScores$landscape <- i
          
          floralNestingScores <- rbind(floralNestingScores, nfScores)
        }
      
        # final landscape floral/ nesting scores
        for(i in 1:length(finalRasters)){
          landcoverRaster <- finalRasters[[i]][["landcoverRaster"]]
          marginsRaster <- finalRasters[[i]][["marginsRaster"]]
          woodlandRaster <- finalRasters[[i]][["woodlandRaster"]]
          
          buffer <- readRDS("data/buffer2km")
          values(landcoverRaster) <- replace(values(landcoverRaster), is.na(values(buffer)==TRUE), 0)
          values(marginsRaster) <- replace(values(marginsRaster), is.na(values(buffer)==TRUE), NA)
          values(woodlandRaster) <- replace(values(woodlandRaster), is.na(values(buffer)==TRUE), NA)
          
          nfScores <- compute.nesting.floral.scores(landscape=landcoverRaster, wedRaster=woodlandRaster, arRaster=marginsRaster, objectives=objectives)
          nfScores$optim <- optimsDF$optim[j]
          nfScores$radius <- radius
          nfScores$rep <- optimsDF$number[j]
          nfScores$pop <- "final"
          nfScores$landscape <- i
          
          floralNestingScores <- rbind(floralNestingScores, nfScores)
        }
      }
      
      if(isTRUE(dens)){
        for(i in 1:length(initRasters)){
          raster <- initRasters[[i]][["optimRaster"]]
          data <- c("optim"=optimsDF$optim[j], "radius"=optimsDF$radius[j], "rep"=optimsDF$rep[j], "pop" = "initial", "landscape"=i, 
                    "patchDensity" = (lsm_l_pd(raster))$value, "patchNo"=(lsm_l_np(raster))$value, "patchArea"=(lsm_l_area_mn(raster))$value, 
                    "totalArea"=(lsm_l_ta(raster))$value)
          patchDensity <- rbind(patchDensity, data)
        }
    
        for(i in 1:length(finalRasters)){
          raster <- finalRasters[[i]][["optimRaster"]]
          data <- c("optim"=optimsDF$optim[j], "radius"=optimsDF$radius[j], "rep"=optimsDF$rep[j], "pop" = "final", "landscape"=i, 
                    "patchDensity" = (lsm_l_pd(raster))$value, "patchNo"=(lsm_l_np(raster))$value, "patchArea"=(lsm_l_area_mn(raster))$value, 
                    "totalArea"=(lsm_l_ta(raster))$value)
          patchDensity <- rbind(patchDensity, data)
        }
        colnames(patchDensity) <- c("optim", "radius", "rep", "pop", "landscape", "patchDensity", "patchNo", "patchArea", "totalArea")
        }
      
      if(isTRUE(pop)){
        
        # replace param values with expOp codes
        for(i in 1:nrow(finalPop)){
          for(k in 1:ncol(finalPop)){
            finalPop[i,k] <- optimLandcovers$Code_ExpOp_GFS[finalPop[i,k]]
          }
        }
  
        for(i in 1:nrow(initialPop)){
          for(k in 1:ncol(initialPop)){
            initialPop[i,k] <- optimLandcovers$Code_ExpOp_GFS[initialPop[i,k]]
          }
        }
  
        #wrangle landscape dataframes
        initialPop$pop <- "Initial"
        initialPop$optim <- optimsDF$optim[j]
        initialPop$radius <- optimsDF$radius[j]
        initialPop$rep <- optimsDF$rep[j]
        initialPop$landscape <- 1:nrow(initialPop)
  
        initialPopWrang <- melt(initialPop, id=c("landscape", "optim", "radius", "rep", "pop"), variable.name="field", value.name = "Code_ExpOp_GFS")
  
        finalPop$pop <- "Final"
        finalPop$optim <- optimsDF$optim[j]
        finalPop$radius <- optimsDF$radius[j]
        finalPop$rep <- optimsDF$rep[j]
        finalPop$landscape <- 1:nrow(finalPop)
  
        finalPopWrang <- melt(finalPop, id=c("landscape", "optim", "radius", "rep", "pop"), variable.name="field", value.name = "Code_ExpOp_GFS")
  
        #append to main landscape dataframe
        populations <- rbind(populations, initialPopWrang, finalPopWrang)
        }

      if(isTRUE(fit)){
        
        #create scores df
        scores<- scores[!(duplicated(colnames(scores)))]
        scores$optim <- optimsDF$optim[j]
        scores$radius <- optimsDF$radius[j]
        scores$rep <- optimsDF$rep[j]
  
        scoresWrang <- melt(scores, id=c("gen", "rank", "optim", "radius", "rep"), variable.name = "objective", value.name = "score")
  
        fitnessScores <- rbind(fitnessScores, scoresWrang)
      }

    }}
  
  dataframes <- list()
  if (isTRUE(pix)){
    dataframes[[length(dataframes)+1]] <- pixels
    names(dataframes)[[length(dataframes)]] <- "pixels"
  }
  if (isTRUE(pop)){
    dataframes[[length(dataframes)+1]] <- populations
    names(dataframes)[[length(dataframes)]] <- "populations"
  }
  if (isTRUE(nf)){
    dataframes[[length(dataframes)+1]] <- floralNestingScores
    names(dataframes)[[length(dataframes)]] <- "floralNestingScores"
  }
  if (isTRUE(fit)){
    dataframes[[length(dataframes)+1]] <- fitnessScores
    names(dataframes)[[length(dataframes)]] <- "fitnessScores"
  }
  if (isTRUE(dens)){
    dataframes[[length(dataframes)+1]] <- patchDensity
    names(dataframes)[[length(dataframes)]] <- "patchDensity"
  }
  
  return(dataframes)

}


