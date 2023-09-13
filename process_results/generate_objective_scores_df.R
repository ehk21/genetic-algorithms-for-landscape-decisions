rm(list=ls())

# load packages
library(reshape2)
library(sf)
library(raster)
library(rgdal)
library(sp)
library(landscapemetrics)
library(rasterVis)
library(tiff)

# source functions
source("code/process_results/output_rasters.R")
source("code/process_results/compute_objective_scores_for_final_landscapes.R")

# read in data
optimsDF <- read.csv("data/optims.csv")
rows <- which(optimsDF$optim %in% c("GNB", "TNB", "GNS", "Farmer", "AllBees", "FarmerOnly") & optimsDF$radius==2000 & optimsDF$complete=="t")

# generate dataframe
objectiveScores <- data.frame(matrix(ncol=7, nrow=0, dimnames=list(NULL, c("optim", "rep", "landscape","gnb", "tnb", "gns", "farmer"))))

# assess scores of results & add to dataframe
for(j in rows){
    
    print(j)
    print(paste0(optimsDF$optim[j], optimsDF$number[j]))
    
    # load results
    result <- readRDS(paste0("results/optimisations/", optimsDF$optim[j], optimsDF$number[j]))
    
    scores <- result[["optim"]][["scores"]]
    finalPop <- data.frame(result[["optim"]][["parameters"]])
    optimLandcovers <- result[["optimLandcovers"]]
    radius <- result[["radius"]]
    pareto <- result[["optim"]][["paretoFrontRank"]]
    objectives <- result[["objectives"]]
    shuffleCodes <- result$shuffleCodes
    gridSquare <- c("SK86", 480000, 490000 ,360000, 370000)
    landcoverData <- read_sf("data/SK86/lcm-2020-vec_4558456.gpkg")
    
    # subset populations to just rank1 (multi-objective) or rank 1-3(single-objective)
    if(length(unique(objectives))==1){
      finalPop <- finalPop[which(c(1,2,3) %in% pareto),]
      colnames(finalPop) <- c(1:ncol(finalPop))
    } else{
      finalPop <- finalPop[which(pareto==1),]
      colnames(finalPop) <- c(1:ncol(finalPop))
    }
    
    finalRasters <- list()
    for (i in 1:nrow(finalPop)){
      finalRasters[[i]] <- output.raster(finalPop, rowNo=i, radius, optimLandcovers)
    }
      
      for(i in 1:length(finalRasters)){
        landcoverRaster <- finalRasters[[i]][["landcoverRaster"]]
        marginsRaster <- finalRasters[[i]][["marginsRaster"]]
        woodlandRaster <- finalRasters[[i]][["woodlandRaster"]]
        
        scores <- compute.objective.scores(landscape=landcoverRaster, wedRaster=woodlandRaster, arRaster=marginsRaster)
        scores$optim <- optimsDF$optim[j]
        scores$rep <- optimsDF$number[j]
        scores$landscape <- i
        
        objectiveScores <- rbind(objectiveScores, scores)
      }
}

# assess scores of original landscape & append to dataframe

## read in data
landcoverData <- read_sf("data/SK86/lcm-2020-vec_4558456.gpkg",as_tibble=F)
cropData <- read_sf("data/SK86/crops_2020/lccm-2020_5090603.gpkg", as_tibble=F)
gridSquare <- c("SK86", 480000, 490000 ,360000, 370000)

# Add new cropData column with numerical crop code (based on codes found in Translations.csv):
crop_no <- data.frame(crop_name=unique(cropData$crop_name), crop_code=unique(cropData$crop_code), crop_no = c(56, 54, 61, 59, 60 ,57, 55, 58, 53, 51, 52, 50, 53))
cropData <- left_join(cropData, crop_no)

# Subset cropData to just fields containing OSR or field beans
relevantCrops <- cropData[which(cropData$crop_name %in% c("Oilseed rape", "Field beans" )),]

# replace spatial landcover codes with expOp codes 
translations <- read.csv("data/Translations.csv")
translations <- subset(translations, select = c(Code_Spatial, Code_ExpOp_GFS))

landcoverData$X_mode <- as.integer(landcoverData$X_mode)
landcoverData <- dplyr::left_join(landcoverData, translations, 
                                  by = c("X_mode" = "Code_Spatial"), copy=T)

relevantCrops$poly_id <- as.integer(relevantCrops$poly_id)
relevantCrops <- dplyr::left_join(relevantCrops, translations, 
                                  by = c("crop_no" = "Code_Spatial"), copy=T)

# rasterize
ref <-raster(xmn=as.numeric(gridSquare[2]), xmx=as.numeric(gridSquare[3]), 
             ymn=as.numeric(gridSquare[4]),ymx= as.numeric(gridSquare[5]), 
             res=c(25,25), crs='+init=EPSG:27700')

landcoverData$Code_ExpOp_GFS<-as.double(landcoverData$Code_ExpOp_GFS)
landcoverRaster <- rasterize(as(landcoverData, 'Spatial'), ref, field='Code_ExpOp_GFS')
plot(landcoverRaster)

relevantCrops$Code_ExpOp_GFS<-as.double(relevantCrops$Code_ExpOp_GFS)
cropRaster <- rasterize(as(relevantCrops, 'Spatial'), ref, field='Code_ExpOp_GFS')
plot(cropRaster)

# superimpose crop raster over landcover raster
values(landcoverRaster)[which(!is.na(values(cropRaster)))] <- values(cropRaster)[which(!is.na(values(cropRaster)))]

# generate margins rasters
margins <- subset(landcoverData, Code_ExpOp_GFS %in% c("30","33", "28", "14"))
margins <- as(as(margins, "Spatial"), "SpatialLinesDataFrame")
marginsRaster <- rasterize(margins, ref)
values(marginsRaster) <- replace(values(marginsRaster), values(marginsRaster)>0, 1)
values(marginsRaster) = replace(values(marginsRaster),is.na(values(marginsRaster))==TRUE,0)
plot(marginsRaster)

#  generate woodland boundaries raster
woodland <- subset(landcoverData, Code_ExpOp_GFS %in% c("3", "4"))
woodland <- as(as(woodland, "Spatial"), "SpatialLinesDataFrame")
woodlandRaster <- rasterize(woodland, ref)
values(woodlandRaster) <- replace(values(woodlandRaster), values(woodlandRaster)>0, 1)
values(woodlandRaster) = replace(values(woodlandRaster),is.na(values(woodlandRaster))==TRUE,0)
plot(woodlandRaster)

scores <- compute.objective.scores(landscape=landcoverRaster, wedRaster=woodlandRaster, arRaster=marginsRaster)
scores$optim <- "SK86"
scores$rep <- 1
scores$landscape <- 1

objectiveScores <- rbind(objectiveScores, scores)

# save as csv
write.csv(objectiveScores, "results/objective_scores.csv")
