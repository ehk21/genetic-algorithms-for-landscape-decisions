# function to output raster of a landscape (as defined in an NSGA2R output population)

output.raster <- function(parameters, rowNo, radius, optimLandcovers){
  
  library(raster)
  library(dplyr)
  library(rasterVis)
  source("code/prepare_landscape.R")
  
  # subset params dataframe to 1 landscape
  x <- parameters[rowNo,]

  # # split landscape according to radius of optim; output template rasters
  # split <- prepare.landscape(landcoverData, shuffleCodes, radius, gridSquare)
  # optimRaster <- split$optim
  # otherRaster <- split$other
  # optimFields <- split$fields
  # optimPixels <- split$pixels
  # boundaries <- split$boundaries
  # otherMargins <- split$otherMarginsRaster
  # otherWoodland <- split$otherWoodlandRaster
  # rm(split)
  # 
  # landscape2000m <- list("optimRaster"=optimRaster, "otherRaster"=otherRaster, "optimFields"=optimFields, "optimPixels"=optimPixels, "boundaries"=boundaries, "otherMargins"=otherMargins, "otherWoodland"=otherWoodland)
  # saveRDS(landscape2000m, "cluster/data/process/2000m")
  
  if(radius==500){
    landscape <- readRDS("data/process/500m")
    optimRaster <- landscape$optimRaster
    otherRaster <- landscape$otherRaster
    optimFields <- landscape$optimFields
    optimPixels <- landscape$optimPixels
    boundaries <- landscape$boundaries
    otherMargins <- landscape$otherMargins
    otherWoodland <- landscape$otherWoodland
  } else if(radius==1000){
    landscape <- readRDS("data/process/1000m")
    optimRaster <- landscape$optimRaster
    otherRaster <- landscape$otherRaster
    optimFields <- landscape$optimFields
    optimPixels <- landscape$optimPixels
    boundaries <- landscape$boundaries
    otherMargins <- landscape$otherMargins
    otherWoodland <- landscape$otherWoodland
  } else if(radius==2000){
    landscape <- readRDS("data/process/2000m")
    optimRaster <- landscape$optimRaster
    otherRaster <- landscape$otherRaster
    optimFields <- landscape$optimFields
    optimPixels <- landscape$optimPixels
    boundaries <- landscape$boundaries
    otherMargins <- landscape$otherMargins
    otherWoodland <- landscape$otherWoodland
  }
  
  # replace values in optim raster with those of input landscape
  shuffle <- rep(NA, length(x)) 
  for (i in 1:length(x)){
    row <- x[,i]
    shuffle[i] <-  optimLandcovers$Code_ExpOp_GFS[row]
  }
  
  optimFields$Code_ExpOp_GFS<- shuffle # Replace land cover codes of optim fields with shuffled allocations
  optimPixels <- dplyr::left_join(optimPixels, optimFields, by = "gid") 
  optimPixels <- subset(optimPixels, select=c("gid", "Code_ExpOp_GFS.y"))
  colnames(optimPixels)[2] <- "Code_ExpOp_GFS"
  values(optimRaster)<-as.numeric(optimPixels$Code_ExpOp_GFS)

  # recombine optim and other raster for whole landscape
  landcoverRaster <- optimRaster
  landcoverRaster[is.na(optimRaster)] <- otherRaster[is.na(landcoverRaster)]

  # Create grassy field margins raster
  if(length(which((optimFields$Code_ExpOp_GFS %in% c("30","33", "28", "14"))==TRUE))>0){
    arableBoundaries <- boundaries[which(optimFields$Code_ExpOp_GFS %in% c("30","33", "28", "14"))]
    marginsRaster <- landcoverRaster
    values(marginsRaster) <- NA
    for(i in 1:length(arableBoundaries)){
      values(marginsRaster) <- replace(values(marginsRaster), is.na(values(arableBoundaries[[i]]))==FALSE, 1)
    }
    values(marginsRaster)[which(values(otherMargins)==1)] <- values(otherMargins)[which(values(otherMargins)==1)]
  } else {
    marginsRaster<-otherMargins
  }

  # Create woodland boundaries raster
  if(length(which((optimFields$Code_ExpOp_GFS %in% c("3","4"))==TRUE))>0){
    woodlandBoundaries <- boundaries[which(optimFields$Code_ExpOp_GFS %in% c("3", "4"))]
    woodlandRaster <- landcoverRaster
    values(woodlandRaster) <- NA
    for(i in 1:length(woodlandBoundaries)){
      values(woodlandRaster) <- replace(values(woodlandRaster), is.na(values(woodlandBoundaries[[i]]))==FALSE, 1)
    }
    values(woodlandRaster)[which(values(otherWoodland)==1)] <- values(otherWoodland)[which(values(otherWoodland)==1)]
  } else {
    woodlandRaster<-otherWoodland
  }

  return(list("optimRaster"=optimRaster, "landcoverRaster"=landcoverRaster, "marginsRaster"=marginsRaster, "woodlandRaster"=woodlandRaster))
  
  # #make values categorical
  # codes <- read.csv("Poll4pop/InputParameters/Landclass_ExpOp_GFS.csv", header=T)
  # codes[nrow(codes)+1,] <- c(0,"Freshwater")
  # codes$Code_ExpOp_GFS <- as.double(codes$Code_ExpOp_GFS)
  # 
  # optimRaster <- as.factor(optimRaster)
  # rat <- levels(optimRaster)[[1]]
  # rat<- left_join(rat, codes, by=c("ID"="Code_ExpOp_GFS"))
  # levels(optimRaster) <- rat
  # optimRaster<- levelplot(optimRaster, col.regions=rev(terrain.colors(length(unique(values(optimRaster))))))
  
  # landcoverRaster <- as.factor(landcoverRaster)
  # rat <- levels(landcoverRaster)[[1]]
  # rat<- left_join(rat, codes, by=c("ID"="Code_ExpOp_GFS"))
  # levels(landcoverRaster) <- rat
  # landcoverRaster<-levelplot(landcoverRaster, col.regions=rev(terrain.colors(length(unique(values(landcoverRaster))))))
  
  # return(optimRaster)
}
