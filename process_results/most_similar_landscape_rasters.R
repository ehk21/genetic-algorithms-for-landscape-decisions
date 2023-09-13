# find the most similar rasters in final populations of optimisations to the mean landcover composition of all landscapes in selected optimisations.

rm(list=ls())

library(dplyr)
library(raster)
library(sf)
library(rasterVis)
library(reshape2)
library(plotrix)
library(diagis)

# read in and format data
results <- readRDS("results/results")
populations <- results$populations
pixels <- results$pixel
pixels <- subset(pixels, optim=c("GNB", "TNB", "GNS", "AllBees", "Farmer", "FarmerOnly")) 
pixels <- subset(pixels, radius==2000)
populations <- subset(populations, optim=c("GNB", "TNB", "GNS", "AllBees", "Farmer", "FarmerOnly") & radius ==2000)
populations <- subset(populations, radius==2000)

# # replace and order optim names
pixels$optim[pixels$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
pixels$optim[pixels$optim=="FarmerOnly"] <- "Farmer"
pixels$optim[pixels$optim=="AllBees"] <- "GNB + TNB + GNS"
pixels$optim <- factor(pixels$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))
populations$optim[populations$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
populations$optim[populations$optim=="FarmerOnly"] <- "Farmer"
populations$optim[populations$optim=="AllBees"] <- "GNB + TNB + GNS"
populations$optim <- factor(populations$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))

pixelDF <- pixels

pixelDF$radius <- paste0(pixelDF$radius, "m")
pixelDF$radius <- factor(pixelDF$radius, levels=c("500m", "1000m", "2000m"))

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

pixelDF <- subset(pixelDF, pop=="final", select=c(Landclass_ExpOp_GFS, pixels, optim, radius, rep, pop, landscape))

# calculate proportions & wrangle dataframes
pixelDF <- pixelDF %>%
  group_by(optim, radius, pop, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)

pixelDF$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

pixelDF <- subset(pixelDF, select=c(Landclass_ExpOp_GFS, optim, radius, rep, landscape, pixelsPerc))

pixCast <- dcast(pixelDF, optim + radius + rep + landscape ~ Landclass_ExpOp_GFS)

pixelSummary <- pixelDF %>%
  group_by(optim, radius, rep, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(meanPerc=mean(pixelsPerc), pixelsSD=sd(pixelsPerc), pixelsSE=std.error(pixelsPerc), landscapeNo=n())

dataSummary <- pixelSummary %>%
  group_by(optim, radius, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo))

sumCast <- dcast(dataSummary, optim + radius ~ Landclass_ExpOp_GFS)
sumCast$landscape <- "mean"
sumCast$rep <- "mean"

#####

result <- sumCast
for(i in 1:nrow(sumCast)){
  data <- subset(pixCast, optim==sumCast$optim[i] & radius==sumCast$radius[i])
  row <- which.min(dist(rbind(sumCast[1,3:10], data[,5:12]))[1:nrow(data)])
  result <- rbind(result, data[row,])
  }

result <- subset(result, landscape!="mean", select=c(optim, radius, rep, landscape))
result$optim <- as.character(result$optim)
result$radius <- as.character(result$radius)

populations$radius <- paste0(populations$radius, "m")
populations <- subset(populations, pop=="Final")

codes <- read.csv("Poll4pop/InputParameters/Landclass_ExpOp_GFS.csv", header=T)
codes[nrow(codes)+1,] <- c(0,"Freshwater")
codes$Code_ExpOp_GFS <- as.double(codes$Code_ExpOp_GFS)

optimLandcovers <- read.csv("data/optim_landcovers.csv")
# "Coniferous Woodland","Deciduous Woodland", "Fallow", "Improved Permanent Grasslands","Unimproved Meadow", 
# "Cereal", "Broad/ Field Beans", "Oilseed Rape"   
optimLandcovers$col <- c("#000000" , "#004949", "#924900", "#24FF24", "#FFB6DB", "#FFFF6D", "#B66DFF", "#6DB6FF")

translations <- read.csv("data/Translations.csv")

SK86 <-raster(xmn=480000, xmx=490000 , ymn=360000, ymx=370000, res=c(25,25), crs='+init=EPSG:27700')

for(i in 1:nrow(result)){
  data <- subset(populations, optim==result$optim[i]  & radius==result$radius[i] & rep==result$rep[i] & landscape==result$landscape[i])
  cast <- dcast(data, optim + radius + rep + pop + landscape ~ field, mean)
  castSub <- cast[,6:ncol(cast)]
  landcovers <- c()
  for(j in 1:ncol(castSub)){
    landcovers <-c(landcovers, castSub[1,j])
  }
  
  radius <- result$radius[i]
  
  if(radius=="500m"){
    landscape <- readRDS("data/process/500m")
    optimFields <- landscape$optimFields
  } else if(radius=="1000m"){
    landscape <- readRDS("data/process/1000m")
    optimFields <- landscape$optimFields
  } else if(radius=="2000m"){
    landscape <- readRDS("data/process/2000m")
    optimFields <- landscape$optimFields
  }
  
  
  optimFields$Code_ExpOp_GFS<- landcovers # Replace land cover codes of optim fields with shuffled allocations
  
  raster <-rasterize(as(optimFields, 'Spatial'), SK86, field='Code_ExpOp_GFS')
  
  # crop:
  if(radius==500){
    raster <- crop(raster, extent(483500, 486500, 363500, 366500)) 
  } else if(radius==1000){
    raster <- crop(raster, extent(483000, 487000, 363000, 367000))
  } else {
    raster <- crop(raster, extent(482000, 488000, 361925, 367775))
  }
  
  r2 <- ratify(raster)
  rat_r2 <- levels(r2)[[1]]
  
  rat_r2$landcover <- NA
  cols <- c()
  for(k in 1:nrow(rat_r2)){
    rat_r2$landcover[k] <- codes$Landclass_ExpOp_GFS[codes$Code_ExpOp_GFS==rat_r2$ID[k]]
    cols <- c(cols, optimLandcovers$col[optimLandcovers$Code_ExpOp_GFS==rat_r2$ID[k]])
  }
  
  levels(r2) <- rat_r2
  assign(result$optim[i], levelplot(r2, col.regions=cols, main = paste0(result$optim[i]), colorkey=FALSE, scales=list(draw=FALSE)))
}

plot(GNB) 
plot(TNB)
plot(GNS)
plot(Farmer)
plot(`GNB + TNB + GNS`)
plot(`GNB + TNB + GNS + Farmer`)
