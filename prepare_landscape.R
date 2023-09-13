################################################################################
################################################################################
#' ehk21@ic.ac.uk - May 2022
#' 
#' Function to separate landcover data into fields which will be shuffled during optimisation and fields which won't be shuffled during optimisation 
#' 
#' @param landcoverData .gpkg of UKCEH landcover data for specified grid square; must contain X_mode (spatial landcover) and gid (field ID) columns.
#' @param shuffleCodes vector containing expOp codes of fields in original landscape which will be shuffled during optimisation.
#' @param radius radius of input landscape which will be optimised (buffer zone will be centred around midpoint of gridsquare).
#' @param gridSquare vector containing details of specified grid square (used for creating reference raster & calling relevent rasters in fitness function).
#'                  format: c(gridsquare name, xmin, xmax, ymin, ymax)
#'
#' @return list of length 4: landcover raster of fields to be optimised, landcover raster of fields not to be optimised, dataframe of optim field ID numbers and their corresponding land cover, and by-pixel dataframe of landcovers of fields to be optimised
#'
################################################################################
################################################################################

# Requires packages: raster, sf

prepare.landscape <- function(landcoverData, shuffleCodes, radius, gridSquare){
  
  # load packages
  # library(raster)
  # library(sf)
  # library(dplyr)
  
  # replace spatial landcover codes with expOp codes 
  translations <- read.csv("data/Translations.csv")
  translations <- subset(translations, select = c(Code_Spatial, Code_ExpOp_GFS))
  landcoverData$X_mode <- as.integer(landcoverData$X_mode)
  landcoverData <- dplyr::left_join(landcoverData, translations, 
                                    by = c("X_mode" = "Code_Spatial"), copy=T)
  
  # Create list of landcover_data rows which correspond to fields within the 
  # specified area to be subsetted (this corresponds to a buffer zone around the 
  # midpoint of the landscape of the specified radius in m)
  midpoint <- c((as.numeric(gridSquare[2])+as.numeric(gridSquare[3]))/2, 
                (as.numeric(gridSquare[4])+as.numeric(gridSquare[5]))/2)
  midpointSFC <- st_sfc(st_point(midpoint), crs='+init=EPSG:27700')
  midpointBuffer <- st_buffer(midpointSFC, radius)
  landscapeSubset <- st_intersects(midpointBuffer, landcoverData)
  
  # Subset to fid and X_mode columns with just fields with specified exPop_GFS 
  # landcover codes and within the specified area
  optim <-subset(landcoverData[landscapeSubset[[1]],], 
                 subset = Code_ExpOp_GFS %in% shuffleCodes, 
                 select=c(gid, Code_ExpOp_GFS))
  
  # Subset to fid and X_mode columns with just fields without the specified 
  # exPop_GFS landcover codes and not within the specified area
  other <- subset(landcoverData, subset = !(gid %in% optim$gid), 
                  select=c(gid, Code_ExpOp_GFS))
  
  # Create reference raster
  ref <-raster(xmn=as.numeric(gridSquare[2]), xmx=as.numeric(gridSquare[3]), 
               ymn=as.numeric(gridSquare[4]),ymx= as.numeric(gridSquare[5]), 
               res=c(25,25), crs='+init=EPSG:27700')
  
  # Rasterise each subset
  optim$Code_ExpOp_GFS<-as.double(optim$Code_ExpOp_GFS)
  optimRaster <- rasterize(as(optim, 'Spatial'), ref, field='Code_ExpOp_GFS')
  
  optim$gid<-as.double(optim$gid)
  optimGIDRaster <- rasterize(as(optim, 'Spatial'), ref, field='gid')
  
  other$Code_ExpOp_GFS<-as.double(other$Code_ExpOp_GFS)
  otherRaster <- rasterize(as(other, 'Spatial'), ref, field='Code_ExpOp_GFS')
  
  # Create by-pixel dataframe of optim raster
  optimPixels <- data.frame(gid=values(optimGIDRaster), 
                            Code_ExpOp_GFS=values(optimRaster))
  
  # Create list of field boundaries rasters for each individual optim field (for creating edgecover rasters later on)
  boundaries <- c()
  for (i in 1:nrow(optim)){
    field <- optim[i,3]
    field <- as(as(field, "Spatial"), "SpatialLinesDataFrame")
    fieldRaster <- rasterize(field, ref)
    boundaries <- c(boundaries, fieldRaster)
  }
  
  names(boundaries) <- optim$gid
  
  # Create arable margins raster for fields not to be optimised 
  if(length(which((other$Code_ExpOp_GFS %in% c("30","33", "28", "14"))==TRUE))>0){
    otherMargins <- subset(other, Code_ExpOp_GFS %in% c("30","33", "28", "14"))
    otherMargins <- as(as(otherMargins, "Spatial"), "SpatialLinesDataFrame")
    otherMarginsRaster <- rasterize(otherMargins, ref)
    values(otherMarginsRaster) <- replace(values(otherMarginsRaster), values(otherMarginsRaster)>0, 1)
    values(otherMarginsRaster) = replace(values(otherMarginsRaster),is.na(values(otherMarginsRaster))==TRUE,0)
  } else {
    otherMarginsRaster <- optimRaster
    values(otherMarginsRaster) <- 0
  }
  
  # Create woodland boundaries raster for fields not to be optimised 
  if(length(which((other$Code_ExpOp_GFS %in% c("3", "4"))==TRUE))>0){
    otherWoodland <- subset(other, Code_ExpOp_GFS %in% c("3", "4"))
    otherWoodland <- as(as(otherWoodland, "Spatial"), "SpatialLinesDataFrame")
    otherWoodlandRaster <- rasterize(otherWoodland, ref)
    values(otherWoodlandRaster) <- replace(values(otherWoodlandRaster), values(otherWoodlandRaster)>0, 1)
    values(otherWoodlandRaster) = replace(values(otherWoodlandRaster),is.na(values(otherWoodlandRaster))==TRUE,0)
  } else {
    otherWoodlandRaster <- optimRaster
    values(otherWoodlandRaster) <- 0
  }
  
  # Return new vector datasets as a list object
  return(list("optim" = optimRaster, "other" = otherRaster, "fields"=optim, 
              "pixels"=optimPixels, "boundaries"=boundaries, "otherMarginsRaster"=otherMarginsRaster, "otherWoodlandRaster"=otherWoodlandRaster))
}
