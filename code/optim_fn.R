################################################################################
################################################################################
#' 
#' Function to run multi-objective optimisation of a specified landscape,
#'
#' @param optimLandcovers list of landcovers that will be shuffled into the landscape during optimisation
#' @param landcoverData .gpkg of UKCEH landcover data for specified grid square; must contain X_mode (spatial landcover) and gid (field ID) columns.
#' @param gridSquare vector containing details of specified grid square (used for creating reference raster & calling relevent rasters in fitness function).
#'                  format: c(gridsquare name, xmin, xmax, ymin, ymax)
#' @param shuffleCodes vector containing expOp codes of fields in original landscape which will be shuffled during optimisation.
#' @param radius radius of input landscape which will be optimised (buffer zone will be centred around midpoint of gridsquare).
#' @param objectives vector of objectives to be optimised. 
#' @param objDim number of objectives to be optimised 
#' @param popSize size of population to run in optimisation (recommended:100)
#' @param generations number of generations to run optimisation for (recommended:50)
#' @param outputName output file name
#' 
#' @return list containing results of optimisation, as well as relevant input parameters for ease of analysis.
#' - List is also saved in results/optimisations directory in the format optim_gridsquare_radius_objectivetype
#' 
#' Related functions: prepare.landscape, nsga2R.edited
#' 
################################################################################
################################################################################

optim.fn <- function(optimLandcovers, landcoverData, gridSquare, shuffleCodes, 
                     radius, objectives, objDim, popSize, generations, outputName) {
  
  # load packages
  library(sf) # only one to call in this function
  library(raster)
  library(sp) # called in separate_landscape
  library(nsga2R) # for running optimisation
  library(plyr)
  library(dplyr)
  library(rgdal)
  library(BiocManager)
  library(EBImage)
  
  # source functions
  source("code/prepare_landscape.R")
  source("code/nsga2r_edited/nsga2R_edited.R")
  
  # replace spatial landcover codes with ExpOp codes, separate landscape into 
  # optim and non-optim fields (using ExpOp codes) & reduce to specified radius
  split <- prepare.landscape(landcoverData=landcoverData, 
                             shuffleCodes=shuffleCodes, radius=radius, 
                             gridSquare=gridSquare)
  optimRaster <- split$optim
  otherRaster <- split$other
  optimFields <- split$fields
  optimPixels <- split$pixels
  boundaries <- split$boundaries
  otherMargins <- split$otherMarginsRaster
  otherWoodland <- split$otherWoodlandRaster
  rm(split)
  
  # run optimisation
  optim <- nsga2R.edited(fn=fitness, varNo=length(optimFields$gid), objDim=objDim,
                         lowerBounds=rep(1,length(optimFields$gid)), 
                         upperBounds=rep(nrow(optimLandcovers),length(optimFields$gid)),
                         popSize=popSize,  tourSize=5, generations=generations, cprob=0.7, 
                         XoverDistIdx=5, mprob=0.2, MuDistIdx=10, objectives, 
                         optimLandcovers=optimLandcovers, optimFields=optimFields,
                         optimRaster=optimRaster, otherRaster=otherRaster, boundaries=boundaries, otherMargins=otherMargins, otherWoodland=otherWoodland, optimPixels=optimPixels, gridSquare=gridSquare)
  
  saveRDS(list("optim"=optim, "optimLandcovers"=optimLandcovers,"shuffleCodes"=shuffleCodes, "radius"=radius, "objectives"=objectives), file=paste0("results/optimisations/",outputName))
 
  warnings()
   
}


