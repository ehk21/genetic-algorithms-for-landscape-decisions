# clear workspace and graphics
rm(list = ls())

# # load packages
library(sf) # only one to call in this function

# read in data
optimLandcovers <- read.csv("data/optim_landcovers.csv")
landcoverData <- read_sf("data/SK86/lcm-2020-vec_4558456.gpkg",as_tibble=F)

# set parameters
gridSquare <- c("SK86", 480000, 490000 ,360000, 370000)
shuffleCodes <- c(30,14)
radius = 2000

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

optim$area <- st_area(optim)

meanFieldSize<-mean(optim$area)
# 44906m^2
# 4.491 ha
