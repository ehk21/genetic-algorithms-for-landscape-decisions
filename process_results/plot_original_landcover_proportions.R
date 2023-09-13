rm(list=ls())

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

# subset to 2000m radius and only fields containing cereal or improved grasslands
midpoint <- c((as.numeric(gridSquare[2])+as.numeric(gridSquare[3]))/2, 
              (as.numeric(gridSquare[4])+as.numeric(gridSquare[5]))/2)
midpointSFC <- st_sfc(st_point(midpoint), crs='+init=EPSG:27700')
midpointBuffer <- st_buffer(midpointSFC, 2000)

landscapeSubset <- st_intersects(midpointBuffer, landcoverData)
optim <-subset(landcoverData[landscapeSubset[[1]],], subset = Code_ExpOp_GFS %in% c(14,30))

cropSubset <- st_intersects(midpointBuffer, relevantCrops)
optimCrops <- relevantCrops[cropSubset[[1]],]


# rasterize
ref <-raster(xmn=as.numeric(gridSquare[2]), xmx=as.numeric(gridSquare[3]), 
             ymn=as.numeric(gridSquare[4]),ymx= as.numeric(gridSquare[5]), 
             res=c(25,25), crs='+init=EPSG:27700')

optim$Code_ExpOp_GFS<-as.double(optim$Code_ExpOp_GFS)
optimRaster <- rasterize(as(optim, 'Spatial'), ref, field='Code_ExpOp_GFS')
# plot(optimRaster)

optimCrops$Code_ExpOp_GFS<-as.double(optimCrops$Code_ExpOp_GFS)
cropRaster <- rasterize(as(optimCrops, 'Spatial'), ref, field='Code_ExpOp_GFS')
# plot(cropRaster)

# superimpose crop raster over landcover raster
values(optimRaster)[which(!is.na(values(cropRaster)))] <- values(cropRaster)[which(!is.na(values(cropRaster)))]

# Create by-pixel dataframe of optim raster
optimPixels <- data.frame(Code_ExpOp_GFS=values(optimRaster))
optimPixels <- subset(optimPixels, !is.na(Code_ExpOp_GFS))

# calculate proportions
optimPixels$Code_ExpOp_GFS <- as.factor(optimPixels$Code_ExpOp_GFS)

proportions <- optimPixels %>%
  count(Code_ExpOp_GFS)

proportions <- proportions %>%
  dplyr::mutate(pixelT= sum(n)) %>%
  dplyr::mutate(pixelsPerc=(n/pixelT)*100)


# add land cover name column
translations <- read.csv("data/Translations.csv")
translations <- select(translations, c("Code_ExpOp_GFS", "Landclass_ExpOp_GFS"))

translations$Code_ExpOp_GFS <- as.factor(translations$Code_ExpOp_GFS)
translations <- translations[which(translations$Code_ExpOp_GFS %in% unique(proportions$Code_ExpOp_GFS)),]
translations <- translations[!duplicated(translations), ]

proportions<- dplyr::left_join(proportions, translations,
                           by = "Code_ExpOp_GFS", copy=T)

proportions$Landclass_ExpOp_GFS <- as.factor(proportions$Landclass_ExpOp_GFS)


# define colour palette
colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")
#(Beans, Cereal, Coniferous woodland, Deciduous woodland, Fallow, Improved perm grasslands, Oilseed rape, Unimproved meadow)

# plot

ggplot(proportions, aes(y=pixelsPerc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
  labs(y="Mean % of Optimised Area", x="Landcover Type") +
  theme_bw() +
  theme( axis.text.y = element_text(size=10), 
         axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
         axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
         legend.title=element_text(size=11, face="bold"), 
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
         legend.position="bottom") +
  # scale_fill_npg() +
  scale_fill_manual(values = colors) 

