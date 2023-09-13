rm(list=ls())

#########################################################################################################
# load packages ---- 
#########################################################################################################

library(gridExtra)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggsci)
library(plotrix)
library(diagis)
library(broom)
library(dplyr)
library(reshape2)

#########################################################################################################
# load data 
#########################################################################################################

results <- readRDS("results/results")
pixels <- results$pixels

# replace and order optim names
pixels$optim[pixels$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
pixels$optim[pixels$optim=="FarmerOnly"] <- "Farmer"
pixels$optim[pixels$optim=="AllBees"] <- "GNB + TNB + GNS"
pixels$optim <- factor(pixels$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))


##############################
# prepare pixel % data
##############################
pixelDF <- pixels

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

data <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB", "TNB", "GNS") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))

data1 <- data %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

pixelSummary <- data1 %>%
  group_by(optim, rep, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(meanPerc=mean(pixelsPerc), landscapeNo=n())

pixelSummary1 <- pixelSummary%>%
  group_by(optim, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo))


pixelSummary1 <- as.data.frame(pixelSummary1)

pixelSummary1$Landclass_ExpOp_GFS <- factor(pixelSummary1$Landclass_ExpOp_GFS)
levels(pixelSummary1$Landclass_ExpOp_GFS)<-c("Broad/Field Beans", "Cereal", "Coniferous Woodland","Deciduous Woodland", "Fallow", "Improved Permanent Grasslands", "Oilseed Rape", "Unimproved Meadow" )

pixelSummary1$optim <- as.factor(pixelSummary1$optim)

names(pixelSummary1)<- c("Guild", "Landclass_ExpOp_GFS", "perc")


########################
# prepare score data
########################

optimLandcovers <- read.csv("data/optim_landcovers.csv")
landcoverScores <- read.csv("data/landcover_scores.csv")

# floral score = floral attractiveness*(floral cover early spring + floral cover late spring + floral cover summer)
landcoverScores$floral <- landcoverScores$Flor_P1_b*(landcoverScores$Flor_Cov_P1_b + landcoverScores$Flor_Cov_P2_b + landcoverScores$Flor_Cov_P3_b)
landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS, select=c(Species_name, Landclass_ExpOp_GFS, floral, Nest_per_ha))  

landcoverScores$Species_name <- factor(landcoverScores$Species_name, levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees"))
levels(landcoverScores$Species_name) <- c("GNB", "TNB", "GNS")

names(landcoverScores)<- c("Guild","Landclass_ExpOp_GFS", "Floral", "Nesting" )

landcoverScores$Landclass_ExpOp_GFS<- factor(landcoverScores$Landclass_ExpOp_GFS)
levels(landcoverScores$Landclass_ExpOp_GFS)<- c("Broad/Field Beans", "Cereal", "Coniferous Woodland", "Deciduous Woodland", "Fallow", "Improved Permanent Grasslands", "Oilseed Rape", "Unimproved Meadow")

########################
# combine data sets
########################

data <- left_join(pixelSummary1, landcoverScores, by=c("Guild", "Landclass_ExpOp_GFS"))

data<-as.data.frame(data)


#######################
#plot
##########################
names(data)

ggplot(data, aes(x=Floral, y=perc, colour=Guild)) +
  geom_point()+
  geom_smooth(method="lm",  fullrange=T, se=F)

ggplot(data, aes(x=Nesting, y=perc, colour=Guild)) +
  geom_point()+
  geom_smooth(method="lm", fullrange=T, se=F)

###################
# correlation tests
###################

hist(data$Floral, breaks=20)
shapiro.test(data$Floral)

hist(data$Nesting, breaks=20)
shapiro.test(data$Nesting)

hist(log(data$perc), breaks=20)
shapiro.test(log(data$perc))

### GNB

hist(data$Floral[data$Guild=="GNB"], breaks=15)
shapiro.test(data$Floral[data$Guild=="GNB"])
# normal

hist(data$Nesting[data$Guild=="GNB"], breaks=15)
shapiro.test(data$Nesting[data$Guild=="GNB"])
# normal

hist(data$perc[data$Guild=="GNB"], breaks=15)
shapiro.test(data$perc[data$Guild=="GNB"])
# not normal

cor.test(data$perc[data$Guild=="GNB"], data$Floral[data$Guild=="GNB"], method="spearman", exact=F)

cor.test(data$perc[data$Guild=="GNB"], data$Nesting[data$Guild=="GNB"], method="spearman", exact=F )


### TNB

hist(data$Floral[data$Guild=="TNB"], breaks=15)
shapiro.test(data$Floral[data$Guild=="TNB"])
# normal

hist(data$Nesting[data$Guild=="TNB"], breaks=15)
shapiro.test(data$Nesting[data$Guild=="TNB"])
# not normal

hist(data$perc[data$Guild=="TNB"], breaks=15)
shapiro.test(data$perc[data$Guild=="TNB"])
# not normal

cor.test(data$perc[data$Guild=="TNB"], data$Floral[data$Guild=="TNB"], method="spearman", exact=F)

cor.test(data$perc[data$Guild=="TNB"], data$Nesting[data$Guild=="TNB"], method="spearman", exact=F )


### GNS

hist(data$Floral[data$Guild=="GNS"], breaks=15)
shapiro.test(data$Floral[data$Guild=="GNS"])
# normal

hist(data$Nesting[data$Guild=="GNS"], breaks=15)
shapiro.test(data$Nesting[data$Guild=="GNS"])
# normal

hist(data$perc[data$Guild=="GNS"], breaks=15)
shapiro.test(data$perc[data$Guild=="GNS"])
# not normal

cor.test(data$perc[data$Guild=="GNS"], data$Floral[data$Guild=="GNS"], method="spearman", exact=F)

cor.test(data$perc[data$Guild=="GNS"], data$Nesting[data$Guild=="GNS"], method="spearman", exact=F )




