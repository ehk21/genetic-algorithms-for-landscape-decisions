rm(list=ls())

# load packages ----
library(dplyr)
library(plotrix)
library(diagis)
library(ggplot2)
library(gridExtra)
library(ggsci)
library(Hmisc)
library(sf)
library(RColorBrewer)
library(broom)
library(reshape2)
library(raster)
library(rasterVis)
library(stringr)

# read in data -----
results <- readRDS("results/results")
populations <- results$populations
fitnessScores <- results$fitnessScores
pixels <- results$pixels
nfScores <- results$floralNestingScores
patchDensity <- results$patchDensity
optimLandcovers <- read.csv("data/optim_landcovers.csv")
landcoverScores <- read.csv("data/landcover_scores.csv")
objectiveScores <- read.csv("results/objective_scores.csv")
objectiveScoresOptim <- read.csv("results/objective_scores_optimised_region.csv")

# prepare data ----
pixels$optim[pixels$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
pixels$optim[pixels$optim=="FarmerOnly"] <- "Farmer"
pixels$optim[pixels$optim=="AllBees"] <- "GNB + TNB + GNS"
pixels$optim <- factor(pixels$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))

pixelDF <- pixels

pixelDF$Code_ExpOp_GFS <- as.factor(pixelDF$Code_ExpOp_GFS)
pixelDF$optim <- factor(pixelDF$optim) #, levels=c("GNB", "TNB", "GNS", "FarmerOnly", "AllBees", "Farmer" ))
pixelDF$rep <- as.factor(pixelDF$rep)
pixelDF$pop <- as.factor(pixelDF$pop)
pixelDF$landscape <- as.factor(pixelDF$landscape)

pixelDF$radius <- paste0(pixelDF$radius, "m")
pixelDF$radius <- factor(pixelDF$radius, levels=c("500m", "1000m", "2000m"))

optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

# bar plot of % pixels of each landcover for all single-obj optims at 2000m radius ----

# define colour palette
colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")

# assign argument details (as plot code is adapted from a function)
# pixelDF <- pixels
bypopulation <- "final"
byoptim <- c("GNB", "TNB", "GNS", "Farmer")
byradius <- "2000m"
proportional <- T
compareby <- NA
facetby <- "optim"
horizontal <- T
plotTitle <- "A) Single-Objectives"
legendTitle <- "Landcover Type"

# calculate proportions
pixelDF <- pixelDF %>%
  group_by(optim, radius, pop, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)

pixelDF$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

pixelSummary <- pixelDF %>%
  group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(meanPerc=mean(pixelsPerc), pixelsSD=sd(pixelsPerc), pixelsSE=std.error(pixelsPerc), landscapeNo=n())

# subset data by: population, optim, radius
data <- subset(pixelSummary, pop %in% bypopulation)
data <- subset(data, optim %in% byoptim)
data <- subset(data, radius %in% byradius)

# weighted mean, standard error and standard deviation of all reps of each optim 
dataSummary <- data %>%
  group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo), se=weighted_se(meanPerc, landscapeNo), sd=sqrt(wtd.var(meanPerc)))

dataSummary <- as.data.frame(dataSummary)


# plot & save

if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
} else {cols=1}

pdf("figures/single_obj_landcovers.pdf",    
    height = (6/2.54), 
    width = (15/2.54))

ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
  geom_errorbar(aes(ymin=perc-sd, ymax=perc+sd), lwd=0.3, width=.5, position=position_dodge(.6), stat="identity") +
  theme_bw() +
  theme( axis.text.y = element_text(size=10), 
         axis.line = element_line(colour = "black", size=0.4), 
         axis.title=element_text(size=10, face="bold"), 
         plot.title=element_text(size=12, face="bold"), 
         axis.title.x=element_blank(), 
         axis.text.x=element_blank(), 
         axis.ticks.x=element_blank(), 
         strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
         strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
         legend.title=element_text(size=8, face="bold"), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         panel.background = element_blank(),
         legend.position="none") +
  labs(y="Mean % of Optimised Area", x="Landcover Type", fill=legendTitle) +
  facet_wrap(~get(facetby), ncol=cols, strip.position="bottom") +
  # scale_fill_npg() +
  scale_fill_manual(values = colors) +
  ggtitle("A) Single-objectives")

dev.off()

# bar plot of % pixels of each landcover for both multi-obj optims at 2000m radius ----

# define colour palette
colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")

# assign argument details (as plot code is adapted from a function)
# pixelDF <- pixels
bypopulation <- "final"
byoptim <- c("GNB + TNB + GNS", "GNB + TNB + GNS + Farmer")
byradius <- "2000m"
proportional <- T
compareby <- NA
facetby <- "optim"
horizontal <- T
plotTitle <- "A) Single-Objectives"
legendTitle <- "Landcover Type"

# calculate proportions
pixelDF <- pixelDF %>%
  group_by(optim, radius, pop, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)

pixelDF$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

pixelSummary <- pixelDF %>%
  group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(meanPerc=mean(pixelsPerc), pixelsSD=sd(pixelsPerc), pixelsSE=std.error(pixelsPerc), landscapeNo=n())

# subset data by: population, optim, radius
data <- subset(pixelSummary, pop %in% bypopulation)
data <- subset(data, optim %in% byoptim)
data <- subset(data, radius %in% byradius)

# weighted mean, standard error and standard deviation of all reps of each optim 
dataSummary <- data %>%
  group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo), se=weighted_se(meanPerc, landscapeNo), sd=sqrt(wtd.var(meanPerc)))

dataSummary <- as.data.frame(dataSummary)

# plot 

if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
} else {cols=1}

pdf("figures/multi_obj_landcovers.pdf",    
    height = (6/2.54), 
    width = (15/2.54))

ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
  geom_errorbar(aes(ymin=perc-sd, ymax=perc+sd), lwd=0.3, width=.5, position=position_dodge(.6), stat="identity") +
  theme_bw() +
  theme( axis.text.y = element_text(size=10), 
         axis.line = element_line(colour = "black", size=0.4), 
         axis.title=element_text(size=10, face="bold"), 
         plot.title=element_text(size=12, face="bold"), 
         axis.title.x=element_blank(), 
         axis.text.x=element_blank(), 
         axis.ticks.x=element_blank(), 
         strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
         strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
         legend.title=element_text(size=8, face="bold"), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         panel.background = element_blank(),
         legend.position="none") +
  labs(y="Mean % of Optimised Area", x="Landcover Type", fill=legendTitle) +
  facet_wrap(~get(facetby), ncol=cols, strip.position="bottom") +
  # scale_fill_npg() +
  scale_fill_manual(values = colors) +
  ggtitle("B) Multi-objectives")

dev.off()

# plot again so can save legend separately...

pdf("figures/landcover_legend.pdf",    
    height = (6/2.54), 
    width = (21/2.54))

ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
  theme_bw() +
  theme( legend.title=element_text(size=14, face="bold"), 
         legend.text=element_text(size=10),
         legend.position="bottom") +
  labs(fill=legendTitle) +
  facet_wrap(~get(facetby), ncol=cols, strip.position="bottom") +
  scale_fill_manual(values = colors) + 
  guides(fill = guide_legend(title.position = "top", title.hjust=0.5))

dev.off()


# bar plot of relative Poll4pop floral/ nesting scores for each land cover type ----

# translate upper/lower bounds into an amount of uncertainty
landcoverScores$Flor_P1_err <- landcoverScores$Flor_P1_b-landcoverScores$Flor_P1_l
landcoverScores <- landcoverScores[,!names(landcoverScores) %in% c("Flor_P2_b","Flor_P3_b","Flor_P1_l", "Flor_P1_u", "Flor_P2_l", "Flor_P2_u", "Flor_P3_l", "Flor_P3_u")]

landcoverScores$Flor_Cov_P1_err <- landcoverScores$Flor_Cov_P1_b-landcoverScores$Flor_Cov_P1_l
landcoverScores <- landcoverScores[,!names(landcoverScores) %in% c("Flor_Cov_P1_l", "Flor_Cov_P1_u")]

landcoverScores$Flor_Cov_P2_err <- landcoverScores$Flor_Cov_P2_b-landcoverScores$Flor_Cov_P2_l
landcoverScores <- landcoverScores[,!names(landcoverScores) %in% c("Flor_Cov_P2_l", "Flor_Cov_P2_u")]

landcoverScores$Flor_Cov_P3_err <- landcoverScores$Flor_Cov_P3_b-landcoverScores$Flor_Cov_P3_l
landcoverScores <- landcoverScores[,!names(landcoverScores) %in% c("Flor_Cov_P3_l", "Flor_Cov_P3_u")]

landcoverScores$Nest_P1_err <- landcoverScores$Nest_P1_b-landcoverScores$Nest_P1_l
landcoverScores <- landcoverScores[,!names(landcoverScores) %in% c("Nest_P1_l", "Nest_P1_u")]


# floral score = floral attractiveness*(floral cover early spring + floral cover late spring + floral cover summer)
landcoverScores$floral <- landcoverScores$Flor_P1_b*(landcoverScores$Flor_Cov_P1_b + landcoverScores$Flor_Cov_P2_b + landcoverScores$Flor_Cov_P3_b)

# propagate floral score errors
landcoverScores$floral_err <- landcoverScores$floral*(
  sqrt(
    (landcoverScores$Flor_P1_err/landcoverScores$Flor_P1_b)^2
    + (
      sqrt(landcoverScores$Flor_Cov_P1_err^2+landcoverScores$Flor_Cov_P2_err^2+landcoverScores$Flor_Cov_P3_err^2)
      /(landcoverScores$Flor_Cov_P1_b+landcoverScores$Flor_Cov_P2_b+landcoverScores$Flor_Cov_P3_b)
    )^2
  ))


# subset and wrangle data
landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS, select=c(Species_name, Landclass_ExpOp_GFS, floral, Nest_per_ha, Nest_P1_b, Nest_P1_err, floral_err))  

landcoverScoresOnly <- subset(landcoverScores, select=c(Species_name, Landclass_ExpOp_GFS, floral, Nest_P1_b))
landcoverWrangScores <- melt(landcoverScoresOnly, ID=c(Species_name, Landclass_ExpOp_GFS), variable.name="scoreType", value.name="Score")
landcoverWrangScores$scoreType <- factor(landcoverWrangScores$scoreType)
levels(landcoverWrangScores$scoreType) <- c("Floral", "Nesting")
landcoverWrangScores$Species_name <- factor(landcoverWrangScores$Species_name, levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees"))
levels(landcoverWrangScores$Species_name) <- c("GNB", "TNB", "GNS")


landcoverScoresError <- subset(landcoverScores, select=c(Species_name, Landclass_ExpOp_GFS, floral_err, Nest_P1_err))
landcoverWrangError <- melt(landcoverScoresError, ID=c(Species_name, Landclass_ExpOp_GFS), variable.name="scoreType", value.name="Error")
landcoverWrangError$scoreType <- factor(landcoverWrangError$scoreType)
levels(landcoverWrangError$scoreType) <- c("Floral", "Nesting")
landcoverWrangError$Species_name <- factor(landcoverWrangError$Species_name, levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees"))
levels(landcoverWrangError$Species_name) <- c("GNB", "TNB", "GNS")

landcoverWrang <- left_join(landcoverWrangScores, landcoverWrangError)

colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")
#(Beans, Cereal, Coniferous woodland, Deciduous woodland, Fallow, Improved perm grasslands, Oilseed rape, Unimproved meadow)

pdf("figures/nf_scores.pdf",    
    height = (6/2.54), 
    width = (10/2.54))

ggplot(landcoverWrang, aes(x=Landclass_ExpOp_GFS, y=Score, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(width=0), stat = "identity", color="black", size=0.4, width=0.8 ,alpha=.8) +
  geom_errorbar(aes(ymin=Score-Error, ymax=Score+Error), lwd=0.3, width=.5, position=position_dodge(.6), stat="identity") +
  facet_grid(rows=vars(scoreType), cols=vars(Species_name), scales="free", switch="y") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), 
        plot.title=element_text(size=12, face="bold"), 
        # axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_text(size=11), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
        strip.text.y = element_text(size = 10, color = "black", face = "bold"), 
        strip.text=element_text(face="bold"),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="bottom") +
  labs(fill="Landcover Type", y="Score (Arbitrary Units)") +
  ggtitle("C) Poll4pop-allocated Scores")

dev.off()

landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS)

ggplot(landcoverScores, aes(x=Landclass_ExpOp_GFS, y=Nest_P1_b, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(width=0), stat = "identity", color="black", size=0.4, width=0.8 ,alpha=.8) +
  facet_wrap(~Species_name) +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), 
        plot.title=element_text(size=12, face="bold"), 
        # axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_text(size=11), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
        strip.text.y = element_text(size = 10, color = "black", face = "bold"), 
        strip.text=element_text(face="bold"),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="none") +
  labs(fill="Landcover Type", y="Score (Arbitrary Units)") +
  ggtitle("C) Poll4pop-allocated Scores")

# bar plot of patch area of landscapes ----

patchDensity$pop <- factor(patchDensity$pop)
patchDensity$patchArea <- as.numeric(patchDensity$patchArea)

# replace and order optim names
patchDensity$optim[patchDensity$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
patchDensity$optim[patchDensity$optim=="FarmerOnly"] <- "Farmer"
patchDensity$optim[patchDensity$optim=="AllBees"] <- "GNB + TNB + GNS"

pd <- subset(patchDensity, radius==2000)

# pd1 <- pd %>%
#   group_by(optim, radius, pop) %>%
#   dplyr::summarise(areaMean = weighted.mean(patchArea), sd=sqrt(wtd.var(patchArea)))

pd1 <- pd %>%
  group_by(optim, radius, pop) %>%
  dplyr::summarise(areaMean = mean(patchArea), se=std.error(patchArea))

pd2 <- subset(pd1, pop=="final" & optim %in% c("GNB + TNB + GNS", "GNB", "GNS", "TNB", "Farmer", "GNB + TNB + GNS + Farmer"))

pd2 <- as.data.frame(pd2)
pd2$optim <- factor(pd2$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))
pd2$areaMean <- as.numeric(pd2$areaMean)

pd2$labels <- c("B", "BC", "C", "D", "B", "A")

pdf("figures/patch_area.pdf",    
    height = (9.5/2.54), 
    width = (15/2.54))

ggplot(pd2, aes(y=areaMean, x=optim)) + #, fill=optim)) +
  geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=areaMean-se, ymax=areaMean+se), width=.22, position=position_dodge(.6), stat="identity") +
  # scale_fill_grey() +
  theme_bw() +
  coord_cartesian(ylim=c(6.5,7)) +
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=10, face="bold"), 
        plot.title=element_text(size=12, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold"),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none") +
  labs(y="Mean Patch Area (Hectares)", x="Optimisation") +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  geom_text(aes(label=labels, y=areaMean+(se)), vjust=-.5)+
  ggtitle("A")

dev.off()

# plot example landscape rasters----

populations <- results$populations
pixels <- results$pixel
pixels <- subset(pixels, optim=c("GNB", "TNB", "GNS", "AllBees", "Farmer", "FarmerOnly")) 
pixels <- subset(pixels, radius==2000)
populations <- subset(populations, optim=c("GNB", "TNB", "GNS", "AllBees", "Farmer", "FarmerOnly") & radius ==2000)
populations <- subset(populations, radius==2000)

# replace and order optim names
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

codes <- read.csv("data/Landclass_ExpOp_GFS.csv", header=T)
codes[nrow(codes)+1,] <- c(0,"Freshwater")
codes$Code_ExpOp_GFS <- as.double(codes$Code_ExpOp_GFS)

optimLandcovers <- read.csv("data/optim_landcovers.csv")

# "Coniferous Woodland","Deciduous Woodland", "Fallow", "Improved Permanent Grasslands","Unimproved Meadow", 
# "Cereal", "Broad/ Field Beans", "Oilseed Rape"   
optimLandcovers$col <- c("#000000" , "#1B5E20", "#924900", "#11C638", "#FFB6DB", "#FFD54F", "#920000", "#6DB6FF")

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
  assign(result$optim[i], levelplot(r2, col.regions=cols, main = paste0(result$optim[i]), cex.main = 25, colorkey=FALSE, scales=list(draw=FALSE)))
}



png("figures/GNB_raster.png",    
    height = (10/2.54), 
    width = (10/2.54), 
    units = "in", 
    res=600)
plot(GNB) 
dev.off()

png("figures/TNB_raster.png",    
    height = (10/2.54), 
    width = (10/2.54),
    units = "in", 
    res=600)
plot(TNB)
dev.off()

png("figures/GNS_raster.png",    
    height = (10/2.54), 
    width = (10/2.54),
    units = "in", 
    res=600)
plot(GNS)
dev.off()

png("figures/Farmer_raster.png",    
    height = (10/2.54), 
    width = (10/2.54),
    units = "in", 
    res=600)
plot(Farmer)
dev.off()

png("figures/AllBees_raster.png",    
    height = (10/2.54), 
    width = (10/2.54),    
    units = "in", 
    res=600)
plot(`GNB + TNB + GNS`)
dev.off()

png("figures/AllBees+Farmer_raster.png",    
    height = (10/2.54), 
    width = (10/2.54),
    units = "in", 
    res=600)
plot(`GNB + TNB + GNS + Farmer`)
dev.off()

# plot normalised objective scores (with standard error)----

# format data
objectiveScores$optim <- factor(objectiveScores$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer", "SK86"))

objectiveScoresOptim[,2:5]<-objectiveScoresOptim[,2:5]*(-1)
objectiveScoresOptim$optim[objectiveScoresOptim$optim=="AllBees"]<-"GNB + TNB + GNS"
objectiveScoresOptim$optim[objectiveScoresOptim$optim=="Farmer"]<-"GNB + TNB + GNS + Farmer"
objectiveScoresOptim$optim[objectiveScoresOptim$optim=="FarmerOnly"]<-"Farmer"
objectiveScoresOptim$optim <- factor(objectiveScoresOptim$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer", "SK86"))

# convert from wide to long
scoresLong <- melt(objectiveScores, id.vars=c("optim", "rep", "landscape"), measure.vars=c("gnb", "gns", "tnb", "farmer"), variable.name="objective", value.name="score")
scoresLongOptim <- melt(objectiveScoresOptim, id.vars=c("optim", "rep", "landscape"), measure.vars=c("gnb", "gns", "tnb", "farmer"), variable.name="objective", value.name="score")

# calculate mean and SD for each objective in each type of optimisation
meanScores <- scoresLong %>%
  group_by(optim, objective) %>%
  dplyr::summarise(scoreAv=mean(score), scoreSE=std.error(score)) # !!!

meanScoresOptim <- scoresLongOptim %>%
  group_by(optim, objective) %>%
  dplyr::summarise(scoreAv=mean(score), scoreSE=std.error(score)) # !!!

meanScores<-as.data.frame(meanScores)
meanScoresOptim<-as.data.frame(meanScoresOptim)

sk86Means<-subset(meanScores, optim=="SK86")
sk86MeansOptim<-subset(meanScoresOptim, optim=="SK86")

meanScores <- meanScores[meanScores$optim != "SK86",]
meanScoresOptim <- meanScoresOptim[meanScoresOptim$optim != "SK86",]

levels(meanScores$objective) <- c("GNB", "GNS", "TNB", "Farmer")
levels(sk86Means$objective) <- c("GNB", "GNS", "TNB", "Farmer")
levels(meanScoresOptim$objective) <- c("GNB", "GNS", "TNB", "Farmer")
levels(sk86MeansOptim$objective) <- c("GNB", "GNS", "TNB", "Farmer")

# normalise scores using Emma's equation:
meanScores$normScore <- NA
meanScores$normSE <- NA
meanScoresOptim$normScore <- NA
meanScoresOptim$normSE <- NA

for(i in 1:nrow(meanScores)){
  meanScores$normScore[i]<-log10(meanScores$scoreAv[i]/sk86Means$scoreAv[sk86Means$objective==meanScores$objective[i]])
  meanScores$normSE[i]<- meanScores$scoreSE[i]/(meanScores$scoreAv[i]*log(10))
  meanScoresOptim$normScore[i]<-log10(meanScoresOptim$scoreAv[i]/sk86MeansOptim$scoreAv[sk86MeansOptim$objective==meanScoresOptim$objective[i]])
  meanScoresOptim$normSE[i]<- meanScoresOptim$scoreSE[i]/(meanScoresOptim$scoreAv[i]*log(10))
}

# combine dataframes
meanScores$area <- "Whole grid square"
meanScoresOptim$area <- "Optimised region"

scoresDF <- rbind(meanScores, meanScoresOptim)

scoresDF$area <- factor(scoresDF$area, levels=c("Whole grid square", "Optimised region"))

scoresDF$objective <- factor(scoresDF$objective, levels=c("GNB", "TNB", "GNS", "Farmer"))

# plot normalised scores

df3=data.frame(newvar = rep("xx",2),    
               area = c("Whole grid square", "Optimised region"),
               newvalue = c(1,12.5),
               objective = NA)

df3$area <- factor(df3$area, levels=c("Whole grid square", "Optimised region"))

pdf("figures/normalised_scores.pdf",    
    height = (18/2.54), 
    width = (27/2.54))

ggplot(scoresDF, aes(x=optim, y=normScore, color=objective)) +
  geom_point(
    size=5, aes(shape=objective)
  ) +
  scale_shape_manual(
    values=c(15:18)
  ) +
  # geom_hline(data=df3, aes(yintercept=newvalue), alpha = 0) +
  geom_errorbar(
    aes(ymin=normScore-normSE, ymax=normScore+normSE), width=.3,
  ) +
  facet_wrap(~area, scales="free") +
  labs(
    y="Log (mean fitness of end-user in optimised landscape / \nfitness in real landscape)", x= "Objective(s) of optimisation", color="Landscape \n end-user", shape="Landscape \n end-user"
  ) +
  geom_hline(aes(yintercept=0, linetype="Score in real-life \n landscape"), color = "red") +
  scale_linetype_manual(values = 2) + 
  labs(linetype = NULL) + 
  theme_bw(
  ) +
  theme(
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12),
    axis.line = element_line(colour = "black", size=0.4),
    axis.title=element_text(size=13, face="bold"),
    plot.title=element_text(size=18, face="bold"),
    legend.title=element_text(size=12, face="bold"),
    legend.position = "bottom",
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.text=element_text(size=10),
    strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
    strip.text= element_text(face="bold", size=12),
    plot.margin = margin(t = 5, r = 25, b = 5, l = 5, unit = "pt")
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  scale_y_continuous(
    n.breaks=6
  ) +
  ggtitle("A")
# +
#   annotation_logticks(sides="l")

dev.off()



# parallel coordinate plots----

scoresDF <- fitnessScores
scoresDF$score <- -scoresDF$score
scoresDF$objective <- factor(scoresDF$objective, levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees", "Farmer"))


# Plot multi-obj with just bees:
beeScores <- subset(scoresDF, optim=="AllBees" & radius==2000 & gen ==100)
beeScores$landscape <- NA

for(i in 1:length(unique(beeScores$rep))){
  for(j in 1:length(unique(beeScores$objective))){
    rep <- unique(beeScores$rep)[i]
    objective <- unique(beeScores$objective)[j]
    no = 1
    for(k in which(beeScores$rep==rep & beeScores$objective==objective)){
      beeScores$landscape[k] <- no
      no <- no + 1
    }
  }
}

beeScores$no <- paste0(beeScores$rep, ".", beeScores$landscape)
beeScores$scoreNorm <- NA


for(i in 1:length(levels(beeScores$objective))){
  obj <- (levels(beeScores$objective))[i]
  rows <- which(beeScores$objective==obj)
  beeScores$scoreNorm[rows] <- (beeScores$score[rows]-min(beeScores$score[rows]))/(max(beeScores$score[rows])-min(beeScores$score[rows]))
}

range(beeScores$scoreNorm)

colourCount = length(beeScores$no)
getPalette = colorRampPalette(brewer.pal(11, "BrBG"))


beePCP <- ggplot(beeScores ,aes(x=objective, y=scoreNorm, group=no, color=no)) +
  # geom_line(color=getPalette(colourCount), size=0.3) +
  geom_line(color="black", size=0.1) +
  labs(y="Normalised Objective Scores", x="Objective") +
  theme_bw() +
  theme( axis.text.y = element_text(size=30), 
         axis.line = element_line(colour = "black", size=0.4), 
         axis.title=element_text(size=30, face="bold"), 
         plot.title=element_text(size=36, face="bold"), 
         axis.text.x=element_text(size=30), 
         legend.title=element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
         plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "pt"),
         axis.ticks.length=unit(0.25, "cm"),
  ) +
  scale_x_discrete(expand = c(0.01, 0), labels=c("GNB", "TNB", "GNS")) +
  scale_y_continuous(expand = c(0.01, 0)) +
  ggtitle("B")

pdf("figures/bee_PCP.pdf",    
    height = (20/2.54), 
    width = (30/2.54))

plot(beePCP)

dev.off()



# Plot multi-obj with farmers and bees:
farmerScores <- subset(scoresDF, optim=="Farmer" & radius==2000 & gen ==100)
farmerScores$landscape <- NA

for(i in 1:length(unique(farmerScores$rep))){
  for(j in 1:length(unique(farmerScores$objective))){
    rep <- unique(farmerScores$rep)[i]
    objective <- unique(farmerScores$objective)[j]
    no = 1
    for(k in which(farmerScores$rep==rep & farmerScores$objective==objective)){
      farmerScores$landscape[k] <- no
      no <- no + 1
    }
  }
}

farmerScores$no <- paste0(farmerScores$rep, ".", farmerScores$landscape)
farmerScores$scoreNorm <- NA


for(i in 1:length(levels(farmerScores$objective))){
  obj <- (levels(farmerScores$objective))[i]
  rows <- which(farmerScores$objective==obj)
  farmerScores$scoreNorm[rows] <- (farmerScores$score[rows]-min(farmerScores$score[rows]))/(max(farmerScores$score[rows])-min(farmerScores$score[rows]))
}

range(farmerScores$scoreNorm)

colourCount = length(farmerScores$no)
getPalette = colorRampPalette(brewer.pal(11, "BrBG"))

farmerPCP <- ggplot(farmerScores ,aes(x=objective, y=scoreNorm, group=no, color=no)) +
  # geom_line(color=getPalette(colourCount), size=0.3) +
  geom_line(color="black", size=0.1) +
  theme(legend.position="none") +
  labs(y="Normalised Objective Scores", x="Objective") +
  theme_bw() +
  theme( axis.text.y = element_text(size=30), 
         axis.line = element_line(colour = "black", size=0.4), 
         axis.title=element_text(size=30, face="bold"), 
         plot.title=element_text(size=36, face="bold"), 
         axis.text.x=element_text(size=30), 
         legend.title=element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
         plot.margin = margin(t = 10, r = 50, b = 10, l = 10, unit = "pt"),
         axis.ticks.length=unit(0.25, "cm"),
  ) +
  scale_x_discrete(expand = c(0.01, 0), labels=c("GNB", "TNB", "GNS","Farmer")) +
  scale_y_continuous(expand = c(0.01, 0)) +
  ggtitle("C")

# PCP <- grid.arrange(beePCP, farmerPCP, ncol=2)

pdf("figures/farmer_PCP.pdf",    
    height = (20/2.54), 
    width = (30/2.54))

plot(farmerPCP)

dev.off()




