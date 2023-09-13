rm(list=ls())

# load packages ---- 

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
library(ggpattern)


# source functions ----

source("code/process_results/load_optims_fn.R")
source("code/process_results/pixel_plots.R")
source("code/process_results/scores_plots.R")
source("code/process_results/plot_floral_nesting_scores.R")
source("code/process_results/landscapemetrics_plots.R")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# compile data (only needs to be done once)
#########################################################################################################

# optims <- read.csv("cluster/data/optims.csv")
# results <- load.optims(optims, rows=c(1:528), pix=T, pop=T, fit=T, nf=T, dens=T)
# saveRDS(results, "cluster/results/results")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# load data 
#########################################################################################################

results <- readRDS("results/results")
populations <- results$populations
fitnessScores <- results$fitnessScores
pixels <- results$pixels
nfScores <- results$floralNestingScores
patchDensity <- results$patchDensity

# # check all data present
# populations <- populations %>%
#   group_by(optim, radius) %>%
#   summarise(tally=length(unique(rep)))
# 
# fitnessScores <- fitnessScores %>%
#   group_by(optim, radius) %>%
#   summarise(tally=length(unique(rep)))
# 
# nfScores <- nfScores %>%
#   group_by(optim, radius) %>%
#   summarise(tally=length(unique(rep)))
# 
# pixels <- pixels %>%
#   group_by(optim, radius) %>%
#   summarise(tally=length(unique(rep)))

# replace and order optim names
pixels$optim[pixels$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
pixels$optim[pixels$optim=="FarmerOnly"] <- "Farmer"
pixels$optim[pixels$optim=="AllBees"] <- "GNB + TNB + GNS"
pixels$optim <- factor(pixels$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))
#--------------------------------------------------------------------------------------------------------
#########################################################################################################
#########################################################################################################
# scale experiment results
#########################################################################################################
#########################################################################################################
#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# bar plot of % pixels of each landcover for single-obj GNB optims at each scale
#########################################################################################################
scalePixelPlot <- plot.pixels(pixelDF=pixels, bypopulation="final", byoptim="GNB", byradius=c("500m", "1000m", "2000m"), proportional=T, 
                    compareby=NA, facetby="radius", horizontal=T, plotTitle="", legendTitle="Landcover Type")

scalePixelPlot1 <- scalePixelPlot + theme(legend.position="bottom", 
                                          axis.text.y = element_text(size=12),
                                          axis.title=element_text(size=14, face="bold"),
                                          legend.title=element_text(size=12, face="bold"),
                                          strip.text=element_text(size=12, face="bold"))

plot(scalePixelPlot1)
ggsave("../write_up/Figures/scalePixelPlot.png", plot=last_plot(), width=24, height=18, units="cm", dpi=600)

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# interaction plot of pixel % for grouped landcovers at each radius (GNB single-obj)
#########################################################################################################

# prepare data
pixelDF <- pixels

pixelDF$Code_ExpOp_GFS <- as.factor(pixelDF$Code_ExpOp_GFS)
pixelDF$optim <- as.factor(pixelDF$optim)
pixelDF$rep <- as.factor(pixelDF$rep)
pixelDF$pop <- as.factor(pixelDF$pop)
pixelDF$landscape <- as.factor(pixelDF$landscape)

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

data <- subset(pixelDF, pop %in% "final" & optim %in% "GNB", select=c(radius, rep, landscape, Landclass_ExpOp_GFS, pixels))

# calculate mean pixel percentages for each repeat
data1 <- data %>%
  group_by(radius, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

data2 <- data1 %>%
  group_by(radius, rep, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(meanPerc=mean(pixelsPerc), pixelsSD=sd(pixelsPerc), pixelsSE=std.error(pixelsPerc), landscapeNo=n())

# group 'other' landcover types together
dataSub1 <- subset(data2, Landclass_ExpOp_GFS %in% c("Deciduous Woodland", "Unimproved Meadow", "Fallow"), select=c("radius", "rep", "Landclass_ExpOp_GFS", "meanPerc", "landscapeNo"))
dataSub2 <- subset(data2, !(Landclass_ExpOp_GFS %in% c("Deciduous Woodland", "Unimproved Meadow", "Fallow")))

dataSub2 <- dataSub2 %>%
  group_by(radius, rep, landscapeNo) %>%
  dplyr::summarise(meanPerc=sum(meanPerc))

dataSub2$Landclass_ExpOp_GFS <- "Other"

data3 <- rbind(dataSub1, dataSub2)

data3$Landclass_ExpOp_GFS <- factor(data3$Landclass_ExpOp_GFS, levels=c("Unimproved Meadow", "Deciduous Woodland", "Fallow", "Other"))

# calculate weighted mean (as different number of rank 1 landscapes in each rep)
dataSummary <- data3 %>%
  group_by(radius, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo), se=weighted_se(meanPerc, landscapeNo), sd=sd(meanPerc))
dataSummary <- as.data.frame(dataSummary)

# define colour palette
colors<-c("#FFB6DB", "#004949", "#924900", "#FF0000")

#plot
scaleInteractionPlot <- ggplot(dataSummary, aes(x=radius, y=perc, color=Landclass_ExpOp_GFS)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  geom_errorbar(aes(ymin=perc-se, ymax=perc+se), width=15) +
  scale_color_manual(values=colors) +
  theme_bw() +
  labs(y="Mean % of Optimised Area", x="Radius (m)", color="Landcover Type") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black", size=.75), 
        legend.position=c(0.87, 0.85), 
        axis.text.x = element_text(hjust=0.5, size=12), axis.text.y = element_text(size=12),
        axis.title=element_text(size=14, face="bold"), 
        legend.text = element_text(size=12),
        legend.title=element_text(size=12, face="bold"),
        plot.title = element_text(size=16,face="bold")) +
  scale_y_continuous(limits = c(0,60), expand = c(0, 1)) +
  scale_x_continuous( limits=c(480, 2020), expand = c(0, 0)) 
  # ggtitle("A")

plot(scaleInteractionPlot)
ggsave("figures/scale_interaction_plot.png", dpi=600, width=23, height=15, units="cm")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics comparing %pixels of each landcover at each radius for GNB
#########################################################################################################

# prepare data
pixelDF <- pixels

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T) # replace landcover codes with names

data <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB"), select=c(radius, rep, landscape, Landclass_ExpOp_GFS, pixels))

# calculate percentages
data1 <- data %>%
  group_by(radius, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

# group 'other' landcovers together
dataSub1 <- subset(data1, Landclass_ExpOp_GFS %in% c("Deciduous Woodland", "Unimproved Meadow", "Fallow"), select=c("radius", "rep", "Landclass_ExpOp_GFS", "landscape", "pixelsPerc"))
dataSub2 <- subset(data1, !(Landclass_ExpOp_GFS %in% c("Deciduous Woodland", "Unimproved Meadow", "Fallow")), select=c("radius", "rep", "landscape", "pixelsPerc"))

dataSub2 <- dataSub2 %>%
  group_by(radius, rep, landscape) %>%
  dplyr::summarise(pixelsPerc=sum(pixelsPerc))

dataSub2$Landclass_ExpOp_GFS <- "Other"

data2 <- rbind(dataSub1, dataSub2)

data2 <- as.data.frame(data2)
data2$Landclass_ExpOp_GFS <- as.factor(data2$Landclass_ExpOp_GFS)

# summarise data
summary(data2)

# visualise spread
par(mfrow=c(1,2))
plot(pixelsPerc ~ radius, data=data2)
plot(pixelsPerc ~ Landclass_ExpOp_GFS, data=data2)
hist(data2$pixelsPerc)

# homogeneity of variances:
dataVarRad <- data2 %>%
  group_by(radius) %>%
  summarise(variance=var(pixelsPerc))

dataVarLandcover <- data2 %>%
  group_by(Landclass_ExpOp_GFS) %>%
  summarise(variance=var(pixelsPerc))

# linear model
model <- lm(pixelsPerc ~ radius * Landclass_ExpOp_GFS, data=data2)

# visually inspect data to check fro normality of residuals
par(mfrow=c(2,2))
plot(model)

# assess model
summary(model)
anova(model)

# save model coefficients
modeldf <- tidy(summary(model))
write.csv(modeldf, "results/GNB_radius_comparison.csv")

ANOVAdf <- tidy(anova(model))
write.csv(ANOVAdf, "results/GNB_radius_comparison_ANOVA.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# violin plot of GNB data spread at each radius
#########################################################################################################

# prepare data
pixelDF <- pixels

pixelDF$Code_ExpOp_GFS <- as.factor(pixelDF$Code_ExpOp_GFS)
pixelDF$optim <- as.factor(pixelDF$optim)
pixelDF$rep <- as.factor(pixelDF$rep)
pixelDF$pop <- as.factor(pixelDF$pop)
pixelDF$landscape <- as.factor(pixelDF$landscape)

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

data <- subset(pixelDF, pop %in% "final" & optim %in% "GNB", select=c(radius, rep, landscape, Landclass_ExpOp_GFS, pixels))

# calculate pixel percentages
data1 <- data %>%
  group_by(radius, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT))
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

# group 'other' landcovers together

dataSub1 <- subset(data1, Landclass_ExpOp_GFS %in% c("Deciduous Woodland", "Unimproved Meadow", "Fallow"), select=c("radius", "rep", "Landclass_ExpOp_GFS", "landscape", "pixelsPerc"))
dataSub2 <- subset(data1, !(Landclass_ExpOp_GFS %in% c("Deciduous Woodland", "Unimproved Meadow", "Fallow")), select=c("radius", "rep", "landscape", "pixelsPerc"))

dataSub2 <- dataSub2 %>%
  group_by(radius, rep, landscape) %>%
  dplyr::summarise(pixelsPerc=sum(pixelsPerc))

dataSub2$Landclass_ExpOp_GFS <- "Other"

data2 <- rbind(dataSub1, dataSub2)

data2 <- as.data.frame(data2)
data2$Landclass_ExpOp_GFS <- as.factor(data2$Landclass_ExpOp_GFS)

# data2$radius <- paste0(data2$radius, "m Radius")
# data2$radius <- factor(data2$radius, levels=c("500m Radius", "1000m Radius", "2000m Radius"))

data2$radius <- factor(data2$radius)


# plot
ScaleViolin <- ggplot(data2, aes(y=(100*pixelsPerc), x=as.factor(radius), fill=as.factor(radius))) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  scale_fill_nejm() +
  labs( y= "Mean % of Optimised Area", x="Radius (m)") +
  theme_bw() +
  facet_wrap(~as.factor(radius), scales="free_x", strip.position = "bottom", ncol=1) +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size=12), 
        axis.ticks.x=element_blank(), axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        strip.text=element_text(size=12, face="bold")) +
  ggtitle("B")

plot(ScaleViolin)
ggsave("../write_up/Figures/scalViolin.png", dpi=600, width=7, height=30, units="cm")


#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# bar plot of % change in nesting/floral scores at each scale (GNB single-obj)
#########################################################################################################

scaleNF <- plot.floral.nesting.scores (scores=nfScores, byoptim=c("GNB", "TNB", "GNS", "AllBees", "AllFarmer"), byradius=c(2000), 
                                     byguild=c("GNB", "TNB", "GNS"), byscore = c("Spring", "Summer", "Nesting"), xby="optim",
                                     compareby="score", facetby="guild", horizontal=T, plotTitle="C", bypopulation=c("compare"), legendTitle="Population", xlab="Radius")

scaleNF1 <- scaleNF + theme(legend.position="bottom", 
                            axis.text.y = element_text(size=12),
                            axis.title=element_text(size=14, face="bold"),
                            strip.text=element_text(size=12, face="bold"),
                            legend.position = "none") +
                      labs(x="Radius (m)")
plot(scaleNF)
ggsave("../write_up/Figures/scaleNF.png", dpi=600, width=21, height=15, units="cm")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics comparing changes in nesting/floral scores between initial/final populations across different radii (single-obj GNB)
#########################################################################################################

results <- readRDS("results/results")
nfScores <- results$floralNestingScores
nfScores <- subset(nfScores, optim=="GNB" & guild=="GroundNestingBumblebees")

# replace NA values
nfScores$spring2f <- replace(nfScores$spring2f, is.na(nfScores$spring2f), 0)
nfScores$pop <- factor(nfScores$pop, levels=c("initial", "final"))

# convert scores to percentage change between final and initial populations
scores2 <- nfScores %>%
  mutate(springf = spring1f + spring2f )

scores3 <- scores2 %>%
  dplyr::select(radius, rep, pop, guild, landscape, springf, summerf, nesting) %>%
  group_by(radius, rep, pop, guild) %>%
  dplyr::summarise(springfAv = mean(springf), summerfAv = mean(summerf), nestingAv=mean(nesting), landscapeNo=n())

scores4 <- scores3 %>%
  group_by(radius, rep) %>%
  dplyr::summarise(Spring = ((springfAv[pop=="final"]-springfAv[pop=="initial"])/springfAv[pop=="initial"])*100, 
                   Summer = ((summerfAv[pop=="final"]-summerfAv[pop=="initial"])/summerfAv[pop=="initial"])*100,
                   Nesting = ((nestingAv[pop=="final"]-nestingAv[pop=="initial"])/nestingAv[pop=="initial"])*100)

scores5 <- melt(scores4, id=c("radius", "rep"), variable.name = "score", value.name = "percChange")

# visualise spread
ggplot(data=scores5, aes(y=percChange, x=as.factor(score), fill=as.factor(radius))) +
  geom_boxplot()

# create model
model <- aov(percChange ~ as.factor(score) * as.factor(radius), data=scores5)

# visually inspect data to check for normality of residuals/ homogeneity of variances
par(mfrow=c(2,2))
plot(model)

# assess model results 
summary(model)
TukeyHSD(model)

# save model coefficients
ANOVAdf <- tidy(TukeyHSD(model))
write.csv(ANOVAdf, "results/GNB_NF_scores_comparison_Tukey.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# bar plot of % pixels of each landcover for all single-obj optims at 2000m radius
#########################################################################################################

singleObjPix <- plot.pixels(pixelDF=pixels, bypopulation=c("final"), byoptim=c("GNB", "TNB", "GNS", "Farmer"), 
                         byradius=c( "2000m"), proportional=T, compareby=NA, 
                         facetby="optim", horizontal=TRUE, plotTitle="A) Single-Objectives", legendTitle="Landcover Type")

singleObjPix1 <- singleObjPix + theme(legend.position="bottom", 
                                      axis.text.y = element_text(size=10),
                                      axis.title=element_text(size=10, face="bold"),
                                      legend.title=element_text(size=10, face="bold"),
                                      legend.text = element_text(size=8),
                                      strip.text=element_text(size=10, face="bold"),
                                      # plot.title=element_text(size=14, face="bold"), 
                                      plot.title = element_blank())
plot(singleObjPix1)
ggsave("figures/singleObjPix.png", dpi=600, width=22, height=5.5, units="cm")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics comparing % pixels of each landcover between single-obj optims at 2000m radius
#########################################################################################################

# prepare data
pixelDF <- pixels

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

data <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB", "TNB", "GNS", "Farmer") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))

data1 <- data %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

data1 <- as.data.frame(data1)
data1$Landclass_ExpOp_GFS <- as.factor(data1$Landclass_ExpOp_GFS)
data1$optim <- as.factor(data1$optim)

# summarise data
summary(data1)

IQR(data1$pixelsPerc[data1$optim=="GNB"])
IQR(data1$pixelsPerc[data1$optim=="GNS"])
IQR(data1$pixelsPerc[data1$optim=="TNB"])
IQR(data1$pixelsPerc[data1$optim=="Farmer"])

var(data1$pixelsPerc[data1$optim=="GNB"])
var(data1$pixelsPerc[data1$optim=="GNS"])
var(data1$pixelsPerc[data1$optim=="TNB"])
var(data1$pixelsPerc[data1$optim=="Farmer"])

n <- data1 %>%
  group_by(optim, rep) %>%
  dplyr::summarise(n=max(landscape))

n1 <- n %>%
  group_by(optim) %>%
  dplyr::summarise(n=sum(n))

# visualise spread
par(mfrow=c(1,3))
plot(pixelsPerc ~ optim, data=data1)
plot(pixelsPerc ~ Landclass_ExpOp_GFS, data=data1)
hist(data1$pixelsPerc)

# homogeneity of variances:
dataVarOpt <- data1 %>%
  group_by(optim) %>%
  dplyr::summarise(variance=var(pixelsPerc))

dataVarLandcover <- data1 %>%
  group_by(Landclass_ExpOp_GFS) %>%
  summarise(variance=var(pixelsPerc))
# ratio of variances here may be very large (see linear models - going big handout)
# this is something to bear in mind when discussing and interpreting results of the linear model

# linear model
model <- aov(sqrt(pixelsPerc) ~ optim * Landclass_ExpOp_GFS, data=data1)

# visually inspect data to check fro normality of residuals
par(mfrow=c(2,2))
plot(model)

# assess model
summary(model)
TukeyHSD(model)

# save model coefficients
TukeyDF <- tidy(TukeyHSD(model))
# write.csv(TukeyDF, "cluster/results/single-obj-perc-compare-Tukey.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# bar plot of % pixels of each landcover for both multi-obj optims at 2000m radius
#########################################################################################################

multiObjPix <- plot.pixels(pixelDF=pixels, bypopulation=c("final"), byoptim=c("GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"), 
                            byradius=c("2000m"), proportional=T, compareby=NA, 
                            facetby="optim", horizontal=TRUE, plotTitle="B) Multi-Objectives", legendTitle="Landcover Type")

multiObjPix1 <- multiObjPix + theme(legend.position="bottom", 
                                   axis.text.y = element_text(size=10),
                                   axis.title=element_text(size=10, face="bold"),
                                   legend.title=element_text(size=14, face="bold"),
                                   strip.text=element_text(size=10, face="bold"),
                                   legend.text=element_text(size=12),
                                   # plot.title=element_text(size=16, face="bold"))
                                   plot.title=element_blank())
plot(multiObjPix1)
ggsave("figures/multiObjPix.png", dpi=600, width=16.83, height=5.5, units="cm")


#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics comparing % pixels of each landcover of multi-obj optims at 2000m radius
#########################################################################################################

# prepare data
pixelDF <- pixels

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

data <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB + TNB + GNS", "GNB + TNB + GNS + Farmer") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))

# calculate percentages
data1 <- data %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

data1 <- as.data.frame(data1)
data1$Landclass_ExpOp_GFS <- as.factor(data1$Landclass_ExpOp_GFS)
data1$optim <- as.factor(data1$optim)

# summarise data
summary(data1)

IQR(data1$pixelsPerc[data1$optim=="GNB + TNB + GNS"])
IQR(data1$pixelsPerc[data1$optim=="GNB + TNB + GNS + Farmer"])

var(data1$pixelsPerc[data1$optim=="GNB + TNB + GNS"])
var(data1$pixelsPerc[data1$optim=="GNB + TNB + GNS + Farmer"])

n <- data1 %>%
  group_by(optim, rep) %>%
  dplyr::summarise(n=max(landscape))

n1 <- n %>%
  group_by(optim) %>%
  dplyr::summarise(n=sum(n))

# visualise spread
par(mfrow=c(1,3))
plot(pixelsPerc ~ optim, data=data1)
plot(pixelsPerc ~ Landclass_ExpOp_GFS, data=data1)
hist(data1$pixelsPerc)

# homogeneity of variances:
dataVarOpt <- data1 %>%
  group_by(optim) %>%
  dplyr::summarise(variance=var(pixelsPerc))

dataVarLandcover <- data1 %>%
  group_by(Landclass_ExpOp_GFS) %>%
  summarise(variance=var(pixelsPerc))
# ratio of variances here may be very large (see linear models - going big handout)
# this is something to bear in mind when discussing and interpreting results of the linear model

# linear model
model <- lm(sqrt(pixelsPerc) ~ optim * Landclass_ExpOp_GFS, data=data1)

# visually inspect data to check fro normality of residuals
par(mfrow=c(2,2))
plot(model)

# assess model
summary(model)
TukeyHSD(model)

# save model coefficients
AnovaDF <- tidy(aov(model))
TukeyDF <- tidy(TukeyHSD(model))
# write.csv(TukeyDF, "cluster/results/multi-obj-perc-compare-Tukey.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# bar plot comparing % pixels of each landcover for multi-obj optims to the means of single-objective optims at 2000m radius
#########################################################################################################

# create dataframe
pixelDF <- pixels
optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

singDataBees <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB", "TNB", "GNS") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
singDataBees <- singDataBees %>%
  group_by(optim, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)
singDataBees$pixelsPerc[which(is.na(singDataBees$pixelsPerc))] <- 0
singDataBees$type <- "Single-objective (Mean)"
singDataBees$objs <- "GNB + TNB + GNS"


multDataBees <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB + TNB + GNS") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
multDataBees <- multDataBees %>%
  group_by(optim, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)
multDataBees$pixelsPerc[which(is.na(multDataBees$pixelsPerc))] <- 0
multDataBees$type <- "Multi-objective"
multDataBees$objs <- "GNB + TNB + GNS"

singDataFarmer <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB", "TNB", "GNS", "Farmer") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
singDataFarmer <- singDataFarmer %>%
  group_by(optim, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)
singDataFarmer$pixelsPerc[which(is.na(singDataFarmer$pixelsPerc))] <- 0
singDataFarmer$type <- "Single-objective (Mean)"
singDataFarmer$objs <- "GNB + TNB + GNS + Farmer"


multDataFarmer <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB + TNB + GNS + Farmer") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
multDataFarmer <- multDataFarmer %>%
  group_by(optim, rep, landscape) %>%
  dplyr::mutate(pixelT= sum(pixels)) %>%
  dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)
multDataFarmer$pixelsPerc[which(is.na(multDataFarmer$pixelsPerc))] <- 0
multDataFarmer$type <- "Multi-objective"
multDataFarmer$objs <- "GNB + TNB + GNS + Farmer"

data <- as.data.frame(rbind(singDataBees, multDataBees, singDataFarmer, multDataFarmer))
data$optim <- as.factor(data$optim)
data$Landclass_ExpOp_GFS <- as.factor(data$Landclass_ExpOp_GFS)
data$type <- as.factor(data$type)

data1 <- data %>%
  group_by(type, objs, rep, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(meanPerc=mean(pixelsPerc), landscapeNo=n())

data2 <- data1 %>%
  group_by(type, objs, Landclass_ExpOp_GFS) %>%
  dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo), se=weighted_se(meanPerc, landscapeNo), sd=sd(meanPerc))

data2 <- as.data.frame(data2)
# data2$type<- factor(data2$type, levels=c("Multi-obj: Bees", "Single-obj Mean: Bees", "Multi-obj: Bees + Farmer", "Single-obj Mean: Bees + Farmer"))
data2$Landclass_ExpOp_GFS <- factor(data2$Landclass_ExpOp_GFS)
# levels(data2$Landclass_ExpOp_GFS) <- c("B/FB", "C", "CW", "DW", "F", "IPG", "OSR", "UM")

#plot
# colors <- c("#920000", rgb(146, 0, 0, alpha=150,maxColorValue = 255),
#                      "#FFD54F", rgb(255, 213, 79, alpha=150, maxColorValue = 255),
#                      "#000000", rgb(0, 0, 0, alpha=150, maxColorValue = 255),
#                      "#1B5E20", rgb(27, 94, 32, alpha=150, maxColorValue = 255),
#                      "#924900", rgb(146, 73, 0, alpha=150, maxColorValue = 255),
#                      "#11C638", rgb(17, 198, 56, alpha=150, maxColorValue = 255),
#                      "#6DB6FF", rgb(109, 182, 255, alpha=150, maxColorValue = 255),
#                      "#FFB6DB", rgb(255, 182, 219, alpha=150, maxColorValue = 255))

# MultSingCompPlot <- ggplot(data2, aes(y=perc, x=Landclass_ExpOp_GFS, fill=interaction(type, Landclass_ExpOp_GFS))) +
#   geom_bar(stat="identity", position=position_dodge(), color="black",)+
#   geom_errorbar(aes(ymin=(perc-se), ymax=(perc+se)), width=.22, position=position_dodge(0.9), stat="identity") +
#   facet_wrap(~objs, ncol=1, strip.position="top", scales="free_x") +
#   labs(y="Mean % of Optimised Landscape", x="Landcover Type", fill="Optim Type") +
#   theme_bw() +
#   theme( axis.text.y = element_text(size=12),
#          axis.text.x = element_text(size=12),
#          axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
#          legend.title=element_text(size=12, face="bold"),
#          legend.position = "bottom",
#          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#          strip.text=element_text(face="bold", size=12),
#          legend.text=element_text(size=11)) +
#   scale_fill_manual(values = colors) +
#   ggtitle("")

colors <- c("#920000","#FFD54F","#000000","#1B5E20","#924900","#11C638","#6DB6FF","#FFB6DB")

MultSingCompPlot <- ggplot(data2, aes(y=perc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS, group=type)) +
    geom_col_pattern(
      aes(pattern = type),
      pattern_colour = "white",
      pattern_fill = "white",
      pattern_key_scale_factor = 0.5,
      pattern_angle = -45,
      pattern_spacing = 0.03,
      pattern_density = 0.05,
      position = position_dodge(),
      color = "black"
      ) +  
    geom_errorbar(
      aes(ymin=(perc-se), ymax=(perc+se)), width=.3,position=position_dodge(.9), 
      stat="identity"
      ) +
    facet_wrap(
      ~objs, ncol=1, strip.position="top", scales="free_x"
      ) +
    labs(
      y="Mean % of Optimised Landscape", x="Landcover Type", 
      fill="Landcover Type", pattern="Optimisation"
      ) +
    scale_pattern_manual(
      values = c("none", "stripe"),
      guide = guide_legend(override.aes = list(fill = "grey70"))
      ) +
    theme_bw(
      ) +
    theme(
      axis.text.y = element_text(size=12),
      # axis.text.x = element_text(size=12),
      axis.title.x = element_blank(),
      axis.text.x=element_blank(),
      axis.line = element_line(colour = "black", size=0.4), 
      axis.ticks.x = element_blank(),
      axis.title=element_text(size=14, face="bold"), 
      plot.title=element_text(size=16, face="bold"),
      legend.title=element_text(size=12, face="bold"),
      legend.position = "right",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      strip.text=element_text(face="bold", size=12),
      legend.text=element_text(size=11)
      ) +
    scale_fill_manual(
      values = colors, 
      guide = guide_legend(override.aes = list(pattern = "none"), ncol=1
      )
    )
  
plot(MultSingCompPlot)

ggsave("figures/multiSingCompPlot.png", dpi=600, width=21, height=27, units="cm")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics comparing multi-objective optims to mean of respective single-objective optims: just bees
#########################################################################################################

# create dataframe
pixelDF <- pixels
optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

singData <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB", "TNB", "GNS") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
singData1 <- singData %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
singData1$pixelsPerc[which(is.na(singData1$pixelsPerc))] <- 0
singData1$type <- "single"


multData <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB + TNB + GNS") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
multData1 <- multData %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
multData1$pixelsPerc[which(is.na(multData1$pixelsPerc))] <- 0
multData1$type <- "multi"

data <- as.data.frame(rbind(singData1, multData1))
data$optim <- as.factor(data$optim)
data$Landclass_ExpOp_GFS <- as.factor(data$Landclass_ExpOp_GFS)
data$type <- as.factor(data$type)

# summarise data
summary(data)

n <- data %>%
  group_by(optim, rep, type) %>%
  summarise(n=length(unique(landscape)))
n2 <- n %>%
  group_by(type) %>%
  summarise(no=sum(n))

# visualise spread
par(mfrow=c(1,3))
plot(pixelsPerc ~ type, data=data)
plot(pixelsPerc ~ Landclass_ExpOp_GFS, data=data)
hist(data$pixelsPerc)

# homogeneity of variances:
dataVarType<- data %>%
  group_by(type) %>%
  dplyr::summarise(variance=var(pixelsPerc))

dataVarLandcover <- data %>%
  group_by(Landclass_ExpOp_GFS) %>%
  summarise(variance=var(pixelsPerc))
# ratio of variances here may be very large (see linear models - going big handout)
# this is something to bear in mind when discussing and interpreting results of the linear model
# example of how to discuss this in the handout

# model
model <- aov(log(pixelsPerc+1) ~ type * Landclass_ExpOp_GFS, data=data)

# visually inspect data to check fro normality of residuals
par(mfrow=c(2,2))
plot(model)

# assess model
summary(model)
TukeyHSD(model)

# save model coefficients
TukeyDF <- tidy(TukeyHSD(model))
# write.csv(ANOVAdf, "cluster/results/multi_single_perc_compare_bees.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics comparing multi-objective optims to mean of respective single-objective optims: bees + farmer
#########################################################################################################

# create dataframe
pixelDF <- pixels
optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

singData <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB", "TNB", "GNS", "Farmer") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
singData1 <- singData %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
singData1$pixelsPerc[which(is.na(singData1$pixelsPerc))] <- 0
singData1$type <- "single"


multData <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB + TNB + GNS + Farmer") &radius==2000, select=c(optim, rep, landscape, Landclass_ExpOp_GFS, pixels))
multData1 <- multData %>%
  group_by(optim, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT)*100)
multData1$pixelsPerc[which(is.na(multData1$pixelsPerc))] <- 0
multData1$type <- "multi"

data <- as.data.frame(rbind(singData1, multData1))
data$optim <- as.factor(data$optim)
data$Landclass_ExpOp_GFS <- as.factor(data$Landclass_ExpOp_GFS)
data$type <- as.factor(data$type)

# summarise data
summary(data)

n <- data %>%
  group_by(optim, rep, type) %>%
  summarise(n=length(unique(landscape)))
n2 <- n %>%
  group_by(type) %>%
  summarise(no=sum(n))

# visualise spread
par(mfrow=c(1,3))
plot(pixelsPerc ~ type, data=data)
plot(pixelsPerc ~ Landclass_ExpOp_GFS, data=data)
hist(data$pixelsPerc)

# homogeneity of variances:
dataVarType<- data %>%
  group_by(type) %>%
  dplyr::summarise(variance=var(pixelsPerc))

dataVarLandcover <- data %>%
  group_by(Landclass_ExpOp_GFS) %>%
  summarise(variance=var(pixelsPerc))
# ratio of variances here may be very large (see linear models - going big handout)
# this is something to bear in mind when discussing and interpreting results of the linear model
# example of how to discuss this in the handout

# model
model <- lm(log(pixelsPerc+1) ~ type * Landclass_ExpOp_GFS, data=data)

# visually inspect data to check fro normality of residuals
par(mfrow=c(2,2))
plot(model)

# assess model
summary(model)
TukeyHSD(model)

# save model coefficients
TukeyDF <- tidy(TukeyHSD(model))
# write.csv(TukeyDF, "cluster/results/multi_single_perc_compare_farmer.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# violin plots of multi-objective optimisations
#########################################################################################################

# prepare data
pixelDF <- pixels

pixelDF$Code_ExpOp_GFS <- as.factor(pixelDF$Code_ExpOp_GFS)
pixelDF$optim <- as.factor(pixelDF$optim)
pixelDF$rep <- as.factor(pixelDF$rep)
pixelDF$pop <- as.factor(pixelDF$pop)
pixelDF$landscape <- as.factor(pixelDF$landscape)

optimLandcovers <- read.csv("data/optim_landcovers.csv")
optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                           by = "Code_ExpOp_GFS", copy=T)

data <- subset(pixelDF, pop %in% "final" & optim %in% c("GNB + TNB + GNS", "GNB + TNB + GNS + Farmer") & radius==2000, select=c(optim, radius, rep, landscape, Landclass_ExpOp_GFS, pixels))

# calculate pixel percentages
data1 <- data %>%
  group_by(radius, rep, landscape) %>%
  mutate(pixelT= sum(pixels)) %>%
  mutate(pixelsPerc=(pixels/pixelT))
data1$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

data1 <- as.data.frame(data1)

# plot
MultiViolin <- ggplot(data1, aes(y=(100*pixelsPerc), x=as.factor(optim), fill=as.factor(optim))) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  scale_fill_npg() +
  labs(x="Objectives", y= "Mean % of Optimised Landscape") +
  theme_bw() +
  theme(axis.text.y = element_text(size=10), 
        # axis.ticks.x=element_blank(), axis.text.x = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"),
        legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 

plot(MultiViolin)

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# parallel coordinate plots for multi-objective optims
#########################################################################################################

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
  theme( axis.text.y = element_text(size=19), 
         axis.line = element_line(colour = "black", size=0.4), 
         axis.title=element_text(size=21, face="bold"), 
         plot.title=element_text(size=23, face="bold"), 
         axis.text.x=element_text(size=20), 
         legend.title=element_blank(),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
         plot.margin = margin(10, 30, 10, 30),
         axis.ticks.length=unit(0.25, "cm"),
         ) +
  scale_x_discrete(expand = c(0.01, 0), labels=c("GNB", "TNB", "GNS")) +
  scale_y_continuous(expand = c(0.01, 0)) +
  ggtitle("B")

plot(beePCP)

# ggsave("figures/PCPBee.png", dpi=600, width=14, height=8, units="cm")


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
  theme( axis.text.y = element_text(size=19), 
         axis.line = element_line(colour = "black", size=0.4), 
         axis.title=element_text(size=21, face="bold"), 
         plot.title=element_text(size=23, face="bold"), 
         axis.text.x=element_text(size=20), 
         legend.title=element_blank(), 
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
         plot.margin = margin(10, 30, 10, 30),
         axis.ticks.length=unit(0.25, "cm")) +
  scale_x_discrete(expand = c(0.01, 0), labels=c("GNB", "TNB", "GNS","Farmer")) +
  scale_y_continuous(expand = c(0.01, 0)) +
  ggtitle("C")

# PCP <- grid.arrange(beePCP, farmerPCP, ncol=2)
plot(farmerPCP)
ggsave("figures/PCPFarmer.png", dpi=5000, width=14, height=8, units="cm")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# compare fitness scores for all multi-obj vs single-obj optims
#########################################################################################################
scores <- subset(fitnessScores, radius==2000 & optim %in% c("GNB", "TNB", "GNS", "FarmerOnly", "AllBees", "Farmer"))

# replace and order optim names
scores$optim[scores$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
scores$optim[scores$optim=="FarmerOnly"] <- "Farmer"
scores$optim[scores$optim=="AllBees"] <- "GNB + TNB + GNS"

# define optim types
scores$type <- NA
scores$type[scores$optim %in% c("GNB + TNB + GNS")] <- "Multi-objective: all bees"
scores$type[scores$optim %in% c("GNB + TNB + GNS + Farmer")] <- "Multi-objective: all bees incl. farmer"
scores$type[scores$optim %in% c("GNB", "TNB", "GNS", "Farmer")] <- "Single-objective"

# format data
scores$radius <- paste0(scores$radius, "m")
levels(scores$objective) <- c("GNB", "TNB", "GNS", "Farmer")

# summarise data
scoreMeans <- scores %>%
  group_by(optim, type, radius, rep, objective, gen) %>%
  dplyr::summarise(scoreAv=mean(score), sd=sd(score), se=std.error(score), n=n())

# weighted mean of all reps of each optim (may need to change std error/ dev calculations)
dataSummary <- scoreMeans %>%
  group_by(optim, type, radius, objective, gen) %>%
  dplyr::summarise(meanScore=weighted.mean(scoreAv, n), se=weighted_se(scoreAv, n), sd=sd(scoreAv))

dataSummary <- as.data.frame(dataSummary)
dataSummary$optim <- as.factor(dataSummary$optim)
dataSummary$radius <- as.factor(dataSummary$radius)
dataSummary$objective <- as.factor(dataSummary$objective)
dataSummary$type <- as.factor(dataSummary$type)

data100 <- subset(dataSummary, gen==100)

fitnessPlot <- ggplot(scores, aes(x=gen, y=-score, color=type)) +
  geom_smooth(stat="smooth", aes(color=type), se=T) +
  # geom_point(size=0.5, aes(color=type)) +
  facet_wrap(~objective, scales="free", ncol=4) +
  scale_color_nejm() +
  theme_bw() +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="top") +
  labs(color="Optim Type", x="Generation", y="Mean Objective Fitness") +
  ggtitle("A")

plot(fitnessPlot)
ggsave("../write_up/Figures/fitness.png", dpi=600, width=24, height=16, units="cm")


#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# plot patch area of landscapes
#########################################################################################################

results <- readRDS("results/results")
patchDensity <- results$patchDensity

patchDensity$pop <- factor(patchDensity$pop)
patchDensity$patchArea <- as.numeric(patchDensity$patchArea)

# replace and order optim names
patchDensity$optim[patchDensity$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
patchDensity$optim[patchDensity$optim=="FarmerOnly"] <- "Farmer"
patchDensity$optim[patchDensity$optim=="AllBees"] <- "GNB + TNB + GNS"

pd <- subset(patchDensity, radius==2000)

pd1 <- pd %>%
  group_by(optim, radius, pop) %>%
  dplyr::summarise(areaMean = mean(patchArea), se=std.error(patchArea))

pd2 <- subset(pd1, pop=="final" & optim %in% c("GNB + TNB + GNS", "GNB", "GNS", "TNB", "Farmer", "GNB + TNB + GNS + Farmer"))


pd2 <- as.data.frame(pd2)
pd2$optim <- factor(pd2$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))
pd2$areaMean <- as.numeric(pd2$areaMean)

PAPlot <- ggplot(pd2, aes(y=areaMean, x=optim)) + #, fill=optim)) +
  geom_bar(stat="identity", color="black", fill="grey") +
  geom_errorbar(aes(ymin=areaMean-se, ymax=areaMean+se), width=.22, position=position_dodge(.6), stat="identity") +
  # scale_fill_grey() +
  theme_bw() +
  coord_cartesian(ylim=c(6.5,6.93)) +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold"),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none") +
  labs(y="Mean Patch Area (Hectares)", x="Optimisation") +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) 

plot(PAPlot)
ggsave("figures/PAPlot.png", dpi=600, width=34, height=21, units="cm")

#########################################################################################################
# plot patch area of landscapes - only land covers that support bees
#########################################################################################################

results <- readRDS("results/results2")
patchDensity <- results$patchDensity

patchDensity$pop <- factor(patchDensity$pop)
patchDensity$patchArea <- as.numeric(patchDensity$patchArea)

# replace and order optim names
patchDensity$optim[patchDensity$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
patchDensity$optim[patchDensity$optim=="FarmerOnly"] <- "Farmer"
patchDensity$optim[patchDensity$optim=="AllBees"] <- "GNB + TNB + GNS"

pd <- subset(patchDensity, radius==2000)

pd1 <- pd %>%
  group_by(optim, radius, pop) %>%
  dplyr::summarise(areaMean = mean(patchArea), se=std.error(patchArea))

pd2 <- subset(pd1, pop=="final" & optim %in% c("GNB + TNB + GNS", "GNB", "GNS", "TNB", "Farmer", "GNB + TNB + GNS + Farmer") & Landclass_ExpOp_GFS %in% c())


pd2 <- as.data.frame(pd2)
pd2$optim <- factor(pd2$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer"))
pd2$areaMean <- as.numeric(pd2$areaMean)

PAPlot <- ggplot(pd2, aes(y=areaMean, x=optim, fill=optim)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=areaMean-se, ymax=areaMean+se), width=.22, position=position_dodge(.6), stat="identity") +
  scale_fill_npg() +
  coord_cartesian(ylim=c(6.5,6.93)) +
  theme(axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12), 
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold"),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="none") +
  labs(y="Mean Patch Area (Hectares)", x="Optimisation")

plot(PAPlot)
ggsave("../write_up/Figures/PAPlot.png", dpi=600, width=34, height=21, units="cm")
#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# statistics to compare patch area of landscapes for each optim at 2000m
#########################################################################################################
 
# prepare data
results <- readRDS("results/results")
patchDensity <- results$patchDensity

patchDensity$pop <- factor(patchDensity$pop)
patchDensity$patchArea <- as.numeric(patchDensity$patchArea)

# replace and order optim names
patchDensity$optim[patchDensity$optim=="Farmer"] <- "GNB + TNB + GNS + Farmer"
patchDensity$optim[patchDensity$optim=="FarmerOnly"] <- "Farmer"
patchDensity$optim[patchDensity$optim=="AllBees"] <- "GNB + TNB + GNS"

pd <- subset(patchDensity, radius==2000 & pop=="final" & optim %in% c("GNB + TNB + GNS", "GNB", "GNS", "TNB", "Farmer", "GNB + TNB + GNS + Farmer"))

pd$optim <- factor(pd$optim)

# summarise data
summary(pd)

n <- pd %>%
  group_by(optim, rep) %>%
  summarise(n=length(unique(landscape)))
n2 <- n %>%
  group_by(optim) %>%
  summarise(no=sum(n))

# visualise spread
par(mfrow=c(1,2))
plot(patchArea ~ optim, data=pd)
hist(pd$patchArea)

# homogeneity of variances:
dataVarOptim<- pd %>%
  group_by(optim) %>%
  dplyr::summarise(variance=var(patchArea))

# model
model <- lm(patchArea ~ optim, data=pd)

# visually inspect data to check fro normality of residuals
par(mfrow=c(2,2))
plot(model)

# assess model
summary(model)
TukeyHSD(aov(model))

# save model coefficients
AnovaDF<- tidy(aov(model))
TukeyDF <- tidy(TukeyHSD(aov(model)))
# write.csv(TukeyDF, "cluster/results/patch_area_Tukey.csv")

#--------------------------------------------------------------------------------------------------------
#########################################################################################################
# plot nesting/ floral scores for each landcover type for each guild (plus means)
#########################################################################################################
optimLandcovers <- read.csv("data/optim_landcovers.csv")
landcoverScores <- read.csv("data/landcover_scores.csv")
landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS, select=c(Species_name, Landclass_ExpOp_GFS, Flor_P1_b, Nest_per_ha))  
landcoverWrang <- melt(landcoverScores, ID=c(Species_name, Landclass_ExpOp_GFS), variable.name="scoreType", value.name="Score")
landcoverWrang$scoreType <- factor(landcoverWrang$scoreType)
levels(landcoverWrang$scoreType) <- c("Floral", "Nesting")
landcoverWrang$Species_name <- factor(landcoverWrang$Species_name)
levels(landcoverWrang$Species_name) <- c("GNB", "GNS", "TNB")

landcoverWrang2 <- landcoverWrang %>%
  group_by(Landclass_ExpOp_GFS, scoreType) %>%
  dplyr::summarise(Score=mean(Score))

landcoverWrang2$Species_name <- "Mean"

data <- rbind(landcoverWrang, landcoverWrang2)

colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")
#(Beans, Cereal, Coniferous woodland, Deciduous woodland, Fallow, Improved perm grasslands, Oilseed rape, Unimproved meadow)


plot <- ggplot(subset(data, !(Species_name %in% "Mean")), aes(x=Landclass_ExpOp_GFS, y=Score, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(width=0), stat = "identity", color="black", size=0.4, width=0.8) + #,alpha=.8) +
  facet_grid(rows=vars(scoreType), cols=vars(Species_name), scales="free", switch="y") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
        # axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_text(size=10), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
        strip.text.y = element_text(size = 10, color = "black", face = "bold"), 
        strip.text=element_text(face="bold"),
        legend.title=element_text(size=11, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="none") +
  labs(fill="Landcover Type", y="Score (Arbitrary Units)") 
  # ggtitle("C")

plot(plot)
ggsave("figures/NF_scores.png", dpi=600, width=21, height=12, units="cm")
