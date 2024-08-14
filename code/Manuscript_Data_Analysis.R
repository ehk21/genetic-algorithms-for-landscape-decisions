rm(list=ls())

# load packages ---- 

library(diagis)
library(broom)
library(dplyr)
library(reshape2)

# load data ----

results <- readRDS("results/results")
populations <- results$populations
fitnessScores <- results$fitnessScores
pixels <- results$pixels
nfScores <- results$floralNestingScores
patchDensity <- results$patchDensity
optimLandcovers <- read.csv("data/optim_landcovers.csv")
landcoverScores <- read.csv("data/landcover_scores.csv")


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

# ANOVA comparing % pixels of each landcover between single-obj optims  ----

# prepare data
pixelDF <- pixels

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


# ANOVA comparing % pixels of each landcover of multi-obj optims  ----

# prepare data
pixelDF <- pixels

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

# linear model
model <- aov(sqrt(pixelsPerc) ~ optim * Landclass_ExpOp_GFS, data=data1)

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

# Composition vs Poll4pop score correlations ----

# prepare pixel % data
pixelDF <- pixels

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


# prepare score data 
# floral score = floral attractiveness*(floral cover early spring + floral cover late spring + floral cover summer)
landcoverScores$floral <- landcoverScores$Flor_P1_b*(landcoverScores$Flor_Cov_P1_b + landcoverScores$Flor_Cov_P2_b + landcoverScores$Flor_Cov_P3_b)
landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS, select=c(Species_name, Landclass_ExpOp_GFS, floral, Nest_P1_b))  

landcoverScores$Species_name <- factor(landcoverScores$Species_name, levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees"))
levels(landcoverScores$Species_name) <- c("GNB", "TNB", "GNS")

names(landcoverScores)<- c("Guild","Landclass_ExpOp_GFS", "Floral", "Nesting" )

landcoverScores$Landclass_ExpOp_GFS<- factor(landcoverScores$Landclass_ExpOp_GFS)
levels(landcoverScores$Landclass_ExpOp_GFS)<- c("Broad/Field Beans", "Cereal", "Coniferous Woodland", "Deciduous Woodland", "Fallow", "Improved Permanent Grasslands", "Oilseed Rape", "Unimproved Meadow")

# combine data sets 
data <- left_join(pixelSummary1, landcoverScores, by=c("Guild", "Landclass_ExpOp_GFS"))

data<-as.data.frame(data)


# correlation tests 

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

# ANOVA to compare patch area of landscapes for each optim ----

# prepare data
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

# model
model <- aov(patchArea ~ optim, data=pd)

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

# nesting/ floral scores

