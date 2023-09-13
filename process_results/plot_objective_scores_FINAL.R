rm(list=ls())

# load packages
library(dplyr)
library(reshape2)
library(plotrix)
library(stringr)
library(ggplot2)
library(ggprism)

# prepare data ----
# read in data
objectiveScores <- read.csv("results/objective_scores.csv")
objectiveScoresOptim <- read.csv("results/objective_scores_optimised_region.csv")

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

# calculate mean and SE for each objective in each type of optimisation
meanScores <- scoresLong %>%
  group_by(optim, objective) %>%
  dplyr::summarise(scoreAv=mean(score), scoreSE=sd(score)) # !!!

meanScoresOptim <- scoresLongOptim %>%
  group_by(optim, objective) %>%
  dplyr::summarise(scoreAv=mean(score), scoreSE=sd(score)) # !!!

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

# # normalise scores by dividing by original landscape scores and propagate error
# meanScores$normScore <- NA
# meanScores$normSE <- NA
# meanScoresOptim$normScore <- NA
# meanScoresOptim$normSE <- NA
# 
# for(i in 1:nrow(meanScores)){
#   meanScores$normScore[i]<-meanScores$scoreAv[i]/sk86Means$scoreAv[sk86Means$objective==meanScores$objective[i]]
#   meanScores$normSE[i]<- meanScores$scoreSE[i]/sk86Means$scoreAv[sk86Means$objective==meanScores$objective[i]]
#   meanScoresOptim$normScore[i]<-meanScoresOptim$scoreAv[i]/sk86MeansOptim$scoreAv[sk86MeansOptim$objective==meanScoresOptim$objective[i]]
#   meanScoresOptim$normSE[i]<-  meanScoresOptim$scoreSE[i]/sk86MeansOptim$scoreAv[sk86MeansOptim$objective==meanScoresOptim$objective[i]]
#   }
# 
# # log scores to aid visualisation
# meanScores$logScore <- NA
# meanScores$logSE <- NA
# meanScoresOptim$logScore <- NA
# meanScoresOptim$logSE <- NA
# 
# for(i in 1:nrow(meanScores)){
#   meanScores$logScore[i]<-log(meanScores$normScore[i])
#   meanScores$logSE[i]<-log(meanScores$normScore[i] + meanScores$normSE[i]) - log(meanScores$normScore[i])
#   meanScoresOptim$logScore[i]<-log(meanScoresOptim$normScore[i])
#   meanScoresOptim$logSE[i]<-log(meanScoresOptim$normScore[i] + meanScoresOptim$normSE[i]) - log(meanScoresOptim$normScore[i])
#   }

# normalise scores using Emma's equation:

meanScores$normScore <- NA
meanScores$normSE <- NA
meanScoresOptim$normScore <- NA
meanScoresOptim$normSE <- NA

for(i in 1:nrow(meanScores)){
  meanScores$normScore[i]<-log10(meanScores$scoreAv[i]/sk86Means$scoreAv[sk86Means$objective==meanScores$objective[i]])
  meanScores$normSE[i]<- meanScores$scoreSE[i]/(meanScores$scoreAv[i]*log(10))
  meanScoresOptim$normScore[i]<-log10(meanScoresOptim$scoreAv[i]/sk86MeansOptim$scoreAv[sk86MeansOptim$objective==meanScoresOptim$objective[i]])
  meanScoresOptim$normSE[i]<-  meanScoresOptim$scoreSE[i]/(meanScoresOptim$scoreAv[i]*log(10))
  }


# combine dataframes
meanScores$area <- "Whole grid square"
meanScoresOptim$area <- "Optimised region"

scoresDF <- rbind(meanScores, meanScoresOptim)

scoresDF$area <- factor(scoresDF$area, levels=c("Whole grid square", "Optimised region"))

# plot data ----

# plot normalised scores
ggplot(scoresDF, aes(x=optim, y=normScore, color=objective)) +
  geom_point(
    size=5, aes(shape=objective)
  ) +
  geom_errorbar(
    aes(ymin=normScore-normSE, ymax=normScore+normSE), width=.3,
    ) +
  facet_wrap(~area, scales="free") + 
  labs(
    y="Log (mean fitness of end-user in optimised landscape / fitness in real landscape)", x= "Objective(s) of optimisation", color="Landscape \n end-user", shape="Landscape \n end-user"
  ) +
  geom_hline(
    yintercept=0, linetype="dashed", color = "red") +
  annotate("text", x=6.3, y=1.06, label="Original score", size=2.5, color="white"
  ) +
  theme_bw(
  ) +
  theme(
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12),
    axis.line = element_line(colour = "black", size=0.4),
    axis.title=element_text(size=14, face="bold"),
    plot.title=element_text(size=16, face="bold"),
    legend.title=element_text(size=12, face="bold"),
    legend.position = "right",
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.text=element_text(size=11),
    strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
    strip.text= element_text(face="bold", size=12),
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  scale_y_continuous(n.breaks=6) 



# plot log(normalised scores)
ggplot(scoresDF, aes(x=optim, y=logScore, color=objective)) +
  geom_point(
    size=5, aes(shape=objective)
  ) +
  geom_errorbar(
    aes(ymin=logScore-logSE, ymax=logScore+logSE), width=.3,
  ) +
  facet_wrap(~area, scales="free") + 
  labs(
    y="Log(mean fitness after optimisation / real landscape fitness)", x= "Objectives of optimisation", color="Objective", shape="Objective"
  ) +
  geom_hline(aes(
    yintercept=0, linetype="Original score"), color = "red"
      ) +
  scale_linetype_manual(
    name = "", 
    values = c(2, 4), 
    guide = guide_legend(override.aes = list(color = c("red")))
    ) +
  # annotate(
  #   "text", x=6.3, y=0.06, label="Original score", size=2.5, color="red"
  # ) +
  theme_bw(
  ) +
  theme(
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12),
    axis.line = element_line(colour = "black", size=0.4),
    axis.title=element_text(size=14, face="bold"),
    plot.title=element_text(size=16, face="bold"),
    legend.title=element_text(size=12, face="bold"),
    legend.position = "right",
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.text=element_text(size=11),
    strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
    strip.text= element_text(face="bold", size=12),
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  scale_y_continuous(n.breaks=6) 

# 

# analyse data ----

