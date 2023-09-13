rm(list=ls())

# load packages
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(ggplot2)
library(ggprism)

# read in data
objectiveScoresOptim <- read.csv("results/objective_scores_optimised_region.csv")
objectiveScoresOptim[,2:5]<-objectiveScoresOptim[,2:5]*(-1)

# format data

objectiveScoresOptim$optim[objectiveScoresOptim$optim=="AllBees"]<-"GNB + TNB + GNS"
objectiveScoresOptim$optim[objectiveScoresOptim$optim=="Farmer"]<-"GNB + TNB + GNS + Farmer"
objectiveScoresOptim$optim[objectiveScoresOptim$optim=="FarmerOnly"]<-"Farmer"

unique(objectiveScoresOptim$optim)

objectiveScoresOptim$optim <- factor(objectiveScoresOptim$optim, levels=c("GNB", "TNB", "GNS", "Farmer", "GNB + TNB + GNS", "GNB + TNB + GNS + Farmer", "SK86"))

# convert from wide to long
scoresLongOptim <- melt(objectiveScoresOptim, id.vars=c("optim", "rep", "landscape"), measure.vars=c("gnb", "gns", "tnb", "farmer"), variable.name="objective", value.name="score")


# normalise scores (by dividing by original SK86 score for each objective)
sk86ScoresOptim <- subset(scoresLongOptim, optim=="SK86")

scoresLongOptim$normScore <- NA
scoresLongOptim$logNormScore <- NA

for(i in 1:nrow(scoresLongOptim)){
  scoresLongOptim$normScore[i] <- scoresLongOptim$score[i]/sk86ScoresOptim$score[which(sk86ScoresOptim$objective==scoresLongOptim$objective[i])]
  scoresLongOptim$logNormScore[i] <- log(scoresLongOptim$normScore[i])
}

# calculate mean objective score for each objective in each type of optimisation
meanScoresOptim <- scoresLongOptim %>%
  group_by(optim, objective) %>%
  dplyr::summarise(scoreAv=mean(score), normScoreAv=mean(normScore), scoreSE=std.error(score), normScoreSE=std.error(normScore))

meanScoresOptim<-as.data.frame(meanScoresOptim)

sk86MeansOptim<-subset(meanScoresOptim, optim=="SK86")

levels(meanScoresOptim$objective) <- c("GNB", "GNS", "TNB", "Farmer")
levels(sk86MeansOptim$objective) <- c("GNB", "GNS", "TNB", "Farmer")

# plot un-normalised scores faceted by objective
ggplot(subset(meanScoresOptim, !(optim %in% "SK86")), aes(x=optim, y=scoreAv)) +
  geom_point(
    size=2
  ) +
  facet_wrap(
    ~objective, scales="free", nrow=1
  ) +
  geom_hline(
    data=sk86MeansOptim, linetype="dashed", aes(yintercept=scoreAv, color="red")
  ) +                                # Modify labels of ggplot2 barplot
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  geom_errorbar(
    aes(ymin=scoreAv-scoreSE, ymax=scoreAv+scoreSE), width=.1,
    position=position_dodge(0.05)
  ) +
  labs(
    x="Optimisation", y="Mean fitness after optimisation"
  ) +
  ggtitle(
    "Optimised region")

# plot normalised scores
ggplot(subset(meanScoresOptim, !(optim %in% "SK86")),aes(x=optim, y=normScoreAv, color=objective)) +
  geom_point(
    size=5, aes(shape=objective)
  ) +
  # geom_errorbar(
  #   aes(ymin=normScoreAv-normScoreSE, ymax=normScoreAv+normScoreSE), width=.1,
  # ) +
  labs(
    y="mean fitness after optimisation / real landscape fitness", x= "Optimisation", color="Objective", shape="Objective"
  ) +
  geom_hline(
    yintercept=1, linetype="dashed", color = "red") +
  annotate("text", x=6.3, y=1.06, label="Original score", size=2.5, color="red"
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
    legend.text=element_text(size=11)
  ) +
  ggtitle(
    "(optimised region)"
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  scale_y_continuous(n.breaks=6) 


# plot normalised scores faceted by objective
ggplot(subset(meanScoresOptim, !(optim %in% "SK86")), aes(x=optim, y=normScoreAv, color=objective)) +
  geom_point(
  ) +
  facet_wrap(
    ~objective, scales="free"
  ) +
  geom_errorbar(
    aes(ymin=normScoreAv-normScoreSE, ymax=normScoreAv+normScoreSE), width=.1,
  ) +
  geom_hline(
    yintercept=1,linetype="dashed", color = "red"
  ) +
  labs(
    y="Mean fitness after optimisation / real landscape fitness", x= "Optimisation", color="Objective"
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  ggtitle(
    "(optimised region)"
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
    legend.text=element_text(size=11)
  ) 

# log normalised scores and plot
meanScoresOptim$logScore<-log(meanScoresOptim$normScoreAv)

ggplot(subset(meanScoresOptim, !(optim %in% "SK86")),aes(x=optim, y=logScore, color=objective)) +
  geom_point(
    size=5, aes(shape=objective)
  ) +
  labs(
    y="Log (mean fitness after optimisation / real landscape fitness)", x= "Optimisation", color="Objective", shape="Objective"
  ) +
  geom_hline(
    yintercept=0, linetype="dashed", color = "red") +
  annotate("text", x=6, y=0.03, label="Original score", size=3, color="red"
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
    legend.text=element_text(size=11)
  ) +
  scale_x_discrete(
    labels = function(x) str_wrap(x, width = 14), guide = guide_axis(angle = -45)
  ) +
  ggtitle(
    "(optimised region)"
  )






