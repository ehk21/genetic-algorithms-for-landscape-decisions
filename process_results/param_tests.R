# plot results of preliminary parameter tests

rm(list=ls())


library(stats)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(viridis)
library(ggsci)

SK86Fitness <- read.csv("data/SK86/original_landscape_fitness_scores.csv")
parameters <- read.csv("data/params.csv", header=TRUE)

################################################################################
################################################################################
# plot curves comparing popsize at each number of gens on same axes:
################################################################################
################################################################################

parameters <- read.csv("data/params.csv", header=TRUE)
df <- data.frame(matrix(ncol=7, nrow=0, dimnames=list(NULL, c("radius", "popSize", "maxGens", "gen", "GNB", "TNB", "GNS"))))

for (i in which(parameters$plot, isTRUE(parameters$plot))){
  optim <- readRDS(paste0("results/optimisations/param_tests/output", i))
  scores <- optim[["optim"]][["scores"]]
  df <- rbind(df, data.frame("radius"=parameters$radius[i], "popSize"=parameters$popSize[i], "maxGens"=paste0(parameters$generations[i], " gens"), "gen"=scores$gen, "GNB"=scores$GroundNestingBumblebees, "TNB"=scores$TreeNestingBumblebees, "GNS"=scores$GroundNestingSolitaryBees))
}

df <- melt(df, id=c("radius", "maxGens", "popSize", "gen"), variable.name = "guild", value.name = "score")

df$popSize <- factor(df$popSize)
df$maxGens <- factor(df$maxGens, levels=c("50 gens", "75 gens", "100 gens", "150 gens", "200 gens"))

#GNB
GNB500m <- ggplot(subset(df, radius==500 & guild=="GNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting bumblebees - 500m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNB" & df$radius==500])), max(-(df$score[df$guild=="GNB" & df$radius==500]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens) +
  theme_bw() +
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(GNB500m)
ggsave("../write_up/Figures/prelim_GNB500.png", dpi=600, width=30, height=16, units="cm")
  

GNB1000m <- ggplot(subset(df, radius==1000 & guild=="GNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting bumblebees - 1000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNB" & df$radius==1000])), max(-(df$score[df$guild=="GNB" & df$radius==1000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens) +
  theme_bw() +
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(GNB1000m)
ggsave("../write_up/Figures/prelim_GNB1000.png", dpi=600, width=30, height=16, units="cm")

GNB2000m <- ggplot(subset(df, radius==2000 & guild=="GNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting bumblebees - 2000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNB" & df$radius==2000])), max(-(df$score[df$guild=="GNB" & df$radius==2000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens, ncol=5) +
  theme_bw() + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(GNB2000m)
ggsave("../write_up/Figures/prelim_GNB2000.png", dpi=600, width=30, height=16, units="cm")



#TNB
TNB500m <- ggplot(subset(df, radius==500 & guild=="TNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Tree nesting bumblebees - 500m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="TNB" & df$radius==500])), max(-(df$score[df$guild=="TNB" & df$radius==500]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens) +
  theme_bw()  + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(TNB500m)
ggsave("../write_up/Figures/prelim_TNB500.png", dpi=600, width=30, height=16, units="cm")

TNB1000m <- ggplot(subset(df, radius==1000 & guild=="TNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Tree nesting bumblebees - 1000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="TNB" & df$radius==1000])), max(-(df$score[df$guild=="TNB" & df$radius==1000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens) +
  theme_bw()  + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(TNB1000m)
ggsave("../write_up/Figures/prelim_TNB1000.png", dpi=600, width=30, height=16, units="cm")

TNB2000m <- ggplot(subset(df, radius==2000 & guild=="TNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Tree nesting bumblebees - 2000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="TNB" & df$radius==2000])), max(-(df$score[df$guild=="TNB" & df$radius==2000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens, ncol=5) +
  theme_bw() +
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(TNB2000m)
ggsave("../write_up/Figures/prelim_TNB2000.png", dpi=600, width=30, height=16, units="cm")

TNB <- grid.arrange(TNB500m, TNB1000m, TNB2000m, top="Tree nesting bumblebees")


#GNS
GNS500m <- ggplot(subset(df, radius==500 & guild=="GNS"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting solitary bees - 500m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNS" & df$radius==500])), max(-(df$score[df$guild=="GNS" & df$radius==500]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens) +
  theme_bw()  + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(GNS500m)
ggsave("../write_up/Figures/prelim_GNS500.png", dpi=600, width=30, height=16, units="cm")

GNS1000m <- ggplot(subset(df, radius==1000 & guild=="GNS"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting solitary bees - 1000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNS" & df$radius==1000])), max(-(df$score[df$guild=="GNS" & df$radius==1000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens) +
  theme_bw()  + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(GNS1000m)
ggsave("../write_up/Figures/prelim_GNS1000.png", dpi=600, width=30, height=16, units="cm")

GNS2000m <- ggplot(subset(df, radius==2000 & guild=="GNS"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting solitary bees - 2000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNS" & df$radius==2000])), max(-(df$score[df$guild=="GNS" & df$radius==2000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens, ncol=5) +
  theme_bw()  + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12), 
        # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=12, face="bold"),
        legend.text=element_text(size=11),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  scale_color_nejm() 

plot(GNS2000m)
ggsave("../write_up/Figures/prelim_GNS2000.png", dpi=600, width=30, height=16, units="cm")

GNS <- grid.arrange(GNS500m, GNS1000m, GNS2000m, top="Ground nesting solitary bees")

pdf("results/optimisations/scores_compiled", width=16, height=20)
plot(GNB)
plot(TNB)
plot(GNS)
dev.off()


################################################################################
################################################################################
# plot max scores for each guild - all popsizes/ gens on same axes
################################################################################
################################################################################

parameters <- read.csv("data/params.csv", header=TRUE)

df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, c("radius", "popSize", "gens", "GNB_max", "TNB_max", "GNS_max"))))

for (i in which(parameters$plot, isTRUE(parameters$plot))){
  optim <- readRDS(paste0("results/optimisations/output", i))
  scores <- (optim[["optim"]][["scores"]])
  scores[,2:4] <- -scores[,2:4]
  df <- rbind(df, c("radius"=paste0(parameters$radius[i],"m"), "popSize"=parameters$popSize[i], "gens"=parameters$generations[i], "GNB_max"=max(scores$GroundNestingBumblebees), "TNB_max"=max(scores$TreeNestingBumblebees), "GNS_max"=max(scores$GroundNestingSolitaryBees)))
}

colnames(df)<-c("radius", "popSize", "gens", "GNB_max", "TNB_max", "GNS_max")

df <- melt(df, id=c("radius", "gens", "popSize"), variable.name = "guild", value.name = "maxScore")

df$popSize <- factor(df$popSize, levels=c("50","75","100"))
df$gens <- factor(df$gens, levels=c("50","75","100"))
df$radius <- factor(df$radius, levels=c("500m", "1000m", "2000m"))
df$maxScore <- as.numeric(df$maxScore)


GNBMax <- ggplot(subset(df, guild=="GNB_max"), aes(x=gens, y=maxScore, color=popSize)) +
  geom_bar(aes(fill=popSize),stat="identity", position=position_dodge(), width=0.5) +
  facet_wrap(~radius, ncol=3) +
  ggtitle("GNB") +
  scale_color_brewer(palette="Spectral") +
  theme_bw()

GNSMax <- ggplot(subset(df, guild=="GNS_max"), aes(x=gens, y=maxScore, color=popSize)) +
  geom_bar(aes(fill=popSize),stat="identity", position=position_dodge(), width=0.5) +
  facet_wrap(~radius, ncol=3) +
  ggtitle("GNS") +
  scale_color_brewer(palette="Spectral") +
  theme_bw()

TNBMax <- ggplot(subset(df, guild=="TNB_max"), aes(x=gens, y=maxScore, color=popSize)) +
  geom_bar(aes(fill=popSize),stat="identity", position=position_dodge(), width=0.5) +
  facet_wrap(~radius, ncol=3) +
  ggtitle("TNB") +
  scale_color_brewer(palette="Spectral") +
  theme_bw()

pdf("results/optimisations/max_fitness_plots", width=12, height=9)
plot(GNBMax)
plot(TNBMax)
plot(GNSMax)
dev.off()


################################################################################
################################################################################
# plot max scores of cprob tests
################################################################################
################################################################################

df <- data.frame(matrix(ncol=4, nrow=0, dimnames=list(NULL, c("cprob", "GNB_max", "TNB_max", "GNS_max"))))

for (i in 1:10){
  optim <- readRDS(paste0("results/optimisations/param_tests/cprob", i))
  scores <- (optim[["optim"]][["scores"]])
  scores[,2:4] <- -scores[,2:4]
  df <- rbind(df, c("cprob"=i/10, "GNB"=max(scores$GroundNestingBumblebees), "TNB"=max(scores$TreeNestingBumblebees), "GNS"=max(scores$GroundNestingSolitaryBees)))
}

colnames(df)<-c("cprob", "GNB", "TNB", "GNS")

df <- melt(df, id="cprob", variable.name = "guild", value.name = "maxScore")

df$cprob <- as.factor(df$cprob)


ggplot(df, aes(x=cprob, y=maxScore, fill=guild)) +
  geom_bar(aes(fill=cprob),stat="identity", position=position_dodge(), width=0.5) +
  # scale_color_brewer(palette="Spectral") +
  theme_bw() +
  facet_wrap(~guild, scales="free_y") +
  labs(x="Crossover Probability", y="Maximum Fitness Score (Visitation Rate)") +
  theme(legend.position="none") +
  scale_fill_npg()

ggsave("../write_up/Figures/prelim_Xover.png", dpi=600, width=24, height=16, units="cm")
