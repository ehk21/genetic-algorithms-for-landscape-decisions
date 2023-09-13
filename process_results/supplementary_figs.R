rm(list=ls())

# load packages
library(stats)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(viridis)
library(ggsci)

# plot results of preliminary parameter tests ----

# load data
parameters <- read.csv("data/params.csv", header=TRUE)
parameters <- subset(parameters, popSize>10 & generations<150)

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

GNB2000m <- ggplot(subset(df, radius==2000 & guild=="GNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting bumblebees - 2000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNB" & df$radius==2000])), max(-(df$score[df$guild=="GNB" & df$radius==2000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens, ncol=5) +
  theme_bw() + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12.5), 
        axis.text.y = element_text(size=12.5), 
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=12.5, face="bold"), 
        plot.title=element_text(size=15, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=15),
        legend.title=element_text(size=12.5, face="bold"),
        legend.text=element_text(size=12.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  scale_color_nejm() 


pdf("figures/prelim_GNB.pdf",    
    height = (12/2.54), 
    width = (30/2.54))

plot(GNB2000m)

dev.off()

#TNB
TNB2000m <- ggplot(subset(df, radius==2000 & guild=="TNB"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Tree nesting bumblebees - 2000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="TNB" & df$radius==2000])), max(-(df$score[df$guild=="TNB" & df$radius==2000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens, ncol=5) +
  theme_bw() +
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12.5), 
        axis.text.y = element_text(size=12.5), 
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=12.5, face="bold"), 
        plot.title=element_text(size=15, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=15),
        legend.title=element_text(size=12.5, face="bold"),
        legend.text=element_text(size=12.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  scale_color_nejm() 

pdf("figures/prelim_TNB.pdf",    
    height = (12/2.54), 
    width = (30/2.54))

plot(TNB2000m)

dev.off()


#GNS
GNS2000m <- ggplot(subset(df, radius==2000 & guild=="GNS"), aes(x=gen, y=-score, color=popSize)) +
  ggtitle("Ground nesting solitary bees - 2000m") +
  geom_point(aes(colour=popSize), size=0.5) +
  ylim(min(-(df$score[df$guild=="GNS" & df$radius==2000])), max(-(df$score[df$guild=="GNS" & df$radius==2000]))) +
  geom_smooth(aes(color=popSize), stat="smooth", se=F) +
  facet_wrap(~maxGens, ncol=5) +
  theme_bw()  + 
  labs(x="Generation", y="Objective Fitness Score (Visitation Rate)", color="Population Size") +
  theme(axis.text.x = element_text(size=12.5), 
        axis.text.y = element_text(size=12.5), 
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=12.5, face="bold"), 
        plot.title=element_text(size=15, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=15),
        legend.title=element_text(size=12.5, face="bold"),
        legend.text=element_text(size=12.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  scale_color_nejm() 


pdf("figures/prelim_GNS.pdf",    
    height = (12/2.54), 
    width = (30/2.54))

plot(GNS2000m)

dev.off()

# plot fitness of objectives across generations of optimisations ----

# read in data
results <- readRDS("results/results")
fitnessScores <- results$fitnessScores
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


pdf("figures/fitness.pdf",    
    height = (15/2.54), 
    width = (30/2.54))

ggplot(scores, aes(x=gen, y=-score, color=type)) +
  geom_smooth(stat="smooth", aes(color=type), se=T) +
  # geom_point(size=0.5, aes(color=type)) +
  facet_wrap(~objective, scales="free", ncol=4) +
  scale_color_npg() +
  theme_bw() +
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=10), 
        axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black", size=0.4), 
        axis.title=element_text(size=10, face="bold"), 
        plot.title=element_text(size=12, face="bold"),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
        strip.text= element_text(face="bold", size=12),
        legend.title=element_text(size=10, face="bold"),
        legend.text=element_text(size=10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        legend.position="bottom") +
  labs(color="Optim Type", x="Generation", y="Mean Objective Fitness") 

dev.off()





