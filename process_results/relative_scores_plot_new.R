rm(list=ls())

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

# optimLandcovers <- read.csv("cluster/data/optim_landcovers.csv")
# landcoverScores <- read.csv("cluster/data/landcover_scores.csv")

# landcoverScores$floral_early_spring <- landcoverScores$Flor_P1_b*landcoverScores$Flor_Cov_P1_b
# landcoverScores$floral_late_spring <- landcoverScores$Flor_P1_b*landcoverScores$Flor_Cov_P2_b
# landcoverScores$floral_summer <- landcoverScores$Flor_P1_b*landcoverScores$Flor_Cov_P3_b
# landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS, select=c(Species_name, Landclass_ExpOp_GFS, floral_early_spring, floral_late_spring, floral_summer, Nest_per_ha))  
# 
# landcoverWrang <- melt(landcoverScores, ID=c(Species_name, Landclass_ExpOp_GFS), variable.name="scoreType", value.name="Score")
# landcoverWrang$scoreType <- factor(landcoverWrang$scoreType)
# levels(landcoverWrang$scoreType) <- c("Floral Early Spring", "Floral Late Spring", "Floral Summer", "Nesting")
# landcoverWrang$Species_name <- factor(landcoverWrang$Species_name)
# levels(landcoverWrang$Species_name) <- c("GNB", "GNS", "TNB")
# 
# # landcoverWrang2 <- landcoverWrang %>%
# #   group_by(Landclass_ExpOp_GFS, scoreType) %>%
# #   dplyr::summarise(Score=mean(Score))
# # 
# # landcoverWrang2$Species_name <- "Mean"
# 
# # data <- rbind(landcoverWrang, landcoverWrang2)
# 
# colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")
# #(Beans, Cereal, Coniferous woodland, Deciduous woodland, Fallow, Improved perm grasslands, Oilseed rape, Unimproved meadow)
# 
# 
# plot <- ggplot(landcoverWrang, aes(x=Landclass_ExpOp_GFS, y=Score, fill=Landclass_ExpOp_GFS)) +
#   geom_bar(position=position_dodge(width=0), stat = "identity", color="black", size=0.4, width=0.8 ,alpha=.8) +
#   facet_grid(rows=vars(scoreType), cols=vars(Species_name), scales="free", switch="y") +
#   scale_fill_manual(values = colors) +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
#         # axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
#         axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
#         axis.title.y=element_text(size=10), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
#         strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
#         strip.text.y = element_text(size = 10, color = "black", face = "bold"), 
#         strip.text=element_text(face="bold"),
#         legend.title=element_text(size=11, face="bold"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
#         legend.position="bottom") +
#   labs(fill="Landcover Type", y="Score (Arbitrary Units)") +
#   ggtitle("C")
# 
# plot(plot)
# ggsave("cluster/figures/NF_scores.png", dpi=600, width=21, height=12, units="cm")


##################################
# add floral cover for each season then multiply by attractiveness...
##################################
rm(list=ls())

optimLandcovers <- read.csv("data/optim_landcovers.csv")
landcoverScores <- read.csv("data/landcover_scores.csv")

# floral score = floral attractiveness*(floral cover early spring + floral cover late spring + floral cover summer)
landcoverScores$floral <- landcoverScores$Flor_P1_b*(landcoverScores$Flor_Cov_P1_b + landcoverScores$Flor_Cov_P2_b + landcoverScores$Flor_Cov_P3_b)
landcoverScores <- subset(landcoverScores, Species %in% c(1,2,8) & Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS, select=c(Species_name, Landclass_ExpOp_GFS, floral, Nest_per_ha))  

landcoverWrang <- melt(landcoverScores, ID=c(Species_name, Landclass_ExpOp_GFS), variable.name="scoreType", value.name="Score")
landcoverWrang$scoreType <- factor(landcoverWrang$scoreType)
levels(landcoverWrang$scoreType) <- c("Floral", "Nesting")
landcoverWrang$Species_name <- factor(landcoverWrang$Species_name, levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees"))
levels(landcoverWrang$Species_name) <- c("GNB", "TNB", "GNS")

# landcoverWrang2 <- landcoverWrang %>%
#   group_by(Landclass_ExpOp_GFS, scoreType) %>%
#   dplyr::summarise(Score=mean(Score))
# 
# landcoverWrang2$Species_name <- "Mean"

# data <- rbind(landcoverWrang, landcoverWrang2)

colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")
#(Beans, Cereal, Coniferous woodland, Deciduous woodland, Fallow, Improved perm grasslands, Oilseed rape, Unimproved meadow)


plot <- ggplot(landcoverWrang, aes(x=Landclass_ExpOp_GFS, y=Score, fill=Landclass_ExpOp_GFS)) +
  geom_bar(position=position_dodge(width=0), stat = "identity", color="black", size=0.4, width=0.8 ,alpha=.8) +
  facet_grid(rows=vars(scoreType), cols=vars(Species_name), scales="free", switch="y") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=16, face="bold"), 
        # axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_text(size=11), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
        strip.text.y = element_text(size = 10, color = "black", face = "bold"), 
        strip.text=element_text(face="bold"),
        legend.title=element_text(size=10, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        legend.position="none") +
  labs(fill="Landcover Type", y="Score (Arbitrary Units)") 
  # ggtitle("C) Poll4pop-allocated Scores")

plot(plot)
ggsave("figures/NF_scores.png", dpi=600, width=13.5, height=7, units="cm")
