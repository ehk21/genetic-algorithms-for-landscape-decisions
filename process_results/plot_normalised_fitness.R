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
levels(scores$objective) <- c("GNB", "TNB", "GNS", "Farmer")





























# # original plot but with y axis labels:
# 
# fitnessPlot <- ggplot(scores, aes(x=gen, y=-score, color=type)) +
#   geom_smooth(stat="smooth", aes(color=type), se=T) +
#   # geom_point(size=0.5, aes(color=type)) +
#   facet_wrap(~objective, scales="free", ncol=4) +
#   scale_color_nejm() +
#   theme_bw() +
#   theme(axis.text.x = element_text(size=12), 
#         # axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#         axis.line = element_line(colour = "black", size=0.4), 
#         axis.title=element_text(size=14, face="bold"), plot.title=element_text(size=16, face="bold"),
#         strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
#         strip.text= element_text(face="bold", size=12),
#         legend.title=element_text(size=12, face="bold"),
#         legend.text=element_text(size=11),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
#         legend.position="top") +
#   labs(color="Optim Type", x="Generation", y="Mean Objective Fitness") +
#   ggtitle("A")
# 
# plot(fitnessPlot)
# ggsave("../write_up/Figures/fitness.png", dpi=600, width=24, height=16, units="cm")




