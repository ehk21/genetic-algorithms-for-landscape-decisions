# function to plot evolution of mean scores of landscapes in each generation of optimisations.

plot.scores <- function(scoresDF, byoptim, byradius, byobjective, compareby, facetby, horizontal, plotTitle, legendTitle){

  #load packages
  library(dplyr)
  library(ggplot2)
  library(plotrix)
  library(diagis)
  library(ggsci)
  
# change objective names for efficiency
  scoresDF$objective <- as.character(scoresDF$objective)
  scoresDF$objective[scoresDF$objective == "GroundNestingBumblebees"] <- "GNB"
  scoresDF$objective[scoresDF$objective == "TreeNestingBumblebees"] <- "TNB"
  scoresDF$objective[scoresDF$objective == "GroundNestingSolitaryBees"] <- "GNS"
  
# add units to radius
  scoresDF$radius <- paste0(scoresDF$radius, "m")
  scoresDF$radius <- factor(scoresDF$radius, levels=c("500m", "1000m", "2000m"))
  for(i in 1:length(byradius)) { 
    byradius[i] <- paste0(byradius[i], "m")
    }
  
# summarise data
scoreMeans <- scoresDF %>%
  group_by(optim, radius, rep, objective, gen) %>%
  dplyr::summarise(scoreAv=mean(score), sd=sd(score), se=std.error(score), n=n())

# subset data by: population, optim, radius, objective
data <- subset(scoreMeans, optim %in% byoptim)
data <- subset(data, radius %in% byradius)
data <- subset(data, objective %in% byobjective)

# weighted mean of all reps of each optim (may need to change std error/ dev calculations)
dataSummary <- data %>%
  group_by(optim, radius, objective, gen) %>%
  dplyr::summarise(meanScore=weighted.mean(scoreAv, n), se=weighted_se(scoreAv, n), sd=sd(scoreAv))

dataSummary <- as.data.frame(dataSummary)

dataSummary$optim <- as.factor(dataSummary$optim)
dataSummary$radius <- as.factor(dataSummary$radius)
dataSummary$objective <- as.factor(dataSummary$objective)


#plot
if(is.na(compareby) && is.na(facetby)){
  
  plot <- ggplot(dataSummary, aes(x=gen, y=-meanScore)) +
    geom_point(size=0.5) +
    geom_smooth(stat="smooth") +
    ggtitle(plotTitle) 

  } else if (!is.na(facetby) && is.na(compareby)){
    
    if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
    } else {cols=1}
    
    plot <- ggplot(dataSummary, aes(x=gen, y=-meanScore)) +
      geom_point(size=0.5, aes(color=get(facetby))) +
      geom_smooth(stat="smooth", aes(color=get(facetby))) +
      facet_wrap(~get(facetby), ncol=cols, scales="free")
      ggtitle(plotTitle) 
    
  } else if (!is.na(compareby) && is.na(facetby)){
    
    plot <- ggplot(dataSummary, aes(x=gen, y=-meanScore, fill=get(compareby))) +
      geom_point(size=0.5, aes(color=get(compareby))) +
      geom_smooth(stat="smooth", aes(color=get(compareby))) +
      ggtitle(plotTitle) 
    
  } else {
    
    if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
    } else {cols=1}
    
    plot <- ggplot(dataSummary, aes(x=gen, y=-meanScore, color=get(compareby))) +
      geom_smooth(stat="smooth", aes(color=get(compareby))) +
      geom_point(size=0.5, aes(color=get(compareby))) +
      facet_wrap(~get(facetby), ncol=cols, scales="free") +
      ggtitle(plotTitle) +
      scale_color_npg() +
      theme_bw() +
      theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10), 
            axis.line = element_line(colour = "black", size=0.4), 
            axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"),
            strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
            legend.title=element_text(size=11, face="bold"), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
      labs(color=legendTitle, x="Generation", y="Mean score")
    
      
  }

return(plot)
}
