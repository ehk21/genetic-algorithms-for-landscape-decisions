# function to plot floral/nesting scores of each landcover in optimised landscapes

# scores <- floralNestingScores
# byoptim=c("GNB", "GNBFixed", "TNB", "TNBFixed", "GNS", "GNSFixed", "AllBees", "AllFarmer")
# byradius=c(500, 1000, 2000)
# byguild = c("GNB", "TNB", "GNS")
# byscore = c("spring", "summer", "nesting")
# bypopulation = c("initial", "final", "compare")
# xby = "optim" "radius" "guild" "score" 
# compareby = "optim" "radius" "guild" "score" NA
# facetby = "optim" "radius" "guild" "score" NA
# horizontal = TRUE FALSE
# plotTitle

plot.floral.nesting.scores <- function(scores, byoptim, byradius, byguild, byscore, bypopulation, xby, compareby, facetby, horizontal, plotTitle, legendTitle, xlab) {

# load packages
library(dplyr)
library(reshape2)
library(plotrix)
library(ggplot2)
library(diagis)
library(ggsci)

# replace NA values
scores$spring2f <- replace(scores$spring2f, is.na(scores$spring2f), 0)

# factorise categories
scores$radius <- as.factor(scores$radius)

# replace guild names for efficiency
scores$guild <- factor(scores$guild,ordered=T,  levels=c("GroundNestingBumblebees", "TreeNestingBumblebees", "GroundNestingSolitaryBees"))
levels(scores$guild) <- c("GNB", "TNB", "GNS")

scores$pop <- factor(scores$pop, levels=c("initial", "final"))
levels(scores$pop) <- c("Initial", "Final")

# wrangle data
scores2 <- scores %>%
  mutate(springf = spring1f + spring2f )

scores3 <- scores2 %>%
  dplyr::select(optim, radius, rep, pop, guild, landscape, springf, summerf, nesting) %>%
  group_by(optim, radius, rep, pop, guild) %>%
  dplyr::summarise(springfAv = mean(springf), summerfAv = mean(summerf), nestingAv=mean(nesting), landscapeNo=n())

if("compare" %in% bypopulation){

      # scores4 <- scores3 %>%
      #   group_by(optim, radius, rep, guild) %>%
      #   dplyr::summarise(Spring = (springfAv[pop=="Final"]-springfAv[pop=="Initial"])/springfAv[pop=="Initial"], 
      #             Summer = (summerfAv[pop=="Final"]-summerfAv[pop=="Initial"])/summerfAv[pop=="Initial"],
      #             Nesting = (nestingAv[pop=="Final"]-nestingAv[pop=="Initial"])/nestingAv[pop=="Initial"])
  
  scores4 <- scores3 %>%
    group_by(optim, radius, rep, guild) %>%
    dplyr::summarise(Spring = ((springfAv[pop=="Final"]-springfAv[pop=="Initial"])/springfAv[pop=="Initial"])*100, 
                     Summer = ((summerfAv[pop=="Final"]-summerfAv[pop=="Initial"])/summerfAv[pop=="Initial"])*100,
                     Nesting = ((nestingAv[pop=="Final"]-nestingAv[pop=="Initial"])/nestingAv[pop=="Initial"])*100)
  
      
      scores5 <- melt(scores4, id=c("optim", "radius", "rep", "guild"), variable.name = "score", value.name = "percChange")
      
      scores6 <- scores5 %>%
        group_by(optim, radius, guild, score) %>%
        dplyr::summarise(meanChange=mean(percChange), se=std.error(percChange), sd=sd(percChange))
      
      scores6 <- as.data.frame(scores6)
      
      # subset data 
      scores6 <- subset(scores6, optim %in% byoptim)
      scores6 <- subset(scores6, radius %in% byradius)
      scores6 <- subset(scores6, guild %in% byguild)
      scores6 <- subset(scores6, score %in% byscore)
      
      # define number of columns
      if(!is.na(facetby)){
        if(isTRUE(horizontal)){cols = length(unique(scores6[,facetby]))
        } else {cols=1}
      }
      
      # plot
      
      scores6$score <- as.character(scores6$score)
      scores6$score[scores6$score=="Spring"] <- "Spring Floral"
      scores6$score[scores6$score=="Summer"] <- "Summer Floral"
      scores6$score <- as.factor(scores6$score)
      
      if (is.na(compareby) && is.na(facetby)){
        #ggplot of percentage with no fill or facet wrap
        plot <- ggplot(scores6, aes(y=meanChange, x=get(xby))) +
                  geom_bar(position=position_dodge(), stat = "identity") +
                  geom_errorbar(aes(ymin=meanChange-se, ymax=meanChange+se), width=.22, position=position_dodge(.9), stat="identity") +
                  theme(axis.text.x = element_text(angle = 290, hjust=0)) +
                  labs(y="Mean % Change") +
                  ggtitle(plotTitle)
              
      } else if (!is.na(compareby) && is.na(facetby)){
        #ggplot of percentage with facetwrap
        plot <- ggplot(scores6, aes(y=meanChange, x=get(xby), fill=get(compareby))) +
                  geom_bar(position=position_dodge(), stat = "identity") +
                  geom_errorbar(aes(ymin=meanChange-se, ymax=meanChange+se), width=.22, position=position_dodge(.9), stat="identity") +
                  theme(axis.text.x = element_text(angle = 290, hjust=0)) +
                  labs(y="Mean % Change") +
                  ggtitle(plotTitle)
        
      } else if (is.na(compareby) && !is.na(facetby)){
        #ggplot of percentage with fill=
        plot <- ggplot(scores6, aes(y=meanChange, x=get(xby))) +
                  geom_bar(position=position_dodge(), stat = "identity") +
                  facet_wrap(~get(facetby), ncol=cols) +
                  geom_errorbar(aes(ymin=meanChange-se, ymax=meanChange+se), width=.22, position=position_dodge(.9), stat="identity") +
                  theme(axis.text.x = element_text(angle = 290, hjust=0)) +
                  labs(y="Mean % Change") +
                  ggtitle(plotTitle)
        
      } else {
        # ggplot of percentage with fill= and facet wrap
        plot <- ggplot(scores6, aes(y=meanChange, x=get(xby), fill=get(compareby))) +
                  geom_bar(position=position_dodge(), stat = "identity") +
                  facet_wrap(~get(facetby), ncol=cols) +
                  geom_errorbar(aes(ymin=meanChange-se, ymax=meanChange+se), width=.22, position=position_dodge(.9), stat="identity") +
                  theme_bw() +
                  theme( axis.text.y = element_text(size=10), 
                         axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
                         strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
                         strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
                         legend.title=element_text(size=11, face="bold"), 
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
                  scale_fill_nejm() +
                  labs(y="Mean % Change", fill=legendTitle, x=xlab) +
                  ggtitle(plotTitle)
      }
      
  } else{
  
  colnames(scores3) <- c("optim", "radius", "rep", "pop", "guild", "Spring", "Summer", "Nesting", "landscapeNo")
  
  scores4 <- melt(scores3, id=c("optim", "radius", "rep", "pop", "guild", "landscapeNo"), variable.name = "score", value.name = "mean")
  
  scores5 <- scores4 %>%
    group_by(optim, radius, guild, pop, score) %>%
    dplyr::summarise(Mean=weighted.mean(mean, landscapeNo), se=weighted_se(mean, landscapeNo))
  
  # subset data 
  scores6 <- subset(scores5, optim %in% byoptim)
  scores6 <- subset(scores6, radius %in% byradius)
  scores6 <- subset(scores6, guild %in% byguild)
  scores6 <- subset(scores6, score %in% byscore)
  scores6 <- subset(scores6, pop %in% bypopulation)
  
  scores6 <- as.data.frame(scores6)
  
  # define number of columns
  if(!is.na(facetby)){
    if(isTRUE(horizontal)){cols = length(unique(scores6[,facetby]))
    } else {cols=1}
  }

  #plot
  
  scores6$score <- as.character(scores6$score)
  scores6$score[scores6$score=="Spring"] <- "Spring Floral"
  scores6$score[scores6$score=="Summer"] <- "Summer Floral"
  scores6$score <- as.factor(scores6$score)
  
  if (is.na(compareby) && is.na(facetby)){
    #ggplot of percentage with no fill or facet wrap
    plot <- ggplot(scores6, aes(y=Mean, x=get(xby))) +
      geom_bar(position=position_dodge(), stat = "identity") +
      geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.22, position=position_dodge(.9), stat="identity") +
      theme(axis.text.x = element_text(angle = 290, hjust=0)) +
      labs(y="Score") +
      ggtitle(plotTitle)
    
  } else if (!is.na(compareby) && is.na(facetby)){
    #ggplot of percentage with facetwrap
    plot <- ggplot(scores6, aes(y=Mean, x=get(xby), fill=get(compareby))) +
      geom_bar(position=position_dodge(), stat = "identity") +
      geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.22, position=position_dodge(.9), stat="identity") +
      theme(axis.text.x = element_text(angle = 290, hjust=0)) +
      labs(y="Score") +
      ggtitle(plotTitle)
    
  } else if (is.na(compareby) && !is.na(facetby)){
    #ggplot of percentage with fill=
    plot <- ggplot(scores6, aes(y=Mean, x=get(xby))) +
      geom_bar(position=position_dodge(), stat = "identity") +
      facet_wrap(~get(facetby), ncol=cols, scales="free") +
      geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.22, position=position_dodge(.9), stat="identity") +
      theme(axis.text.x = element_text(angle = 290, hjust=0)) +
      labs(y="Score") +
      ggtitle(plotTitle)
    
  } else {
    # ggplot of percentage with fill= and facet wrap
    plot <- ggplot(scores6, aes(y=Mean, x=get(xby), fill=get(compareby))) +
      geom_bar(position=position_dodge(), stat = "identity") +
      facet_wrap(~get(facetby), ncol=cols, scales="free") +
      geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), width=.22, position=position_dodge(.9), stat="identity") +
      theme_bw() +
      theme( axis.text.y = element_text(size=10), 
             axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
             strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
             strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
             legend.title=element_text(size=11, face="bold"), 
             panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
      scale_fill_npg() +
      labs(y="Score", fill=legendTitle, x=xlab) +
      ggtitle(plotTitle)
  }
  
  
}
}


