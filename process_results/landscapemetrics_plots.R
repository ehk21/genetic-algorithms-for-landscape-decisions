# pixelDF = pixels 
# bypopulation=c("initial", "final")
# byoptim=c("GNB", "GNBFixed", "TNB", "TNBFixed", "GNS", "GNSFixed", "AllBees", "AllFarmer")
# byradius=c(500, 1000, 2000)
# proportional = TRUE FALSE
# compareby = "pop" "optim" "radius" NA
# facetby = "pop" "optim" "radius" NA
# horizontal = TRUE FALSE


plot.metric <- function(pixelDF, metric, bypopulation, byoptim, byradius, compareby, facetby, horizontal, plotTitle, legendTitle){
  
  library(dplyr)
  library(plotrix)
  library(diagis)
  library(ggplot2)
  library(gridExtra)
  library(ggsci)
  
  ##### prepare data #####
  pixelDF$Code_ExpOp_GFS <- as.factor(pixelDF$Code_ExpOp_GFS)
  pixelDF$optim <- as.factor(pixelDF$optim)
  pixelDF$rep <- as.factor(pixelDF$rep)
  pixelDF$pop <- as.factor(pixelDF$pop)
  pixelDF$landscape <- as.factor(pixelDF$landscape)
  
  pixelDF$radius <- paste0(pixelDF$radius, "m")
  pixelDF$radius <- factor(pixelDF$radius, levels=c("500m", "1000m", "2000m"))

  
  optimLandcovers <- read.csv("data/optim_landcovers.csv")
  optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
  pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                             by = "Code_ExpOp_GFS", copy=T)
  
if(metric=="clumpiness"){  
#########################
  
    # summarise data
    clumpSummary <- pixelDF %>%
      group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(clumpAv=mean(clumpiness), clumpSD=sd(clumpiness), clumpSE=std.error(clumpiness), landscapeNo=n())
    
    # subset data by: population, optim, radius
    data <- subset(clumpSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)
    
    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanClumps=weighted.mean(clumpAv, landscapeNo), se=weighted_se(clumpAv, landscapeNo), sd=sd(clumpAv) )
    
    dataSummary <- as.data.frame(dataSummary)
    
    #plot
    
    if (is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with no fill or facet wrap
     
       plot <- ggplot(dataSummary, aes(y=meanClumps, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanClumps-se, ymax=meanClumps+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
              axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
              axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
              legend.title=element_text(size=11, face="bold"), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        labs(y="Average clumpiness", x="Landcover Type", fill=legendTitle) +
        theme_bw() +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else if (!is.na(facetby) && is.na(compareby)){
      #ggplot of pixels with facetwrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
        plot <- ggplot(dataSummary, aes(y=meanClumps, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
          geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
          geom_errorbar(aes(ymin=meanClumps-se, ymax=meanClumps+se), width=.22, position=position_dodge(.6), stat="identity") +
          theme( axis.text.y = element_text(size=10), 
                 axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
                 axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                 strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
                 strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
                 legend.title=element_text(size=11, face="bold"), 
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
          theme_bw() +
          labs(y="Average clumpiness", x="Landcover Type", fill=legendTitle) +
          facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
          scale_fill_npg() +
          ggtitle(plotTitle)
    
      
    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with fill=
      plot <- ggplot(dataSummary, aes(y=meanClumps, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanClumps-se, ymax=meanClumps+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Average clumpiness", x="Landcover Type", fill=legendTitle) +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else {
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
        plot <- ggplot(dataSummary, aes(y=meanClumps, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
          geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
          geom_errorbar(aes(ymin=meanClumps-se, ymax=meanClumps+se), width=.22, position=position_dodge(.6), stat="identity") +
          theme( axis.text.y = element_text(size=10), 
                 axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
                 axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                 strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
                 strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
                 legend.title=element_text(size=11, face="bold"), 
                 panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
          theme_bw() +
          labs(y="Average clumpiness", x="Landcover Type", fill=legendTitle) +
          facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
          scale_fill_npg() +
          ggtitle(plotTitle)
    }
}
  
  
else if(metric=="patchDensity"){  
###############################
    # summarise data
    pdSummary <- pixelDF %>%
      group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(pdAv=mean(patchDensity), pdSD=sd(patchDensity), pdSE=std.error(patchDensity), landscapeNo=n())
    
    # subset data by: population, optim, radius
    data <- subset(pdSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)
    
    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanPD=weighted.mean(pdAv, landscapeNo), se=weighted_se(pdAv, landscapeNo), sd=sd(pdAv) )
    
    dataSummary <- as.data.frame(dataSummary)
    
    #plot
    
    if (is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with no fill or facet wrap
      
      plot <- ggplot(dataSummary, aes(y=meanPD, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanPD-se, ymax=meanPD+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        labs(y="Mean Patch Density", x="Landcover Type", fill=legendTitle) +
        theme_bw() +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else if (!is.na(facetby) && is.na(compareby)){
      #ggplot of pixels with facetwrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanPD, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanPD-se, ymax=meanPD+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Patch Density", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
      
    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with fill=
      plot <- ggplot(dataSummary, aes(y=meanPD, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanPD-se, ymax=meanPD+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Patch Density", x="Landcover Type", fill=legendTitle) +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else {
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanPD, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanPD-se, ymax=meanPD+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Patch Density", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
    }
}
  
  
  else if(metric=="patchNo"){  
    ###############################
    # summarise data
    noSummary <- pixelDF %>%
      group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(noAv=mean(patchNo), noSD=sd(patchNo), noSE=std.error(patchNo), landscapeNo=n())
    
    # subset data by: population, optim, radius
    data <- subset(noSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)
    
    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanNo=weighted.mean(noAv, landscapeNo), se=weighted_se(noAv, landscapeNo), sd=sd(noAv) )
    
    dataSummary <- as.data.frame(dataSummary)
    
    #plot
    
    if (is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with no fill or facet wrap
      
      plot <- ggplot(dataSummary, aes(y=meanNo, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanNo-se, ymax=meanNo+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        labs(y="Mean Number of Patches", x="Landcover Type", fill=legendTitle) +
        theme_bw() +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else if (!is.na(facetby) && is.na(compareby)){
      #ggplot of pixels with facetwrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanNo, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanNo-se, ymax=meanNo+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Number of Patches", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
      
    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with fill=
      plot <- ggplot(dataSummary, aes(y=meanNo, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanNo-se, ymax=meanNo+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Number of Patches", x="Landcover Type", fill=legendTitle) +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else {
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanNo, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanNo-se, ymax=meanNo+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Number of Patches", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
    }
  }
  
  
  else if(metric=="patchArea"){  
    ###############################
    # summarise data
    areaSummary <- pixelDF %>%
      group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(areaAv=mean(patchArea), areaSD=sd(patchArea), areaSE=std.error(patchArea), landscapeNo=n())
    
    # subset data by: population, optim, radius
    data <- subset(areaSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)
    
    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanArea=weighted.mean(areaAv, landscapeNo), se=weighted_se(areaAv, landscapeNo), sd=sd(areaAv))
    
    dataSummary <- as.data.frame(dataSummary)
    
    #plot
    
    if (is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with no fill or facet wrap
      
      plot <- ggplot(dataSummary, aes(y=meanArea, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanArea-se, ymax=meanArea+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        labs(y="Mean Area of Patches", x="Landcover Type", fill=legendTitle) +
        theme_bw() +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else if (!is.na(facetby) && is.na(compareby)){
      #ggplot of pixels with facetwrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanArea, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanArea-se, ymax=meanArea+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Area of Patches", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
      
    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with fill=
      plot <- ggplot(dataSummary, aes(y=meanArea, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanArea-se, ymax=meanArea+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Area of Patches", x="Landcover Type", fill=legendTitle) +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else {
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanArea, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanArea-se, ymax=meanArea+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Mean Area of Patches", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
    }
  }
  
  else if(metric=="classArea"){  
    ###############################
    # summarise data
    caSummary <- pixelDF %>%
      group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(caAv=mean(classArea), caSD=sd(classArea), caSE=std.error(classArea), landscapeNo=n())
    
    # subset data by: population, optim, radius
    data <- subset(caSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)
    
    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanCA=weighted.mean(caAv, landscapeNo), se=weighted_se(caAv, landscapeNo), sd=sd(caAv) )
    
    dataSummary <- as.data.frame(dataSummary)
    
    #plot
    
    if (is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with no fill or facet wrap
      
      plot <- ggplot(dataSummary, aes(y=meanCA, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanCA-se, ymax=meanCA+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        labs(y="Total Class Area", x="Landcover Type", fill=legendTitle) +
        theme_bw() +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else if (!is.na(facetby) && is.na(compareby)){
      #ggplot of pixels with facetwrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanCA, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
        geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
        geom_errorbar(aes(ymin=meanCA-se, ymax=meanCA+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Total Class Area", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
      
    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with fill=
      plot <- ggplot(dataSummary, aes(y=meanCA, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanCA-se, ymax=meanCA+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Total Class Area", x="Landcover Type", fill=legendTitle) +
        scale_fill_npg() +
        ggtitle(plotTitle)
      
    } else {
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=meanCA, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanCA-se, ymax=meanCA+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme( axis.text.y = element_text(size=10), 
               axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
               axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
               strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
               strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
               legend.title=element_text(size=11, face="bold"), 
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
        theme_bw() +
        labs(y="Total Class Area", x="Landcover Type", fill=legendTitle) +
        facet_wrap(~get(facetby), ncol=cols, scales="free_y") +
        scale_fill_npg() +
        ggtitle(plotTitle)
    }
  }
  

return(plot)

}

