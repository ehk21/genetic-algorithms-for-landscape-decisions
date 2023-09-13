# pixelDF = pixels 
# bypopulation=c("initial", "final")
# byoptim=c("GNB", "GNBFixed", "TNB", "TNBFixed", "GNS", "GNSFixed", "AllBees", "AllFarmer")
# byradius=c(500, 1000, 2000)
# proportional = TRUE FALSE
# compareby = "pop" "optim" "radius" NA
# facetby = "pop" "optim" "radius" NA
# horizontal = TRUE FALSE


plot.pixels <- function(pixelDF, bypopulation, byoptim, byradius, proportional, compareby, facetby, horizontal, plotTitle, legendTitle){

  library(dplyr)
  library(plotrix)
  library(diagis)
  library(ggplot2)
  library(gridExtra)
  library(ggsci)
  
  # define colour palette
  colors <- c("#920000", "#FFD54F", "#000000", "#1B5E20", "#924900", "#11C638", "#6DB6FF", "#FFB6DB")
    #(Beans, Cereal, Coniferous woodland, Deciduous woodland, Fallow, Improved perm grasslands, Oilseed rape, Unimproved meadow)
   ##### prepare data #####
  pixelDF$Code_ExpOp_GFS <- as.factor(pixelDF$Code_ExpOp_GFS)
  pixelDF$optim <- factor(pixelDF$optim) #, levels=c("GNB", "TNB", "GNS", "FarmerOnly", "AllBees", "Farmer" ))
  pixelDF$rep <- as.factor(pixelDF$rep)
  pixelDF$pop <- as.factor(pixelDF$pop)
  pixelDF$landscape <- as.factor(pixelDF$landscape)
  
  pixelDF$radius <- paste0(pixelDF$radius, "m")
  pixelDF$radius <- factor(pixelDF$radius, levels=c("500m", "1000m", "2000m"))

  optimLandcovers <- read.csv("data/optim_landcovers.csv")
  optimLandcovers$Code_ExpOp_GFS <- as.factor(optimLandcovers$Code_ExpOp_GFS)
  pixelDF<- dplyr::left_join(pixelDF, optimLandcovers,
                             by = "Code_ExpOp_GFS", copy=T)

  if(isTRUE(proportional)){

    # calculate proportions
    pixelDF <- pixelDF %>%
      group_by(optim, radius, pop, rep, landscape) %>%
      dplyr::mutate(pixelT= sum(pixels)) %>%
      dplyr::mutate(pixelsPerc=(pixels/pixelT)*100)

    pixelDF$pixelsPerc[which(is.na(pixelDF$pixelsPerc))] <- 0

    pixelSummary <- pixelDF %>%
      group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanPerc=mean(pixelsPerc), pixelsSD=sd(pixelsPerc), pixelsSE=std.error(pixelsPerc), landscapeNo=n())

    # subset data by: population, optim, radius
    data <- subset(pixelSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)

    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(perc=weighted.mean(meanPerc, landscapeNo), se=weighted_se(meanPerc, landscapeNo), sd=sd(meanPerc))
    
    dataSummary <- as.data.frame(dataSummary)
    

    #plot
    if (is.na(compareby) && is.na(facetby)){
      #ggplot of percentage with no fill or facet wrap
      plot <- ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
                geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
                geom_errorbar(aes(ymin=perc-se, ymax=perc+se), width=.22, position=position_dodge(.6), stat="identity") +
                labs(y="Mean % of Optimised Area", x="Landcover Type") +
                theme_bw() +
                theme( axis.text.y = element_text(size=10), 
                       axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
                       axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                       legend.title=element_text(size=11, face="bold"), 
                       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
                       legend.position="bottom") +
                # scale_fill_npg() +
                scale_fill_manual(values = colors) +
                ggtitle(plotTitle)

    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of percentage with facetwrap
      plot <- ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=perc-se, ymax=perc+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold")) +
        labs(y="Mean % of Optimised Area", x="Landcover Type", fill=compareby) +
        # scale_fill_npg() +
        scale_fill_manual(values = colors) +
        ggtitle(plotTitle)

    } else if (is.na(compareby) && !is.na(facetby)){
      #ggplot of percentage with fill=
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
                geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
                geom_errorbar(aes(ymin=perc-se, ymax=perc+se), width=.22, position=position_dodge(.6), stat="identity") +
                theme_bw() +
                theme( axis.text.y = element_text(size=10), 
                       axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), 
                      axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
                      strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"), 
                      strip.text.x = element_text(size = 10, color = "black", face = "bold"), 
                      legend.title=element_text(size=11, face="bold"), 
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
                      legend.position="bottom") +
                labs(y="Mean % of Optimised Area", x="Landcover Type", fill=legendTitle) +
                facet_wrap(~get(facetby), ncol=cols, strip.position="bottom") +
                # scale_fill_npg() +
                scale_fill_manual(values = colors) +
                ggtitle(plotTitle)
      
    } else {
      # ggplot of percentage with fill= and facet wrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
      plot <- ggplot(dataSummary, aes(y=perc, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
                geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4, width=0.8) +
                geom_errorbar(aes(ymin=perc-se, ymax=perc+se), width=.22, position=position_dodge(0.8), stat="identity") +
                labs(y="Mean % of Optimised Area", x="Landcover Type", fill=legendTitle) +
                facet_wrap(~get(facetby), ncol=cols, strip.position="bottom") +
                # scale_fill_npg() +
                scale_fill_manual(values = colors) +
                ggtitle(plotTitle) +
                theme_bw() +
                # scale_y_continuous( expand = c(0, 1)) +
                theme(axis.text.x = element_text(angle = 300, hjust=0, size=10), axis.text.y = element_text(size=10), 
                       axis.line = element_line(colour = "black", size=0.4), 
                      axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"),
                      strip.background = element_rect(color="black", fill="gray80", size=0.7, linetype="solid"),
                      legend.title=element_text(size=11, face="bold"), 
                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
                  }

  } else {
    # summarise data
    pixelSummary <- pixelDF %>%
    group_by(optim, radius, pop, rep, Landclass_ExpOp_GFS) %>%
    dplyr::summarise(pixelsAv=mean(pixels), pixelsSD=sd(pixels), pixelsSE=std.error(pixels), landscapeNo=n())

    # subset data by: population, optim, radius
    data <- subset(pixelSummary, pop %in% bypopulation)
    data <- subset(data, optim %in% byoptim)
    data <- subset(data, radius %in% byradius)

    # weighted mean of all reps of each optim (may need to change std error/ dev calculations)
    dataSummary <- data %>%
      group_by(optim, radius, pop, Landclass_ExpOp_GFS) %>%
      dplyr::summarise(meanPixels=weighted.mean(pixelsAv, landscapeNo), se=weighted_se(pixelsAv, landscapeNo), sd=sd(pixelsAv) )

    dataSummary <- as.data.frame(dataSummary)

    #plot

    if (is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with no fill or facet wrap
      plot <- ggplot(dataSummary, aes(y=meanPixels, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
              geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
              geom_errorbar(aes(ymin=meanPixels-se, ymax=meanPixels+se), width=.22, position=position_dodge(.6), stat="identity") +
              theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold")) +
              labs(y="Average number of pixels", x="Landcover Type") +
              # scale_fill_npg() +
              scale_fill_manual(values = colors) +
              ggtitle(plotTitle)

    } else if (!is.na(facetby) && is.na(compareby)){
      #ggplot of pixels with facetwrap
      
      if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
      } else {cols=1}
      
        if(facetby == "radius"){
          p <- list()
          for(i in 1:length(unique(dataSummary$radius))){
            p[[i]] <- ggplot(subset(dataSummary, radius == (levels(dataSummary$radius))[i]), aes(y=meanPixels, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
                      geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
                      geom_errorbar(aes(ymin=meanPixels-se, ymax=meanPixels+se), width=.22, position=position_dodge(.6), stat="identity") +
                      theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold"), legend.position="none") +
                      labs(y="Average number of pixels", x="Landcover Type") +
                      # scale_fill_npg() +
                      scale_fill_manual(values = colors) +
                      ggtitle(byradius[i])
          }
          
          plot <- do.call(grid.arrange, list(grobs=p, ncol=cols, top=plotTitle))
          
          } else{
          plot <- ggplot(dataSummary, aes(y=meanPixels, x=Landclass_ExpOp_GFS, fill=Landclass_ExpOp_GFS)) +
            geom_bar(position=position_dodge(), stat = "identity", color="black", size=0.4) +
            geom_errorbar(aes(ymin=meanPixels-se, ymax=meanPixels+se), width=.22, position=position_dodge(.6), stat="identity") +
            theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold")) +
            labs(y="Average number of pixels", x="Landcover Type") +
            facet_wrap(~get(facetby), ncol=cols) +
            # scale_fill_npg() +
            scale_fill_manual(values = colors) +            
            ggtitle(plotTitle)
            }

    } else if (!is.na(compareby) && is.na(facetby)){
      #ggplot of pixels with fill=
      plot <- ggplot(dataSummary, aes(y=meanPixels, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
        geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
        geom_errorbar(aes(ymin=meanPixels-se, ymax=meanPixels+se), width=.22, position=position_dodge(.6), stat="identity") +
        theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold")) +
        labs(y="Average number of pixels", x="Landcover Type", fill=compareby) +
        # scale_fill_npg() +
        scale_fill_manual(values = colors) +
        ggtitle(plotTitle)

      } else {
        
        if(isTRUE(horizontal)){cols = length(unique(dataSummary[,facetby]))
        } else {cols=1}

        if(facetby == "radius"){
          p <- list()
          for(i in 1:length(unique(dataSummary$radius))){
            p[[i]] <- ggplot(subset(dataSummary, radius == (levels(dataSummary$radius))[i]), aes(y=meanPixels, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
                      geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
                      geom_errorbar(aes(ymin=meanPixels-se, ymax=meanPixels+se), width=.22, position=position_dodge(.6), stat="identity") +
                      theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                            panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold")) +
                      labs(y="Average number of pixels", x="Landcover Type", fill=compareby) +
                      # scale_fill_npg() +
                      scale_fill_manual(values = colors) +
                      ggtitle(byradius[i])
            
          }
          
          plot <- do.call(grid.arrange, list(grobs=p, ncol=cols, top=plotTitle))
          
        } else{
        # ggplot of pixels with fill= and facet wrap
        plot <- ggplot(dataSummary, aes(y=meanPixels, x=Landclass_ExpOp_GFS, fill=get(compareby))) +
          geom_bar(position=position_dodge(width=0.6), stat = "identity", color="black", size=0.4, width=0.5) +
          geom_errorbar(aes(ymin=meanPixels-se, ymax=meanPixels+se), width=.22, position=position_dodge(.6), stat="identity") +
          theme(axis.text.x = element_text(angle = 310, hjust=0, size=10), axis.text.y = element_text(size=10), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.4), axis.title=element_text(size=11, face="bold"), plot.title=element_text(size=14, face="bold")) +
          labs(y="Average number of pixels", x="Landcover Type", fill=compareby) +
          facet_wrap(~get(facetby), ncol=cols) +
          # scale_fill_npg() +
          scale_fill_manual(values = colors) +
          ggtitle(plotTitle)
      }
  }}
 return(plot)

  }

