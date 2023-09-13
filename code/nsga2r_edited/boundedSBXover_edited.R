boundedSBXover_edited <-
function(parent_chromosome,cprob,optimLandcovers,objectives){
  popSize=nrow(parent_chromosome);
  varNo=ncol(parent_chromosome);
  child <- parent_chromosome;
  p <- 1;
  for (i in 1:(popSize/2)) {
    #if the random probability is less than cprob, then crossover
    if (runif(1) < cprob) { 
      for (j in 1:varNo) {
        parent1 <- child[p,j];
        parent2 <- child[p+1,j];

        # SBX (Simulated Binary Crossover)
        rnd = runif(1);          
        if (rnd <= 0.5 && parent1 != parent2) { # Variable selected EDITED: only bother crossing over if parents don't have same value

          ## EDITS START HERE ##

          # read in scores dataframe and subset based on the landcovers being shuffled and specified objectives of the optimisation
          scores <- read.csv("data/landcover_scores.csv", header=T)
          scores <- subset(scores, Code_ExpOp_GFS %in% optimLandcovers$Code_ExpOp_GFS )
          scores <- subset(scores, Species_name %in% objectives)

          # create average score across all specified objectives for each landcover class
          scale <- data.frame("Code_ExpOp_GFS"=optimLandcovers$Code_ExpOp_GFS, "av_score"=NA)
          for (i in 1:nrow(scale)){
          average <- mean(scores$Total_score[which(scores$Code_ExpOp_GFS == scale$Code_ExpOp_GFS[i])])
          scale$av_score[i] <- average
          }

          # put scores in order so that they can be used as a scale of worst-best landcovers (on average for all of the guilds included in optimisation)
          scale <- scale[order(scale$av_score),]

          # assign the parent with the biggest value to y2 and smallest value to y1
          if (parent2 > parent1){
            y2 <- parent2;
            y1 <- parent1;
          } else {
            y2 <- parent1;
            y1 <- parent2;
          }

          # find the row numbers of scale$Code_ExpOp_GFS which correspond to the optimLandcovers$Code_ExpOp_GFS values indicated by the values of parent1 and parent2
          y1Row <- which(scale$Code_ExpOp_GFS == optimLandcovers$Code_ExpOp_GFS[y1])
          y2Row <- which(scale$Code_ExpOp_GFS == optimLandcovers$Code_ExpOp_GFS[y2])

          # generate new values for children
          childVals <- c()
          for (i in 1:2){
            # if the two values are not next to each other on the scale
            if ((y2Row-y1Row)>1){
              # find the row value of optimLandcovers which corresponds to the expOp code of a randomly selected row in the scale between the two parents
              newRow <- which(optimLandcovers$Code_ExpOp_GFS == scale$Code_ExpOp_GFS[sample((y1Row+1):(y2Row-1), 1)])
            } else {
              newRow <- sample(c(y1Row, y2Row), 1)
            }
            childVals <- c(childVals, newRow)
          }

          child1 <- childVals[1]
          child2 <- childVals[2]

        ## EDITS END HERE ##

        } else { # Variable NOT selected
        # Copying parents to children
          child1 = parent1;
          child2 = parent2;
        } # Variable selection ends here
        child[p,j] <- child1; #EDITED: round() function added
        child[p+1,j] <- child2; #EDITED: round() function added
      } # next j (var)
    } # Xover ends here
    p <- p + 2;
  } # next i
  return(child);
}






