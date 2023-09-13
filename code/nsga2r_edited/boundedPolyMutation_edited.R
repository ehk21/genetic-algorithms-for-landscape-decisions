boundedPolyMutation_edited <-
function(parent_chromosome,lowerBounds,upperBounds,mprob,mum){
  popSize=nrow(parent_chromosome);
  varNo=ncol(parent_chromosome);
  child <- parent_chromosome;
    for (i in 1:popSize) {
      for (j in 1:varNo){
        # if the random probability is less than mprob, then mutate that variable
        if (runif(1) < mprob) {
          child[i,j] = round(runif(1, (lowerBounds[j] - 0.5) , (upperBounds[j] + 0.5))); # EDITED - this line replaces nearly all code in original function. Assign random integer between specified bounds.
        } # runif(1) > mprob, do not perform mutation
      } # next j
    } # next i
  return(child);
}
