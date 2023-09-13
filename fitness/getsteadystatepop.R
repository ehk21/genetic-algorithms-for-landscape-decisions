getsteadystatepop <- function(nf,parameters,beesp,nbees,nfloral,cellsize){
  
  firstyrfac = rep(1.0,nbees)
  
  #Call runpoll once to intialise for landscape
  poll0<-runpoll_3seasons(M_poll0=numeric(0),firstyear=TRUE,firstyearfactor=firstyrfac,bees=beesp,cell.size=cellsize,paramList=parameters,nest=nf$nest,floral=nf$floral,cutoff=0.99,loc_managed)
  #Extract no. of females surviving
  nsurv0 = numeric(nbees)
  for (s in 1:nbees)
    {nsurv0[s] = round(sum(values(poll0$M_poll[[s]][[(nfloral+1)]])))
  }
  #print(nsurv0)
  
  #Call runpoll again until results converge
  diff = 1000
  while (diff!=0.0)
    {poll<-runpoll_3seasons(M_poll0=poll0$M_poll, firstyear=FALSE, firstyearfactor=firstyrfac, bees=beesp, cell.size=cellsize, paramList=parameters, nest=nf$nest, floral=nf$floral, cutoff=0.99, loc_managed)
    #Extract no. of females surviving
    nsurv = numeric(nbees)
    for (s in 1:nbees)
      {nsurv[s] = round(sum(values(poll$M_poll[[s]][[(nfloral+1)]])))
    }
    #Calculate difference
    #print(nsurv)
    diff = sum((nsurv0-nsurv)/nsurv0)
    if ((nsurv0==0.0)&(nsurv==0.0)) 
      {diff = 0.0
    }
    #Update
    poll0 = poll
    nsurv0 = nsurv
  }
  
  return(poll)
}