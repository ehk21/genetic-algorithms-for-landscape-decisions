  ##############################################################################
  #############################################################################
  #' ehk21@ic.ac.uk - May 2022
  #' 
  #' calculates economic income of landscape for specified crops, adjusting pollinator-dependent crop values by visitation rate as calculated in fitness function. 
  #'
  #' @param visrate late spring visitation rate raster for the guild in question 
  #' @param cropcode <- optimLandcovers$Code_ExpOp_GFS
  #' @param landscape landscape raster
  #' @param yieldyr year crop data is taken from (yieldyr <- 2020)
  #' @param lcarea area of crops in landscape minus edge features (calculated earlier in poll4pop)
  #' @param refyr year crop data is taken from (refyr <- 2020)
  #' @param cellsize cellsize of landscape raster (specified earlier in poll4pop)
  #' @param visitationtoyield visitationtoyield <- read.csv("cluster/data/fitness/input_parameters/VisitationYieldRelationship.csv")
  #' @param yieldfactors_temporal yieldfactors_temporal <- read.csv("cluster/data/fitness/input_parameters/NationalCropProductivityperYear.csv")
  #' @param objectives vector of objectives of the optimisation
  #' 
  #' @returns raster of per-pixel income of input landscape
  #' 
  ##############################################################################
  ##############################################################################

getcropyieldsandincome <- function(visrate,cropcode,landscape,lcarea,refyr,cellsize,visitationtoyield,yieldfactors_temporal, objectives){
  
  
  # create vector used to reference specified crops 
  rowc = which(visitationtoyield$Code_ExpOp_GFS %in% cropcode)
  if (FALSE%in%(cropcode%in%visitationtoyield$Code_ExpOp_GFS)){
    print(paste("!!!getcropyieldsandincome function: input cropcode (",cropcode[which(cropcode %in% visitationtoyield$Code_ExpOp_GFS == F)],") does not occur in visitationtoyield parameter file!!!",sep=""))
    }

  # subset yield data to specified reference year
  yieldfactors_temporal_refyr = subset(yieldfactors_temporal,yieldfactors_temporal$Year==refyr)
  if ((refyr%in%yieldfactors_temporal$Year)==FALSE){
    print(paste("!!!getcropyieldsandincome function: input refyr (",refyr, ") does not occur in yieldfactors_temporal parameter file!!!",sep=""))
    }

  #Make crop mask
  cropmask = landscape
  values(cropmask) = ifelse(values(cropmask) %in% cropcode,1,0) # convert cells with the specified crop code values to 1, and everything else to 0.
  
  #Get visitation rates of specified crops
  cropvis = cropmask*visrate 
  
  #Convert visitation rate units from per cell to per m2
  cropvis = cropvis/(cellsize*cellsize)
  
  # calculate income for each crop type
  cropIncome <- cropvis
  
  for(i in 1:length(rowc)){ # EDIT: added for loop to account for multiple cropcodes
    
    landcoverpixels <- which(values(landscape)==visitationtoyield$Code_ExpOp_GFS[rowc[i]])
    
    if(!any(grepl("bees",objectives))){ # if there are no bees as objectives, don't bother normalising pollinator-dependent crops by visitation rate
      # Get crop income
      GBPperha <- yieldfactors_temporal_refyr$Price_GPBperha[which(yieldfactors_temporal_refyr$Code_ExpOp_GFS==visitationtoyield$Code_ExpOp_GFS[rowc[i]])]
      values(cropIncome)[landcoverpixels] <- GBPperha
      
      # Remove edges and convert to ha
      values(cropIncome)[landcoverpixels] = values(cropIncome)[landcoverpixels]*values(lcarea)[landcoverpixels]/10000.0
    
    } else {

        if (visitationtoyield$relationship[rowc[i]]!="none"){
        
          # Convert to visitation rate to yield (arbitrary units)
          if (visitationtoyield$relationship[rowc[i]]=="linear"){
              values(cropIncome)[landcoverpixels]<-values(cropIncome)[landcoverpixels]*visitationtoyield$coefficient[rowc[i]]
            } else if (visitationtoyield$relationship[rowc[i]]=="inverse_exp"){
              values(cropIncome)[landcoverpixels]<- (1.0-exp(-1.0*values(cropIncome))[landcoverpixels])*exp(visitationtoyield$coefficient[rowc[i]])
            }
          
          # print(paste0("Range of arbitrary yield units: ", min(values(cropIncome)[landcoverpixels])," : ",max(values(cropIncome)[landcoverpixels])))
          
          # extract average arbritrary yield of optimised landscape
          av <- mean(values(cropIncome)[landcoverpixels])
          
          # input: original SK86 mean arbritrary yield for crop pixels (add to dataframe)
          avOrig <- visitationtoyield$av_original_arbitrary[rowc[i]]
          
          # Normalise by national yield for specified reference year
          meanyield = yieldfactors_temporal_refyr$Yield_tonnesperha[which(yieldfactors_temporal_refyr$Code_ExpOp_GFS==visitationtoyield$Code_ExpOp_GFS[rowc[i]])] 
          # values(cropIncome)[landcoverpixels] = ((values(cropIncome)[landcoverpixels]-avOrig)/avOrig)*meanyield
          values(cropIncome)[landcoverpixels] = values(cropIncome)[landcoverpixels]-avOrig+meanyield #absolute change in arbitrary yield
          
          # range(values(cropIncome)[landcoverpixels])
    
          #Get crop mass produced per pixel 
          #NB: cropyield is in tonnes per ha, lcarea is in m2, so converting lcarea to ha in order to get cropmass in tonnes
          values(cropIncome)[landcoverpixels] = values(cropIncome)[landcoverpixels]*values(lcarea)[landcoverpixels]/10000.0
    
          #Get crop income
          #NB: payrate is in GBP per tonne, cropmass is in tonnes, giving cropincome in GBP
          payrate = yieldfactors_temporal_refyr$Price_GBPpertonne[which(yieldfactors_temporal_refyr$Code_ExpOp_GFS==visitationtoyield$Code_ExpOp_GFS[rowc[i]])]
          values(cropIncome)[landcoverpixels]<-values(cropIncome)[landcoverpixels]*payrate
        
          } else {
          
          # Get crop income
          GBPperha <- yieldfactors_temporal_refyr$Price_GPBperha[which(yieldfactors_temporal_refyr$Code_ExpOp_GFS==visitationtoyield$Code_ExpOp_GFS[rowc[i]])]
          values(cropIncome)[landcoverpixels] <- GBPperha
          
          # Remove edges and convert to ha
          values(cropIncome)[landcoverpixels] = values(cropIncome)[landcoverpixels]*values(lcarea)[landcoverpixels]/10000.0
          
          }
        }
      }
  return(cropIncome)
  
  }
