##Outbound
#Function for restricting data set to only outbound (1) or inbound (2) ants
#Outbound/inbound is determined by the sum of the x-components of velocity being either negative (outbound) or inbound (positive)

#Some future reader may wonder why I don't just use the difference of the starting and ending locations.  This is because the data that is usually
#loaded (trajs) does not contain that information and I would have to import and process a secondary one (locs) in order to do that.  

Outbound <- function(trajs, input, locs = locs, returnlocs = FALSE){
  if(input == 1){
    filtered <- colSums(trajs[,,1], na.rm = TRUE) < 0
    
    if(returnlocs == FALSE){
      filteredoutput <- trajs[,filtered,]
    }else{
      filteredoutput <- locs[,filtered,]
    }
    
  }else if(input == 2){
    filtered <- colSums(trajs[,,1], na.rm = TRUE) > 0
    
    if(returnlocs == FALSE){
      filteredoutput <- trajs[,filtered,]
    }else{
      filteredoutput <- locs[,filtered,]
    }
  }
  return(filteredoutput)
}