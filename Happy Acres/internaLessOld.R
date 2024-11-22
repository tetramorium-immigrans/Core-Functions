#interna
#Interpolates missing values in a data set

#dat = data in sparse matrix form
#intmin/intmax = min/max times of activity for the associated trajectories
#inter = whether to replace 0s with linear interpolation (TRUE) or the last known value (FALSE)

interna <- function(dat, intmin, intmax, inter = TRUE){
  intran <- cbind(intmin, intmax)
  
  # intout <- apply(dat, 1, FUN = function(z){
  #   intemp <- dat[z,]
  #   intemp[intran[z,1]:intran[z,2]] <- replace(intemp[intran[z,1]:intran[z,2]], which(intemp[intran[z,1]:intran[z,2]] == 0), NA)
  #   intemp[intran[z,1]:intran[z,2]] <- approx(intemp, xout = intran[z,1]:intran[z,2])$y
  #   
  #   return(intemp)
  #   
  #   })
  
  intout <- dat
  
  if(inter == TRUE){
    #would nested apply work for this?  One function taking in a range, the other the data?
    for(z in 1:dim(dat)[1]){
      intemp <- dat[z,]
      intemp[intran[z,1]:intran[z,2]] <- replace(intemp[intran[z,1]:intran[z,2]], which(intemp[intran[z,1]:intran[z,2]] == 0), NA)
      intout[z, intran[z,1]:intran[z,2]] <- approx(intemp, xout = intran[z,1]:intran[z,2])$y
    }
  }else{
    #Locate all situations of X0 and then replace subsequent zeros with X?
    
    for(z in 1:dim(dat)[1]){
      
      for(i in intran[z,1]:intran[z,2]){
        if(i != 0){
          holder <- dat[z,i]
        }else{
          intout[z,i] <- holder
        }
      }
      
      # intemp <- dat[z,]
      # intemp[intran[z,1]:intran[z,2]] <- replace(intemp[intran[z,1]:intran[z,2]], which(intemp[intran[z,1]:intran[z,2]] == -1), NA)
      # intout[z, intran[z,1]:intran[z,2]] <- approx(intemp, xout = intran[z,1]:intran[z,2])$y
    }
  }
  
  return(intout)
  
}
