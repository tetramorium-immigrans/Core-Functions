#circrowVars

#dat is a trajs array
#byrow determines whether it returns a vector of row variances (TRUE) or column variances (FALSE)

circVars <- function(dat = trajs, byrow = TRUE){
  dat <- dat[,,3]
  cVars <- c()
  if(byrow == TRUE){
    for(i in 1:dim(dat)[1]){
      redux <- dat[i, is.na(dat[i,]) == FALSE]
      if(length(redux) == 0){
        cVars[i] <- 0
      }else{
        cVars[i] <- circ.summary(redux)$circvariance
        #cVars[i] <- circ.disp(redux)$var
      }
    } #close of byrow = TRUE
    
  }else if(byrow == FALSE){
    for(i in 1:dim(dat)[2]){
      redux <- dat[is.na(dat[,i]) == FALSE, i]
      if(length(redux) == 0){
        cVars[i] <- 0
      }else{
        cVars[i] <- circ.summary(redux)$circvariance
        #cVars[i] <- circ.disp(redux)$var
      }
    }
  }
  
  return(cVars)
  
}
