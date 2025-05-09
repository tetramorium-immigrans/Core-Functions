#interna
#Interpolates missing values in a sparse matrix and returns a filled-in sparse matrix

#dat = data in sparse matrix form

interna <- function(dat){
  
  #print("Interpolating values")
  
  intout <- pbapply(dat, 1, function(z){
    tmin.interna <- min(which(z != 0))
    tmax.interna <- max(which(z != 0))

    z[tmin.interna:tmax.interna] <- approx(z[tmin.interna:tmax.interna], n = length(z[tmin.interna:tmax.interna]))$y
    
    # z[tmin.interna:tmax.interna] <- approx(replace(z[tmin.interna:tmax.interna], which(z[tmin.interna:tmax.interna] == 0), NA), 
    #                                        n = length(z[tmin.interna:tmax.interna]))$y
    z
    
  })

  return(Matrix(t(intout), sparse = TRUE))
  
}


#Old, slow version that is no longer used

# interna2 <- function(dat, datrange){
#   
#   intout <- dat
#   
#   for(z in 1:dim(dat)[1]){
#     intemp <- dat[z,]
#     intemp[datrange[z,1]:datrange[z,2]] <- replace(intemp[datrange[z,1]:datrange[z,2]], which(intemp[datrange[z,1]:datrange[z,2]] == 0), NA)
#     intout[z, datrange[z,1]:datrange[z,2]] <- approx(intemp, xout = datrange[z,1]:datrange[z,2])$y
#   }
#   
#   return(intout)
#   
# }
# 
# 
