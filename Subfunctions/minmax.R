##Minmax
#Function for finding/converting mintime, maxtime, and lspace
#Outputs a vector indexed as mintime[1], maxtime[2], and lspace[3]

#dat is data, assumed to be in trajs or locs form (see Import.R)
#mintime and maxtime are arguments given by the calling function, usually restricting the time frame of the data
#lspace (line space) is the a graphical parameter for how far apart the vertical time marking lines are in the graph
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s * 60 s/m)

Minmax <- function(dat, mintime, maxtime, lspace, frate = 5){
  if(missing(mintime)){
    mintimef <- min(which(rowSums(!is.na(dat[,,1])) > 0))  #Finds the earliest frame that has data
    
    #mintimef <- 1                                     #Mintime is set to 1 for purposes of indexing in the functions
  }else{
    mintimef <- mintime*frate*60
    if(mintimef == 0){
      mintimef <- 1
    }
  }
  
  if(missing(maxtime)){
    maxtimef <- dim(dat)[1]
  }else{
    maxtimef <- maxtime*frate*60
  }
  
  if(missing(lspace)){
    lspacef <- round((maxtimef - mintimef) / 12)
  }else{
    lspacef <- lspace*frate*60
  }
  
  return(c(mintimef,maxtimef,lspacef))
  
}

