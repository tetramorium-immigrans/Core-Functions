##Andheaddist
#Distribution of variance in headings; produces a histogram

#dat is data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#rm0 is whether to remove trajectories with a median speed of 0 (typically dead ants or other inconsequential objects); 1 removes, 0 keeps
#mintime and maxtime restrict the time

Antheaddist <- function(dat = trajs, outbound = 0, rm0 = 1, 
                        mintime = 0, maxtime,
                        titles = "Distribution of variance in"){
  Antheaddist.f(dat, outbound, rm0, mintime, maxtime, titles)
}

Antheaddist.f <- function(trajs.antheaddist, outbound.antheaddist, rm0.antheaddist,
                          mintime.antheaddist, maxtime.antheaddist,
                          titles.antheaddist){
  
  #Removing trajectories with a median speed of 0
  if(rm0.antheaddist == 1){
    trajsm <- colMedians(trajs.antheaddist[,,4], na.rm = TRUE)
    trajs.antheaddist <- trajs.antheaddist[,!trajsm == 0,]
  }
  
  #mintime: converting input argument from minutes to frames, then setting min frame to 1 if it is 0 to avoid indexing problems below
  mintimef.antheaddist <- mintime.antheaddist*300
  if(mintimef.antheaddist == 0){
    mintimef.antheaddist <- 1
  }
  
  #maxtime: ending frame found if not given as an argument, else converting input from minutes to frames
  if(missing(maxtime.antheaddist)){
    maxtimef.antheaddist <- dim(trajs.antheaddist)[1]
  }else{
    maxtimef.antheaddist <- maxtime.antheaddist*300                                #Converting argument from minutes to frames
  }
  
  #get mean speed of all trajectories during the window (note, this is a simple version; a more complex version to come to check whether the track is active during window, not just speeds during window)
  vardist <- colVars(trajs.antheaddist[mintimef.antheaddist:maxtimef.antheaddist,,3], na.rm = TRUE)
  
  #outbound: further filters data based on whether the selected trajectories are outbound (1) or inbound (2)
  if(outbound.antheaddist == 1){
    vardist <- vardist[colSums(trajs.antheaddist[,,1], na.rm = TRUE) < 0]
  }else if(outbound.antheaddist == 2){
    vardist <- vardist[colSums(trajs.antheaddist[,,1], na.rm = TRUE) > 0]
  }
  
  #Assembles the main title of the plot based on input arguments
  plotitle <- paste0(titles.antheaddist,
   if(outbound.antheaddist==1){
     ' outbound'
   }else if(outbound.antheaddist==2){
     ' inbound'
   }else{
     ' inbound and outbound'
   },
   ' heading (', mintime.antheaddist, 'm to ', round(maxtimef.antheaddist/300),'m)',
   if(rm0.antheaddist==1){
     ', zeros removed'
   }else{
     ', zeros not removed'
   }
  )
  
  #Plotting function
  hist(vardist,
       main = plotitle, xlab = "Variance in heading")
}