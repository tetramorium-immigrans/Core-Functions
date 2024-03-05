##Antnumspeed
#A comparison of the mean trajectory speed with the number of ants in the frame

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#conversion is the factor for turning mintime/maxtime into frames (300 = 5 frames/s * 60 s/m by default in the Minmax subfunction)
#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

Antnumspeed2 <- function(dat = trajs, outbound = 0,
                        mintime = 0, maxtime, conversion = 300,
                        binno = 6, legloc = 2, keepscale = FALSE, 
                        titles, ...){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax <- max(rowSums(!is.na(trajs[,,4])))             
  
  #mintime/maxtime: finding and converting
  minmaxf <- Minmax(trajs, mintime = mintime, maxtime = maxtime, conversion = conversion)
  mintimef <- minmaxf[1]
  maxtimef <- minmaxf[2]
  
  #X-axis and graphcol found first from total data set
  eks <- rowSums(!is.na(trajs[mintimef:maxtimef,,4]))
  graphcol <- floor( (rowSums(!is.na(trajs[mintimef:maxtimef,,4]))/antmax*binno)+1 )
  
  #Y-axis (means) found after set is filtered for inbound/outbound
  trajs <- Outbound(trajs, outbound)
  wye <- rowMeans(trajs[mintimef:maxtimef,,4], na.rm = TRUE)
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles)){
    plotitle <- paste0('Relationship of total number of ants in region to mean',
     if(outbound==1){
       ' outbound'
     }else if(outbound==2){
       ' inbound'
     }else{
       ' inbound and outbound'
     },
     ' trajectory speed (', mintime, 'm to ', round(maxtimef/300),'m)'
    )
    
  }else{plotitle <- titles}
  
  
  #Plotting function with KEEPSCALE option
  if(keepscale == FALSE){
    plot(x = eks, y = wye,
         main = plotitle, xlab = '# of ants in region', ylab = ' mean speed (cm/s)',
         col = graphcol)
  }else{
    wyekeep <- rowMeans(trajs[,,4], na.rm = TRUE)
    plot(x = eks, y = wye,
         main = plotitle, xlab = '# of ants in region', ylab = ' mean speed (cm/s)',
         xlim = c(0,antmax), ylim = c(min(wyekeep, na.rm = TRUE), max(wyekeep, na.rm = TRUE)),
         col = graphcol)
  }
  
  #Adding visuals to graph
  abline(mean(trajs[,,4], na.rm = TRUE), 0, lty = 5)  #Mean of mean speeds of ALL data (not just mintime-maxtime)
  abline(lm(wye~eks), lty = 3)                                    #Linear trend line (of only selected time)
  Legendno(type = 1, binno = binno, legloc = legloc, antmax = antmax) #Legend
}

