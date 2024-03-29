##Antnumspeed
#A comparison of the mean trajectory speed with the number of ants in the frame

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antnumspeed <- function(dat = trajs, outbound = 0,
                       mintime, maxtime, frate = 5,
                       binno = 6, legloc = 2, keepscale = FALSE, 
                       titles, ...){
  Antnumspeed.f(dat, outbound, mintime, maxtime, frate, binno, legloc, keepscale, titles, ...)
}

Antnumspeed.f <- function(trajs.antnumspeed, outbound.antnumspeed,
                         mintime.antnumspeed, maxtime.antnumspeed, frate.antnumspeed,
                         binno.antnumspeed, legloc.antnumspeed, keepscale.antnumspeed,
                         titles.antnumspeed, ...){
  
  #Greatest number of trajectories in region during any frame (for purposes of coloring)
  antmax.antnumspeed <- max(rowSums(!is.na(trajs.antnumspeed[,,4])))             
  
  #mintime/maxtime: finding and converting
  minmaxf.antnumspeed <- Minmax(trajs.antnumspeed, mintime = mintime.antnumspeed, maxtime = maxtime.antnumspeed, frate = frate.antnumspeed)
  mintimef.antnumspeed <- minmaxf.antnumspeed[1]
  maxtimef.antnumspeed <- minmaxf.antnumspeed[2]
  
  #X-axis and graphcol found first from total data set
  eks <- rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))
  graphcol <- floor( (rowSums(!is.na(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4]))/antmax.antnumspeed*binno.antnumspeed)+1 )
  
  #Y-axis (means) found after set is filtered for inbound/outbound
  if(outbound.antnumspeed != 0){
    trajs.antnumspeed <- Outbound(trajs.antnumspeed, outbound.antnumspeed)
  }
  wye <- rowMeans(trajs.antnumspeed[mintimef.antnumspeed:maxtimef.antnumspeed,,4], na.rm = TRUE)
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antnumspeed)){
    plotitle <- paste0('# of ants vs. mean',
     if(outbound.antnumspeed==1){
       ' outbound'
     }else if(outbound.antnumspeed==2){
       ' inbound'
     }else{
       ' inbound and outbound'
     },
     ' trajectory speed (', round(mintimef.antnumspeed / (frate.antnumspeed*60)), 'm to ', round(maxtimef.antnumspeed/(frate.antnumspeed*60)),'m)'
    )
    
  }else{plotitle <- titles.antnumspeed}
  
  
  #Plotting function with KEEPSCALE option
  if(keepscale.antnumspeed == FALSE){
    plot(x = eks, y = wye,
         main = plotitle, xlab = '# of ants in region', ylab = ' mean speed (cm/s)',
         col = graphcol)
  }else{
    wyekeep <- rowMeans(trajs.antnumspeed[,,4], na.rm = TRUE)
    plot(x = eks, y = wye,
         main = plotitle, xlab = '# of ants in region', ylab = ' mean speed (cm/s)',
         xlim = c(0,antmax.antnumspeed), ylim = c(min(wyekeep, na.rm = TRUE), max(wyekeep, na.rm = TRUE)),
         col = graphcol)
  }
  
  #Adding visuals to graph
  abline(mean(trajs.antnumspeed[,,4], na.rm = TRUE), 0, lty = 5)  #Mean of mean speeds of ALL data (not just mintime-maxtime)
  abline(lm(wye~eks), lty = 3)                                    #Linear trend line (of only selected time)
  Legendno(type = 1, binno = binno.antnumspeed, legloc = legloc.antnumspeed, antmax = antmax.antnumspeed) #Legend
}

