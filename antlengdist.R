##Antlengdist (CURRENTLY NOT WORKING WITH MASS FUNCTION)
#Produces a histogram of the path lengths/durations of the trajectories active during a given time frame

#dat is vector data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#datlocs is vector data; by default it is indexed as (time in frames) x (trajectory #) x (x coordinate/y coordinate/path length/distance_to_start)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only

#chron is whether to measure path length in terms of frames (time) rather than cm (distance)
#lrestrict (lower restrict) removes all trajectories with a length/duration below the given value
#urestrict (upper restrict) removes all trajectories with a length/duration greater than the given value

#mintime and maxtime restrict the time, specifically it cuts off any portions of trajectories outside the window
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#keepscale retains the graph's scale at that of the whole data set, even when restricted in time (currently not working)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antlengdist <- function(dat = trajs, datlocs = locs, outbound = 0, 
                         chron = FALSE, lrestrict, urestrict,
                         mintime, maxtime, frate = 5,
                         keepscale = FALSE, titles){
  
  Antlengdist.f(dat, datlocs, outbound, chron, lrestrict, urestrict, mintime, maxtime, frate, keepscale, titles)
}

Antlengdist.f <- function(trajs.antlengdist, locs.antlengdist, outbound.antlengdist, 
                           chron.antlengdist, lrestrict.antlengdist, urestrict.antlengdist,
                           mintime.antlengdist, maxtime.antlengdist, frate.antlengdist,
                           keepscale.antlengdist, titles.antlengdist){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes; UNUSED)
  #antmax.antlengdist <- max(rowSums(!is.na(trajs.antlengdist[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.antlengdist <- Minmax(trajs.antlengdist, mintime = mintime.antlengdist, maxtime = maxtime.antlengdist, frate = frate.antlengdist)
  mintimef.antlengdist <- minmaxf.antlengdist[1]
  maxtimef.antlengdist <- minmaxf.antlengdist[2]
  
  #Outbound: filtering (has to come before trajlengs calculations)
  if(outbound.antlengdist != 0){
    trajs.antlengdist <- Outbound(trajs.antlengdist, outbound.antlengdist)
    locs.antlengdist <- Outbound(locs.antlengdist, outbound.antlengdist)
  }
  
  #Calculate data, either the pathlength (FALSE) or else the number of frames (converted to seconds with frate) it existed
  activetrajs <- Active(locs.antlengdist, mintimef.antlengdist:maxtimef.antlengdist)    #Get active trajectories
  
  if(chron.antlengdist == FALSE){
    trajlengs <- colMaxs(locs.antlengdist[,activetrajs,3], na.rm = TRUE)                #calculate the active trajectories' lengths or...
  }else{
    trajlengs <- colSums(!is.na(locs.antlengdist[,activetrajs,3])) / frate.antlengdist  #...calculate the active trajectories' durations
  }
   
  #restrict: removes trajectories if the restrict argument was entered
  if(!missing(lrestrict.antlengdist)){
    trajlengs <- trajlengs[trajlengs > lrestrict.antlengdist]
  }
  
  if(!missing(urestrict.antlengdist)){
    trajlengs <- trajlengs[trajlengs < urestrict.antlengdist]
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antlengdist)){
    plotitle <- paste0('Distribution of',
     if(outbound.antlengdist==1){
       ' outbound'
     }else if(outbound.antlengdist==2){
       ' inbound'
     }else{
       ' inbound and outbound'
     },
     ' trajectories by',
     if(chron.antlengdist == FALSE){
       ' path length'
     }else{
       ' time length'
     },
     ' (', round(mintimef.antlengdist / (frate.antlengdist*60)), 'm to ', round(maxtimef.antlengdist/(frate.antlengdist*60)),'m)')
  }else{plotitle <- titles.antlengdist}
  
  
  #Plotting function with if/else for keepscale (TRUE NOT WORKING)
  if(keepscale.antlengdist == FALSE){
    hist(trajlengs,
         main = plotitle, xlab = if(chron.antlengdist == FALSE){"Lenth (cm)"}else{"Duration (s)"})
  }else{
    bighist <- hist(meandist, plot = FALSE)
    
    hist(meandist,
         xlim = range(bighist$breaks), ylim = range(bighist$counts),
         main = plotitle, xlab = "Mean speed (cm/s)")
  }
  
}


#see if I can get keepscale to work