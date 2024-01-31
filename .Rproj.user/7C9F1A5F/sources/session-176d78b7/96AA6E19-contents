##Antlengdist
#Produces a histogram of the path lengths of the trajectories in a designated time frame

#dat is vector data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#datlocs is location data; by default it is indexed as (time in frames) x (trajectory #) x (x/y coordinates)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#chron is whether to measure path length in terms of frames (time) rather than cm (distance)
#rm0 is whether to remove trajectories with a median speed of zero from the data (usually dead ants or inconsequential objects)
#mintime and maxtime restrict the time, specifically it cuts off any portions of trajectories outside the window
#restrict removes all trajectories with a length/duration longer than the given value
#keepscale retains the graph's scale at that of the whole data set, even when restricted in time (currently not working)


Antlengdist <- function(dat = trajs, datlocs = locs, 
                         outbound = 0, chron = FALSE, rm0 = 1,
                         mintime = 0, maxtime, restrict,
                         keepscale = FALSE,
                         titles = "Distribution of"){
  
  Antlengdist.f(dat, datlocs, outbound, chron, rm0, mintime, maxtime, restrict, keepscale, titles)
}

Antlengdist.f <- function(trajs.antlengdist, locs.antlengdist, 
                           outbound.antlengdist, chron.antlengdist, rm0.antlengdist,
                           mintime.antlengdist, maxtime.antlengdist, restrict.antlengdist,
                           keepscale.antlengdist,
                           titles.antlengdist){
  
  #rm0: Removing trajectories with a median speed of 0
  if(rm0.antlengdist == 1){
    trajsm <- colMedians(trajs.antlengdist[,,4], na.rm = TRUE)
    locs.antlengdist <- locs.antlengdist[,!trajsm == 0,]
  }
  
  #mintime: converting input argument from minutes to frames, then setting min frame to 1 if it is 0 to avoid indexing problems below
   mintimef.antlengdist <- mintime.antlengdist*300
   if(mintimef.antlengdist == 0){
      mintimef.antlengdist <- 1
    } 
  
  #maxtime: ending frame found if not given as an argument, else converting input from minutes to frames
   if(missing(maxtime.antlengdist)){
     maxtimef.antlengdist <- dim(locs.antlengdist)[1]
   }else{
     maxtimef.antlengdist <- maxtime.antlengdist*300                         
   }
   
  #Greatest number of trajectories in region during any frame (needed for barplot)
  #antmax.antlengdist <- max(rowSums(!is.na(trajs.antlengdist[,,1])))             
  
  #Calculate data, either the pathlength (FALSE) or else the number of frames (converted to seconds) it existed
  if(chron.antlengdist == FALSE){
    trajlengs <- colMaxs(locs.antlengdist[mintimef.antlengdist:maxtimef.antlengdist,,3], na.rm = TRUE)
  }else{
    trajlengs <- colSums(!is.na(locs.antlengdist[mintimef.antlengdist:maxtimef.antlengdist,,3])) / 5 #Five frames/sec
  }
   
  #restrict: removes data if the restrict argument was entered
  if(!missing(restrict.antlengdist)){
    trajlengs <- trajlengs[trajlengs < restrict.antlengdist]
  }
  
  #outbound: further filters data based on whether the selected trajectories are outbound (1) or inbound (2)
  if(outbound.antlengdist == 1){
    trajlengs <- trajlengs[colSums(trajs.antlengdist[,,1], na.rm = TRUE) < 0]
  }else if(outbound.antlengdist == 2){
    trajlengs <- trajlengs[colSums(trajs.antlengdist[,,1], na.rm = TRUE) > 0]
  }
  
  #Assembles the main title of the plot based on input arguments
  plotitle <- paste0(titles.antlengdist,
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
     ' (', mintime.antlengdist, 'm to ', round(maxtimef.antlengdist/300),'m)',
   if(rm0.antlengdist==1){
     ', zeros removed'
   }else{
     ', zeros not removed'
   }
  )

  
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

Antlengdist.mass <- function(dat = trajs, datlocs = locs,
                             outbound = 0, rm0 = 1,
                             mintime = 0, maxtime, restrict, winsize = 10,
                             xdim, ydim,
                             keepscale = FALSE){
  
  #Ending frame found if not given as an argument
  if(missing(maxtime)){
    maxtime <- floor(dim(dat)[1]/300)
  }
  
  #Getting how many graphs to be produced then converting it to a vector for the loop
  graphno <- floor((maxtime - mintime) / winsize)
  graphnoi <- seq(from = mintime, by = winsize, length.out = graphno)
  
  #Getting xdim and ydim if not supplied and setting up graph grid (need to refine so it does better than a square always)
  if(missing(xdim)){
    xdim <- ceiling(sqrt(graphno))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(graphno))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  #Loop that populates the graph grid
  for(i in graphnoi){
    #if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    
    Antlengdist(dat = dat, datlocs = datlocs,
                outbound = outbound, rm0 = rm0, 
                mintime = i, maxtime = i + winsize, restrict = restrict,
                keepscale = keepscale)
    
  }
  
  par(mfrow = c(1,1)) #return to default 1x1 graph
  
}