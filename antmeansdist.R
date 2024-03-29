##Antmeansdist
#Produces a histogram of the mean speeds of the trajectories in a designated time frame

#dat is data; by default it is indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#mintime and maxtime restrict the time, specifically it cuts off any portions of trajectories outside the window


Antmeansdist <- function(dat = trajs, outbound = 0, rm0 = 1,
                        mintime = 0, maxtime,
                        keepscale = FALSE,
                        titles = "Distribution of mean", ...){
  
  #maxtime: ending frame found if not given as an argument, else converting input from minutes to frames
  #(for some reason can't get this to work in the .f function)
  if(missing(maxtime)){
    maxtime <- dim(dat)[1]
  }else{
    maxtime <- maxtime*300
  }
  
  Antmeansdist.f(dat, outbound, rm0, mintime, maxtime, keepscale, titles, ...)
}

Antmeansdist.f <- function(trajs.antmeansdist, outbound.antmeansdist, rm0.antmeansdist,
                         mintime.antmeansdist, maxtime.antmeansdist,
                         keepscale.antmeansdist,
                         titles.antmeansdist, ...){
 
  #maxtime: ending frame found if not given as an argument, else converting input from minutes to frames
  # if(missing(maxtime.antmeansdist)){
  #   maxtimef.antmeansdist <- dim(trajs.antmeansdist)[1]
  # }else{
  #   maxtimef.antmeansdist <- maxtime.antmeansdist*300                         
  # }
   
  #rm0: Removing trajectories with a median of 0
  if(rm0.antmeansdist == 1){
    trajsm <- colMedians(trajs.antmeansdist[,,4], na.rm = TRUE)
    trajs.antmeansdist <- trajs.antmeansdist[,!trajsm == 0,]
  }
  
  #For some reason I can't get the if(missing) above to work, so in order to avoid editing all the code with maxtime vs. maxtimef I convert it here
  maxtimef.antmeansdist <- maxtime.antmeansdist
  
  #mintime: converting input argument from minutes to frames, then setting min frame to 1 if it is 0 to avoid indexing problems below
  mintimef.antmeansdist <- mintime.antmeansdist*300
  if(mintimef.antmeansdist == 0){
    mintimef.antmeansdist <- 1
  }

  #Greatest number of trajectories in region during any frame (needed for barplot)
  #antmax.antmeansdist <- max(rowSums(!is.na(trajs.antmeansdist[,,1])))             
  
  #get mean speed of all trajectories during the window (note, this is a simple version; a more complex version to come to check whether the track is active during window, not just speeds during window)
  meandist <- colMeans(trajs.antmeansdist[mintimef.antmeansdist:maxtimef.antmeansdist,,4], na.rm = TRUE)
  
  #outbound: further filters data based on whether the selected trajectories are outbound (1) or inbound (2)
  if(outbound.antmeansdist == 1){
    meandist <- meandist[colSums(trajs.antmeansdist[,,1], na.rm = TRUE) < 0]
  }else if(outbound.antmeansdist == 2){
    meandist <- meandist[colSums(trajs.antmeansdist[,,1], na.rm = TRUE) > 0]
  }
  
  #Assembles the main title of the plot based on input arguments
  plotitle <- paste0(titles.antmeansdist,
    if(outbound.antmeansdist==1){
      ' outbound'
    }else if(outbound.antmeansdist==2){
      ' inbound'
    }else{
      ' inbound and outbound'
    },
      ' trajectory speed (', mintime.antmeansdist, 'm to ', round(maxtimef.antmeansdist/300),'m)',
    if(rm0.antmeansdist==1){
      ', zeros removed'
    }else{
      ', zeros not removed'
    }
  )
  
  
  
  #Histogram itself with KEEPSCALE condition (TRUE isn't working right)
  if(keepscale.antmeansdist == FALSE){
    hist(meandist,
       main = plotitle, xlab = "Mean speed (cm/s)")
  }else{
    bighist <- hist(meandist, plot = FALSE)
    
    hist(meandist,
         xlim = range(bighist$breaks), ylim = range(bighist$counts),
         main = plotitle, xlab = "Mean speed (cm/s)")
  }
  
}

Antmeansdist.mass <- function(dat = trajs, outbound = 0, rm0 = 1,
                             mintime = 0, maxtime, winsize = 10,
                             xdim, ydim,
                             binno = 6, legloc = 1, keepscale = TRUE){
  
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
    
    Antmeansdist(dat = dat, outbound = outbound, rm0 = rm0, 
                  mintime = i, maxtime = i + winsize,
                  keepscale = keepscale)
    
  }
  
  par(mfrow = c(1,1)) #return to default 1x1 graph
  
}