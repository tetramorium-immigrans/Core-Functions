#Antpropx
#Net direction of x motion of ants

#dat is data

#bytime determines how the output is calculated:
#(TRUE): calculates inbound/outbound based on instantaneous heading
#(FALSE): Calculates inbound/outbound based on overall movement of the trajectory
#Threshold is the minimum difference between the x-coordinates of the start and end to count as an outbound or inbound trajectory for bytime=FALSE

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace is the "line spacing" for graphical purposes (default: 10m)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

antpropx <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                    bytime = TRUE, threshold = 0,
                    mintime, maxtime, frate = 5,
                    winspace = 30, winsize = 30,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  
  antpropx.f(dat, graph, datreturn, 
             bytime, threshold,
             mintime, maxtime, frate, 
             winspace, winsize, 
             binno, lspace, legloc, keepscale, titles, ...)
}

antpropx.f <- function(dat.antpropx, graph.antpropx, datreturn.antpropx,
                      bytime.antpropx, threshold.antpropx,
                      mintime.antpropx, maxtime.antpropx, frate.antpropx,
                      winspace.antpropx, winsize.antpropx, 
                      binno.antpropx, lspace.antpropx, legloc.antpropx, keepscale.antpropx, titles.antpropx, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antpropx <- max(colSums((dat.antpropx$x) > 0, na.rm = TRUE))
  
  #mintime/maxtime: finding and converting
  minmaxf.antpropx <- Minmax(dat.antpropx, mintime = mintime.antpropx, maxtime = maxtime.antpropx, lspace = lspace.antpropx, frate = frate.antpropx)
  mintimef.antpropx <- minmaxf.antpropx[1]
  maxtimef.antpropx <- minmaxf.antpropx[2]
  lspacef.antpropx <- minmaxf.antpropx[3]
  
  #Converting other input arguments to frames
  winspacef.antpropx <- winspace.antpropx*frate.antpropx
  winsizef.antpropx <- round(winsize.antpropx * frate.antpropx / 2)     #Halved since the value will be used to extend to either side of a point
  
  
  ##Main calculating function (graphcol must come first)
  graphcol <- floor( (colSums((dat.antpropx$x[,mintimef.antpropx:maxtimef.antpropx]) != 0, na.rm = TRUE)/antmax.antpropx*binno.antpropx)+1 ) #determining overall graph color (must come before filtering)
  
  activetrajs <- apply(dat.antpropx$x, 2, function(z){which(z != 0)})              #Identifying active trajectories in each frame
  
  if(bytime.antpropx == TRUE){
    filterdat <- dat.antpropx$xvel
    
    # keepdat <- pbsapply(1:length(activetrajs), function(z){
    #   outbound <- sum(filterdat[activetrajs[[z]],z] < 0) / length(activetrajs[[z]])
    #   inbound <- sum(filterdat[activetrajs[[z]],z] > 0) / length(activetrajs[[z]])
    #   (outbound - inbound)
    # })
    
    keepdat <- pbapply(filterdat, 2, function(z){
      outbound <- sum(z < 0, na.rm = TRUE)
      inbound <- sum(z > 0, na.rm = TRUE)
      (inbound - outbound)
    }) / unlist(lapply(activetrajs, length))
    
    plotdat <- keepdat[mintimef.antpropx:maxtimef.antpropx]                     #Subset those to be graphed
    ylab.antpropx <- "Bytime difference of outbound/inbound ants"
    
  }else if(bytime.antpropx == FALSE){
    filterdat <- dat.antpropx$x
    
    trajdirs <- apply(filterdat, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]})
    outbound <- which((trajdirs + threshold.antpropx) < 0)
    inbound <- which((trajdirs - threshold.antpropx) > 0)
    
    # keepdat <- pblapply(activetrajs, function(z){
    #   (sum(z == outbound) - sum(z == inbound))
    # }) / unlist(lapply(activetrajs, length))
    
    keepdat <- pbsapply(1:length(activetrajs), function(z){
      sum(activetrajs[[z]] == inbound) - sum(activetrajs[[z]] == outbound)
    }) / unlist(lapply(activetrajs, length))
    
    plotdat <- keepdat[mintimef.antpropx:maxtimef.antpropx]                     #Subset those to be graphed
    ylab.antpropx <- "Bytrajectory difference of outbound/inbound ants"
    
  }
  
  
  #Output data if requested
  
  if(graph.antpropx == FALSE & datreturn.antpropx == TRUE){
    return(keepdat)
  }
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antpropx)){
    plotitle <- paste0(
      if(bytime.antpropx==TRUE){
        ' By-time difference of outbound/inbound trajectories ('
      }else if(bytime.antpropx==FALSE){
        ' By-trajectory difference of outbound/inbound trajectories ('
      }, round(mintimef.antpropx / (frate.antpropx*60)), 'm to ', round(maxtimef.antpropx/(frate.antpropx*60)),'m)'
    )
    
  }else{plotitle <- titles.antpropx}
  
  #Plotting
  
  antplot(mintimef.antplot = mintimef.antpropx, maxtimef.antplot = maxtimef.antpropx, plotdat.antplot = plotdat, keepdat.antplot = keepdat,
          #ylim.antplot = ylim.antpropx,
          main.antplot = plotitle, ylab.antplot = ylab.antpropx,
          antmax.antplot = antmax.antpropx, col.antplot = graphcol,
          frate.antplot = frate.antpropx, lspacef.antplot = lspacef.antpropx, winspacef.antplot = winspacef.antpropx, winsizef.antplot = winsizef.antpropx,
          binno.antplot = binno.antpropx, legloc.antplot = legloc.antpropx, keepscale.antplot = keepscale.antpropx)
  
  if(datreturn.antpropx == TRUE){
    return(keepdat)
  }
  
}