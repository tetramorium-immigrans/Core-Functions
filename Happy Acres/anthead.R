##ANDHEAD
#Change in heading with time.  Graphs a mean line as well.

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#bytime is whether to take the variance between all trajectory headings in a frame (TRUE) or the mean variance of all trajectories active during that frame (FALSE)

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace is the "line spacing" for graphical purposes (default: 10m)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Anthead <- function(dat = trajs, outbound = 0, bytime = FALSE,
                    mintime, maxtime, frate = 5,
                    winspace = 30, winsize = 30,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  Anthead.f(dat, outbound, bytime, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

Anthead.f <- function(trajs.anthead, outbound.anthead, bytime.anthead,
                      mintime.anthead, maxtime.anthead, frate.anthead,
                      winspace.anthead, winsize.anthead, 
                      binno.anthead, lspace.anthead, legloc.anthead, keepscale.anthead, titles.anthead, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.anthead <- max(rowSums(!is.na(trajs.anthead[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.anthead <- Minmax(trajs.anthead, mintime = mintime.anthead, maxtime = maxtime.anthead, lspace = lspace.anthead, frate = frate.anthead)
  mintimef.anthead <- minmaxf.anthead[1]
  maxtimef.anthead <- minmaxf.anthead[2]
  lspacef.anthead <- minmaxf.anthead[3]
  
  #Converting other input arguments to frames
  winspacef.anthead <- winspace.anthead*frate.anthead
  winsizef.anthead <- round(winsize.anthead * frate.anthead / 2)     #Halved since the value will be used to extend to either side of a point
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.anthead)){
    plotitle <- paste0('Mean angle of',
     if(outbound.anthead==1){
       ' outbound'
     }else if(outbound.anthead==2){
       ' inbound'
     }else{
       ' inbound and outbound'
     },
     ' ant heading (', round(mintimef.anthead / (frate.anthead*60)), 'm to ', round(maxtimef.anthead/(frate.anthead*60)),'m)'
    )
    
  }else{plotitle <- titles.anthead}
  
  
  #Main plotting function (Outbound must follow graphcol)
  graphcol <- floor( (rowSums(!is.na(trajs.anthead[mintimef.anthead:maxtimef.anthead,,1]))/antmax.anthead*binno.anthead)+1 ) #determining overall graph color (must come before filtering)
  if(outbound.anthead != 0){trajs.anthead <- Outbound(trajs.anthead, outbound.anthead)}
  
  if(bytime.anthead == TRUE){
    
    rvars <- apply(trajs.anthead[,,3], 1, mean.circular, na.rm = TRUE)          #calculate mean angle
    
    keepvars <- rvars[mintimef.anthead:maxtimef.anthead]                        #Subset those to be graphed
    plotvars <- keepvars                                                        #Redundant in bytime==TRUE, but used for average dots below
    ylab.anthead <- "Mean angle of ant headings (radians)"
    
  }else if(bytime.anthead == FALSE){
    print("Calculating angles and average points")
    
    cvars <- apply(trajs.anthead[,,3], 2, mean.circular, na.rm = TRUE)          #Calculate mean angles
    
    keepvars <- pbsapply(1:dim(trajs.anthead)[1], FUN = function(x){            #Find the mean of the angles for total time (keepscale again)
      activetrajs <- Active(trajs.anthead, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        mean.circular(cvars[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    
    plotvars <- keepvars[mintimef.anthead:maxtimef.anthead]                     #Subset those to be graphed

    ylab.anthead <- "Mean angle of trajectory headings (radians)"
  }
  
  plot(x = mintimef.anthead:maxtimef.anthead, y = plotvars, 
       ylim = if(keepscale.anthead == TRUE){c(-pi, pi)}else{NULL},
       main = plotitle, xlab = "Frame Number", ylab = ylab.anthead, 
       col = graphcol)
  

  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.anthead <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  abline(h = 0, lty = 3)                                                        #x-axis
  abline(h = mean(plotvars, na.rm = TRUE), lty = 3)                             #mean line for whole data set (for reference even in keepscale)
  #if(mintimef.anthead != 1 | maxtimef.anthead != dim(trajs.anthead)[1]){
  if(keepscale.anthead == TRUE){
    abline(h = mean(plotvars, na.rm = TRUE), lty = 3, col = "orange")           #mean line for selected range if it isn't the same as the whole
  }
  
  
  linseq <- seq(from = 0, to = maxtimef.anthead, by = lspacef.anthead)          #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.anthead[5], labels = paste0(linseq / (frate.anthead*60), "m"), #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  dotseq <- seq(from = mintimef.anthead, to = maxtimef.anthead, by = winspacef.anthead) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.anthead) > 0 & (dotseq + winsizef.anthead) < maxtimef.anthead] #Remove points that would cause the function to out-of-bounds
  varseq <- pbsapply(dotseq, FUN = function(x){mean(keepvars[(x - winsizef.anthead):(x + winsizef.anthead)], na.rm = TRUE)}) #Get mean of mean variances in range

  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.anthead, legloc = legloc.anthead,            #Add legend
           antmax = antmax.anthead) 

  
}