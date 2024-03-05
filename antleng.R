##Antleng
#Change in trajectory length with time.  Graphs a mean line as well.

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#active is whether to take the variance between all trajectory headings in a frame (TRUE) or the mean variance of all trajectories active during that frame (FALSE)

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace is the "line spacing" for graphical purposes (default: 10m)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antleng <- function(dat = trajs, datlocs = locs, outbound = 0, 
                       chron = FALSE, lrestrict, urestrict,
                       mintime, maxtime, frate = 5,
                       winspace = 30, winsize = 30,
                       binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, 
                       titles, ...){
  Antleng.f(dat, datlocs, outbound, chron, lrestrict, urestrict, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

Antleng.f <- function(trajs.antleng, locs.antleng, outbound.antleng, 
                         chron.antleng, lrestrict.antleng, urestrict.antleng,
                         mintime.antleng, maxtime.antleng, frate.antleng,
                         winspace.antleng, winsize.antleng,
                         binno.antleng, lspace.antleng, legloc.antleng, keepscale.antleng,
                         titles.antleng, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antleng <- max(rowSums(!is.na(trajs.antleng[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.antleng <- Minmax(trajs.antleng, mintime = mintime.antleng, maxtime = maxtime.antleng, lspace = lspace.antleng, frate = frate.antleng)
  mintimef.antleng <- minmaxf.antleng[1]
  maxtimef.antleng <- minmaxf.antleng[2]
  lspacef.antleng <- minmaxf.antleng[3]
  
  #Converting other input arguments to frames
  winspacef.antleng <- winspace.antleng*frate.antleng
  winsizef.antleng <- round(winsize.antleng * frate.antleng / 2)     #Halved since the value will be used to extend to either side of a point
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antleng)){
    plotitle <- paste0(if(chron.antleng==FALSE){'Length of'}else{'Duration of'},
       if(outbound.antleng==1){
         ' outbound'
       }else if(outbound.antleng==2){
         ' inbound'
       }else{
         ' inbound and outbound'
       },
       ' ant trajectories (', round(mintimef.antleng / (frate.antleng*60)), 'm to ', round(maxtimef.antleng/(frate.antleng*60)),'m)'
    )
    
  }else{plotitle <- titles.antleng}
  
  
  #Main plotting function (Outbound must follow graphcol)
  graphcol <- floor( (rowSums(!is.na(trajs.antleng[mintimef.antleng:maxtimef.antleng,,1]))/antmax.antleng*binno.antleng)+1 ) #determining overall graph color (must come before filtering)
  if(outbound.antleng != 0){
    trajs.antleng <- Outbound(trajs.antleng, outbound.antleng)
    locs.antleng <- Outbound(trajs.antleng, outbound.antleng, locs = locs.antleng, returnlocs = TRUE)
  }
  
  if(chron.antleng == TRUE){
    csums <- colSums(!is.na(trajs.antleng[,,4])) / frate.antleng
    
    keepvars <- pbsapply(1:dim(trajs.antleng)[1], FUN = function(x){            #Find the mean of the trajectory durations for total time (keepscale again)
      activetrajs <- Active(trajs.antleng, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        mean(csums[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    
    plotvars <- keepvars[mintimef.antleng:maxtimef.antleng]                     #Subset those to be graphed
    
    ylab.antleng <- "Mean duration of active trajectories (seconds)"
    
  }else if(chron.antleng == FALSE){
    csums <- apply(locs.antleng[,,3], 2, max, na.rm = TRUE)
    
    keepvars <- pbsapply(1:dim(trajs.antleng)[1], FUN = function(x){            #Find the mean of the trajectory lengths for total time (keepscale again)
      activetrajs <- Active(trajs.antleng, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        mean(csums[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    
    plotvars <- keepvars[mintimef.antleng:maxtimef.antleng]                     #Subset those to be graphed
    
    ylab.antleng <- "Mean length of active trajectories (cm)"
  }
  
  plot(x = mintimef.antleng:maxtimef.antleng, y = plotvars, 
       ylim = if(keepscale.antleng == TRUE){c(min(keepvars, na.rm = TRUE),max(keepvars, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = "Frame Number", ylab = ylab.antleng, 
       col = graphcol)
  
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.antleng <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  abline(h = 0, lty = 3)                                                        #x-axis
  abline(h = mean(plotvars, na.rm = TRUE), lty = 3)                             #mean line for whole data set (for reference even in keepscale)
  #if(mintimef.antleng != 1 | maxtimef.antleng != dim(trajs.antleng)[1]){
  if(keepscale.antleng == TRUE){
    abline(h = mean(plotvars, na.rm = TRUE), lty = 3, col = "orange")           #mean line for selected range if it isn't the same as the whole
  }
  
  
  linseq <- seq(from = 0, to = maxtimef.antleng, by = lspacef.antleng)          #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.antleng[5], labels = paste0(linseq / (frate.antleng*60), "m"), #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  dotseq <- seq(from = mintimef.antleng, to = maxtimef.antleng, by = winspacef.antleng) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.antleng) > 0 & (dotseq + winsizef.antleng) < maxtimef.antleng] #Remove points that would cause the function to out-of-bounds
  varseq <- sapply(dotseq, FUN = function(x){mean(keepvars[(x - winsizef.antleng):(x + winsizef.antleng)], na.rm = TRUE)}) #Get mean of mean in range
  
  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.antleng, legloc = legloc.antleng,            #Add legend
           antmax = antmax.antleng) 
  
  
}