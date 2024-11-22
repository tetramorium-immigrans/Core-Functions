##ANDHEADVAR
#Change in heading variance with time.  Graphs a mean line as well.

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

Antheadvar <- function(dat = trajs, outbound = 0, bytime = FALSE,
                    mintime, maxtime, frate = 5,
                    winspace = 30, winsize = 30,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  Antheadvar.f(dat, outbound, bytime, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

Antheadvar.f <- function(trajs.antheadvar, outbound.antheadvar, bytime.antheadvar,
                      mintime.antheadvar, maxtime.antheadvar, frate.antheadvar,
                      winspace.antheadvar, winsize.antheadvar, 
                      binno.antheadvar, lspace.antheadvar, legloc.antheadvar, keepscale.antheadvar, titles.antheadvar, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antheadvar <- max(rowSums(!is.na(trajs.antheadvar[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.antheadvar <- Minmax(trajs.antheadvar, mintime = mintime.antheadvar, maxtime = maxtime.antheadvar, lspace = lspace.antheadvar, frate = frate.antheadvar)
  mintimef.antheadvar <- minmaxf.antheadvar[1]
  maxtimef.antheadvar <- minmaxf.antheadvar[2]
  lspacef.antheadvar <- minmaxf.antheadvar[3]
  
  #Converting other input arguments to frames
  winspacef.antheadvar <- winspace.antheadvar*frate.antheadvar
  winsizef.antheadvar <- round(winsize.antheadvar * frate.antheadvar / 2)     #Halved since the value will be used to extend to either side of a point
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antheadvar)){
    plotitle <- paste0('Circular variance of',
     if(outbound.antheadvar==1){
       ' outbound'
     }else if(outbound.antheadvar==2){
       ' inbound'
     }else{
       ' inbound and outbound'
     },
     ' ant heading (', round(mintimef.antheadvar / (frate.antheadvar*60)), 'm to ', round(maxtimef.antheadvar/(frate.antheadvar*60)),'m)'
    )
    
  }else{plotitle <- titles.antheadvar}
  
  
  #Main plotting function (Outbound must follow graphcol)
  graphcol <- floor( (rowSums(!is.na(trajs.antheadvar[mintimef.antheadvar:maxtimef.antheadvar,,1]))/antmax.antheadvar*binno.antheadvar)+1 ) #determining overall graph color (must come before filtering)
  if(outbound.antheadvar != 0){trajs.antheadvar <- Outbound(trajs.antheadvar, outbound.antheadvar)}
  
  if(bytime.antheadvar == TRUE){
    
    #rvars <- rowVars(trajs.antheadvar[,,3], na.rm = TRUE)                         #Find all the row variances (for purposes of keepscale) NAIVE VARIANCE
    #rvars <- circVars(trajs.antheadvar, bytime.antheadvar)                           #CIRCVARS VERSION
    rvars <- (apply(trajs.antheadvar[,,3], 1, angular.variance, na.rm = TRUE))/2       #CIRCULAR VERSION (divide by 2 because a.v returns value as *2)
    
    keepvars <- rvars[mintimef.antheadvar:maxtimef.antheadvar]                        #Subset those to be graphed
    plotvars <- keepvars                                                        #Redundant in bytime==TRUE, but used for average dots below
    ylab.antheadvar <- "Circular variance of ant headings"
    
  }else if(bytime.antheadvar == FALSE){
    print("Calculating mean variances and average points")
    #cvars <- colVars(trajs.antheadvar[,,3], na.rm = TRUE)                         #Find all the column (trajectory) variances (for purposes of keepscale) NAIVE VARIANCE
    #cvars <- circVars(trajs.antheadvar, bytime.antheadvar)                           #CIRCVARS VERSION
    cvars <- (apply(trajs.antheadvar[,,3], 2, angular.variance, na.rm = TRUE))/2       #CIRCULAR VERSION (divide by 2 because a.v returns value as *2)
    
    keepvars <- pbsapply(1:dim(trajs.antheadvar)[1], FUN = function(x){            #Find the mean of the trajectory variances for total time (keepscale again)
      activetrajs <- Active(trajs.antheadvar, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        mean(cvars[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    
    plotvars <- keepvars[mintimef.antheadvar:maxtimef.antheadvar]                     #Subset those to be graphed

    ylab.antheadvar <- "Mean circular variance of trajectory headings"
  }
  
  plot(x = mintimef.antheadvar:maxtimef.antheadvar, y = plotvars, 
       ylim = if(keepscale.antheadvar == TRUE){c(min(keepvars, na.rm = TRUE),max(keepvars, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = "Frame Number", ylab = ylab.antheadvar, 
       col = graphcol)
  

  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.antheadvar <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  abline(h = 0, lty = 3)                                                        #x-axis
  abline(h = mean(plotvars, na.rm = TRUE), lty = 3)                             #mean line for whole data set (for reference even in keepscale)
  #if(mintimef.antheadvar != 1 | maxtimef.antheadvar != dim(trajs.antheadvar)[1]){
  if(keepscale.antheadvar == TRUE){
    abline(h = mean(plotvars, na.rm = TRUE), lty = 3, col = "orange")           #mean line for selected range if it isn't the same as the whole
  }
  
  
  linseq <- seq(from = 0, to = maxtimef.antheadvar, by = lspacef.antheadvar)          #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.antheadvar[5], labels = paste0(linseq / (frate.antheadvar*60), "m"), #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  dotseq <- seq(from = mintimef.antheadvar, to = maxtimef.antheadvar, by = winspacef.antheadvar) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.antheadvar) > 0 & (dotseq + winsizef.antheadvar) < maxtimef.antheadvar] #Remove points that would cause the function to out-of-bounds
  varseq <- pbsapply(dotseq, FUN = function(x){mean(keepvars[(x - winsizef.antheadvar):(x + winsizef.antheadvar)], na.rm = TRUE)}) #Get mean of mean variances in range

  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.antheadvar, legloc = legloc.antheadvar,            #Add legend
           antmax = antmax.antheadvar) 

  
}