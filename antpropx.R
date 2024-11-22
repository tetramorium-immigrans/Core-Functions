#Antpropx
#Net direction of x motion of ants

#dat is data, indexed as (time in frames) x (trajectory #) x (x vec/y vec/heading/magnitude of motion)
#outbound is how the analysis is restricted; 0 = all trajectories, 1 = outbound only, 2 = inbound only
#bytime is whether to take:
#(1) the sum of all instantaneous x-velocities in a frame; this is where the ants are going at that instant
#(2) the sum of the total x-velocity of each trajectory active in that frame; this is where ants are going over time
#(3) the mean of the total x-velocity of each trajectory active in that frame; this is where the ants are going around that time

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace is the "line spacing" for graphical purposes (default: 10m)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

Antpropx <- function(dat = trajs, outbound = 1, bytime = FALSE,
                    mintime, maxtime, frate = 5,
                    winspace = 30, winsize = 30,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  Antpropx.f(dat, outbound, bytime, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

Antpropx.f <- function(trajs.antpropx, outbound.antpropx, bytime.antpropx,
                      mintime.antpropx, maxtime.antpropx, frate.antpropx,
                      winspace.antpropx, winsize.antpropx, 
                      binno.antpropx, lspace.antpropx, legloc.antpropx, keepscale.antpropx, titles.antpropx, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antpropx <- max(rowSums(!is.na(trajs.antpropx[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.antpropx <- Minmax(trajs.antpropx, mintime = mintime.antpropx, maxtime = maxtime.antpropx, lspace = lspace.antpropx, frate = frate.antpropx)
  mintimef.antpropx <- minmaxf.antpropx[1]
  maxtimef.antpropx <- minmaxf.antpropx[2]
  lspacef.antpropx <- minmaxf.antpropx[3]
  
  #Converting other input arguments to frames
  winspacef.antpropx <- winspace.antpropx*frate.antpropx
  winsizef.antpropx <- round(winsize.antpropx * frate.antpropx / 2)     #Halved since the value will be used to extend to either side of a point
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antpropx)){
    plotitle <- paste0('Proportion of',
       if(outbound.antpropx==1){
         ' outbound'
       }else if(outbound.antpropx==2){
         ' inbound'
       },
       
       if(bytime.antpropx==TRUE){
         ' ants ('
       }else if(bytime.antpropx==FALSE){
         ' active trajectories ('
       }, round(mintimef.antpropx / (frate.antpropx*60)), 'm to ', round(maxtimef.antpropx/(frate.antpropx*60)),'m)'
    )
    
  }else{plotitle <- titles.antpropx}
  
  #Main plotting function (Outbound must follow graphcol)
  graphcol <- floor( (rowSums(!is.na(trajs.antpropx[mintimef.antpropx:maxtimef.antpropx,,1]))/antmax.antpropx*binno.antpropx)+1 ) #determining overall graph color (must come before filtering)
  #if(outbound.antpropx != 0){trajs.antpropx <- Outbound(trajs.antpropx, outbound.antpropx)}
  
  if(bytime.antpropx == TRUE){
    rprops <- rowSums(!is.na(Outbound(trajs.antpropx, outbound.antpropx)))/rowSums(!is.na(trajs.antpropx[,,1]))
    
    rsums <- rowSums(trajs.antpropx[,,1], na.rm = TRUE)
    keepsums <- rsums[mintimef.antpropx:maxtimef.antpropx]                        #Subset those to be graphed
    plotsums <- keepsums                                                        #Redundant in bytime==1, but used for average dots below
    ylab.antpropx <- "Sum of the x-component of velocities (cm/sec)"
    
  }else if(bytime.antpropx == FALSE){
    csums <- colSums(trajs.antpropx[,,1], na.rm = TRUE)                          #Find all the column (trajectory) sums (for purposes of keepscale)
    keepsums <- pbsapply(1:dim(trajs.antpropx)[1], FUN = function(x){            #Find the sum of the trajectory variances for total time (keepscale again)
      activetrajs <- Active(trajs.antpropx, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        sum(csums[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    plotsums <- keepsums[mintimef.antpropx:maxtimef.antpropx]*abs(keepsums[mintimef.antpropx:maxtimef.antpropx] )    #Experiment for how squaring values looked
    ylab.antpropx <- "Squared sum of the total x-velocity of each trajectory (cm/s)"
    
  }
  
  plot(x = mintimef.antpropx:maxtimef.antpropx, y = plotsums, 
       ylim = if(keepscale.antpropx == TRUE){c(min(keepsums, na.rm = TRUE),max(keepsums, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = "Frame Number", ylab = ylab.antpropx, 
       col = graphcol)
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.antpropx <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  abline(h = 0, lty = 3)                                                        #x-axis
  abline(h = mean(keepsums, na.rm = TRUE), lty = 3)                             #mean line for whole data set (for reference even in keepscale)
  #if(mintimef.antpropx != 1 | maxtimef.antpropx != dim(trajs.antpropx)[1]){
  if(keepscale.antpropx == TRUE){
    abline(h = mean(plotsums, na.rm = TRUE), lty = 3, col = "orange")           #mean line for selected range if it isn't the same as the whole
  }
  
  linseq <- seq(from = 0, to = maxtimef.antpropx, by = lspacef.antpropx)          #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.antpropx[5], labels = paste0(linseq / (frate.antpropx*60), "m"), #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  dotseq <- seq(from = mintimef.antpropx, to = maxtimef.antpropx, by = winspacef.antpropx) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.antpropx) > 0 & (dotseq + winsizef.antpropx) < maxtimef.antpropx] #Remove points that would cause the function to out-of-bounds
  varseq <- sapply(dotseq, FUN = function(x){mean(keepsums[(x - winsizef.antpropx):(x + winsizef.antpropx)], na.rm = TRUE)}) #Get mean of mean x-vecs in range
  
  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.antpropx, legloc = legloc.antpropx,            #Add legend
           antmax = antmax.antpropx) 
  
}