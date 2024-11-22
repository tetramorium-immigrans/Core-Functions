#Antnetx
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

Antnetx <- function(dat = trajs, outbound = 0, bytime = 2,
                    mintime, maxtime, frate = 5,
                    winspace = 30, winsize = 30,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  Antnetx.f(dat, outbound, bytime, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

Antnetx.f <- function(trajs.antnetx, outbound.antnetx, bytime.antnetx,
                      mintime.antnetx, maxtime.antnetx, frate.antnetx,
                      winspace.antnetx, winsize.antnetx, 
                      binno.antnetx, lspace.antnetx, legloc.antnetx, keepscale.antnetx, titles.antnetx, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antnetx <- max(rowSums(!is.na(trajs.antnetx[,,1])))
  
  #mintime/maxtime: finding and converting
  minmaxf.antnetx <- Minmax(trajs.antnetx, mintime = mintime.antnetx, maxtime = maxtime.antnetx, lspace = lspace.antnetx, frate = frate.antnetx)
  mintimef.antnetx <- minmaxf.antnetx[1]
  maxtimef.antnetx <- minmaxf.antnetx[2]
  lspacef.antnetx <- minmaxf.antnetx[3]
  
  #Converting other input arguments to frames
  winspacef.antnetx <- winspace.antnetx*frate.antnetx
  winsizef.antnetx <- round(winsize.antnetx * frate.antnetx / 2)     #Halved since the value will be used to extend to either side of a point
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.antnetx)){
    plotitle <- paste0(if(bytime.antnetx == 3){'Mean'}else{'Summed'}, ' x-movement of',
       if(outbound.antnetx==1){
         ' outbound'
       }else if(outbound.antnetx==2){
         ' inbound'
       }else{
         ' inbound and outbound'
       },
       
       if(bytime.antnetx==1){
         ' ants ('
       }else if(bytime.antnetx==2){
         ' active trajectories ('
       }else{
         ' active trajectories ('
       }, round(mintimef.antnetx / (frate.antnetx*60)), 'm to ', round(maxtimef.antnetx/(frate.antnetx*60)),'m)'
    )
    
  }else{plotitle <- titles.antnetx}
  
  #Main plotting function (Outbound must follow graphcol)
  graphcol <- floor( (rowSums(!is.na(trajs.antnetx[mintimef.antnetx:maxtimef.antnetx,,1]))/antmax.antnetx*binno.antnetx)+1 ) #determining overall graph color (must come before filtering)
  if(outbound.antnetx != 0){trajs.antnetx <- Outbound(trajs.antnetx, outbound.antnetx)}
  
  if(bytime.antnetx == 1){
    rsums <- rowSums(trajs.antnetx[,,1], na.rm = TRUE)
    keepsums <- rsums[mintimef.antnetx:maxtimef.antnetx]                        #Subset those to be graphed
    plotsums <- keepsums                                                        #Redundant in bytime==1, but used for average dots below
    ylab.antnetx <- "Sum of the x-component of velocities (cm/sec)"
    
  }else if(bytime.antnetx == 2){
    csums <- colSums(trajs.antnetx[,,1], na.rm = TRUE)                          #Find all the column (trajectory) sums (for purposes of keepscale)
    #csums <- colSums(trajs.antnetx[,,1]*abs(trajs.antnetx[,,1]), na.rm = TRUE)                          #Find all the column (trajectory) sums (for purposes of keepscale)
    
    keepsums <- pbsapply(1:dim(trajs.antnetx)[1], FUN = function(x){            #Find the sum of the trajectory variances for total time (keepscale again)
      activetrajs <- Active(trajs.antnetx, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        sum(csums[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    #plotsums <- keepsums[mintimef.antnetx:maxtimef.antnetx]                     #Subset those to be graphed
    plotsums <- keepsums[mintimef.antnetx:maxtimef.antnetx]*abs(keepsums[mintimef.antnetx:maxtimef.antnetx] )    #Experiment for how squaring values looked
    ylab.antnetx <- "Squared sum of the total x-velocity of each trajectory (cm/s)"
    
  }else if(bytime.antnetx == 3){
    csums <- colSums(trajs.antnetx[,,1], na.rm = TRUE)                          #Find all the column (trajectory) sums (for purposes of keepscale)
    keepsums <- pbsapply(1:dim(trajs.antnetx)[1], FUN = function(x){            #Find the mean of the trajectory variances for total time (keepscale again)
      activetrajs <- Active(trajs.antnetx, x)
      if(length(activetrajs) > 0){                                              #If there are no trajectories active at a time return 0, otherwise mean
        mean(csums[activetrajs], na.rm = TRUE)
      }else{return(NA)}
    })
    plotsums <- keepsums[mintimef.antnetx:maxtimef.antnetx]                     #Subset those to be graphed
    
    ylab.antnetx <- "Mean of the total x-velocity of each trajectory (cm/s)"
  }
  
  plot(x = mintimef.antnetx:maxtimef.antnetx, y = plotsums, 
       ylim = if(keepscale.antnetx == TRUE){c(min(keepsums, na.rm = TRUE),max(keepsums, na.rm = TRUE))}else{NULL},
       main = plotitle, xlab = "Frame Number", ylab = ylab.antnetx, 
       col = graphcol)
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.antnetx <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  abline(h = 0, lty = 3)                                                        #x-axis
  abline(h = mean(keepsums, na.rm = TRUE), lty = 3)                             #mean line for whole data set (for reference even in keepscale)
  #if(mintimef.antnetx != 1 | maxtimef.antnetx != dim(trajs.antnetx)[1]){
  if(keepscale.antnetx == TRUE){
    abline(h = mean(plotsums, na.rm = TRUE), lty = 3, col = "orange")           #mean line for selected range if it isn't the same as the whole
  }
  
  arrows(x0 = rep(dims.antnetx[1]+(par("usr")[2]-par("usr")[1])/60, 2), y0 = c((par("usr")[4]-par("usr")[3])/60, (par("usr")[3]-par("usr")[4])/60), y1 = c((par("usr")[4]-par("usr")[3])/12, (par("usr")[3]-par("usr")[4])/12), length = 0.2)
  text(x = rep(dims.antnetx[1]+(par("usr")[2]-par("usr")[1])/60, 2), y = c((par("usr")[4]-par("usr")[3])/9, (par("usr")[3]-par("usr")[4])/9), labels = c("Nest", "Bait"), srt = 90)
  
  linseq <- seq(from = 0, to = maxtimef.antnetx, by = lspacef.antnetx)          #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.antnetx[5], labels = paste0(linseq / (frate.antnetx*60), "m"), #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  dotseq <- seq(from = mintimef.antnetx, to = maxtimef.antnetx, by = winspacef.antnetx) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.antnetx) > 0 & (dotseq + winsizef.antnetx) < maxtimef.antnetx] #Remove points that would cause the function to out-of-bounds
  varseq <- sapply(dotseq, FUN = function(x){mean(keepsums[(x - winsizef.antnetx):(x + winsizef.antnetx)]*abs(keepsums[(x - winsizef.antnetx):(x + winsizef.antnetx)]), na.rm = TRUE)}) #Get mean of mean x-vecs in range
  
  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.antnetx, legloc = legloc.antnetx,            #Add legend
           antmax = antmax.antnetx) 
  
}