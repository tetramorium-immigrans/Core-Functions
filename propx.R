#Propx
#Find proportion of inbound/outbound ants

#dat is the data
#graph is whether to return a graph or just the calculations

#outbound is measuring which group:
#(1) Outbound
#(0) Inbound
#bytime changes how the data is analyzed in each frame:
#TRUE uses the instantaneous outbound of each ant at that moment
#FALSE uses the overall trajectory outbound of each ant that is active in that frame

#mintime and maxtime restrict the time being examined, entered in minutes (note mintimef and maxtimef within the function are in units of frames)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s); anywhere you see "frate*60" is converting frames to minutes or back

#winspace (window space) and winsize (window size) are how far apart to place the average points and the window within which the average is taken (default: 30s)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace is the "line spacing" for graphical purposes (default: 10m)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)
#titles is the option to manually enter a plot title; utilized by Mass.Function to pass file names for graph identification

propx <- function(dat = trajs, graph = TRUE,
                    outbound = 1, bytime = FALSE,
                    mintime, maxtime, frate = 5,
                    winspace = 30, winsize = 30,
                    binno = 6, lspace = 10, legloc = 2, keepscale = FALSE, titles, ...){
  propx.f(dat, graph, outbound, bytime, mintime, maxtime, frate, winspace, winsize, binno, lspace, legloc, keepscale, titles, ...)
}

propx.f <- function(dat.propx, graph.propx,
                      outbound.propx, bytime.propx,
                      mintime.propx, maxtime.propx, frate.propx,
                      winspace.propx, winsize.propx, 
                      binno.propx, lspace.propx, legloc.propx, keepscale.propx, titles.propx, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.propx <- max(colSums((dat.propx$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.propx <- Minmax(dat.propx, mintime = mintime.propx, maxtime = maxtime.propx, lspace = lspace.propx, frate = frate.propx)
  mintimef.propx <- minmaxf.propx[1]
  maxtimef.propx <- minmaxf.propx[2]
  lspacef.propx <- minmaxf.propx[3]
  
  #Converting other input arguments to frames
  winspacef.propx <- winspace.propx*frate.propx
  winsizef.propx <- round(winsize.propx * frate.propx / 2)     #Halved since the value will be used to extend to either side of a point
  
  ##Main calculating function
  graphcol <- floor( (colSums((dat.propx$x[,mintimef.propx:maxtimef.propx]) != 0)/antmax.propx*binno.propx)+1 ) #figuring out graph point coloration
  
  if(bytime.propx == TRUE){
    
    if(outbound.propx == 1){                                                    #keepsums calculated for potential use in keepscale based on overall data
      keepsums <- colSums(dat.propx$xvel < 0) / colSums(dat.propx$x != 0)
      #keepsums[is.nan(keepsums)] <- 0
      ylab.propx <- "Proportion of outbound ants"
    }else{
      keepsums <- colSums(dat.propx$xvel > 0) / colSums(dat.propx$x != 0)
      #keepsums[is.nan(keepsums)] <- 0
      ylab.propx <- "Proportion of inbound ants"
    }
    
    plotsums <- keepsums[mintimef.propx:maxtimef.propx]                         #Subset values to be graphed
    
  }else if(bytime.propx == FALSE){                          
    
    act <- apply(dat.propx$x, 2, FUN = function(z){which(z != 0)})              #Find which trajectories are active in each frame
    
    if(outbound.propx == 1){
      outie <- which((apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are outbound
      
      print('Calculating proportions')
      keepsums <- pbsapply(1:dim(dat.propx$x)[2], FUN = function(z){            
        length(intersect(act[[z]], outie)) / length(act[[z]])
      })
      
      ylab.propx <- "Proportion of outbound trajectories"
      yrang <- c(1,0)
      
    }else if(outbound.propx == 0){
      innie <- which((apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
      
      print('Calculating proportions')
      keepsums <- pbsapply(1:dim(dat.propx$x)[2], FUN = function(z){
        length(intersect(act[[z]], innie)) / length(act[[z]])
      })
      
      ylab.propx <- "Proportion of inbound trajectories"
      yrang <- c(0,1)
      
    }
    
    plotsums <- keepsums[mintimef.propx:maxtimef.propx]                         #Subset values to be graphed
    
  }
  
  ##Return data if not graphing
  if(graph.propx == FALSE){
    return(keepsums)
  }
  
  ##Graphing function
  
  #If not provided, assembles the main title of the plot based on input arguments
  if(missing(titles.propx)){
    plotitle <- paste0('Proportion of',
       if(outbound.propx==1){
         ' outbound'
       }else if(outbound.propx==2){
         ' inbound'
       },
       
       if(bytime.propx==TRUE){
         ' ants ('
       }else if(bytime.propx==FALSE){
         ' active trajectories ('
       }, round(mintimef.propx / (frate.propx*60)), 'm to ', round(maxtimef.propx/(frate.propx*60)),'m)'
    )
    
  }else{plotitle <- titles.propx}
  
  plot(x = mintimef.propx:maxtimef.propx, y = plotsums, 
       ylim = yrang,
       main = plotitle, xlab = "Frame Number", ylab = ylab.propx, 
       col = graphcol)
  
  #Values for time lines below:
  #dims[1:4] are size of the plot
  #dims[5] is calculating where to put the text just above the bottom of the frame
  #dims[6] is where the dotted line ends just above the text (currently unused)
  dims.propx <- c(par("usr"), par("usr")[3]+(par("usr")[4]-par("usr")[3])/60, par("usr")[3]+2*((par("usr")[4]-par("usr")[3])/60)) 
  
  #Adding visuals to graph
  # abline(h = 0, lty = 3)                                                        #Min value
  # abline(h = 1, lty = 3)                                                        #Max value
  abline(h = 0.5, lty = 1)                                                      #Half value
  
  abline(h = mean(keepsums, na.rm = TRUE), lty = 3)                             #mean line for whole data set (for reference even in keepscale)

  if(keepscale.propx == TRUE){
    abline(h = mean(plotsums, na.rm = TRUE), lty = 3, col = "orange")           #mean line for selected range if it isn't the same as the whole
  }
  
  linseq <- seq(from = 0, to = maxtimef.propx, by = lspacef.propx)              #Find where to put minute lines
  abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  text(linseq, dims.propx[5], labels = paste0(linseq / (frate.propx*60), "m"),  #Add labels to minute lines
       col = rgb(0,0,0, alpha = 0.45))
  
  dotseq <- seq(from = mintimef.propx, to = maxtimef.propx, by = winspacef.propx) #Find where to put the points
  dotseq <- dotseq[(dotseq - winsizef.propx) > 0 & (dotseq + winsizef.propx) < maxtimef.propx] #Remove points that would cause the function to out-of-bounds
  
  varseq <- sapply(dotseq, FUN = function(x){mean(keepsums[(x - winsizef.propx):(x + winsizef.propx)])}) #Get mean of mean proportions in range
  
  points(x = dotseq, y = varseq, pch = 18, col = "gray26")                      #Add average points
  lines(x = dotseq, y = varseq, col = "gray26")                                 #Add connection lines between average points
  
  Legendno(type = 1, binno = binno.propx, legloc = legloc.propx,                #Add legend
           antmax = antmax.propx) 
  
}


#Holding space for stats calculations
#sum(apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) / dim(dat.propx$x)[1]
