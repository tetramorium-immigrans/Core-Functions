##Antphase
#Calculates rate of change of ant numbers and returns the distance between peaks

#dat is data
#graph is whether to return a graph or just the calculations
#datreturn is whether to return the data

#outbound is how the analysis is restricted
#(0) - "false" or inbound only
#(1) - "true" or outbound only
#(2) - "both"
#prop is whether to calculate the values as a fraction of the maximum rather than counts
#peaks is whether to look at maxima or minima
#fn is function, or whether to return all the values or just the variance ("vari")

#mintime and maxtime restrict time being examined (entered in minutes)
#frate = "frame rate", or frames/s

#winspace is how far apart to calculate the slope points (default every frame)
#winsize is the size of window around the points to use to calculate the average (default 1m)

#binno (bin number) is the number of colors used to code for relative numbers on the trail
#lspace (line space) is a graphical argument for how far apart the vertical time lines are spaced on the graph (in minutes)
#legloc (legend location) takes values 1-4, starting in the upper left and going clockwise
#keepscale is whether the axes should be scaled to the data in the restricted time (FALSE) or the data as a whole (TRUE)

#titles is used by mass.function to title the graphs, otherwise the title self-assembles

antphase <- function(dat = trajs, graph = TRUE, datreturn = FALSE,
                  outbound = 2, prop = FALSE, peaks = "maxima", fn = "none",
                  mintime, maxtime, frate = 5,
                  winspace = 1, winsize = 300,
                  binno = 6, lspace = 10, legloc = 2, keepscale = FALSE,
                  titles, ...){
  
  antphase.f(dat, graph, datreturn, 
             outbound, prop, peaks, fn,
             mintime, maxtime, frate, 
             winspace, winsize,
             binno, lspace, legloc, keepscale, 
             titles, ...)
}

antphase.f <- function(dat.antphase, graph.antphase, datreturn.antphase,
                    outbound.antphase, prop.antphase, peaks.antphase, fn.antphase,
                    mintime.antphase, maxtime.antphase, frate.antphase,
                    winspace.antphase, winsize.antphase,
                    binno.antphase, lspace.antphase, legloc.antphase, keepscale.antphase,
                    titles.antphase, ...){
  
  #Greatest number of trajectories in region during any frame (for coloring purposes)
  antmax.antphase <- max(colSums((dat.antphase$x) > 0))
  
  #mintime/maxtime: finding and converting
  minmaxf.antphase <- Minmax(dat.antphase, mintime = mintime.antphase, maxtime = maxtime.antphase, lspace = lspace.antphase, frate = frate.antphase)
  mintimef.antphase <- minmaxf.antphase[1]
  maxtimef.antphase <- minmaxf.antphase[2]
  lspacef.antphase <- minmaxf.antphase[3]
  
  graphcol <- floor( (colSums((dat.antphase$x[,mintimef.antphase:maxtimef.antphase]) != 0)/antmax.antphase*binno.antphase)+1 ) #figuring out graph point coloration
  
  if(outbound.antphase == 0){                                                      #keepsums calculated for potential use in keepscale based on overall data
    innie <- which((apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) > 0) == TRUE) #Find which trajectories are inbound
    keepdat <- colSums(dat.antphase$x[innie,] > 0)
    plotdat <- keepdat[mintimef.antphase:maxtimef.antphase]
    ylab.antphase <- "inbound ants in region"
    
  }else if(outbound.antphase == 1){
    outie <- which((apply(dat.propx$x, 1, function(z){z[max(which(z != 0))] - z[min(which(z != 0))]}) < 0) == TRUE) #Find which trajectories are outbound
    keepdat <- colSums(dat.antphase$x[outie,] > 0)
    plotdat <- keepdat[mintimef.antphase:maxtimef.antphase]
    ylab.antphase <- "outbound ants in region"
    
  }else{
    keepdat <- colSums(dat.antphase$x > 0)
    plotdat <- keepdat[mintimef.antphase:maxtimef.antphase]
    ylab.antphase <- "ants in region"
  }
  
  if(prop.antphase == TRUE){
    plotdat <- plotdat / antmax.antphase
  }
  
  
  ##Finding periodicity in the data
  
  winsize.antphase <- round(winsize.antphase / 2)                               #Converting to a half-size for purposes of window calculation 
  
  #Find the slope at all points (default window of 1m)
  slope <- sapply((1 + winsize.antphase):(length(keepdat) - winsize.antphase), FUN = function(z){
    (keepdat[z + winsize.antphase] - keepdat[z - winsize.antphase]) / (winsize.antphase * 2)
  })
  
  #Smooth data and keep output length constant by adding 0's onto either end
  slope <- c(rep(0, winsize.antphase), smooth.spline(slope)$y, rep(0, winsize.antphase))
  
  #Find the maxima or minima and their periodicity
  
  if(peaks.antphase == "maxima"){
    slopefreq <- diff(localMaxima(slope))
  }else if(peaks.antphase == "minima"){
    slopefreq <- diff(localMinima(slope))
  }else{
    print("Invalid peak parameter")
    break
  }
  
  if(fn.antphase == "vari"){
    slopefreq <- var(slopefreq)
  }
  
  
  ##Return data if not graphing
  if(graph.antphase == FALSE & datreturn.antphase == TRUE){
    return(slopefreq)
  }
  
  
  ##Plotting function
  
  #Assembles the main title of the plot based on input arguments
  if(missing(titles.antphase)){
    plotitle <- paste0('Rate of change of number of',
       if(outbound.antphase==1){
         ' outbound'
       }else if(outbound.antphase==0){
         ' inbound'
       },
       ' ants in region (', round(mintimef.antphase / (frate.antphase*60)), 'm to ', round(maxtimef.antphase/(frate.antphase*60)),'m)'
    )
  }else{plotitle <- titles.antphase}
  
  #Find y limits if not supplied
  if(missing(ylim.antphase)){
    if(keepscale.antphase == TRUE){
      ylim.antphase <- c(min(keepdat.antphase, na.rm = TRUE),max(keepdat.antphase, na.rm = TRUE))
    }else{
      ylim.antphase <- NULL
    }
  }
  
  antplot(mintimef.antplot = mintimef.antphase, maxtimef.antplot = maxtimef.antphase, plotdat.antplot = plotdat, keepdat.antplot = keepdat,
          ylim.antplot = ylim.antphase,
          main.antplot = plotitle, ylab.antplot = ylab.antphase,
          antmax.antplot = antmax.antphase, col.antplot = graphcol,
          frate.antplot = frate.antphase, lspacef.antplot = lspacef.antphase, winspacef.antplot = winspacef.antphase, winsizef.antplot = winsizef.antphase,
          binno.antplot = binno.antphase, legloc.antplot = legloc.antphase, keepscale.antplot = keepscale.antphase)
  
}


#Necessary sub-function for antphase to run
#Got from https://stackoverflow.com/a/6836924

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
