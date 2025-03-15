#Phases
#finding the phases in data

phases <- function(dat.antno, winspace = 1, winsize = 150, ...){
  phases.f(dat.antno, winspace, winsize, ...)
}

phases.f <- function(antno.phases, winspace.phases, winsize.phases, ...){
  #Write code that checks for whether antno.phases exists and runs antno if not
  
  
  ##Finding periodicity in the data
  
  #Find the slope at all points (default window of 1m)
  slope <- sapply((1 + winsize.phases):(length(antno.phases) - winsize.phases), FUN = function(z){
    (antno.phases[z + winsize.phases] - antno.phases[z - winsize.phases]) / (winsize.phases * 2)
  })
  
  #Smooth data and keep output length constant
  slope <- c(rep(0, winsize.phases), smooth.spline(slope)$y, rep(0, winsize.phases))
  
  #Find the maxima
  maxslope <- localMaxima(slope)
  
  #find periodicity
  slopefreq <- diff(maxslope)
  
  #hist(slopefreq)
  
  #Return list
  return(slopefreq)
  #return(list(slope = slope, maxima = maxslope, frequency = slopefreq))
  
  # plot(slope)
  
  
  #Adding visuals to graph
  # linseq <- seq(from = 0, to = maxtimef.antno, by = lspacef.antno)          #Find where to put minute lines
  # abline(v = linseq, lty = 3, lwd = 0.5, col = rgb(0,0,0, alpha = 0.25))        #Draw minute lines
  # text(linseq, dims.antno[5], labels = paste0(linseq / (frate.antno*60), "m"), #Add labels to minute lines
  #      col = rgb(0,0,0, alpha = 0.45))

}

#slopes function: if data not supplied run antno, get its output, then run my own calculations

#Got from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima?rq=4

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


