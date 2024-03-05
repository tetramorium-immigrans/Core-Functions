##Timeslice
#Takes a single data file and produces multiple graphs over sequential windows of time

#dat.ts and datlocs.ts are data, indexed as (time in frames) x (trajectory #) x (4)
#fun is the function being used to produce the graphs
#xdim and ydim are the dimensions of the graph grid (i.e. xdim = 3, ydim = 4 will produce 12 graphs in a 3x4 grid)
#t.start and t.end are the start and end times (in minutes) of the sequence of slices
#t.slice is the size of each slice (in minutes, 10m by default)
#frate (frame rate) is used for converting from minutes to frames (default: 5 frames/s * 60 s/m)
#... allows for any function-specific arguments to be passed

Timeslice <- function(dat.ts = trajs, datlocs.ts = locs,
                      fun, xdim, ydim,
                      t.start = 0, t.end, t.slice = 10, frate.ts = 5, ...){
  
  #Ending frame found if not given as an argument (preferably from trajs, but from locs if necessary)
  if(missing(t.end)){
    if(missing(dat.ts)){
      t.end <- round(Minmax(datlocs.ts)[2])/(frate.ts*60)
    }else{
      t.end <- (Minmax(dat.ts)[2])/(frate.ts*60)
    }
  }
  
  #getting how many graphs to be produced then converting it to a vector for the loop
  graphno <- floor((t.end - t.start) / t.slice)
  graphnoi <- seq(from = t.start, by = t.slice, length.out = graphno)
  
  #Getting xdim and ydim if not supplied and setting up graph grid (need to refine so it does better than a square always)
  if(missing(xdim)){
    xdim <- ceiling(sqrt(graphno))
  }
  
  if(missing(ydim)){
    ydim <- ceiling(sqrt(graphno))
  }
  
  nf <- par()
  par(mfrow = c(ydim,xdim))
  
  #graphing fn
  
  for(i in 1:graphno){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    
    fun(dat = dat.ts, datlocs = datlocs.ts,
        mintime = graphnoi[i], 
        maxtime = graphnoi[i] + t.slice, 
        frate = frate.ts, ...)
    
  }
  
  par(mfrow = c(1,1)) #return to default 1x1 graph
  
}