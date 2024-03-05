##Timeframe

Timeframe <- function(fun,
                      dat = trajs, datlocs = locs, outbound = 0,
                      xdim, ydim, framesize = 10, 
                      mintime = 0, maxtime, conversion = 300,
                      winspace = 20, winsize = 20,                              #Anthead, Antnetx, and Antpropx; for the spacing and averaging range (size) of the mean line
                      chron = FALSE, restrict,                                  #Antlengdist; for whether to have lengths measured in time and whether to restrict
                      prop = FALSE,                                             #Antno.; for whether y-axis is shown as a proportion of max
                      binno = 6, lspace = 10, legloc = 0, keepscale = TRUE){
  
  Timeframe.f(fun.tf = fun,
              dat.tf = dat, datlocs.tf = datlocs, outbound.tf = outbound,
              xdim.tf = xdim, ydim.tf = ydim, framesize.tf = framesize, 
              mintime.tf = mintime, maxtime.tf = maxtime, conversion.tf = conversion,
              winspace.tf = winspace, winsize.tf = winsize,                              
              chron.tf = chron, restrict.tf = restrict,                                  
              prop.tf = prop,                                             
              binno.tf = binno, lspace.tf = lspace, legloc.tf = legloc, keepscale.tf = keepscale)
}

Timeframe.f <- function(fun.tf,
                      dat.tf, datlocs.tf, outbound.tf,
                      xdim.tf, ydim.tf, framesize.tf, 
                      mintime.tf, maxtime.tf, conversion.tf,
                      winspace.tf, winsize.tf,                                  #Anthead, Antnetx, and Antpropx; for the spacing and averaging range (size) of the mean line
                      chron.tf, restrict.tf,                                    #Antlengdist; for whether to have lengths measured in time and whether to restrict
                      prop.tf,                                                  #Antno.; for whether y-axis is shown as a proportion of max
                      binno.tf, lspace.tf, legloc.tf, keepscale.tf){
  
  #Ending frame found if not given as an argument (preferably from trajs, but from locs if necessary)
  if(missing(maxtime.tf)){
    if(missing(dat.tf)){
      maxtime.tf <- Minmax(datlocs.tf)[2]
    }else{
      maxtime.tf <- Minmax(dat.tf)[2]
    }
  }
  
  #getting how many graphs to be produced then converting it into a vector for the loop
  graphno <- floor((maxtime.tf - mintime.tf) / framesize.tf)
  graphnoi <- seq(from = mintime.tf, by = framesize.tf, length.out = graphno)
  
  #Getting xdim and ydim if not supplied and setting up graph grid (need to refine so it does better than a square always)
  if(missing(xdim.tf)){
    xdim.tf <- ceiling(sqrt(graphno))
  }
  
  if(missing(ydim.tf)){
    ydim.tf <- ceiling(sqrt(graphno))
  }
  
  nf <- par()
  par(mfrow = c(ydim.tf,xdim.tf))
  
  # Antnumspeed(dat = dat.tf, outbound = outbound.tf,
  #             mintime = graphnoi, maxtime = graphnoi + framesize.tf,
  #             binno = binno.tf, legloc = legloc.tf, keepscale = keepscale.tf)
  
   #Loop that populates the graph grid
   # for(i in graphnoi){
   #   #if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
   #   
   #   Antnumspeed(dat = dat.tf, outbound = outbound.tf,
   #               mintime = graphnoi[i], maxtime = graphnoi[i] + framesize.tf,
   #               binno = binno.tf, legloc = legloc.tf, keepscale = keepscale.tf)
   #   
   # }
  
  for(i in graphno){
    if(i > (xdim * ydim)){break}                                          #stops loop if it goes beyond the designated grid size
    
    Antnumspeed(dat = dat.tf, outbound = outbound.tf,
                mintime = mintime.tf + (i * framesize.tf), 
                maxtime = mintime.tf + ((i + 1) * framesize.tf),
                binno = binno.tf, legloc = legloc.tf, keepscale = keepscale.tf)
    
  }
  
  par(mfrow = c(1,1)) #return to default 1x1 graph
}

